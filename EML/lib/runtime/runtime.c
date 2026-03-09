#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef void *eml_value;

static int64_t tag_int_val(int64_t n) { return (n << 1) | 1; }

#define TO_ML_INTEGER(tagged_n) ((int64_t)(tagged_n) >> 1)

void print_int(int64_t tagged_n) {
  printf("%ld\n", (long)TO_ML_INTEGER(tagged_n));
}

#define TAG_TUPLE   0
#define TAG_CLOSURE 1
#define TAG_LAST    2
#define HEADER_WORDS 1

#define SIZE_HEAP_DEFAULT 800
#define MAX_STACK_SCAN_SLOTS (128 * 1024)

#define IS_INT(v) ((v) & 0x1)
#define IS_PTR(v) ((v) != 0 && !IS_INT(v))

typedef struct {
  uint8_t  tag;
  uint8_t  _pad1;
  uint16_t size;
  uint32_t _pad2;
} box_header_t;

static inline box_header_t *get_header(uint64_t *payload) {
  return (box_header_t *)((uint64_t *)payload - 1);
}
static inline uint64_t *get_payload(box_header_t *hdr) {
  return (uint64_t *)(hdr + 1);
}

static const size_t TAG_SCAN_START[] = {
  [TAG_TUPLE]   = 1,
  [TAG_CLOSURE] = 3,
};

typedef struct {
  uint64_t *start[2];
  uint64_t *end[2];
  uint64_t *alloc_ptr;
  int       current_bank;
  uint64_t  allocations;
  uint64_t  collections;
  uint64_t  words_allocated_total;
} gc_state;

static gc_state GC;
static uint64_t *PTR_STACK = NULL;
static uint64_t *STACK_SCAN_LOW = NULL;
static uint64_t *STACK_SCAN_HIGH = NULL;
static bool gc_enabled = false;
static size_t size_heap = SIZE_HEAP_DEFAULT;

static inline int get_current_bank_idx(void) { return GC.current_bank; }
static inline int get_another_bank_idx(void) { return GC.current_bank ^ 1; }
static inline bool in_bank(uint64_t *ptr, int bank_idx) {
  return (GC.start[bank_idx] <= ptr) && (ptr < GC.end[bank_idx]);
}

#if defined(ENABLE_GC)

static void mark_and_copy(uint64_t *stack_slot);

static uint64_t *forward_or_copy(uint64_t *old_payload, bool *did_copy) {
  int from_bank = get_another_bank_idx();
  if (old_payload <= (uint64_t *)GC.start[from_bank] + HEADER_WORDS - 1) {
    *did_copy = false;
    return old_payload;
  }
  box_header_t *old_header = get_header(old_payload);
  if (old_header->tag >= TAG_LAST || old_header->size == 0 ||
      old_header->size > size_heap) {
    *did_copy = false;
    return old_payload;
  }
  uint64_t possible_forward_ptr = *((uint64_t *)old_payload - 1);
  if (in_bank((uint64_t *)possible_forward_ptr, get_current_bank_idx())) {
    *did_copy = false;
    return (uint64_t *)possible_forward_ptr;
  }
  *did_copy = true;
  uint16_t payload_words = old_header->size;
  uint8_t  object_tag    = old_header->tag;
  if (GC.alloc_ptr + payload_words + HEADER_WORDS > GC.end[GC.current_bank]) {
    *did_copy = false;
    return old_payload;
  }
  box_header_t *new_header = (box_header_t *)GC.alloc_ptr;
  new_header->tag  = object_tag;
  new_header->size = payload_words;
  uint64_t *new_payload = get_payload(new_header);
  memcpy(new_payload, old_payload, payload_words * sizeof(uint64_t));
  GC.alloc_ptr += payload_words + HEADER_WORDS;
  GC.words_allocated_total += payload_words + HEADER_WORDS;
  *((uint64_t *)old_payload - 1) = (uint64_t)new_payload;
  return new_payload;
}

static void scan_object(uint64_t *obj) {
  box_header_t *header = get_header(obj);
  size_t start = (header->tag < TAG_LAST) ? TAG_SCAN_START[header->tag] : 0;
  for (size_t i = start; i < header->size; i++)
    mark_and_copy(obj + i);
}

static void mark_and_copy(uint64_t *stack_slot) {
  uint64_t raw_value = *stack_slot;
  if (!IS_PTR(raw_value)) return;
  uint64_t *old_object_payload = (uint64_t *)raw_value;
  int another_bank = get_another_bank_idx();
  if (!in_bank(old_object_payload, another_bank)) {
    if (STACK_SCAN_LOW && STACK_SCAN_HIGH) {
      uint64_t *low  = STACK_SCAN_LOW < STACK_SCAN_HIGH ? STACK_SCAN_LOW : STACK_SCAN_HIGH;
      uint64_t *high = STACK_SCAN_LOW < STACK_SCAN_HIGH ? STACK_SCAN_HIGH : STACK_SCAN_LOW;
      if (old_object_payload >= low && old_object_payload <= high)
        return;
    }
    return;
  }
  if (old_object_payload < (uint64_t *)GC.start[another_bank] + HEADER_WORDS)
    return;
  bool object_was_copied_now;
  uint64_t *new_object_payload =
    forward_or_copy(old_object_payload, &object_was_copied_now);
  *stack_slot = (uint64_t)new_object_payload;
  if (object_was_copied_now) scan_object(new_object_payload);
}

static void allocate_banks(void) {
  for (int i = 0; i < 2; i++) {
    GC.start[i] = (uint64_t *)malloc(size_heap * sizeof(uint64_t));
    if (GC.start[i] == NULL) {
      fprintf(stderr, "Failed to allocate GC bank\n");
      abort();
    }
    GC.end[i] = GC.start[i] + size_heap;
  }
}

eml_value collect(void) {
  uint64_t dummy;
  uint64_t *current_stack_top = &dummy;
  if (!PTR_STACK || current_stack_top > PTR_STACK)
    return (eml_value)(uintptr_t)tag_int_val(0);
  STACK_SCAN_LOW = current_stack_top;
  STACK_SCAN_HIGH = PTR_STACK;
  GC.current_bank ^= 1;
  GC.alloc_ptr = GC.start[GC.current_bank];
  {
    uint64_t *stack_slot = current_stack_top;
    size_t n = 0;
    for (; n < MAX_STACK_SCAN_SLOTS && stack_slot <= PTR_STACK; n++, stack_slot++)
      mark_and_copy(stack_slot);
  }
  STACK_SCAN_LOW = NULL;
  STACK_SCAN_HIGH = NULL;
  GC.collections++;
  return (eml_value)(uintptr_t)tag_int_val(0);
}

uint64_t *gc_alloc(size_t words, uint64_t tag) {
  size_t total_words = words + HEADER_WORDS;
  if (GC.alloc_ptr + total_words > GC.end[GC.current_bank]) {
    collect();
    if (GC.alloc_ptr + total_words > GC.end[GC.current_bank]) {
      fprintf(stderr, "Out of memory\n");
      abort();
    }
  }
  box_header_t *header = (box_header_t *)GC.alloc_ptr;
  header->tag  = (uint8_t)tag;
  header->size = (uint16_t)words;
  uint64_t *obj = get_payload(header);
  memset(obj, 0, words * sizeof(uint64_t));
  GC.alloc_ptr += total_words;
  GC.allocations++;
  GC.words_allocated_total += total_words;
  return obj;
}

void init_gc(void) {
  if (gc_enabled) return;
  gc_enabled = true;
  size_heap = SIZE_HEAP_DEFAULT;
  {
    const char *heap_size_env = getenv("EML_HEAP_SIZE");
    if (heap_size_env) {
      int heap_size_val = atoi(heap_size_env);
      if (heap_size_val >= 400 && heap_size_val <= 1024 * 1024)
        size_heap = (size_t)heap_size_val;
    }
  }
  allocate_banks();
  GC.current_bank = 0;
  GC.alloc_ptr = GC.start[0];
  GC.allocations = 0;
  GC.collections = 0;
  GC.words_allocated_total = 0;
}

void destroy_gc(void) {
  if (!gc_enabled) return;
  for (int i = 0; i < 2; i++) {
    free(GC.start[i]);
    GC.start[i] = NULL;
    GC.end[i] = NULL;
  }
  GC.alloc_ptr = NULL;
  PTR_STACK = NULL;
  gc_enabled = false;
}

void set_ptr_stack(uint64_t *stack_bottom) { PTR_STACK = stack_bottom; }

eml_value print_gc_status(void) {
  int bank = GC.current_bank;
  ptrdiff_t current_alloc = GC.alloc_ptr - GC.start[bank];
  ptrdiff_t free_space = GC.end[bank] - GC.alloc_ptr;
  printf("=== GC Status ===\n");
  printf("Current  allocated: %td\n", current_alloc);
  printf("Total    allocated: %" PRIu64 "\n", GC.words_allocated_total);
  printf("Free         space: %td\n", free_space);
  printf("Heap          size: %zu\n", size_heap);
  printf("Current bank index: %d\n", bank);
  printf("GC     collections: %" PRIu64 "\n", GC.collections);
  printf("GC     allocations: %" PRIu64 "\n", GC.allocations);
  printf("=================\n");
  fflush(stdout);
  return (eml_value)(uintptr_t)tag_int_val(0);
}

static void *eml_alloc(size_t bytes, uint64_t tag) {
  if (gc_enabled) {
    size_t words = (bytes + sizeof(uint64_t) - 1) / sizeof(uint64_t);
    return gc_alloc(words, tag);
  }
  (void)tag;
  return malloc(bytes);
}

int64_t get_heap_start(void) { return tag_int_val(0); }
int64_t get_heap_final(void) { return tag_int_val((int64_t)size_heap); }

#else /* !ENABLE_GC */

void init_gc(void) {}
void destroy_gc(void) {}
void set_ptr_stack(uint64_t *stack_bottom) { (void)stack_bottom; }

eml_value collect(void) {
  return (eml_value)(uintptr_t)tag_int_val(0);
}

eml_value print_gc_status(void) {
  (void)printf("GC disabled\n");
  return (eml_value)(uintptr_t)tag_int_val(0);
}

static void *eml_alloc(size_t bytes, uint64_t tag) {
  (void)tag;
  return malloc(bytes);
}

int64_t get_heap_start(void) { return tag_int_val(0); }
int64_t get_heap_final(void) { return tag_int_val(0); }

#endif /* ENABLE_GC */

typedef struct {
  void *code;
  int64_t arity;
  int64_t received;
  void *args[];
} closure;

#if defined(EML_LLVM)

extern void *llvm_call_indirect(void *fn, void **args, int64_t n);

static void *call_closure_full(closure *c, void **args) {
  return llvm_call_indirect(c->code, args, c->arity);
}

#else /* EML_RISCV */

#define RISCV_REG_ARGS 8

static void *call_closure_full(closure *c, void **args) {
  int64_t arity = c->arity;
  int64_t args_in_stack = (arity > RISCV_REG_ARGS) ? (arity - RISCV_REG_ARGS) : 0;
  size_t storage_for_stack_args = (size_t)args_in_stack * sizeof(void *);
  void **stack_args = (args_in_stack > 0) ? args + RISCV_REG_ARGS : NULL;
  void *result;

  asm volatile(
      "mv   t0, %[storage_for_stack_args]\n"
      "sub  sp, sp, t0\n"
      "beqz %[args_in_stack], .Lend_stack_push\n"
      "mv   t1, sp\n"
      "mv   t2, %[stack_args]\n"
      "mv   t3, %[args_in_stack]\n"
      "li   t4, 0\n"
      ".Lloop_stack_push:\n"
      "beq  t4, t3, .Lend_stack_push\n"
      "slli t5, t4, 3\n"
      "add  t6, t2, t5\n"
      "ld   t0, 0(t6)\n"
      "sd   t0, 0(t1)\n"
      "addi t1, t1, 8\n"
      "addi t4, t4, 1\n"
      "j    .Lloop_stack_push\n"
      ".Lend_stack_push:\n"
      "mv   a0, %[a0]\n"
      "mv   a1, %[a1]\n"
      "mv   a2, %[a2]\n"
      "mv   a3, %[a3]\n"
      "mv   a4, %[a4]\n"
      "mv   a5, %[a5]\n"
      "mv   a6, %[a6]\n"
      "mv   a7, %[a7]\n"
      "mv   t6, %[fn]\n"
      "jalr ra, t6, 0\n"
      "mv   t0, %[storage_for_stack_args]\n"
      "add  sp, sp, t0\n"
      "mv   %[result], a0\n"
      : [result] "=r"(result)
      : [fn] "r"(c->code),
        [a0] "r"(args[0]), [a1] "r"(args[1]), [a2] "r"(args[2]), [a3] "r"(args[3]),
        [a4] "r"(args[4]), [a5] "r"(args[5]), [a6] "r"(args[6]), [a7] "r"(args[7]),
        [stack_args] "r"(stack_args), [args_in_stack] "r"(args_in_stack),
        [storage_for_stack_args] "r"(storage_for_stack_args)
      : "t0", "t1", "t2", "t3", "t4", "t5", "t6",
        "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "memory");

  return result;
}

#endif /* EML_LLVM / EML_RISCV */

closure *alloc_closure(void *code, int64_t arity) {
  size_t slots = (arity > 0) ? (size_t)arity : 1;
  size_t sz = sizeof(closure) + slots * sizeof(void *);
  closure *c = (closure *)eml_alloc(sz, TAG_CLOSURE);
  c->code = code;
  c->arity = arity;
  c->received = 0;
  memset(c->args, 0, slots * sizeof(void *));
  return c;
}

static closure *copy_closure(const closure *src) {
  size_t slots = (src->arity > 0) ? (size_t)src->arity : 1;
  size_t sz = sizeof(closure) + slots * sizeof(void *);
  closure *dst = (closure *)eml_alloc(sz, TAG_CLOSURE);
  memcpy(dst, src, sz);
  return dst;
}

void *eml_applyN(closure *c, int64_t argc, void **argv) {
  int64_t all = c->received + argc;
  if (all == c->arity) {
    void **all_args = (void **)eml_alloc((size_t)c->arity * sizeof(void *), TAG_CLOSURE);
    for (int64_t i = 0; i < c->received; i++) all_args[i] = c->args[i];
    for (int64_t i = 0; i < argc; i++) all_args[c->received + i] = argv[i];
    void *result = call_closure_full(c, all_args);
#if !defined(ENABLE_GC)
    free(all_args);
#endif
    return result;
  }
  closure *partial = copy_closure(c);
  for (int64_t i = 0; i < argc; i++)
    partial->args[partial->received++] = argv[i];
  return partial;
}

typedef struct {
  int64_t arity;
  void *args[];
} tuple;

tuple *create_tuple(int64_t argc, void **args) {
  size_t words = 1 + (size_t)argc;
  tuple *t = (tuple *)eml_alloc(words * sizeof(uint64_t), TAG_TUPLE);
  t->arity = argc;
  for (size_t i = 0; i < (size_t)argc; i++) t->args[i] = args[i];
  return t;
}

void *field(tuple *t, long n) { return t->args[n >> 1]; }

#if defined(EML_LLVM) && !defined(EML_LLVM_STANDALONE)
/* When linking with RTS that provides main (e.g. custom runner), call eml_main. */
extern void eml_main(void);

int main(void) {
  eml_main();
  return 0;
}
#endif /* EML_LLVM && !EML_LLVM_STANDALONE */

