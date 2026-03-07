#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TO_ML_INTEGER(n) ((uint64_t)((uint64_t)(n) >> 1))

void print_int(long n) { printf("%ld", TO_ML_INTEGER(n)); }

#define TAG_TUPLE 246
#define TAG_CLOSURE 247
#define RISCV_REG_ARGS 8
#define SIZE_HEAP 800
#define HEADER_WORDS 1

typedef struct {
  uint64_t raw;
} box_header_t;

static inline uint16_t header_tag(const box_header_t *h) { return (uint16_t)(h->raw & 0xFFFFu); }
static inline uint16_t header_size(const box_header_t *h) { return (uint16_t)((h->raw >> 16) & 0xFFFFu); }
static inline void set_header(box_header_t *h, uint16_t tag, uint16_t size) {
  h->raw = ((uint64_t)size << 16) | (uint64_t)tag;
}

static inline box_header_t *get_header(uint64_t *payload) {
  return (box_header_t *)((uint64_t *)payload - 1);
}

static inline uint64_t *get_payload(box_header_t *hdr) { return (uint64_t *)(hdr + 1); }

#define IS_INT(v) ((v)&0x1)
#define IS_PTR(v) ((v) != 0 && !IS_INT(v))

typedef struct {
  uint64_t *start[2];
  uint64_t *end[2];
  uint64_t *alloc_ptr;
  int current_bank;
  uint64_t allocations;
  uint64_t collections;
  uint64_t words_allocated_total;
} gc_state;

static gc_state GC;
static uint64_t *PTR_STACK = NULL;
static bool gc_enabled = false;

static inline int get_current_bank_idx() { return GC.current_bank; }
static inline int get_another_bank_idx() { return GC.current_bank ^ 1; }
static inline bool in_bank(uint64_t *ptr, int bank_idx) {
  return (GC.start[bank_idx] <= ptr) && (ptr < GC.end[bank_idx]);
}

static size_t scan_start_for_tag(uint16_t tag) {
  if (tag == TAG_TUPLE) {
    return 1;
  }
  if (tag == TAG_CLOSURE) {
    return 3;
  }
  return 0;
}

void init_gc(void) {
  if (gc_enabled) {
    return;
  }
  for (int i = 0; i < 2; ++i) {
    GC.start[i] = (uint64_t *)malloc(SIZE_HEAP * sizeof(uint64_t));
    if (GC.start[i] == NULL) {
      fprintf(stderr, "Failed to allocate GC bank\n");
      abort();
    }
    GC.end[i] = GC.start[i] + SIZE_HEAP;
  }
  GC.current_bank = 0;
  GC.alloc_ptr = GC.start[0];
  GC.allocations = 0;
  GC.collections = 0;
  GC.words_allocated_total = 0;
  gc_enabled = true;
}

void destroy_gc(void) {
  if (!gc_enabled) {
    return;
  }
  for (int i = 0; i < 2; ++i) {
    free(GC.start[i]);
    GC.start[i] = NULL;
    GC.end[i] = NULL;
  }
  GC.alloc_ptr = NULL;
  PTR_STACK = NULL;
  gc_enabled = false;
}

void set_ptr_stack(uint64_t *ptr) { PTR_STACK = ptr; }

static void mark_and_copy(uint64_t *stack_slot);

static uint64_t *forward_or_copy(uint64_t *old_payload, bool *did_copy) {
  uint64_t possible_forward_ptr = *((uint64_t *)old_payload - 1);
  if (in_bank((uint64_t *)possible_forward_ptr, get_current_bank_idx())) {
    *did_copy = false;
    return (uint64_t *)possible_forward_ptr;
  }

  box_header_t *old_header = get_header(old_payload);
  uint16_t payload_words = header_size(old_header);
  uint16_t object_tag = header_tag(old_header);
  size_t need_words = (size_t)payload_words + HEADER_WORDS;

  if (GC.alloc_ptr + need_words > GC.end[GC.current_bank]) {
    fprintf(stderr, "Out of memory during GC copy\n");
    abort();
  }

  box_header_t *new_header = (box_header_t *)GC.alloc_ptr;
  set_header(new_header, object_tag, payload_words);
  uint64_t *new_payload = get_payload(new_header);
  memcpy(new_payload, old_payload, (size_t)payload_words * sizeof(uint64_t));

  GC.alloc_ptr += need_words;
  GC.words_allocated_total += need_words;
  *((uint64_t *)old_payload - 1) = (uint64_t)new_payload;
  *did_copy = true;
  return new_payload;
}

static void scan_object(uint64_t *obj) {
  box_header_t *header = get_header(obj);
  size_t start = scan_start_for_tag(header_tag(header));
  size_t payload_words = (size_t)header_size(header);
  for (size_t i = start; i < payload_words; i++) {
    mark_and_copy(obj + i);
  }
}

static void mark_and_copy(uint64_t *stack_slot) {
  uint64_t raw_value = *stack_slot;
  if (!IS_PTR(raw_value)) {
    return;
  }

  uint64_t *old_object_payload = (uint64_t *)raw_value;
  int old_bank = get_another_bank_idx();
  if (!in_bank(old_object_payload, old_bank)) {
    return;
  }

  bool copied_now = false;
  uint64_t *new_object_payload = forward_or_copy(old_object_payload, &copied_now);
  *stack_slot = (uint64_t)new_object_payload;

  if (copied_now) {
    scan_object(new_object_payload);
  }
}

void collect(void) {
  uint64_t dummy;
  uint64_t *current_stack_top = &dummy;
  if (!PTR_STACK || current_stack_top > PTR_STACK) {
    return;
  }

  GC.current_bank ^= 1;
  GC.alloc_ptr = GC.start[GC.current_bank];

  for (uint64_t *stack_slot = current_stack_top; stack_slot <= PTR_STACK; stack_slot++) {
    mark_and_copy(stack_slot);
  }

  GC.collections++;
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

  box_header_t *h = (box_header_t *)GC.alloc_ptr;
  set_header(h, (uint16_t)tag, (uint16_t)words);
  uint64_t *obj = get_payload(h);
  memset(obj, 0, words * sizeof(uint64_t));

  GC.alloc_ptr += total_words;
  GC.allocations++;
  GC.words_allocated_total += total_words;
  return obj;
}

int64_t get_heap_start(void) { return 1; }
int64_t get_heap_final(void) { return (int64_t)((SIZE_HEAP << 1) | 1); }


void print_gc_status() {
    int bank = GC.current_bank;
    ptrdiff_t current_alloc = GC.alloc_ptr - GC.start[bank];
    ptrdiff_t free_space = GC.end[bank] - GC.alloc_ptr;
    uint64_t total = GC.words_allocated_total;
    uint64_t collections = GC.collections;
    uint64_t allocations = GC.allocations;

    printf("=== GC Status ===\n");
    printf("Current allocated: %td\n", current_alloc);
    printf("Total   allocated: %" PRIu64 "\n", total);
    printf("Free        space: %td\n", free_space);
    printf("Heap         size: %d\n", SIZE_HEAP);
    printf("Current      bank index: %d\n", bank);
    printf("GC    collections: %" PRIu64 "\n", collections);
    printf("GC    allocations: %" PRIu64 "\n", allocations);
    printf("=================\n");
    fflush(stdout);
}

static void *eml_alloc(size_t size_in_bytes, uint64_t tag) {
  if (gc_enabled) {
    uint64_t size_in_words =
        ((uint64_t)size_in_bytes + sizeof(uint64_t) - 1) / sizeof(uint64_t);
    return gc_alloc(size_in_words, tag);
  }
  (void)tag;
  return malloc(size_in_bytes);
}

typedef struct {
  void *code;
  int64_t arity;
  int64_t received;
  void *args[];
} closure;


closure *alloc_closure(void *code, int64_t arity) {
  size_t size_in_bytes = sizeof(closure) + arity * sizeof(void *);

  closure *c = (closure *)eml_alloc(size_in_bytes, TAG_CLOSURE);

  c->code = code;
  c->arity = arity;
  c->received = 0;

  memset(c->args, 0, sizeof(void *) * arity);
  return c;
}

static closure *copy_closure(const closure *src) {
  size_t total_size = sizeof(closure) + src->arity * sizeof(void *);

  closure *dst = (closure *)eml_alloc(total_size, TAG_CLOSURE);

  memcpy(dst, src, total_size);
  return dst;
}


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
      "j .Lloop_stack_push\n"
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
        [a0] "r"(args[0]), [a1] "r"(args[1]),
        [a2] "r"(args[2]), [a3] "r"(args[3]),
        [a4] "r"(args[4]), [a5] "r"(args[5]),
        [a6] "r"(args[6]), [a7] "r"(args[7]),
        [stack_args] "r"(stack_args), [args_in_stack] "r"(args_in_stack),
        [storage_for_stack_args] "r"(storage_for_stack_args)
      : "t0", "t1", "t2", "t3", "t4", "t5", "t6",
        "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "memory");

  return result;
}

void *eml_applyN(closure *c, int64_t argc, void **argv) {

  int64_t all_receive_args = c->received + argc;

  if (all_receive_args == c->arity) {
    int64_t total_count_args = c->arity;
    void **args = (void **)eml_alloc(total_count_args * sizeof(void *), TAG_CLOSURE);

    for (int64_t i = 0; i < c->received; i++) {
      args[i] = c->args[i];
    }
    for (int64_t i = 0; i < argc; i++) {
      args[c->received + i] = argv[i];
    }
    return call_closure_full(c, args);
  }

  closure *partial = copy_closure(c);

  for (int64_t i = 0; i < argc; i++) {
    partial->args[partial->received++] = argv[i];
  }

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
  for (size_t i = 0; i < (size_t)argc; i++) {
    t->args[i] = args[i];
  }
  return t;
}

void *field(tuple *t, long n) { return t->args[n >> 1]; }
