#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define TO_ML_INTEGER(n) ((uint64_t)((uint64_t)(n) >> 1))

void print_int(long n) { printf("%ld", TO_ML_INTEGER(n)); }

#define TAG_CLOSURE 247
#define RISCV_REG_ARGS 8



static void *eml_alloc(size_t size_in_bytes, uint64_t tag) {
#ifdef ENABLE_GC
  uint64_t size_in_words =
      ((uint64_t)size_in_bytes + sizeof(uint64_t) - 1) / sizeof(uint64_t);
  return gc_alloc(size_in_words, tag);
#else
  return malloc(size_in_bytes);
#endif
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
