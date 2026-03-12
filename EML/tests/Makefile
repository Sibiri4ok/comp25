SHELL := /bin/bash

EML_ROOT   ?= $(CURDIR)/..
EML_BIN    ?= $(if $(findstring _build,$(EML_ROOT)),$(EML_ROOT)/bin/EML.exe,$(EML_ROOT)/_build/default/bin/EML.exe)
RUNTIME_A  := $(if $(wildcard $(EML_ROOT)/lib/runtime/rv64_runtime.o),$(EML_ROOT)/lib/runtime/rv64_runtime.o,$(EML_ROOT)/_build/default/lib/runtime/rv64_runtime.o)
RUNTIME_LLVM_O := $(if $(wildcard $(EML_ROOT)/lib/runtime/llvm_runtime.o),$(EML_ROOT)/lib/runtime/llvm_runtime.o,$(EML_ROOT)/_build/default/lib/runtime/llvm_runtime.o)

ARGS       := $(filter-out compile_riscv infer compile_llvm,$(MAKECMDGOALS))
INPUT      := $(firstword $(ARGS))
EXTRA_GOALS := $(ARGS)
GC_FLAG    := $(if $(filter 1 true yes on,$(GC)),-gc,)
CFLAGS_GC  := $(if $(filter 1 true yes on,$(GC)),-DENABLE_GC,)

.PHONY: compile_riscv infer compile_llvm $(EXTRA_GOALS)

compile_riscv:
	@set -euo pipefail; \
	FILE="$(INPUT)"; \
	[[ -n "$$FILE" ]] || { echo "Usage: make compile_riscv [GC=1] <file.ml|file.s>" >&2; exit 1; }; \
	TMP_SRC="$$(mktemp -d)"; TMP_BIN="$$(mktemp -d)"; \
	trap 'rm -rf "$$TMP_SRC" "$$TMP_BIN"' EXIT; \
	if [[ "$$FILE" == *.s ]]; then \
		ASM_FILE="$$FILE"; \
	else \
		[[ -f "$$FILE" ]] || { echo "Source file not found: $$FILE" >&2; exit 1; }; \
		SRC="$$(realpath "$$FILE")"; \
		ASM_FILE="$$TMP_SRC/prog.s"; \
		"$(EML_BIN)" $(GC_FLAG) -fromfile "$$SRC" -o "$$ASM_FILE"; \
	fi; \
	OBJ_FILE="$$TMP_BIN/prog.o"; EXE_FILE="$$TMP_BIN/prog.exe"; \
	riscv64-linux-gnu-as -march=rv64gc "$$ASM_FILE" -o "$$OBJ_FILE"; \
	riscv64-linux-gnu-gcc "$$OBJ_FILE" "$(RUNTIME_A)" -o "$$EXE_FILE"; \
	qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu max "$$EXE_FILE"

infer:
	@set -euo pipefail; \
	FILE="$(INPUT)"; \
	[[ -n "$$FILE" ]] || { echo "Usage: make infer [GC=1] <file.ml>" >&2; exit 1; }; \
	[[ "$$FILE" != *.s ]] || { echo "Infer mode expects .ml source, got assembly: $$FILE" >&2; exit 1; }; \
	[[ -f "$$FILE" ]] || { echo "Source file not found: $$FILE" >&2; exit 1; }; \
	SRC="$$(realpath "$$FILE")"; \
	"$(EML_BIN)" $(GC_FLAG) -infer -fromfile "$$SRC"

compile_llvm:
	@set -euo pipefail; \
	[[ -f "$(RUNTIME_LLVM_O)" ]] || (cd "$(EML_ROOT)" && dune build lib/runtime/llvm_runtime.o); \
	FILE="$(INPUT)"; \
	[[ -n "$$FILE" ]] || { echo "Usage: make compile_llvm [GC=1] <file.ml>" >&2; exit 1; }; \
	[[ -f "$$FILE" ]] || { echo "Source file not found: $$FILE" >&2; exit 1; }; \
	SRC="$$(realpath "$$FILE")"; \
	TMP="$$(mktemp -d)"; trap 'rm -rf "$$TMP"' EXIT; \
	"$(EML_BIN)" $(GC_FLAG) -backend llvm -fromfile "$$SRC" -o "$$TMP/prog.ll"; \
	clang -target riscv64-unknown-linux-gnu -O0 -Wno-override-module "$$TMP/prog.ll" "$(RUNTIME_LLVM_O)" -o "$$TMP/prog.exe"; \
	qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu max "$$TMP/prog.exe" || true

$(EXTRA_GOALS):
	@:
