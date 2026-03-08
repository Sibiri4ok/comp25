SHELL := /bin/bash

EML_ROOT ?= $(CURDIR)/..
EML_BIN ?= $(if $(findstring _build,$(EML_ROOT)),$(EML_ROOT)/bin/EML.exe,$(EML_ROOT)/_build/default/bin/EML.exe)
RUNTIME_A := $(if $(wildcard ../lib/runtime/rv64_runtime.a),../lib/runtime/rv64_runtime.a,../_build/default/lib/runtime/rv64_runtime.a)

ARGS := $(filter-out compile compile_llvm,$(MAKECMDGOALS))
INPUT := $(firstword $(ARGS))
EXTRA_GOALS := $(ARGS)
GC_FLAG := $(if $(filter 1 true yes on,$(GC)),-gc,)

.PHONY: compile compile_llvm $(EXTRA_GOALS)

compile:
	@set -euo pipefail; \
	FILE="$(INPUT)"; \
	[[ -n "$$FILE" ]] || { echo "Usage: make compile [GC=1] <file.ml|file.s>" >&2; exit 1; }; \
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
	OBJ_FILE="$$TMP_BIN/prog.o"; \
	EXE_FILE="$$TMP_BIN/prog.exe"; \
	riscv64-linux-gnu-as -march=rv64gc "$$ASM_FILE" -o "$$OBJ_FILE"; \
	riscv64-linux-gnu-gcc "$$OBJ_FILE" "$(RUNTIME_A)" -o "$$EXE_FILE"; \
	qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 "$$EXE_FILE"

# LLVM: compile source to .ll (no link/run; needs llvm_call_indirect for full run)
compile_llvm:
	@set -euo pipefail; \
	FILE="$(INPUT)"; \
	[[ -n "$$FILE" ]] || { echo "Usage: make compile_llvm <file.ml>" >&2; exit 1; }; \
	[[ -f "$$FILE" ]] || { echo "Source file not found: $$FILE" >&2; exit 1; }; \
	SRC="$$(realpath "$$FILE")"; \
	TMP="$$(mktemp -d)"; trap 'rm -rf "$$TMP"' EXIT; \
	"$(EML_BIN)" -backend llvm -fromfile "$$SRC" -o "$$TMP/prog.ll"; \
	cat "$$TMP/prog.ll"

$(EXTRA_GOALS):
	@:
