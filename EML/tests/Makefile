SHELL := /bin/bash

RUNTIME_A_SANDBOX := ../lib/runtime/rv64_runtime.a
RUNTIME_A_DEFAULT := ../_build/default/lib/runtime/rv64_runtime.a
EML_BIN ?= /home/danil/comp25/EML/_build/default/bin/EML.exe

ARGS := $(filter-out compile,$(MAKECMDGOALS))
USE_GC := $(if $(filter 1 true yes on,$(GC)),1,0)
INPUT := $(firstword $(ARGS))
EXTRA_GOALS := $(ARGS)

.PHONY: compile $(EXTRA_GOALS)

compile:
	@set -euo pipefail; \
	FILE="$(INPUT)"; \
	if [[ -z "$$FILE" ]]; then \
		echo "Usage: make compile [GC=1] <file.ml|file.s>" >&2; \
		exit 1; \
	fi; \
	if [[ "$$FILE" == *.s ]]; then \
		ASM_FILE="$$FILE"; \
	else \
		if [[ -f "$$FILE" ]]; then SRC="$$FILE"; \
		else echo "Source file not found: $$FILE" >&2; exit 1; fi; \
		SRC="$$(realpath "$$SRC")"; \
		TMP_SRC_DIR="$$(mktemp -d)"; \
		trap 'rm -rf "$$TMP_SRC_DIR"' EXIT; \
		ASM_FILE="$$TMP_SRC_DIR/prog.s"; \
		if [[ "$(USE_GC)" == "1" ]]; then \
			"$(EML_BIN)" -gc -fromfile "$$SRC" -o "$$ASM_FILE"; \
		else \
			"$(EML_BIN)" -fromfile "$$SRC" -o "$$ASM_FILE"; \
		fi; \
	fi; \
	if [[ -f "$(RUNTIME_A_SANDBOX)" ]]; then \
		RUNTIME_A="$(RUNTIME_A_SANDBOX)"; \
	else \
		RUNTIME_A="$(RUNTIME_A_DEFAULT)"; \
	fi; \
	TMP_BIN_DIR="$$(mktemp -d)"; \
	trap 'rm -rf "$$TMP_BIN_DIR"' EXIT; \
	OBJ_FILE="$$TMP_BIN_DIR/prog.o"; \
	EXE_FILE="$$TMP_BIN_DIR/prog.exe"; \
	riscv64-linux-gnu-as -march=rv64gc "$$ASM_FILE" -o "$$OBJ_FILE"; \
	riscv64-linux-gnu-gcc "$$OBJ_FILE" "$$RUNTIME_A" -o "$$EXE_FILE"; \
	qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 "$$EXE_FILE"

$(EXTRA_GOALS):
	@:
