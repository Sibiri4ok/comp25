SHELL := /bin/bash

EML_BIN ?= /home/danil/comp25/EML/_build/default/bin/EML.exe
RUNTIME_A := $(firstword $(wildcard ../lib/runtime/rv64_runtime.a ../_build/default/lib/runtime/rv64_runtime.a))
INPUT := $(firstword $(filter-out compile,$(MAKECMDGOALS)))
EXTRA_GOALS := $(filter-out compile,$(MAKECMDGOALS))
GC_ON := $(filter 1 true yes on,$(GC))

.PHONY: compile $(EXTRA_GOALS)

compile:
	@set -euo pipefail; \
	FILE="$(INPUT)"; [[ -n "$$FILE" ]] || { echo "Usage: make compile [GC=1] <file.ml|file.s>" >&2; exit 1; }; \
	SRC="$$FILE"; \
	[[ -f "$$SRC" ]] || { \
		if [[ "$$FILE" == manytests/* && -f "many_tests/$${FILE#manytests/}" ]]; then \
			SRC="many_tests/$${FILE#manytests/}"; \
		elif [[ "$$FILE" == many_tests/* && -f "manytests/$${FILE#many_tests/}" ]]; then \
			SRC="manytests/$${FILE#many_tests/}"; \
		else \
			echo "Source file not found: $$FILE" >&2; exit 1; \
		fi; \
	}; \
	TMP_SRC="$$(mktemp -d)"; TMP_BIN="$$(mktemp -d)"; trap 'rm -rf "$$TMP_SRC" "$$TMP_BIN"' EXIT; \
	ASM_FILE="$$SRC"; \
	if [[ "$$FILE" != *.s ]]; then \
		ASM_FILE="$$TMP_SRC/prog.s"; SRC="$$(realpath "$$SRC")"; \
		"$(EML_BIN)" $$( [[ -n "$(GC_ON)" ]] && echo -gc ) -fromfile "$$SRC" -o "$$ASM_FILE"; \
	fi; \
	OBJ="$$TMP_BIN/prog.o"; EXE="$$TMP_BIN/prog.exe"; \
	riscv64-linux-gnu-as -march=rv64gc "$$ASM_FILE" -o "$$OBJ"; \
	riscv64-linux-gnu-gcc "$$OBJ" "$(RUNTIME_A)" -o "$$EXE"; \
	qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 "$$EXE"

$(EXTRA_GOALS):
	@:
