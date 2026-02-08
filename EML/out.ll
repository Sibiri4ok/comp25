; ModuleID = 'main'
source_filename = "main"
target triple = "x86_64-pc-linux-gnu"

declare void @print_int(i64)

define i64 @main() {
entry:
  call void @print_int(i64 70)
  ret i64 0
}
