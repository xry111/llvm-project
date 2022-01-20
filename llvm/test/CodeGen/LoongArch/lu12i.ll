; RUN: llc -march=loongarch64 -o - %s | FileCheck %s

define i32 @foo() {
; CHECK:    lu12i.w $r4, -1
entry:
  ret i32 -4096
}
