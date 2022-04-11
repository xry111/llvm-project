; RUN: llc -march=loongarch64 < %s | FileCheck %s

define i32 @foo(i32 signext %a) {
; CHECK-LABEL: foo:
; CHECK:       # %bb.0:
; CHECK-NEXT:    slli.w $r4, $r4, 0
; CHECK-NEXT:    jr $ra
  ret i32 %a
}
