; RUN: llc -march=loongarch64 -o - %s | FileCheck %s

define i32 @bstrpickw_and24(i32 signext %a) {
; CHECK-LABEL: bstrpickw_and24:
; CHECK:       bstrpick.w $r[[REG:[0-9]+]], $r[[REG:[0-9]+]], 23, 0
entry:
  %and = and i32 %a, 16777215
  ret i32 %and
}

define i32 @bstrpickw_lshr_and(i32 %s, i32 %pos, i32 %sz) nounwind readnone {
; CHECK-LABEL: bstrpickw_lshr_and:
; CHECK:       bstrpick.w $r[[REG:[0-9]+]], $r[[REG:[0-9]+]], 13, 5
entry:
  %shr = lshr i32 %s, 5
  %and = and i32 %shr, 511
  ret i32 %and
}
