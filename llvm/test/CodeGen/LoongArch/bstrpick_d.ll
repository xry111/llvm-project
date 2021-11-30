; RUN: llc -march=loongarch64 -o - %s | FileCheck %s

define i64 @bstrpickd_add_zext(i32 signext %n) {
entry:
  %add = add i32 %n, 1
  %res = zext i32 %add to i64
  ret i64 %res

; CHECK-LABEL: bstrpickd_add_zext:
; CHECK:       bstrpick.d $r[[REG:[0-9]+]], $r[[REG:[0-9]+]], 31, 0

}

define i64 @bstrpickd_and12(i64 zeroext %a) {
entry:
  %and = and i64 %a, 4095
  ret i64 %and

; CHECK-LABEL: bstrpickd_and12:
; CHECK:       andi $r[[REG:[0-9]+]], $r[[REG:[0-9]+]], 4095

}

define i64 @bstrpickd_and13(i64 zeroext %a) {
entry:
  %and = and i64 %a, 8191
  ret i64 %and

; CHECK-LABEL: bstrpickd_and13:
; CHECK:       bstrpick.d $r[[REG:[0-9]+]], $r[[REG:[0-9]+]], 12, 0

}

define i64 @bstrpickd_lsr_and8(i64 zeroext %a) {
entry:
  %shr = lshr i64 %a, 40
  %and = and i64 %shr, 255
  ret i64 %and

; CHECK-LABEL: bstrpickd_lsr_and8:
; CHECK:       bstrpick.d $r[[REG:[0-9]+]], $r[[REG:[0-9]+]], 47, 40

}

define i64 @bstrpickd_zext(i32 signext %a) {
entry:
  %conv = zext i32 %a to i64
  ret i64 %conv

; CHECK-LABEL: bstrpickd_zext:
; CHECK:       bstrpick.d $r[[REG:[0-9]+]], $r[[REG:[0-9]+]], 31, 0

}

define i64 @bstrpickd_and_lsr(i64 zeroext %n) {
entry:
  %and = lshr i64 %n, 8
  %shr = and i64 %and, 4095
  ret i64 %shr

; CHECK-LABEL: bstrpickd_and_lsr:
; CHECK:       bstrpick.d $r[[REG:[0-9]+]], $r[[REG:[0-9]+]], 19, 8

}
