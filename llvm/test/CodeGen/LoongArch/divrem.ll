; RUN: llc -march=loongarch64 -relocation-model=pic < %s | FileCheck %s -check-prefixes=CHECK,CHECK-TRAP

; RUN: llc -march=loongarch64 -mnocheck-zero-division -relocation-model=pic < %s | FileCheck %s -check-prefixes=CHECK,NOCHECK

; FileCheck Prefixes:
;   CHECK-TRAP - trap
;   NOCHECK - Division by zero will not be detected

define i32 @sdiv1(i32 signext %a0, i32 signext %a1) nounwind readnone {
entry:
; CHECK-LABEL: sdiv1:

; CHECK:         div.w $r4, $r4, $r5
; CHECK-TRAP:    bne $r5, $zero, 8
; CHECK-TRAP:    break 7

; NOCHECK-NOT:   bne
; NOCHECK-NOT:   break

  %div = sdiv i32 %a0, %a1
  ret i32 %div
}

define i32 @srem1(i32 signext %a0, i32 signext %a1) nounwind readnone {
entry:
; CHECK-LABEL: srem1:

; CHECK:         mod.w $r4, $r4, $r5
; CHECK-TRAP:    bne $r5, $zero, 8
; CHECK-TRAP:    break 7

; NOCHECK-NOT:   bne
; NOCHECK-NOT:   break

  %rem = srem i32 %a0, %a1
  ret i32 %rem
}

define i32 @udiv1(i32 signext %a0, i32 signext %a1) nounwind readnone {
entry:
; CHECK-LABEL: udiv1:

; CHECK:         div.wu $r4, $r4, $r5
; CHECK-TRAP:    bne $r5, $zero, 8
; CHECK-TRAP:    break 7

; NOCHECK-NOT:   bne
; NOCHECK-NOT:   break

  %div = udiv i32 %a0, %a1
  ret i32 %div
}

define i32 @urem1(i32 signext %a0, i32 signext %a1) nounwind readnone {
entry:
; CHECK-LABEL: urem1:


; CHECK:         mod.wu $r4, $r4, $r5
; CHECK-TRAP:    bne $r5, $zero, 8
; CHECK-TRAP:    break 7

; NOCHECK-NOT:   bne
; NOCHECK-NOT:   break

  %rem = urem i32 %a0, %a1
  ret i32 %rem
}
