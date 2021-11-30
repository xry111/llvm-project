; RUN: llc -march=loongarch64 -o - %s | FileCheck %s

define void @bstrins_w(i32 %s, i32* nocapture %d) nounwind {
; CHECK-LABEL: bstrins_w:
; CHECK:       bstrins.w $r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]], 13, 5
entry:
  %and = shl i32 %s, 5
  %shl = and i32 %and, 16352
  %tmp3 = load i32, i32* %d, align 4
  %and5 = and i32 %tmp3, -16353
  %or = or i32 %and5, %shl
  store i32 %or, i32* %d, align 4
  ret void
}

define i32 @no_bstrinsw(i32* nocapture %d) {
; CHECK-LABEL: no_bstrinsw:
; CHECK:       addi.w $r[[REG2:[0-9]+]], $zero, -4
; CHECK:       and $r[[REG1:[0-9]+]], $r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]]
; CHECK:       ori $r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]], 8
; CHECK-NOT:   bstrins.w {{[[:space:]].*}}
entry:
  %tmp = load volatile i32, i32* %d, align 4
  %and = and i32 %tmp, -4
  %or = or i32 %and, 8
  store volatile i32 %or, i32* %d, align 4
  ret i32 %and
}
