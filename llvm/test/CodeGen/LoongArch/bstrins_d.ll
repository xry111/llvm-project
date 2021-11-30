; RUN: llc -march=loongarch64 -o - %s | FileCheck %s

define void @bstrinsd_63_27(i64* nocapture %d) nounwind {
; CHECK-LABEL: bstrinsd_63_27:
; CHECK:    addi.d $r[[REG1:[0-9]+]], $zero, 123
; CHECK:    bstrins.d $r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]], 63, 27
entry:
  %tmp = load i64, i64* %d, align 8
  %and5 = and i64 %tmp, 134217727
  %or = or i64 %and5, 16508780544
  store i64 %or, i64* %d, align 8
  ret void
}

define void @bstrinsd_33_28(i64* nocapture %d) nounwind {
; CHECK-LABEL: bstrinsd_33_28:
; CHECK:    addi.d $r[[REG1:[0-9]+]], $zero, 4
; CHECK:    bstrins.d $r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]], 33, 28
entry:
  %tmp = load i64, i64* %d, align 8
  %and5 = and i64 %tmp, -16911433729
  %or = or i64 %and5, 1073741824
  store i64 %or, i64* %d, align 8
  ret void
}

define void @bstrinsd_49_34(i64* nocapture %d) nounwind {
; CHECK-LABEL: bstrinsd_49_34:
; CHECK:    srli.d $r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]], 50
; CHECK:    bstrins.d $r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]], 49, 34
entry:
  %tmp0 = load i64, i64* %d, align 8
  %lshr = lshr i64 %tmp0, 50
  %tmp1 = load i64, i64* %d, align 8
  %shl = shl nuw nsw i64 %lshr, 34
  %and = and i64 %tmp1, -1125882726973441
  %or = or i64 %and, %shl
  store i64 %or, i64* %d, align 8
  ret void
}
