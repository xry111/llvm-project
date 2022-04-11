; Test that the last immediate 0 operand of amtomic instruction is printed

; RUN: llc -march=loongarch64 -o - %s | FileCheck %s

define void @test_i32(i32* %dst, i32 %val) {
; CHECK: ammax_db.wu $r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]], $r[[REG3:[0-9]+]], 0
entry:
  %a = atomicrmw umax i32* %dst, i32 %val monotonic
  ret void
}

define void @test_i64(i64* %dst, i64 %val) {
; CHECK: ammax_db.du $r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]], $r[[REG3:[0-9]+]], 0
entry:
  %a = atomicrmw umax i64* %dst, i64 %val monotonic
  ret void
}
