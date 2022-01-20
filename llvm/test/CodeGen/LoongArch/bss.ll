; RUN: llc -march=loongarch64 -o - %s | FileCheck %s

; CHECK: .section .bss,"aw",@nobits
; CHECK: .globl a
@a = global i32 0, align 4
