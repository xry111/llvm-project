# RUN: not llvm-mc %s -triple=loongarch64-unknown-linux-gnu 2>&1 | FileCheck %s
.text
csrxchg        $r6, $r0, 214                # CHECK: :[[@LINE]]:1: error: invalid operand ($zero) for instruction
csrxchg        $r6, $r1, 214                # CHECK: :[[@LINE]]:1: error: invalid operand ($r1) for instruction
