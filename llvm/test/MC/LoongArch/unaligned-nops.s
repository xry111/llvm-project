# RUN: not --crash llvm-mc -filetype=obj  -triple=loongarch64 %s -o %t \
# RUN: 2>&1 | FileCheck %s
.byte 1
# CHECK: LLVM ERROR: unable to write nop sequence of 3 bytes
.p2align 2
foo:
