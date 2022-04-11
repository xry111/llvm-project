# RUN: not llvm-mc %s -triple=loongarch64-unknown-linux-gnu 2>&1 | FileCheck %s
.text
csrxchg        $r6, $r0, 214                # CHECK: :[[@LINE]]:1: error: invalid operand ($zero) for instruction
csrxchg        $r6, $r1, 214                # CHECK: :[[@LINE]]:1: error: invalid operand ($r1) for instruction

## out-of-bound immediate
### simm16 << 2
beq            $r10, $r7, -0x20000-4           # CHECK: :[[@LINE]]:1: error: branch target out of range
beq            $r10, $r7,  0x1FFFC+4           # CHECK: :[[@LINE]]:1: error: branch target out of range
bne            $r10, $r7, -0x20000-4           # CHECK: :[[@LINE]]:1: error: branch target out of range
bne            $r10, $r7,  0x1FFFC+4           # CHECK: :[[@LINE]]:1: error: branch target out of range
blt            $r10, $r7, -0x20000-4           # CHECK: :[[@LINE]]:1: error: branch target out of range
blt            $r10, $r7,  0x1FFFC+4           # CHECK: :[[@LINE]]:1: error: branch target out of range
bge            $r10, $r7, -0x20000-4           # CHECK: :[[@LINE]]:1: error: branch target out of range
bge            $r10, $r7,  0x1FFFC+4           # CHECK: :[[@LINE]]:1: error: branch target out of range
bltu           $r10, $r7, -0x20000-4           # CHECK: :[[@LINE]]:1: error: branch target out of range
bltu           $r10, $r7,  0x1FFFC+4           # CHECK: :[[@LINE]]:1: error: branch target out of range
bgeu           $r10, $r7, -0x20000-4           # CHECK: :[[@LINE]]:1: error: branch target out of range
bgeu           $r10, $r7,  0x1FFFC+4           # CHECK: :[[@LINE]]:1: error: branch target out of range
### simm21 << 2
beqz           $r9, -0x400000-4                # CHECK: :[[@LINE]]:1: error: branch target out of range
beqz           $r9,  0x3FFFFC+4                # CHECK: :[[@LINE]]:1: error: branch target out of range
bnez           $r9, -0x400000-4                # CHECK: :[[@LINE]]:1: error: branch target out of range
bnez           $r9,  0x3FFFFC+4                # CHECK: :[[@LINE]]:1: error: branch target out of range
bceqz          $fcc6, -0x400000-4              # CHECK: :[[@LINE]]:1: error: branch target out of range
bceqz          $fcc6,  0x3FFFFC+4              # CHECK: :[[@LINE]]:1: error: branch target out of range
bcnez          $fcc6, -0x400000-4              # CHECK: :[[@LINE]]:1: error: branch target out of range
bcnez          $fcc6,  0x3FFFFC+4              # CHECK: :[[@LINE]]:1: error: branch target out of range
### simm26 << 2
b              -0x8000000-4                    # CHECK: :[[@LINE]]:1: error: branch target out of range
b               0x7FFFFFC+4                    # CHECK: :[[@LINE]]:1: error: branch target out of range
bl             -0x8000000-4                    # CHECK: :[[@LINE]]:1: error: branch target out of range
bl              0x7FFFFFC+4                    # CHECK: :[[@LINE]]:1: error: branch target out of range

## unaligned immediate
### simm16 << 2
beq            $r10, $r7,  0x1FFFC+1           # CHECK: :[[@LINE]]:1: error: branch to misaligned address
bne            $r10, $r7,  0x1FFFC+1           # CHECK: :[[@LINE]]:1: error: branch to misaligned address
blt            $r10, $r7,  0x1FFFC+1           # CHECK: :[[@LINE]]:1: error: branch to misaligned address
bge            $r10, $r7,  0x1FFFC+1           # CHECK: :[[@LINE]]:1: error: branch to misaligned address
bltu           $r10, $r7,  0x1FFFC+1           # CHECK: :[[@LINE]]:1: error: branch to misaligned address
bgeu           $r10, $r7,  0x1FFFC+1           # CHECK: :[[@LINE]]:1: error: branch to misaligned address
### simm21 << 2
beqz           $r9,  0x3FFFFC+1                # CHECK: :[[@LINE]]:1: error: branch to misaligned address
bnez           $r9,  0x3FFFFC+1                # CHECK: :[[@LINE]]:1: error: branch to misaligned address
bceqz          $fcc6,  0x3FFFFC+1              # CHECK: :[[@LINE]]:1: error: branch to misaligned address
bcnez          $fcc6,  0x3FFFFC+1              # CHECK: :[[@LINE]]:1: error: branch to misaligned address
### simm26 << 2
b               0x7FFFFFC+1                    # CHECK: :[[@LINE]]:1: error: branch to misaligned address
bl              0x7FFFFFC+1                    # CHECK: :[[@LINE]]:1: error: branch to misaligned address
