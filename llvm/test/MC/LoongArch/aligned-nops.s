# RUN: llvm-mc -filetype=obj -triple loongarch64 < %s \
# RUN:     | llvm-objdump -d - | FileCheck -check-prefix=CHECK-INST %s

# alpha and main are 8 byte alignment
# but the alpha function's size is 4
# So assembler will insert a nop to make sure 8 byte alignment.

        .text
       .p2align        3
       .type   alpha,@function
alpha:
# BB#0:
       addi.d  $sp, $sp, -16
# CHECK-INST: nop
.Lfunc_end0:
       .size   alpha, .Lfunc_end0-alpha
                                        # -- End function
       .globl  main
       .p2align        3
       .type   main,@function
main:                                   # @main
# BB#0:
.Lfunc_end1:
       .size   main, .Lfunc_end1-main
                                        # -- End function
