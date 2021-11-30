# RUN: llvm-mc %s -triple=loongarch64-unknown-linux-gnu -show-encoding | FileCheck %s
addu16i.d      $r9, $r23, 23                 # CHECK: addu16i.d  $r9, $r23, 23                 # encoding: [0xe9,0x5e,0x00,0x10]
lu12i.w        $r16, 49                      # CHECK: lu12i.w    $r16, 49                      # encoding: [0x30,0x06,0x00,0x14]
lu32i.d        $sp, 196                      # CHECK: lu32i.d    $sp, 196                      # encoding: [0x83,0x18,0x00,0x16]
pcaddi         $r9, 187                      # CHECK: pcaddi     $r9, 187                      # encoding: [0x69,0x17,0x00,0x18]
pcalau12i      $r10, 89                      # CHECK: pcalau12i  $r10, 89                      # encoding: [0x2a,0x0b,0x00,0x1a]
pcaddu12i      $zero, 37                     # CHECK: pcaddu12i  $zero, 37                     # encoding: [0xa0,0x04,0x00,0x1c]
pcaddu18i      $r12, 26                      # CHECK: pcaddu18i  $r12, 26                      # encoding: [0x4c,0x03,0x00,0x1e]
