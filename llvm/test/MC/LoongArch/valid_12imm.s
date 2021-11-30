# RUN: llvm-mc %s -triple=loongarch64-unknown-linux-gnu -show-encoding | FileCheck %s
slti           $r27, $ra, 235                # CHECK: slti       $r27, $ra, 235                # encoding: [0x3b,0xac,0x03,0x02]
sltui          $zero, $r8, 162               # CHECK: sltui      $zero, $r8, 162               # encoding: [0x00,0x89,0x42,0x02]
addi.w         $r5, $r7, 246                 # CHECK: addi.w     $r5, $r7, 246                 # encoding: [0xe5,0xd8,0x83,0x02]
addi.d         $r28, $r6, 75                 # CHECK: addi.d     $r28, $r6, 75                 # encoding: [0xdc,0x2c,0xc1,0x02]
lu52i.d        $r13, $r4, 195                # CHECK: lu52i.d    $r13, $r4, 195                # encoding: [0x8d,0x0c,0x03,0x03]
andi           $r25, $zero, 106              # CHECK: andi       $r25, $zero, 106              # encoding: [0x19,0xa8,0x41,0x03]
ori            $r17, $r5, 47                 # CHECK: ori        $r17, $r5, 47                 # encoding: [0xb1,0xbc,0x80,0x03]
xori           $r18, $r23, 99                # CHECK: xori       $r18, $r23, 99                # encoding: [0xf2,0x8e,0xc1,0x03]
