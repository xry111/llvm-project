# RUN: llvm-mc %s -triple=loongarch64-unknown-linux-gnu -show-encoding | FileCheck %s

## random operands
beqz           $r9, 96                       # CHECK: beqz       $r9, 96                       # encoding: [0x20,0x61,0x00,0x40]
bnez           $sp, 212                      # CHECK: bnez       $sp, 212                      # encoding: [0x60,0xd4,0x00,0x44]
bceqz          $fcc6, 12                     # CHECK: bceqz      $fcc6, 12                     # encoding: [0xc0,0x0c,0x00,0x48]
bcnez          $fcc6, 72                     # CHECK: bcnez      $fcc6, 72                     # encoding: [0xc0,0x49,0x00,0x48]
b              248                           # CHECK: b          248                           # encoding: [0x00,0xf8,0x00,0x50]
bl             236                           # CHECK: bl         236                           # encoding: [0x00,0xec,0x00,0x54]
beq            $r10, $r7, 176                # CHECK: beq        $r10, $r7, 176                # encoding: [0x47,0xb1,0x00,0x58]
bne            $r25, $ra, 136                # CHECK: bne        $r25, $ra, 136                # encoding: [0x21,0x8b,0x00,0x5c]
blt            $r15, $r30, 168               # CHECK: blt        $r15, $r30, 168               # encoding: [0xfe,0xa9,0x00,0x60]
bge            $r12, $r15, 148               # CHECK: bge        $r12, $r15, 148               # encoding: [0x8f,0x95,0x00,0x64]
bltu           $r17, $r5, 4                  # CHECK: bltu       $r17, $r5, 4                  # encoding: [0x25,0x06,0x00,0x68]
bgeu           $r6, $r23, 140                # CHECK: bgeu       $r6, $r23, 140                # encoding: [0xd7,0x8c,0x00,0x6c]

## immediate lower/upper boundary
### simm16 << 2
beq            $r10, $r7, -0x20000           # CHECK: beq        $r10, $r7, -131072            # encoding: [0x47,0x01,0x00,0x5a]
beq            $r10, $r7,  0x1FFFC           # CHECK: beq        $r10, $r7,  131068            # encoding: [0x47,0xfd,0xff,0x59]
bne            $r10, $r7, -0x20000           # CHECK: bne        $r10, $r7, -131072            # encoding: [0x47,0x01,0x00,0x5e]
bne            $r10, $r7,  0x1FFFC           # CHECK: bne        $r10, $r7,  131068            # encoding: [0x47,0xfd,0xff,0x5d]
blt            $r10, $r7, -0x20000           # CHECK: blt        $r10, $r7, -131072            # encoding: [0x47,0x01,0x00,0x62]
blt            $r10, $r7,  0x1FFFC           # CHECK: blt        $r10, $r7,  131068            # encoding: [0x47,0xfd,0xff,0x61]
bge            $r10, $r7, -0x20000           # CHECK: bge        $r10, $r7, -131072            # encoding: [0x47,0x01,0x00,0x66]
bge            $r10, $r7,  0x1FFFC           # CHECK: bge        $r10, $r7,  131068            # encoding: [0x47,0xfd,0xff,0x65]
bltu           $r10, $r7, -0x20000           # CHECK: bltu       $r10, $r7, -131072            # encoding: [0x47,0x01,0x00,0x6a]
bltu           $r10, $r7,  0x1FFFC           # CHECK: bltu       $r10, $r7,  131068            # encoding: [0x47,0xfd,0xff,0x69]
bgeu           $r10, $r7, -0x20000           # CHECK: bgeu       $r10, $r7, -131072            # encoding: [0x47,0x01,0x00,0x6e]
bgeu           $r10, $r7,  0x1FFFC           # CHECK: bgeu       $r10, $r7,  131068            # encoding: [0x47,0xfd,0xff,0x6d]
### simm21 << 2
beqz           $r9, -0x400000                # CHECK: beqz       $r9, -4194304                 # encoding: [0x30,0x01,0x00,0x40]
beqz           $r9,  0x3FFFFC                # CHECK: beqz       $r9,  4194300                 # encoding: [0x2f,0xfd,0xff,0x43]
bnez           $r9, -0x400000                # CHECK: bnez       $r9, -4194304                 # encoding: [0x30,0x01,0x00,0x44]
bnez           $r9,  0x3FFFFC                # CHECK: bnez       $r9,  4194300                 # encoding: [0x2f,0xfd,0xff,0x47]
bceqz          $fcc6, -0x400000              # CHECK: bceqz      $fcc6, -4194304               # encoding: [0xd0,0x00,0x00,0x48]
bceqz          $fcc6,  0x3FFFFC              # CHECK: bceqz      $fcc6,  4194300               # encoding: [0xcf,0xfc,0xff,0x4b]
bcnez          $fcc6, -0x400000              # CHECK: bcnez      $fcc6, -4194304               # encoding: [0xd0,0x01,0x00,0x48]
bcnez          $fcc6,  0x3FFFFC              # CHECK: bcnez      $fcc6,  4194300               # encoding: [0xcf,0xfd,0xff,0x4b]
### simm26 << 2
b              -0x8000000                    # CHECK: b          -134217728                    # encoding: [0x00,0x02,0x00,0x50]
b               0x7FFFFFC                    # CHECK: b           134217724                    # encoding: [0xff,0xfd,0xff,0x53]
bl             -0x8000000                    # CHECK: bl         -134217728                    # encoding: [0x00,0x02,0x00,0x54]
bl              0x7FFFFFC                    # CHECK: bl          134217724                    # encoding: [0xff,0xfd,0xff,0x57]
