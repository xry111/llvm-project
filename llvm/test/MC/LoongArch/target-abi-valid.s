# RUN: llvm-mc -triple loongarch64 -filetype=obj < %s \
# RUN:   | llvm-readelf -h - \
# RUN:   | FileCheck -check-prefix=CHECK-NONE %s

# RUN: llvm-mc -triple loongarch64 -target-abi lp64s -filetype=obj < %s \
# RUN:   | llvm-readelf -h - \
# RUN:   | FileCheck -check-prefix=CHECK-LP64S %s

# RUN: llvm-mc -triple loongarch64 -target-abi lp64f -filetype=obj < %s \
# RUN:   | llvm-readelf -h - \
# RUN:   | FileCheck -check-prefix=CHECK-LP64F %s

# RUN: llvm-mc -triple loongarch64 -target-abi lp64d -filetype=obj < %s \
# RUN:   | llvm-readelf -h - \
# RUN:   | FileCheck -check-prefix=CHECK-LP64D %s

# CHECK-NONE:        Flags:  0x3, LP64D

# CHECK-LP64S:       Flags:  0x1, LP64S

# CHECK-LP64F:       Flags:  0x2, LP64F

# CHECK-LP64D:       Flags:  0x3, LP64D
