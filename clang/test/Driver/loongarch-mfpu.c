// Check passing -mfpu=<FPU> options to the backend.

// check default feature for loongarch64
// RUN: %clang -target loongarch64 %s -### 2>&1 \
// RUN:   | FileCheck --check-prefix=FEATURE-D %s
// check -mfpu=64 option for loongarch64
// RUN: %clang -target loongarch64 %s -mfpu=64 -### 2>&1 \
// RUN:   | FileCheck --check-prefix=FEATURE-D %s
// check -mfpu=32 option for loongarch64
// RUN: %clang -target loongarch64 %s -mfpu=32 -### 2>&1 \
// RUN:   | FileCheck --check-prefix=ERRLP64D-ONLY-FPU64 %s
// check -mfpu=none option for loongarch64
// RUN: %clang -target loongarch64 %s -mfpu=none -### 2>&1 \
// RUN:   | FileCheck --check-prefix=ERRLP64D-ONLY-FPU64 %s
// check -mfpu=x option for loongarch64
// RUN: %clang -target loongarch64 %s -mfpu=x -### 2>&1 \
// RUN:   | FileCheck --check-prefix=INVALID-FPU %s

// FEATURE-D: "-target-feature" "+d"
// INVALID-FPU: error: invalid loongarch FPU value 'x'. Please specify FPU = 64,32 or none
// ERRLP64D-ONLY-FPU64: error: option 'lp64d' cannot be specified without '-mfpu=64'
