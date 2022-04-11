/// Check passing -mabi=<ABIName> and -mfpu=<FPU> options to the backend.

// RUN: %clang -target loongarch64 %s -mabi=lp64s -mfpu=none -### 2>&1 \
// RUN:   | FileCheck --check-prefix=FEATURE-NF-ND %s
// RUN: %clang -target loongarch64 %s -mabi=lp64s -mfpu=32 -### 2>&1 \
// RUN:   | FileCheck --check-prefix=FEATURE-F %s
// RUN: %clang -target loongarch64 %s -mabi=lp64s -mfpu=64 -### 2>&1 \
// RUN:   | FileCheck --check-prefix=FEATURE-D %s
// RUN: %clang -target loongarch64 %s -mabi=lp64f -mfpu=none -### 2>&1 \
// RUN:   | FileCheck --check-prefix=ERRLP64F-WITH-FPUNONE %s
// RUN: %clang -target loongarch64 %s -mabi=lp64f -mfpu=32 -### 2>&1 \
// RUN:   | FileCheck --check-prefix=FEATURE-F %s
// RUN: %clang -target loongarch64 %s -mabi=lp64f -mfpu=64 -### 2>&1 \
// RUN:   | FileCheck --check-prefix=FEATURE-D %s
// RUN: %clang -target loongarch64 %s -mabi=lp64d -mfpu=none -### 2>&1 \
// RUN:   | FileCheck --check-prefix=ERRLP64D-ONLY-FPU64 %s
// RUN: %clang -target loongarch64 %s -mabi=lp64d -mfpu=32 -### 2>&1 \
// RUN:   | FileCheck --check-prefix=ERRLP64D-ONLY-FPU64 %s
// RUN: %clang -target loongarch64 %s -mabi=lp64d -mfpu=64 -### 2>&1 \
// RUN:   | FileCheck --check-prefix=FEATURE-D %s

// FEATURE-D: "-target-feature" "+d"
// FEATURE-F: "-target-feature" "+f"
// FEATURE-NF-ND: "-target-feature" "-f" "-target-feature" "-d"
// ERRLP64D-ONLY-FPU64: error: option 'lp64d' cannot be specified without '-mfpu=64'
// ERRLP64F-WITH-FPUNONE: error: option 'lp64f' cannot be specified with '-mfpu=none'
