// Check passing -mabi=<ABIName> options to the backend.

// check default ABI for loongarch64
// RUN: %clang -target loongarch64 %s -### 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-LP64D %s
// check -mabi=lp64d option for loongarch64
// RUN: %clang -target loongarch64 %s -mabi=lp64d -### 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-LP64D %s
// check -mabi=lp64f option for loongarch64
// RUN: %clang -target loongarch64 %s -mabi=lp64f -### 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-LP64F %s
// check -mabi=lp64s option for loongarch64
// RUN: %clang -target loongarch64 %s -mabi=lp64s -### 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-LP64S %s
// check invalid -mabi=x option for loongarch64
// RUN: not %clang -target loongarch64 %s -mabi=x 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-X %s

// CHECK-LP64D: "-target-abi" "lp64d"
// CHECK-LP64F: "-target-abi" "lp64f"
// CHECK-LP64S: "-target-abi" "lp64s"
// CHECK-X: error: unknown target ABI 'x'
