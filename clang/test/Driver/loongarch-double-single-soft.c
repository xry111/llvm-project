// Check passing -m*-float options to the backend.

// RUN: %clang -target loongarch64 %s -mdouble-float -### 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-DOUBLE %s
// RUN: %clang -target loongarch64 %s -msingle-float -### 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-SINGLE %s
// RUN: %clang -target loongarch64 %s -msoft-float -### 2>&1 \
// RUN:   | FileCheck --check-prefix=CHECK-SOFT %s

// CHECK-DOUBLE: "-target-feature" "+d" "-target-abi" "lp64d"
// CHECK-SINGLE: "-target-feature" "+f" "-target-abi" "lp64f"
// CHECK-SOFT: "-target-feature" "-f" "-target-feature" "-d" "-target-abi" "lp64s"
