// REQUIRES: loongarch-registered-target
// RUN: %clang_cc1 %s -fsyntax-only -triple loongarch64-linux -target-feature +lasx
// RUN: %clang_cc1 %s -fsyntax-only -triple loongarch64-linux -target-feature +lasx -flax-vector-conversions=none
// RUN: %clang_cc1 %s -fsyntax-only -triple loongarch64-linux -target-feature +lasx -flax-vector-conversions=none -fno-signed-char

#include <lasxintrin.h>
