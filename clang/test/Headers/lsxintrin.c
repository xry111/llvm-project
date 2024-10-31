// REQUIRES: loongarch-registered-target
// RUN: %clang_cc1 %s -fsyntax-only -triple loongarch64-linux -target-feature +lsx
// RUN: %clang_cc1 %s -fsyntax-only -triple loongarch64-linux -target-feature +lsx -flax-vector-conversions=none
// RUN: %clang_cc1 %s -fsyntax-only -triple loongarch64-linux -target-feature +lsx -flax-vector-conversions=none -fno-signed-char

#include <lsxintrin.h>

static int dummy;
