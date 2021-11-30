//===-- LoongArchTargetInfo.cpp - LoongArch Target Implementation -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "TargetInfo/LoongArchTargetInfo.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

Target &llvm::getTheLoongArch32Target() {
  static Target TheLoongArch32Target;
  return TheLoongArch32Target;
}

Target &llvm::getTheLoongArch64Target() {
  static Target TheLoongArch64Target;
  return TheLoongArch64Target;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeLoongArchTargetInfo() {
#if 0
  //TODO: support it in futrue
  RegisterTarget<Triple::loongarch32,
                 /*HasJIT=*/false>
      X(getTheLoongArch32Target(), "loongarch32", "LoongArch (32-bit)", "LoongArch");
#endif
  RegisterTarget<Triple::loongarch64,
                 /*HasJIT=*/false>
      A(getTheLoongArch64Target(), "loongarch64", "LoongArch (64-bit)", "LoongArch");
}
