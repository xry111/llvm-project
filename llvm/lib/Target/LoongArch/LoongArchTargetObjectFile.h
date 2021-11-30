//===-- llvm/Target/LoongArchTargetObjectFile.h - LoongArch Object Info ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_LOONGARCHTARGETOBJECTFILE_H
#define LLVM_LIB_TARGET_LOONGARCH_LOONGARCHTARGETOBJECTFILE_H

#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"

namespace llvm {
class LoongArchTargetMachine;
  class LoongArchTargetObjectFile : public TargetLoweringObjectFileELF {

  public:

    void Initialize(MCContext &Ctx, const TargetMachine &TM) override;
  };
} // end namespace llvm

#endif
