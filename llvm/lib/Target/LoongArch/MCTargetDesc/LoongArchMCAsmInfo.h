//===-- LoongArchMCAsmInfo.h - LoongArch Asm Info ------------------------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the LoongArchMCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHMCASMINFO_H
#define LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHMCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {
class Triple;

class LoongArchMCAsmInfo : public MCAsmInfoELF {
  void anchor() override;

public:
  explicit LoongArchMCAsmInfo(const Triple &TheTriple,
                              const MCTargetOptions &Options);
};

} // namespace llvm

#endif
