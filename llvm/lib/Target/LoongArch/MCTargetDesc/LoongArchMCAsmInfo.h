//===-- LoongArchMCAsmInfo.h - LoongArch Asm Info ----------------------*- C++ -*--===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the LoongArchMCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LoongArch_MCTARGETDESC_LoongArchMCASMINFO_H
#define LLVM_LIB_TARGET_LoongArch_MCTARGETDESC_LoongArchMCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {
class Triple;

class LoongArchMCAsmInfo : public MCAsmInfoELF {
  void anchor() override;

public:
  explicit LoongArchMCAsmInfo(const Triple &TargetTriple);

  const MCExpr *getExprForFDESymbol(const MCSymbol *Sym, unsigned Encoding,
                                    MCStreamer &Streamer) const override;
};

} // namespace llvm

#endif
