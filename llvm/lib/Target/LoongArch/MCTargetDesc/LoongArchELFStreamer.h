//===-- LoongArchELFStreamer.h - LoongArch ELF Target Streamer ---------*- C++ -*--===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LoongArch_LoongArchELFSTREAMER_H
#define LLVM_LIB_TARGET_LoongArch_LoongArchELFSTREAMER_H

#include "LoongArchTargetStreamer.h"
#include "llvm/MC/MCELFStreamer.h"

namespace llvm {

class LoongArchTargetELFStreamer : public LoongArchTargetStreamer {
private:

public:
  MCELFStreamer &getStreamer();
  LoongArchTargetELFStreamer(MCStreamer &S, const MCSubtargetInfo &STI);

  void emitDirectiveOptionPush() override;
  void emitDirectiveOptionPop() override;
  void emitDirectiveOptionPIC() override;
  void emitDirectiveOptionNoPIC() override;
  void emitDirectiveOptionRVC() override;
  void emitDirectiveOptionNoRVC() override;
  void emitDirectiveOptionRelax() override;
  void emitDirectiveOptionNoRelax() override;
};

MCStreamer *createLoongArchELFStreamer(const Triple &T, MCContext &C,
                                       std::unique_ptr<MCAsmBackend> &&MAB,
                                       std::unique_ptr<MCObjectWriter> &&MOW,
                                       std::unique_ptr<MCCodeEmitter> &&MCE,
                                       bool RelaxAll);
}
#endif
