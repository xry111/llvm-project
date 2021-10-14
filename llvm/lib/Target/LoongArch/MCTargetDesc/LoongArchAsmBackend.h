//===-- LoongArchAsmBackend.h - LoongArch Assembler Backend -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LoongArch_MCTARGETDESC_LoongArchASMBACKEND_H
#define LLVM_LIB_TARGET_LoongArch_MCTARGETDESC_LoongArchASMBACKEND_H

#include "MCTargetDesc/LoongArchBaseInfo.h"
#include "MCTargetDesc/LoongArchFixupKinds.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"

namespace llvm {
class MCAssembler;
class MCObjectTargetWriter;
class raw_ostream;

enum LoongArchRelType { LARCH_NONE };

class LoongArchAsmBackend : public MCAsmBackend {
  const MCSubtargetInfo &STI;
  uint8_t OSABI;
  bool Is64Bit;
  bool ForceRelocs = false;
  const MCTargetOptions &TargetOptions;
  LoongArchABI::ABI TargetABI = LoongArchABI::ABI_Unknown;

  virtual void anchor();

public:
  LoongArchAsmBackend(const MCSubtargetInfo &STI, uint8_t OSABI, bool Is64Bit,
                  const MCTargetOptions &Options)
      : MCAsmBackend(support::little), STI(STI), OSABI(OSABI), Is64Bit(Is64Bit),
        TargetOptions(Options) {
    TargetABI = LoongArchABI::computeTargetABI(
        STI.getTargetTriple(), STI.getFeatureBits(), Options.getABIName());
    LoongArchFeatures::validate(STI.getTargetTriple(), STI.getFeatureBits());
  }
  ~LoongArchAsmBackend() override {}

  void setForceRelocs() { ForceRelocs = true; }

  // Returns true if relocations will be forced for shouldForceRelocation by
  // default. This will be true if relaxation is enabled or had previously
  // been enabled.
  bool willForceRelocations() const {
    return ForceRelocs;
  }

  std::unique_ptr<MCObjectTargetWriter>
  createObjectTargetWriter() const override;

  unsigned getNumFixupKinds() const override {
	  return LoongArch::NumTargetFixupKinds;
  }

  bool fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                            const MCRelaxableFragment *DF,
                            const MCAsmLayout &Layout) const override {
    return 0;
  }

  bool shouldForceRelocation(const MCAssembler &Asm, const MCFixup &Fixup,
                             const MCValue &Target) override;

  void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                  const MCValue &Target,
                  MutableArrayRef<char> Data, uint64_t Value,
                  bool IsResolved,
                  const MCSubtargetInfo *STI) const override;

  const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override;

  bool writeNopData(raw_ostream &OS, uint64_t Count) const override;

  const MCTargetOptions &getTargetOptions() const { return TargetOptions; }
  LoongArchABI::ABI getTargetABI() const { return TargetABI; }
};
}

#endif
