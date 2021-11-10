//===-- LoongArchELFStreamer.cpp - LoongArch ELF Target Streamer Methods --===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides LoongArch specific target streamer methods.
//
//===----------------------------------------------------------------------===//

#include "LoongArchELFStreamer.h"
#include "LoongArchAsmBackend.h"
#include "LoongArchBaseInfo.h"
#include "LoongArchMCTargetDesc.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/LEB128.h"

using namespace llvm;

// This part is for ELF object output.
LoongArchTargetELFStreamer::LoongArchTargetELFStreamer(
    MCStreamer &S, const MCSubtargetInfo &STI)
    : LoongArchTargetStreamer(S) {
  MCAssembler &MCA = getStreamer().getAssembler();
  auto &MAB = static_cast<LoongArchAsmBackend &>(MCA.getBackend());
  LoongArchABI::ABI ABI = MAB.getTargetABI();
  assert(ABI != LoongArchABI::ABI_Unknown &&
         "Improperly initialised target ABI");

  unsigned EFlags = MCA.getELFHeaderEFlags();

  switch (ABI) {
  case LoongArchABI::ABI_LP64D:
    EFlags = 0x3;
    break;
  case LoongArchABI::ABI_LP64F:
    EFlags = 0x2;
    break;
  case LoongArchABI::ABI_LP64S:
    EFlags = 0x1;
    break;
  case LoongArchABI::ABI_ILP32D:
    EFlags = 0x7;
    break;
  case LoongArchABI::ABI_ILP32F:
    EFlags = 0x6;
    break;
  case LoongArchABI::ABI_ILP32S:
    EFlags = 0x5;
    break;
  case LoongArchABI::ABI_Unknown:
    llvm_unreachable("Improperly initialised target ABI");
  }

  MCA.setELFHeaderEFlags(EFlags);
}

MCELFStreamer &LoongArchTargetELFStreamer::getStreamer() {
  return static_cast<MCELFStreamer &>(Streamer);
}

void LoongArchTargetELFStreamer::emitDirectiveOptionPush() {}
void LoongArchTargetELFStreamer::emitDirectiveOptionPop() {}
void LoongArchTargetELFStreamer::emitDirectiveOptionPIC() {}
void LoongArchTargetELFStreamer::emitDirectiveOptionNoPIC() {}
void LoongArchTargetELFStreamer::emitDirectiveOptionRVC() {}
void LoongArchTargetELFStreamer::emitDirectiveOptionNoRVC() {}
void LoongArchTargetELFStreamer::emitDirectiveOptionRelax() {}
void LoongArchTargetELFStreamer::emitDirectiveOptionNoRelax() {}

namespace {
class LoongArchELFStreamer : public MCELFStreamer {
  static std::pair<unsigned, unsigned> getRelocPairForSize(unsigned Size) {
    switch (Size) {
    default:
      llvm_unreachable("unsupported fixup size");
    case 1:
      return std::make_pair(LoongArch::fixup_larch_add_8,
                            LoongArch::fixup_larch_sub_8);
    case 2:
      return std::make_pair(LoongArch::fixup_larch_add_16,
                            LoongArch::fixup_larch_sub_16);
    case 4:
      return std::make_pair(LoongArch::fixup_larch_add_32,
                            LoongArch::fixup_larch_sub_32);
    case 8:
      return std::make_pair(LoongArch::fixup_larch_add_64,
                            LoongArch::fixup_larch_sub_64);
    }
  }

  static bool requiresFixups(MCContext &C, const MCExpr *Value,
                             const MCExpr *&LHS, const MCExpr *&RHS) {
    const auto *MBE = dyn_cast<MCBinaryExpr>(Value);
    if (MBE == nullptr)
      return false;

    MCValue E;
    if (!Value->evaluateAsRelocatable(E, nullptr, nullptr))
      return false;
    if (E.getSymA() == nullptr || E.getSymB() == nullptr)
      return false;

    const auto &A = E.getSymA()->getSymbol();
    const auto &B = E.getSymB()->getSymbol();

    LHS =
        MCBinaryExpr::create(MCBinaryExpr::Add, MCSymbolRefExpr::create(&A, C),
                             MCConstantExpr::create(E.getConstant(), C), C);
    RHS = E.getSymB();

    return (A.isInSection() ? A.getSection().hasInstructions()
                            : !A.getName().empty()) ||
           (B.isInSection() ? B.getSection().hasInstructions()
                            : !B.getName().empty());
  }

public:
  LoongArchELFStreamer(MCContext &C, std::unique_ptr<MCAsmBackend> MAB,
                       std::unique_ptr<MCObjectWriter> MOW,
                       std::unique_ptr<MCCodeEmitter> MCE)
      : MCELFStreamer(C, std::move(MAB), std::move(MOW), std::move(MCE)) {}
  void emitValueImpl(const MCExpr *Value, unsigned Size, SMLoc Loc) override;
};

void LoongArchELFStreamer::emitValueImpl(const MCExpr *Value, unsigned Size,
                                         SMLoc Loc) {
  const MCExpr *A, *B;
  if (!requiresFixups(getContext(), Value, A, B))
    return MCELFStreamer::emitValueImpl(Value, Size, Loc);

  MCStreamer::emitValueImpl(Value, Size, Loc);

  MCDataFragment *DF = getOrCreateDataFragment();
  flushPendingLabels(DF, DF->getContents().size());
  MCDwarfLineEntry::make(this, getCurrentSectionOnly());

  unsigned Add, Sub;
  std::tie(Add, Sub) = getRelocPairForSize(Size);

  DF->getFixups().push_back(MCFixup::create(
      DF->getContents().size(), A, static_cast<MCFixupKind>(Add), Loc));
  DF->getFixups().push_back(MCFixup::create(
      DF->getContents().size(), B, static_cast<MCFixupKind>(Sub), Loc));

  DF->getContents().resize(DF->getContents().size() + Size, 0);
}
} // namespace

namespace llvm {
MCStreamer *createLoongArchELFStreamer(const Triple &T, MCContext &C,
                                       std::unique_ptr<MCAsmBackend> &&MAB,
                                       std::unique_ptr<MCObjectWriter> &&MOW,
                                       std::unique_ptr<MCCodeEmitter> &&MCE,
                                       bool RelaxAll) {
  LoongArchELFStreamer *S = new LoongArchELFStreamer(
      C, std::move(MAB), std::move(MOW), std::move(MCE));
  S->getAssembler().setRelaxAll(RelaxAll);
  return S;
}
} // namespace llvm
