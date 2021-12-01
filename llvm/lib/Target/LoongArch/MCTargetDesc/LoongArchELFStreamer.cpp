//===-------- LoongArchELFStreamer.cpp - ELF Object Output ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "LoongArchELFStreamer.h"
#include "LoongArchFixupKinds.h"
#include "LoongArchTargetStreamer.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDwarf.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/Casting.h"

using namespace llvm;

static std::pair<unsigned, unsigned> getRelocPairForSize(unsigned Size) {
  switch (Size) {
  default:
    llvm_unreachable("unsupported fixup size");
  case 1:
    return std::make_pair(LoongArch::fixup_LARCH_ADD8,
                          LoongArch::fixup_LARCH_SUB8);
  case 2:
    return std::make_pair(LoongArch::fixup_LARCH_ADD16,
                          LoongArch::fixup_LARCH_SUB16);
  case 4:
    return std::make_pair(LoongArch::fixup_LARCH_ADD32,
                          LoongArch::fixup_LARCH_SUB32);
  case 8:
    return std::make_pair(LoongArch::fixup_LARCH_ADD64,
                          LoongArch::fixup_LARCH_SUB64);
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

  if (A.getName().empty() && B.getName().empty())
    return false;

  if (!A.isInSection() && !B.isInSection() &&
      !A.getName().empty() && !B.getName().empty())
    return false;

  LHS =
      MCBinaryExpr::create(MCBinaryExpr::Add, MCSymbolRefExpr::create(&A, C),
                           MCConstantExpr::create(E.getConstant(), C), C);
  RHS = E.getSymB();

  return (A.isInSection() ? A.getSection().hasInstructions()
                          : !A.getName().empty()) ||
         (B.isInSection() ? B.getSection().hasInstructions()
                          : !B.getName().empty());
}


LoongArchELFStreamer::LoongArchELFStreamer(MCContext &Context,
                                 std::unique_ptr<MCAsmBackend> MAB,
                                 std::unique_ptr<MCObjectWriter> OW,
                                 std::unique_ptr<MCCodeEmitter> Emitter)
    : MCELFStreamer(Context, std::move(MAB), std::move(OW),
                    std::move(Emitter)) {
  }

void LoongArchELFStreamer::emitCFIStartProcImpl(MCDwarfFrameInfo &Frame) {
  Frame.Begin = getContext().createTempSymbol();
  MCELFStreamer::emitLabel(Frame.Begin);
}

MCSymbol *LoongArchELFStreamer::emitCFILabel() {
  MCSymbol *Label = getContext().createTempSymbol("cfi", true);
  MCELFStreamer::emitLabel(Label);
  return Label;
}

void LoongArchELFStreamer::emitCFIEndProcImpl(MCDwarfFrameInfo &Frame) {
  Frame.End = getContext().createTempSymbol();
  MCELFStreamer::emitLabel(Frame.End);
}

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

MCELFStreamer *llvm::createLoongArchELFStreamer(
    MCContext &Context, std::unique_ptr<MCAsmBackend> MAB,
    std::unique_ptr<MCObjectWriter> OW, std::unique_ptr<MCCodeEmitter> Emitter,
    bool RelaxAll) {
  return new LoongArchELFStreamer(Context, std::move(MAB), std::move(OW),
                             std::move(Emitter));
}
