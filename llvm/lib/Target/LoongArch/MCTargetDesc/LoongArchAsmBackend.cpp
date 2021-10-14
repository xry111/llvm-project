//===-- LoongArchAsmBackend.cpp - LoongArch Assembler Backend -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "LoongArchAsmBackend.h"
#include "LoongArchMCExpr.h"
#include "llvm/ADT/APInt.h"
#include "llvm/MC/MCAsmLayout.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

void LoongArchAsmBackend::anchor() {}

bool LoongArchAsmBackend::writeNopData(raw_ostream &OS, uint64_t Count) const {
  unsigned MinNopLen = 4;

  if ((Count % MinNopLen) != 0)
    return false;

  // The canonical nop on RISC-V is addi x0, x0, 0.
  for (; Count >= 4; Count -= 4)
    OS.write("\x13\0\0\0", 4);

  return true;
}

MCAsmBackend *llvm::createLoongArchAsmBackend(const Target &T,
                                              const MCSubtargetInfo &STI,
                                              const MCRegisterInfo &MRI,
                                              const MCTargetOptions &Options) {
  const Triple &TT = STI.getTargetTriple();
  uint8_t OSABI = MCELFObjectTargetWriter::getOSABI(TT.getOS());
  return new LoongArchAsmBackend(STI, OSABI, TT.isArch64Bit(), Options);
}

const MCFixupKindInfo &
LoongArchAsmBackend::getFixupKindInfo(MCFixupKind Kind) const {
  const static MCFixupKindInfo Infos[] = {
      {"fixup_larch_sop_push_pcrel", 0, 0, MCFixupKindInfo::FKF_IsPCRel},
      {"fixup_larch_sop_push_absolute", 0, 0, 0},
      {"fixup_larch_sop_push_gprel", 0, 0, 0},
      {"fixup_larch_sop_push_tls_tprel", 0, 0, 0},
      {"fixup_larch_sop_push_tls_got", 0, 0, 0},
      {"fixup_larch_sop_push_tls_gd", 0, 0, 0},
      {"fixup_larch_sop_push_plt_pcrel", 0, 0, MCFixupKindInfo::FKF_IsPCRel},
      {"fixup_larch_sop_sub", 0, 0, 0},
      {"fixup_larch_sop_sl", 0, 0, 0},
      {"fixup_larch_sop_sr", 0, 0, 0},
      {"fixup_larch_sop_add", 0, 0, 0},
      {"fixup_larch_sop_and", 0, 0, 0},
      {"fixup_larch_sop_pop_32_u_10_12", 10, 12, 0},
      {"fixup_larch_sop_pop_32_s_10_12", 10, 12, 0},
      {"fixup_larch_sop_pop_32_s_10_16_s2", 6, 16, 0},
      {"fixup_larch_sop_pop_32_s_5_20", 7, 20, 0},
      {"fixup_larch_sop_pop_32_s_0_5_10_16_s2", 6, 26, 0},
      {"fixup_larch_sop_pop_32_s_0_10_10_16_s2", 6, 26, 0},
      {"fixup_larch_add8", 0, 8, 0},
      {"fixup_larch_add16", 0, 16, 0},
      {"fixup_larch_add32", 0, 32, 0},
      {"fixup_larch_add64", 0, 64, 0},
      {"fixup_larch_sub8", 0, 8, 0},
      {"fixup_larch_sub16", 0, 16, 0},
      {"fixup_larch_sub32", 0, 32, 0},
      {"fixup_larch_sub64", 0, 64, 0},
  };

  static_assert((array_lengthof(Infos)) == LoongArch::NumTargetFixupKinds,
                "Not all fixup kinds added to Infos array");
  if (Kind >= FirstLiteralRelocationKind)
    return MCAsmBackend::getFixupKindInfo(FK_NONE);

  if (Kind < FirstTargetFixupKind)
    return MCAsmBackend::getFixupKindInfo(Kind);

  assert(unsigned(Kind - FirstTargetFixupKind) < getNumFixupKinds() &&
         "Invalid kind!");
  return Infos[Kind - FirstTargetFixupKind];
}

std::unique_ptr<MCObjectTargetWriter>
LoongArchAsmBackend::createObjectTargetWriter() const {
  return createLoongArchELFObjectWriter(OSABI, Is64Bit);
}

bool LoongArchAsmBackend::shouldForceRelocation(const MCAssembler &Asm,
                                                const MCFixup &Fixup,
                                                const MCValue &Target) {
  switch (Fixup.getTargetKind()) {
  default:
    break;
  case FK_Data_1:
  case FK_Data_2:
  case FK_Data_4:
  case FK_Data_8:
    if (Target.isAbsolute())
      return false;
  }

  return true;
}

void LoongArchAsmBackend::applyFixup(const MCAssembler &Asm,
                                     const MCFixup &Fixup,
                                     const MCValue &Target,
                                     MutableArrayRef<char> Data, uint64_t Value,
                                     bool IsResolved,
                                     const MCSubtargetInfo *STI) const {
  MCFixupKind Kind = Fixup.getKind();
  if (Kind >= FirstLiteralRelocationKind)
    return;
  MCFixupKindInfo Info = getFixupKindInfo(Kind);
  if (!Value)
    return; // Doesn't change encoding.

  switch (Kind) {
  default:
    llvm_unreachable("unexpected fixup kind to be applied");
  case FK_Data_1:
  case FK_Data_2:
  case FK_Data_4:
  case FK_Data_8:
    break;
  };

  // Shift the value into position.
  Value <<= Info.TargetOffset;

  unsigned Offset = Fixup.getOffset();
  unsigned NumBytes = alignTo(Info.TargetSize + Info.TargetOffset, 8) / 8;

  assert(Offset + NumBytes <= Data.size() && "Invalid fixup offset!");

  // For each byte of the fragment that the fixup touches, mask in the
  // bits from the fixup value.
  for (unsigned i = 0; i != NumBytes; ++i) {
    Data[Offset + i] |= uint8_t((Value >> (i * 8)) & 0xff);
  }
}
