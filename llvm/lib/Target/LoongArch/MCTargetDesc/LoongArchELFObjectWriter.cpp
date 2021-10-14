//===-- LoongArchELFObjectWriter.cpp - LoongArch ELF Writer ---------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/LoongArchFixupKinds.h"
#include "MCTargetDesc/LoongArchMCExpr.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
class LoongArchELFObjectWriter : public MCELFObjectTargetWriter {
public:
  LoongArchELFObjectWriter(uint8_t OSABI, bool Is64Bit);

  ~LoongArchELFObjectWriter() override;

  // Return true if the given relocation must be with a symbol rather than
  // section plus offset.
  bool needsRelocateWithSymbol(const MCSymbol &Sym,
                               unsigned Type) const override {
    // TODO: this is very conservative, update once LoongArch psABI
    // requirements are clarified.
    return true;
  }

protected:
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const override;
};
} // namespace

LoongArchELFObjectWriter::LoongArchELFObjectWriter(uint8_t OSABI, bool Is64Bit)
    : MCELFObjectTargetWriter(Is64Bit, OSABI, ELF::EM_LOONGARCH,
                              /*HasRelocationAddend*/ true) {}

LoongArchELFObjectWriter::~LoongArchELFObjectWriter() {}

unsigned LoongArchELFObjectWriter::getRelocType(MCContext &Ctx,
                                                const MCValue &Target,
                                                const MCFixup &Fixup,
                                                bool IsPCRel) const {
  // Determine the type of the relocation
  unsigned Kind = Fixup.getTargetKind();
  if (Kind >= FirstLiteralRelocationKind)
    return Kind - FirstLiteralRelocationKind;

  switch (Kind) {
  case LoongArch::fixup_larch_add_8:
    return ELF::R_LARCH_ADD8;
  case LoongArch::fixup_larch_add_16:
    return ELF::R_LARCH_ADD16;
  case LoongArch::fixup_larch_add_32:
    return ELF::R_LARCH_ADD32;
  case LoongArch::fixup_larch_add_64:
    return ELF::R_LARCH_ADD64;
  case LoongArch::fixup_larch_sub_8:
    return ELF::R_LARCH_SUB8;
  case LoongArch::fixup_larch_sub_16:
    return ELF::R_LARCH_SUB16;
  case LoongArch::fixup_larch_sub_32:
    return ELF::R_LARCH_SUB32;
  case LoongArch::fixup_larch_sub_64:
    return ELF::R_LARCH_SUB64;
  }

  if (IsPCRel) {
    switch (Kind) {
    default:
      Ctx.reportError(Fixup.getLoc(), "Unsupported relocation type");
      return ELF::R_LARCH_NONE;
    case LoongArch::fixup_larch_sop_push_pcrel:
      return ELF::R_LARCH_SOP_PUSH_PCREL;
    case LoongArch::fixup_larch_sop_push_plt_pcrel:
      return ELF::R_LARCH_SOP_PUSH_PLT_PCREL;
    }
  }

  switch (Kind) {
  default:
    Ctx.reportError(Fixup.getLoc(), "Unsupported relocation type");
    return ELF::R_LARCH_NONE;
  case FK_Data_1:
    Ctx.reportError(Fixup.getLoc(), "1-byte data relocations not supported");
    return ELF::R_LARCH_NONE;
  case FK_Data_2:
    Ctx.reportError(Fixup.getLoc(), "2-byte data relocations not supported");
    return ELF::R_LARCH_NONE;
  case FK_Data_4:
    return ELF::R_LARCH_32;
  case FK_Data_8:
    return ELF::R_LARCH_64;
  case LoongArch::fixup_larch_sop_push_absolute:
    return ELF::R_LARCH_SOP_PUSH_ABSOLUTE;
  case LoongArch::fixup_larch_sop_push_gprel:
    return ELF::R_LARCH_SOP_PUSH_GPREL;
  case LoongArch::fixup_larch_sop_push_tls_tprel:
    return ELF::R_LARCH_SOP_PUSH_TLS_TPREL;
  case LoongArch::fixup_larch_sop_push_tls_got:
    return ELF::R_LARCH_SOP_PUSH_TLS_GOT;
  case LoongArch::fixup_larch_sop_push_tls_gd:
    return ELF::R_LARCH_SOP_PUSH_TLS_GD;
  case LoongArch::fixup_larch_sop_sub:
    return ELF::R_LARCH_SOP_SUB;
  case LoongArch::fixup_larch_sop_sl:
    return ELF::R_LARCH_SOP_SL;
  case LoongArch::fixup_larch_sop_sr:
    return ELF::R_LARCH_SOP_SR;
  case LoongArch::fixup_larch_sop_add:
    return ELF::R_LARCH_SOP_ADD;
  case LoongArch::fixup_larch_sop_and:
    return ELF::R_LARCH_SOP_AND;
  case LoongArch::fixup_larch_sop_pop_32_u_10_12:
    return ELF::R_LARCH_SOP_POP_32_U_10_12;
  case LoongArch::fixup_larch_sop_pop_32_s_10_12:
    return ELF::R_LARCH_SOP_POP_32_S_10_12;
  case LoongArch::fixup_larch_sop_pop_32_s_10_16_s2:
    return ELF::R_LARCH_SOP_POP_32_S_10_16_S2;
  case LoongArch::fixup_larch_sop_pop_32_s_5_20:
    return ELF::R_LARCH_SOP_POP_32_S_5_20;
  case LoongArch::fixup_larch_sop_pop_32_s_0_5_10_16_s2:
    return ELF::R_LARCH_SOP_POP_32_S_0_5_10_16_S2;
  case LoongArch::fixup_larch_sop_pop_32_s_0_10_10_16_s2:
    return ELF::R_LARCH_SOP_POP_32_S_0_10_10_16_S2;
  }
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createLoongArchELFObjectWriter(uint8_t OSABI, bool Is64Bit) {
  return std::make_unique<LoongArchELFObjectWriter>(OSABI, Is64Bit);
}
