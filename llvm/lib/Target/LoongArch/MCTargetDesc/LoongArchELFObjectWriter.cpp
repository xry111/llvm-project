//===-- LoongArchELFObjectWriter.cpp - LoongArch ELF Writer -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/LoongArchFixupKinds.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <iterator>
#include <list>
#include <utility>

#define DEBUG_TYPE "loongarch-elf-object-writer"

using namespace llvm;

namespace {

class LoongArchELFObjectWriter : public MCELFObjectTargetWriter {
public:
  LoongArchELFObjectWriter(uint8_t OSABI, bool HasRelocationAddend, bool Is64);

  ~LoongArchELFObjectWriter() override = default;

  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const override;
  bool needsRelocateWithSymbol(const MCSymbol &Sym,
                               unsigned Type) const override {
    return true;
  }
};

} // end anonymous namespace

LoongArchELFObjectWriter::LoongArchELFObjectWriter(uint8_t OSABI,
                                         bool HasRelocationAddend, bool Is64)
    : MCELFObjectTargetWriter(Is64, OSABI, ELF::EM_LOONGARCH, HasRelocationAddend) {}

unsigned LoongArchELFObjectWriter::getRelocType(MCContext &Ctx,
                                           const MCValue &Target,
                                           const MCFixup &Fixup,
                                           bool IsPCRel) const {
  // Determine the type of the relocation.
  ///XXX:Reloc
  unsigned Kind = (unsigned)Fixup.getKind();

  switch (Kind) {
    default:
      return ELF::R_LARCH_NONE;
      //llvm_unreachable("invalid fixup kind!");
    case FK_Data_4:
    case LoongArch::fixup_LARCH_32:
      return ELF::R_LARCH_32;
    case FK_GPRel_4:
    case FK_Data_8:
    case LoongArch::fixup_LARCH_64:
      return ELF::R_LARCH_64;
    case LoongArch::fixup_LARCH_NONE:
      return ELF::R_LARCH_NONE;
    case LoongArch::fixup_LARCH_RELATIVE:
      return ELF::R_LARCH_RELATIVE;
    case LoongArch::fixup_LARCH_COPY:
      return ELF::R_LARCH_COPY;
    case LoongArch::fixup_LARCH_JUMP_SLOT:
      return ELF::R_LARCH_JUMP_SLOT;
    case LoongArch::fixup_LARCH_TLS_DTPMOD32:
      return ELF::R_LARCH_TLS_DTPMOD32;
    case LoongArch::fixup_LARCH_TLS_DTPMOD64:
      return ELF::R_LARCH_TLS_DTPMOD64;
    case LoongArch::fixup_LARCH_TLS_DTPREL32:
      return ELF::R_LARCH_TLS_DTPREL32;
    case LoongArch::fixup_LARCH_TLS_DTPREL64:
      return ELF::R_LARCH_TLS_DTPREL64;
    case LoongArch::fixup_LARCH_TLS_TPREL32:
      return ELF::R_LARCH_TLS_TPREL32;
    case LoongArch::fixup_LARCH_TLS_TPREL64:
      return ELF::R_LARCH_TLS_TPREL64;
    case LoongArch::fixup_LARCH_IRELATIVE:
      return ELF::R_LARCH_IRELATIVE;
    case LoongArch::fixup_LARCH_MARK_LA:
      return ELF::R_LARCH_MARK_LA;
    case LoongArch::fixup_LARCH_MARK_PCREL:
      return ELF::R_LARCH_MARK_PCREL;
    case LoongArch::fixup_LARCH_SOP_PUSH_PCREL:
      return ELF::R_LARCH_SOP_PUSH_PCREL;
    case LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE:
      return ELF::R_LARCH_SOP_PUSH_ABSOLUTE;
    case LoongArch::fixup_LARCH_SOP_PUSH_DUP:
      return ELF::R_LARCH_SOP_PUSH_DUP;
    case LoongArch::fixup_LARCH_SOP_PUSH_GPREL:
      return ELF::R_LARCH_SOP_PUSH_GPREL;
    case LoongArch::fixup_LARCH_SOP_PUSH_TLS_TPREL:
      return ELF::R_LARCH_SOP_PUSH_TLS_TPREL;
    case LoongArch::fixup_LARCH_SOP_PUSH_TLS_GOT:
      return ELF::R_LARCH_SOP_PUSH_TLS_GOT;
    case LoongArch::fixup_LARCH_SOP_PUSH_TLS_GD:
      return ELF::R_LARCH_SOP_PUSH_TLS_GD;
    case LoongArch::fixup_LARCH_SOP_PUSH_PLT_PCREL:
      return ELF::R_LARCH_SOP_PUSH_PLT_PCREL;
    case LoongArch::fixup_LARCH_SOP_ASSERT:
      return ELF::R_LARCH_SOP_ASSERT;
    case LoongArch::fixup_LARCH_SOP_NOT:
      return ELF::R_LARCH_SOP_NOT;
    case LoongArch::fixup_LARCH_SOP_SUB:
      return ELF::R_LARCH_SOP_SUB;
    case LoongArch::fixup_LARCH_SOP_SL:
      return ELF::R_LARCH_SOP_SL;
    case LoongArch::fixup_LARCH_SOP_SR:
      return ELF::R_LARCH_SOP_SR;
    case LoongArch::fixup_LARCH_SOP_ADD:
      return ELF::R_LARCH_SOP_ADD;
    case LoongArch::fixup_LARCH_SOP_AND:
      return ELF::R_LARCH_SOP_AND;
    case LoongArch::fixup_LARCH_SOP_IF_ELSE:
      return ELF::R_LARCH_SOP_IF_ELSE;
    case LoongArch::fixup_LARCH_SOP_POP_32_S_10_5:
      return ELF::R_LARCH_SOP_POP_32_S_10_5;
    case LoongArch::fixup_LARCH_SOP_POP_32_U_10_12:
      return ELF::R_LARCH_SOP_POP_32_U_10_12;
    case LoongArch::fixup_LARCH_SOP_POP_32_S_10_12:
      return ELF::R_LARCH_SOP_POP_32_S_10_12;
    case LoongArch::fixup_LARCH_SOP_POP_32_S_10_16:
      return ELF::R_LARCH_SOP_POP_32_S_10_16;
    case LoongArch::fixup_LARCH_SOP_POP_32_S_10_16_S2:
      return ELF::R_LARCH_SOP_POP_32_S_10_16_S2;
    case LoongArch::fixup_LARCH_SOP_POP_32_S_5_20:
      return ELF::R_LARCH_SOP_POP_32_S_5_20;
    case LoongArch::fixup_LARCH_SOP_POP_32_S_0_5_10_16_S2:
      return ELF::R_LARCH_SOP_POP_32_S_0_5_10_16_S2;
    case LoongArch::fixup_LARCH_SOP_POP_32_S_0_10_10_16_S2:
      return ELF::R_LARCH_SOP_POP_32_S_0_10_10_16_S2;
    case LoongArch::fixup_LARCH_SOP_POP_32_U:
      return ELF::R_LARCH_SOP_POP_32_U;
    case LoongArch::fixup_LARCH_ADD8:
      return ELF::R_LARCH_ADD8;
    case LoongArch::fixup_LARCH_ADD16:
      return ELF::R_LARCH_ADD16;
    case LoongArch::fixup_LARCH_ADD24:
      return ELF::R_LARCH_ADD24;
    case LoongArch::fixup_LARCH_ADD32:
      return ELF::R_LARCH_ADD32;
    case LoongArch::fixup_LARCH_ADD64:
      return ELF::R_LARCH_ADD64;
    case LoongArch::fixup_LARCH_SUB8:
      return ELF::R_LARCH_SUB8;
    case LoongArch::fixup_LARCH_SUB16:
      return ELF::R_LARCH_SUB16;
    case LoongArch::fixup_LARCH_SUB24:
      return ELF::R_LARCH_SUB24;
    case LoongArch::fixup_LARCH_SUB32:
      return ELF::R_LARCH_SUB32;
    case LoongArch::fixup_LARCH_SUB64:
      return ELF::R_LARCH_SUB64;
    case LoongArch::fixup_LARCH_GNU_VTINHERIT:
      return ELF::R_LARCH_GNU_VTINHERIT;
    case LoongArch::fixup_LARCH_GNU_VTENTRY:
      return ELF::R_LARCH_GNU_VTENTRY;
  }
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createLoongArchELFObjectWriter(const Triple &TT, bool IsLPX32) {
  uint8_t OSABI = MCELFObjectTargetWriter::getOSABI(TT.getOS());
  bool IsLP64 = TT.isArch64Bit() && !IsLPX32;
  bool HasRelocationAddend = TT.isArch64Bit();
  return std::make_unique<LoongArchELFObjectWriter>(OSABI, HasRelocationAddend,
                                                IsLP64);
}
