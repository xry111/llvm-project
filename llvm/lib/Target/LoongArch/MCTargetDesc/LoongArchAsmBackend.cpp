//===-- LoongArchAsmBackend.cpp - LoongArch Asm Backend  ----------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the LoongArchAsmBackend class.
//
//===----------------------------------------------------------------------===//
//

#include "MCTargetDesc/LoongArchAsmBackend.h"
#include "MCTargetDesc/LoongArchABIInfo.h"
#include "MCTargetDesc/LoongArchFixupKinds.h"
#include "MCTargetDesc/LoongArchMCExpr.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCFixupKindInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

// Prepare value for the target space for it
static unsigned adjustFixupValue(const MCFixup &Fixup, uint64_t Value,
                                 MCContext &Ctx) {

  unsigned Kind = Fixup.getKind();

  // TODO: reloc
  switch (Kind) {
  default:
    return 0;
  case FK_Data_2:
    Value &= 0xffff;
    break;
  case FK_Data_4:
  case FK_Data_8:
    break;
  }

  return Value;
}

std::unique_ptr<MCObjectTargetWriter>
LoongArchAsmBackend::createObjectTargetWriter() const {
  return createLoongArchELFObjectWriter(TheTriple, IsLPX32);
}

/// ApplyFixup - Apply the \p Value for given \p Fixup into the provided
/// data fragment, at the offset specified by the fixup and following the
/// fixup kind as appropriate.
void LoongArchAsmBackend::applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                                const MCValue &Target,
                                MutableArrayRef<char> Data, uint64_t Value,
                                bool IsResolved,
                                const MCSubtargetInfo *STI) const {
  MCFixupKind Kind = Fixup.getKind();
  MCContext &Ctx = Asm.getContext();
  Value = adjustFixupValue(Fixup, Value, Ctx);

  if (!Value)
    return; // Doesn't change encoding.

  // Where do we start in the object
  unsigned Offset = Fixup.getOffset();
  // Number of bytes we need to fixup
  unsigned NumBytes = (getFixupKindInfo(Kind).TargetSize + 7) / 8;


  // Grab current value, if any, from bits.
  uint64_t CurVal = 0;

  for (unsigned i = 0; i != NumBytes; ++i)
    CurVal |= (uint64_t)((uint8_t)Data[Offset + i]) << (i*8);

  uint64_t Mask = ((uint64_t)(-1) >>
                    (64 - getFixupKindInfo(Kind).TargetSize));
  CurVal |= Value & Mask;

  // Write out the fixed up bytes back to the code/data bits.
  for (unsigned i = 0; i != NumBytes; ++i)
    Data[Offset + i] = (uint8_t)((CurVal >> (i*8)) & 0xff);
}

Optional<MCFixupKind> LoongArchAsmBackend::getFixupKind(StringRef Name) const {
  return StringSwitch<Optional<MCFixupKind>>(Name)
      .Case("R_LARCH_NONE", (MCFixupKind)LoongArch::fixup_LARCH_NONE)
      .Case("R_LARCH_32", FK_Data_4)
      .Case("R_LARCH_64", FK_Data_8)
      .Default(MCAsmBackend::getFixupKind(Name));
}

const MCFixupKindInfo &LoongArchAsmBackend::
getFixupKindInfo(MCFixupKind Kind) const {
  const static MCFixupKindInfo Infos[] = {
    // This table *must* be in same the order of fixup_* kinds in
    // LoongArchFixupKinds.h.
    //
    // name                    offset  bits  flags
    { "fixup_LARCH_NONE",         0,      0,   0 },
    { "fixup_LARCH_SOP_PUSH_ABSOLUTE",	0,	0,	0},
    { "fixup_LARCH_SOP_PUSH_PCREL",	0,	0,	0},
    { "fixup_LARCH_SOP_PUSH_GPREL",	0,	0,	0},
    { "fixup_LARCH_SOP_PUSH_TLS_TPREL",	0,	0,	0},
    { "fixup_LARCH_SOP_PUSH_TLS_GOT",	0,	0,	0},
    { "fixup_LARCH_SOP_PUSH_TLS_GD",	0,	0,	0},
    { "fixup_LARCH_SOP_PUSH_PLT_PCREL",	0,	0,	0},
    { "fixup_LARCH_32",	0,	0,	0},
    { "fixup_LARCH_64",	0,	0,	0},
    { "fixup_LARCH_RELATIVE",	0,	0,	0},
    { "fixup_LARCH_COPY",	0,	0,	0},
    { "fixup_LARCH_JUMP_SLOT",	0,	0,	0},
    { "fixup_LARCH_TLS_DTPMOD32",	0,	0,	0},
    { "fixup_LARCH_TLS_DTPMOD64",	0,	0,	0},
    { "fixup_LARCH_TLS_DTPREL32",	0,	0,	0},
    { "fixup_LARCH_TLS_DTPREL64",	0,	0,	0},
    { "fixup_LARCH_TLS_TPREL32",	0,	0,	0},
    { "fixup_LARCH_TLS_TPREL64",	0,	0,	0},
    { "fixup_LARCH_IRELATIVE",	0,	0,	0},
    { "fixup_LARCH_MARK_LA",	0,	0,	0},
    { "fixup_LARCH_MARK_PCREL",	0,	0,	0},
    { "fixup_LARCH_SOP_PUSH_DUP",	0,	0,	0},
    { "fixup_LARCH_SOP_ASSERT",	0,	0,	0},
    { "fixup_LARCH_SOP_NOT",	0,	0,	0},
    { "fixup_LARCH_SOP_SUB",	0,	0,	0},
    { "fixup_LARCH_SOP_SL",	0,	0,	0},
    { "fixup_LARCH_SOP_SR",	0,	0,	0},
    { "fixup_LARCH_SOP_ADD",	0,	0,	0},
    { "fixup_LARCH_SOP_AND",	0,	0,	0},
    { "fixup_LARCH_SOP_IF_ELSE",	0,	0,	0},
    { "fixup_LARCH_SOP_POP_32_S_10_5",	0,	0,	0},
    { "fixup_LARCH_SOP_POP_32_U_10_12",	0,	0,	0},
    { "fixup_LARCH_SOP_POP_32_S_10_12",	0,	0,	0},
    { "fixup_LARCH_SOP_POP_32_S_10_16",	0,	0,	0},
    { "fixup_LARCH_SOP_POP_32_S_10_16_S2",	0,	0,	0},
    { "fixup_LARCH_SOP_POP_32_S_5_20",	0,	0,	0},
    { "fixup_LARCH_SOP_POP_32_S_0_5_10_16_S2",	0,	0,	0},
    { "fixup_LARCH_SOP_POP_32_S_0_10_10_16_S2",	0,	0,	0},
    { "fixup_LARCH_SOP_POP_32_U",	0,	0,	0},
    { "fixup_LARCH_ADD8",	0,	0,	0},
    { "fixup_LARCH_ADD16",	0,	0,	0},
    { "fixup_LARCH_ADD24",	0,	0,	0},
    { "fixup_LARCH_ADD32",	0,	0,	0},
    { "fixup_LARCH_ADD64",	0,	0,	0},
    { "fixup_LARCH_SUB8",	0,	0,	0},
    { "fixup_LARCH_SUB16",	0,	0,	0},
    { "fixup_LARCH_SUB24",	0,	0,	0},
    { "fixup_LARCH_SUB32",	0,	0,	0},
    { "fixup_LARCH_SUB64",	0,	0,	0},
  };

  if (Kind < FirstTargetFixupKind)
    return MCAsmBackend::getFixupKindInfo(Kind);

  assert(unsigned(Kind - FirstTargetFixupKind) < getNumFixupKinds() &&
          "Invalid kind!");

  return Infos[Kind - FirstTargetFixupKind];
}

/// WriteNopData - Write an (optimal) nop sequence of Count bytes
/// to the given output. If the target cannot generate such a sequence,
/// it should return an error.
///
/// \return - True on success.
bool LoongArchAsmBackend::writeNopData(raw_ostream &OS, uint64_t Count) const {
  // Check for a less than instruction size number of bytes
  if ((Count % 4) != 0)
    return false;

  // The nop on LoongArch is andi r0, r0, 0.
  for (; Count >= 4; Count -= 4)
    OS.write("\x03\x40\0\0", 4);

  return true;
}

bool LoongArchAsmBackend::shouldForceRelocation(const MCAssembler &Asm,
                                           const MCFixup &Fixup,
                                           const MCValue &Target) {
  const unsigned FixupKind = Fixup.getKind();
  switch (FixupKind) {
  default:
    return false;
  // All these relocations require special processing
  // at linking time. Delegate this work to a linker.
  case LoongArch::fixup_LARCH_SOP_PUSH_PLT_PCREL:
  case LoongArch::fixup_LARCH_SOP_PUSH_PCREL:
  case LoongArch::fixup_LARCH_SOP_PUSH_GPREL:
  case LoongArch::fixup_LARCH_SOP_PUSH_TLS_GD:
  case LoongArch::fixup_LARCH_SOP_PUSH_TLS_GOT:
  case LoongArch::fixup_LARCH_SOP_PUSH_TLS_TPREL:
  case LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE:
  case LoongArch::fixup_LARCH_SOP_IF_ELSE:
  case LoongArch::fixup_LARCH_SOP_ADD:
  case LoongArch::fixup_LARCH_SOP_SUB:
  case LoongArch::fixup_LARCH_SOP_AND:
  case LoongArch::fixup_LARCH_SOP_SL:
  case LoongArch::fixup_LARCH_SOP_SR:
  case LoongArch::fixup_LARCH_SOP_POP_32_S_10_5:
  case LoongArch::fixup_LARCH_SOP_POP_32_S_5_20:
  case LoongArch::fixup_LARCH_SOP_POP_32_S_10_12:
  case LoongArch::fixup_LARCH_SOP_POP_32_U_10_12:
  case LoongArch::fixup_LARCH_SOP_POP_32_S_10_16_S2:
  case LoongArch::fixup_LARCH_SOP_POP_32_S_0_5_10_16_S2:
  case LoongArch::fixup_LARCH_SOP_POP_32_S_0_10_10_16_S2:
    return true;
  }
}

MCAsmBackend *llvm::createLoongArchAsmBackend(const Target &T,
                                              const MCSubtargetInfo &STI,
                                              const MCRegisterInfo &MRI,
                                              const MCTargetOptions &Options) {
  LoongArchABIInfo ABI = LoongArchABIInfo::computeTargetABI(
                           STI.getTargetTriple(), STI.getCPU(), Options);
  return new LoongArchAsmBackend(T, MRI, STI.getTargetTriple(), STI.getCPU(),
                                 ABI.IsLPX32());
}
