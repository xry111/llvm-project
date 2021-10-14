//=== LoongArchMCCodeEmitter.cpp - Convert LoongArch code to machine code -===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the LoongArchMCCodeEmitter class.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/LoongArchBaseInfo.h"
#include "MCTargetDesc/LoongArchFixupKinds.h"
#include "MCTargetDesc/LoongArchMCExpr.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstBuilder.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "mccodeemitter"

STATISTIC(MCNumEmitted, "Number of MC instructions emitted");
STATISTIC(MCNumFixups, "Number of MC fixups created");

namespace {
class LoongArchMCCodeEmitter : public MCCodeEmitter {
  LoongArchMCCodeEmitter(const LoongArchMCCodeEmitter &) = delete;
  void operator=(const LoongArchMCCodeEmitter &) = delete;
  MCContext &Ctx;
  MCInstrInfo const &MCII;

public:
  LoongArchMCCodeEmitter(MCContext &ctx, MCInstrInfo const &MCII)
      : Ctx(ctx), MCII(MCII) {}

  ~LoongArchMCCodeEmitter() override {}

  void encodeInstruction(const MCInst &MI, raw_ostream &OS,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const override;

  void expandFunctionCall(const MCInst &MI, raw_ostream &OS,
                          SmallVectorImpl<MCFixup> &Fixups,
                          const MCSubtargetInfo &STI) const;

  void expandAddTPRel(const MCInst &MI, raw_ostream &OS,
                      SmallVectorImpl<MCFixup> &Fixups,
                      const MCSubtargetInfo &STI) const;

  void expandLoadAddr(const MCInst &MI, raw_ostream &OS,
                      SmallVectorImpl<MCFixup> &Fixups,
                      const MCSubtargetInfo &STI) const;

  int expandLoadAddrTprel(const MCInst &MI, raw_ostream &OS,
                          SmallVectorImpl<MCFixup> &Fixups,
                          const MCSubtargetInfo &STI) const;

  /// TableGen'erated function for getting the binary encoding for an
  /// instruction.
  uint64_t getBinaryCodeForInstr(const MCInst &MI,
                                 SmallVectorImpl<MCFixup> &Fixups,
                                 const MCSubtargetInfo &STI) const;

  /// Return binary encoding of operand. If the machine operand requires
  /// relocation, record the relocation and return zero.
  unsigned getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                             SmallVectorImpl<MCFixup> &Fixups,
                             const MCSubtargetInfo &STI) const;

  unsigned getImmOpValueAsr2(const MCInst &MI, unsigned OpNo,
                             SmallVectorImpl<MCFixup> &Fixups,
                             const MCSubtargetInfo &STI) const;

  unsigned getImmOpValueMinus1(const MCInst &MI, unsigned OpNo,
                               SmallVectorImpl<MCFixup> &Fixups,
                               const MCSubtargetInfo &STI) const;

  unsigned getImmOpValue(const MCInst &MI, unsigned OpNo,
                         SmallVectorImpl<MCFixup> &Fixups,
                         const MCSubtargetInfo &STI) const;

  void relocSymbol(const MCExpr *SymExpr, const MCExpr *GOTExpr,
                   const MCExpr *Dummy, unsigned FixupGOT, const SMLoc &Loc,
                   int64_t Offset, int64_t Addend,
                   SmallVectorImpl<MCFixup> &F) const;

  void relocShiftRight(const MCExpr *Dummy, int64_t Offset, int64_t Shmat,
                       const SMLoc &Loc,
                       SmallVectorImpl<MCFixup> &Fixups) const;

private:
  FeatureBitset computeAvailableFeatures(const FeatureBitset &FB) const;
  void
  verifyInstructionPredicates(const MCInst &MI,
                              const FeatureBitset &AvailableFeatures) const;
};
} // end anonymous namespace

MCCodeEmitter *llvm::createLoongArchMCCodeEmitter(const MCInstrInfo &MCII,
                                                  const MCRegisterInfo &MRI,
                                                  MCContext &Ctx) {
  return new LoongArchMCCodeEmitter(Ctx, MCII);
}

void LoongArchMCCodeEmitter::relocSymbol(
    const MCExpr *SymExpr, const MCExpr *GOTExpr, const MCExpr *Dummy,
    unsigned FixupGOT, const SMLoc &Loc, int64_t Offset, int64_t Addend,
    SmallVectorImpl<MCFixup> &Fixups) const {
  Offset *= 4;

  const MCExpr *Expr = GOTExpr ? GOTExpr : SymExpr;
  const MCExpr *FixupExpr = MCBinaryExpr::createAdd(
      Expr, MCConstantExpr::create(Offset + Addend, Ctx), Ctx);

  Fixups.push_back(
      MCFixup::create(Offset, FixupExpr,
                      MCFixupKind(LoongArch::fixup_larch_sop_push_pcrel), Loc));
  ++MCNumFixups;

  // If we are using GOT, add the offset of the entry in the GOT.
  if (GOTExpr) {
    Fixups.push_back(
        MCFixup::create(Offset, SymExpr, MCFixupKind(FixupGOT), Loc));
    ++MCNumFixups;

    Fixups.push_back(MCFixup::create(
        Offset, Dummy, MCFixupKind(LoongArch::fixup_larch_sop_add), Loc));
    ++MCNumFixups;
  }
}

void LoongArchMCCodeEmitter::relocShiftRight(
    const MCExpr *Dummy, int64_t Offset, int64_t Shmat, const SMLoc &Loc,
    SmallVectorImpl<MCFixup> &Fixups) const {
  Offset *= 4;
  unsigned FixupKind = LoongArch::fixup_larch_sop_sr;
  if (Shmat < 0) {
    Shmat = -Shmat;
    FixupKind = LoongArch::fixup_larch_sop_sl;
  }

  const MCExpr *ShmatExpr = MCConstantExpr::create(Shmat, Ctx);
  Fixups.push_back(MCFixup::create(
      Offset, ShmatExpr, MCFixupKind(LoongArch::fixup_larch_sop_push_absolute),
      Loc));
  ++MCNumFixups;

  Fixups.push_back(MCFixup::create(Offset, Dummy, MCFixupKind(FixupKind), Loc));
  ++MCNumFixups;
}

// Expand:
// la.global to pcaddu12i and ld.[d|w]
// la.local to pcaddu12i and addi.[d|w]
void LoongArchMCCodeEmitter::expandLoadAddr(const MCInst &MI, raw_ostream &OS,
                                            SmallVectorImpl<MCFixup> &Fixups,
                                            const MCSubtargetInfo &STI) const {
  MCOperand DestReg = MI.getOperand(0);
  MCOperand SrcSymbol = MI.getOperand(1);
  assert(DestReg.isReg() && "Expected register as the dest of address load");

  assert(SrcSymbol.isExpr() &&
         "Expected expression as the source of address load");

  const MCExpr *Expr = SrcSymbol.getExpr();
  const MCSymbolRefExpr *SymRef = cast<MCSymbolRefExpr>(Expr);
  assert(Expr->getKind() == MCExpr::SymbolRef &&
         SymRef->getKind() == MCSymbolRefExpr::VK_None &&
         "Expected the source of address load to be a bare symbol ref");

  MCRegister Reg = DestReg.getReg();

  MCInst TmpInst = MCInstBuilder(LoongArch::PCADDU12I).addReg(Reg).addImm(0);
  uint32_t Binary = getBinaryCodeForInstr(TmpInst, Fixups, STI);
  support::endian::write(OS, Binary, support::little);

  unsigned Opcode;
  bool IsTLS = false;

  switch (MI.getOpcode()) {
  default:
    llvm_unreachable("unexpected pseudo instruction");
  case LoongArch::PseudoLA_TLS_GD:
  case LoongArch::PseudoLA_TLS_LD:
    IsTLS = true;
    // fall through
  case LoongArch::PseudoLLA:
    Opcode = STI.hasFeature(LoongArch::Feature64Bit) ? LoongArch::ADDI_D
                                                     : LoongArch::ADDI_W;
    break;
  case LoongArch::PseudoLA_TLS_IE:
    IsTLS = true;
    // fall through
  case LoongArch::PseudoLA:
    Opcode = STI.hasFeature(LoongArch::Feature64Bit) ? LoongArch::LD_D
                                                     : LoongArch::LD_W;
    break;
  }

  if (IsTLS)
    cast<MCSymbolELF>(SymRef->getSymbol()).setType(ELF::STT_TLS);

  TmpInst = MCInstBuilder(Opcode).addReg(Reg).addReg(Reg).addImm(0);
  Binary = getBinaryCodeForInstr(TmpInst, Fixups, STI);
  support::endian::write(OS, Binary, support::little);

  // PseudoLA and all three TLS access models need GOT.
  bool UseGOT = (MI.getOpcode() != LoongArch::PseudoLLA);

  unsigned FixupGOT = LoongArch::fixup_larch_invalid;
  switch (MI.getOpcode()) {
  case LoongArch::PseudoLA:
    FixupGOT = LoongArch::fixup_larch_sop_push_gprel;
    break;
  case LoongArch::PseudoLA_TLS_GD:
  case LoongArch::PseudoLA_TLS_LD:
    FixupGOT = LoongArch::fixup_larch_sop_push_tls_gd;
    break;
  case LoongArch::PseudoLA_TLS_IE:
    FixupGOT = LoongArch::fixup_larch_sop_push_tls_got;
    break;
  }

  // Now the two insturctions are emitted.  Generate fixups for them.
  const MCExpr *GOTExpr = nullptr;
  const MCExpr *Dummy = MCConstantExpr::create(0, Ctx);
  if (UseGOT) {
    // Get GOT as a hidden (non-encoding) operand from PseudoLA.
    MCOperand GOTSymbol = MI.getOperand(2);
    assert(GOTSymbol.isExpr() && "Expected expression as the GOT");
    GOTExpr = GOTSymbol.getExpr();
    assert(GOTExpr->getKind() == MCExpr::SymbolRef &&
           (cast<MCSymbolRefExpr>(GOTExpr)->getKind() ==
            MCSymbolRefExpr::VK_None) &&
           "Expected the GOT to be a bare symbol ref");
  }

  relocSymbol(Expr, GOTExpr, Dummy, FixupGOT, MI.getLoc(), 0, 0x800, Fixups);
  relocShiftRight(Dummy, 0, 12, MI.getLoc(), Fixups);
  Fixups.push_back(MCFixup::create(
      0, Dummy, MCFixupKind(LoongArch::fixup_larch_sop_pop_32_s_5_20),
      MI.getLoc()));
  ++MCNumFixups;

  relocSymbol(Expr, GOTExpr, Dummy, FixupGOT, MI.getLoc(), 1, 0, Fixups);
  relocSymbol(Expr, GOTExpr, Dummy, FixupGOT, MI.getLoc(), 1, 0x800, Fixups);
  relocShiftRight(Dummy, 1, 12, MI.getLoc(), Fixups);
  relocShiftRight(Dummy, 1, -12, MI.getLoc(), Fixups);
  Fixups.push_back(MCFixup::create(
      4, Dummy, MCFixupKind(LoongArch::fixup_larch_sop_sub), MI.getLoc()));
  ++MCNumFixups;
  Fixups.push_back(MCFixup::create(
      4, Dummy, MCFixupKind(LoongArch::fixup_larch_sop_pop_32_s_10_12),
      MI.getLoc()));
  ++MCNumFixups;
}

int LoongArchMCCodeEmitter::expandLoadAddrTprel(
    const MCInst &MI, raw_ostream &OS, SmallVectorImpl<MCFixup> &Fixups,
    const MCSubtargetInfo &STI) const {
  MCOperand DestReg = MI.getOperand(0);
  MCOperand SrcSymbol = MI.getOperand(1);
  assert(DestReg.isReg() && "Expected register as the dest of address load");

  assert(SrcSymbol.isExpr() &&
         "Expected expression as the source of address load");

  const MCExpr *Expr = SrcSymbol.getExpr();
  const MCSymbolRefExpr *SymRef = cast<MCSymbolRefExpr>(Expr);
  assert(Expr->getKind() == MCExpr::SymbolRef &&
         SymRef->getKind() == MCSymbolRefExpr::VK_None &&
         "Expected the source of address load to be a bare symbol ref");

  cast<MCSymbolELF>(SymRef->getSymbol()).setType(ELF::STT_TLS);

  MCRegister Reg = DestReg.getReg();
  bool IsLA64 = STI.getFeatureBits()[LoongArch::Feature64Bit];

  MCInst TmpInst = MCInstBuilder(LoongArch::LU12I_W).addReg(Reg).addImm(0);
  uint32_t Binary = getBinaryCodeForInstr(TmpInst, Fixups, STI);
  support::endian::write(OS, Binary, support::little);

  TmpInst = MCInstBuilder(LoongArch::ORI).addReg(Reg).addReg(Reg).addImm(0);
  Binary = getBinaryCodeForInstr(TmpInst, Fixups, STI);
  support::endian::write(OS, Binary, support::little);

  // Relocation for the lower 32-bit of the offset from TP.
  MCFixup FixupPush = MCFixup::create(
      0, Expr, MCFixupKind(LoongArch::fixup_larch_sop_push_tls_tprel),
      MI.getLoc());

  Fixups.push_back(FixupPush);
  ++MCNumFixups;

  const MCExpr *Dummy = MCConstantExpr::create(0, Ctx);
  int64_t Shmat = IsLA64 ? 32 : 0;
  if (IsLA64)
    relocShiftRight(Dummy, 0, -Shmat, MI.getLoc(), Fixups);
  relocShiftRight(Dummy, 0, Shmat + 12, MI.getLoc(), Fixups);
  Fixups.push_back(MCFixup::create(
      0, Dummy, MCFixupKind(LoongArch::fixup_larch_sop_pop_32_s_5_20),
      MI.getLoc()));
  ++MCNumFixups;

  FixupPush.setOffset(4);
  Fixups.push_back(FixupPush);
  ++MCNumFixups;

  const MCExpr *MaskExpr = MCConstantExpr::create(0xfff, Ctx);
  Fixups.push_back(MCFixup::create(
      4, MaskExpr, MCFixupKind(LoongArch::fixup_larch_sop_push_absolute),
      MI.getLoc()));
  ++MCNumFixups;

  Fixups.push_back(MCFixup::create(
      4, Dummy, MCFixupKind(LoongArch::fixup_larch_sop_and), MI.getLoc()));
  ++MCNumFixups;

  Fixups.push_back(MCFixup::create(
      4, Dummy, MCFixupKind(LoongArch::fixup_larch_sop_pop_32_u_10_12),
      MI.getLoc()));
  ++MCNumFixups;

  if (!IsLA64)
    return 2;

  TmpInst = MCInstBuilder(LoongArch::LU32I_D).addReg(Reg).addReg(Reg).addImm(0);
  Binary = getBinaryCodeForInstr(TmpInst, Fixups, STI);
  support::endian::write(OS, Binary, support::little);

  TmpInst = MCInstBuilder(LoongArch::LU52I_D).addReg(Reg).addReg(Reg).addImm(0);
  Binary = getBinaryCodeForInstr(TmpInst, Fixups, STI);
  support::endian::write(OS, Binary, support::little);

  FixupPush.setOffset(8);
  Fixups.push_back(FixupPush);
  ++MCNumFixups;

  relocShiftRight(Dummy, 2, -12, MI.getLoc(), Fixups);
  relocShiftRight(Dummy, 2, 44, MI.getLoc(), Fixups);

  Fixups.push_back(MCFixup::create(
      8, Dummy, MCFixupKind(LoongArch::fixup_larch_sop_pop_32_s_5_20),
      MI.getLoc()));
  ++MCNumFixups;

  FixupPush.setOffset(12);
  Fixups.push_back(FixupPush);
  ++MCNumFixups;

  relocShiftRight(Dummy, 3, 52, MI.getLoc(), Fixups);

  Fixups.push_back(MCFixup::create(
      12, Dummy, MCFixupKind(LoongArch::fixup_larch_sop_pop_32_s_10_12),
      MI.getLoc()));
  ++MCNumFixups;

  return 4;
}

void LoongArchMCCodeEmitter::encodeInstruction(
    const MCInst &MI, raw_ostream &OS, SmallVectorImpl<MCFixup> &Fixups,
    const MCSubtargetInfo &STI) const {
  verifyInstructionPredicates(MI,
                              computeAvailableFeatures(STI.getFeatureBits()));

  const MCInstrDesc &Desc = MCII.get(MI.getOpcode());
  // Get byte count of instruction.
  unsigned Size = Desc.getSize();

  // LoongArchInstrInfo::getInstSizeInBytes hard-codes the number of expanded
  // instructions for each pseudo, and must be updated when adding new pseudos
  // or changing existing ones.

  switch (MI.getOpcode()) {
  case LoongArch::PseudoLA:
  case LoongArch::PseudoLLA:
  case LoongArch::PseudoLA_TLS_GD:
  case LoongArch::PseudoLA_TLS_LD:
  case LoongArch::PseudoLA_TLS_IE:
    expandLoadAddr(MI, OS, Fixups, STI);
    MCNumEmitted += 2;
    return;
  case LoongArch::PseudoLA_TLS_LE:
    MCNumEmitted += expandLoadAddrTprel(MI, OS, Fixups, STI);
    return;
  }

  switch (Size) {
  default:
    llvm_unreachable("Unhandled encodeInstruction length!");
  case 2: {
    uint16_t Bits = getBinaryCodeForInstr(MI, Fixups, STI);
    support::endian::write<uint16_t>(OS, Bits, support::little);
    break;
  }
  case 4: {
    uint32_t Bits = getBinaryCodeForInstr(MI, Fixups, STI);
    support::endian::write(OS, Bits, support::little);
    break;
  }
  }

  ++MCNumEmitted; // Keep track of the # of mi's emitted.
}

unsigned
LoongArchMCCodeEmitter::getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                                          SmallVectorImpl<MCFixup> &Fixups,
                                          const MCSubtargetInfo &STI) const {

  if (MO.isReg())
    return Ctx.getRegisterInfo()->getEncodingValue(MO.getReg());

  if (MO.isImm())
    return static_cast<unsigned>(MO.getImm());

  llvm_unreachable("Unhandled expression!");
  return 0;
}

unsigned
LoongArchMCCodeEmitter::getImmOpValueAsr2(const MCInst &MI, unsigned OpNo,
                                          SmallVectorImpl<MCFixup> &Fixups,
                                          const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);

  if (MO.isImm()) {
    unsigned Res = MO.getImm();
    assert((Res & 3) == 0 && "LSB is non-zero");
    return Res >> 2;
  }

  return getImmOpValue(MI, OpNo, Fixups, STI);
}

unsigned
LoongArchMCCodeEmitter::getImmOpValueMinus1(const MCInst &MI, unsigned OpNo,
                                            SmallVectorImpl<MCFixup> &Fixups,
                                            const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);

  if (MO.isImm()) {
    unsigned Res = MO.getImm();
    assert(1 <= Res && Res <= 4 && "Illegal shift amount for ALSL");
    return Res - 1;
  }

  return getImmOpValue(MI, OpNo, Fixups, STI);
}

unsigned
LoongArchMCCodeEmitter::getImmOpValue(const MCInst &MI, unsigned OpNo,
                                      SmallVectorImpl<MCFixup> &Fixups,
                                      const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);

  MCInstrDesc const &Desc = MCII.get(MI.getOpcode());

  // If the destination is an immediate, there is nothing to do.
  if (MO.isImm())
    return MO.getImm();

  assert(MO.isExpr() && "getImmOpValue expects only expressions or immediates");
  const MCExpr *Expr = MO.getExpr();
  MCExpr::ExprKind Kind = Expr->getKind();
  LoongArch::Fixups FixupKind = LoongArch::fixup_larch_invalid;
  if (Kind == MCExpr::Target) {
    const LoongArchMCExpr *RVExpr = cast<LoongArchMCExpr>(Expr);

    switch (RVExpr->getKind()) {
    case LoongArchMCExpr::VK_LoongArch_CALL:
      Fixups.push_back(MCFixup::create(
          0, Expr, MCFixupKind(LoongArch::fixup_larch_sop_push_pcrel),
          MI.getLoc()));
      ++MCNumFixups;
      FixupKind = LoongArch::fixup_larch_sop_pop_32_s_0_10_10_16_s2;
      break;
    case LoongArchMCExpr::VK_LoongArch_CALL_PLT:
      Fixups.push_back(MCFixup::create(
          0, Expr, MCFixupKind(LoongArch::fixup_larch_sop_push_plt_pcrel),
          MI.getLoc()));
      ++MCNumFixups;
      FixupKind = LoongArch::fixup_larch_sop_pop_32_s_0_10_10_16_s2;
      break;
    case LoongArchMCExpr::VK_LoongArch_None:
    case LoongArchMCExpr::VK_LoongArch_Invalid:
    case LoongArchMCExpr::VK_LoongArch_32_PCREL:
      llvm_unreachable("Unhandled fixup kind!");
    }
  } else if (Kind == MCExpr::SymbolRef &&
             cast<MCSymbolRefExpr>(Expr)->getKind() ==
                 MCSymbolRefExpr::VK_None) {
    Fixups.push_back(MCFixup::create(
        0, Expr, MCFixupKind(LoongArch::fixup_larch_sop_push_pcrel),
        MI.getLoc()));
    ++MCNumFixups;

    switch (Desc.getOpcode()) {
    case LoongArch::B:
    case LoongArch::BL:
      FixupKind = LoongArch::fixup_larch_sop_pop_32_s_0_10_10_16_s2;
      break;
    case LoongArch::BCEQZ:
    case LoongArch::BCNEZ:
    case LoongArch::BEQZ:
    case LoongArch::BNEZ:
      FixupKind = LoongArch::fixup_larch_sop_pop_32_s_0_5_10_16_s2;
      break;
    case LoongArch::BEQ:
    case LoongArch::BNE:
    case LoongArch::BLT:
    case LoongArch::BGE:
    case LoongArch::BLTU:
    case LoongArch::BGEU:
      FixupKind = LoongArch::fixup_larch_sop_pop_32_s_10_16_s2;
      break;
    }
  }

  assert(FixupKind != LoongArch::fixup_larch_invalid &&
         "Unhandled expression!");

  const MCExpr *Dummy = MCConstantExpr::create(0, Ctx);
  Fixups.push_back(
      MCFixup::create(0, Dummy, MCFixupKind(FixupKind), MI.getLoc()));
  ++MCNumFixups;

  return 0;
}

#define ENABLE_INSTR_PREDICATE_VERIFIER
#include "LoongArchGenMCCodeEmitter.inc"
