//===-- LoongArchMCCodeEmitter.cpp - Convert LoongArch Code to Machine Code ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the LoongArchMCCodeEmitter class.
//
//===----------------------------------------------------------------------===//

#include "LoongArchMCCodeEmitter.h"
#include "MCTargetDesc/LoongArchFixupKinds.h"
#include "MCTargetDesc/LoongArchMCExpr.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "MCTargetDesc/LoongArchInstPrinter.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <cstdint>

using namespace llvm;

#define DEBUG_TYPE "mccodeemitter"

#define GET_INSTRMAP_INFO
#include "LoongArchGenInstrInfo.inc"
#undef GET_INSTRMAP_INFO

namespace llvm {

MCCodeEmitter *createLoongArchMCCodeEmitter(const MCInstrInfo &MCII,
                                         const MCRegisterInfo &MRI,
                                         MCContext &Ctx) {
  return new LoongArchMCCodeEmitter(MCII, Ctx);
}

} // end namespace llvm

void LoongArchMCCodeEmitter::EmitByte(unsigned char C, raw_ostream &OS) const {
  OS << (char)C;
}

void LoongArchMCCodeEmitter::EmitInstruction(uint64_t Val, unsigned Size,
                                        const MCSubtargetInfo &STI,
                                        raw_ostream &OS) const {
  for (unsigned i = 0; i < Size; ++i) {
    unsigned Shift = i * 8;
    EmitByte((Val >> Shift) & 0xff, OS);
  }
}

/// encodeInstruction - Emit the instruction.
/// Size the instruction with Desc.getSize().
void LoongArchMCCodeEmitter::
encodeInstruction(const MCInst &MI, raw_ostream &OS,
                  SmallVectorImpl<MCFixup> &Fixups,
                  const MCSubtargetInfo &STI) const
{
  MCInst TmpInst = MI;

  uint32_t Binary = getBinaryCodeForInstr(TmpInst, Fixups, STI);

  const MCInstrDesc &Desc = MCII.get(TmpInst.getOpcode());

  // Get byte count of instruction
  unsigned Size = Desc.getSize();
  if (!Size)
    llvm_unreachable("Desc.getSize() returns 0");

  EmitInstruction(Binary, Size, STI, OS);
}

/// getBranchTargetOpValue - Return binary encoding of the branch
/// target operand. If the machine operand requires relocation,
/// record the relocation and return zero.
unsigned LoongArchMCCodeEmitter::
getBranchTargetOpValue(const MCInst &MI, unsigned OpNo,
                       SmallVectorImpl<MCFixup> &Fixups,
                       const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);

  // If the destination is an immediate, divide by 4.
  if (MO.isImm()) return MO.getImm() >> 2;

  assert(MO.isExpr() &&
         "getBranchTargetOpValue expects only expressions or immediates");

  // XXX: brtarget reloc EncoderMethod.
  const MCExpr *Expr = MO.getExpr();
  int64_t Value = 0x0;
  const MCConstantExpr *tmpExpr = MCConstantExpr::create(Value, Ctx);
  Fixups.push_back(MCFixup::create(0, Expr,
        MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_PCREL)));
  switch (MI.getOpcode()) {
  default:
    llvm_unreachable("Unhandled reloc instruction!");
    break;
  case LoongArch::BEQZ:
  case LoongArch::BEQZ32:
  case LoongArch::BNEZ:
  case LoongArch::BNEZ32:
  case LoongArch::BCEQZ:
  case LoongArch::BCNEZ:
    Fixups.push_back(MCFixup::create(0, tmpExpr,
        MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_0_5_10_16_S2)));
    break;
  case LoongArch::BEQ:
  case LoongArch::BEQ32:
  case LoongArch::BNE:
  case LoongArch::BNE32:
  case LoongArch::BLT:
  case LoongArch::BLT32:
  case LoongArch::BGE:
  case LoongArch::BGE32:
  case LoongArch::BLTU:
  case LoongArch::BLTU32:
  case LoongArch::BGEU:
  case LoongArch::BGEU32:
    Fixups.push_back(MCFixup::create(0, tmpExpr,
        MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_10_16_S2)));
    break;
  }
  return 0;
}

/// getJumpTargetOpValue - Return binary encoding of the jump
/// target operand. If the machine operand requires relocation,
/// record the relocation and return zero.
unsigned LoongArchMCCodeEmitter::
getJumpTargetOpValue(const MCInst &MI, unsigned OpNo,
                     SmallVectorImpl<MCFixup> &Fixups,
                     const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);
  // If the destination is an immediate, divide by 4.
  if (MO.isImm()) return MO.getImm()>>2;

  assert(MO.isExpr() &&
         "getJumpTargetOpValue expects only expressions or an immediate");

  const MCExpr *Expr = MO.getExpr();
  int64_t Value = 0x0;
  const MCConstantExpr *tmpExpr = MCConstantExpr::create(Value, Ctx);
  Fixups.push_back(MCFixup::create(0, Expr,
        MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_PLT_PCREL)));
  if (MI.getOpcode() == LoongArch::JIRL)
    Fixups.push_back(MCFixup::create(0, tmpExpr,
          MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_10_16_S2)));
  else // B or BL
    Fixups.push_back(MCFixup::create(0, tmpExpr,
          MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_0_10_10_16_S2)));
  return 0;
}

unsigned LoongArchMCCodeEmitter::
getSImm11Lsl1Encoding(const MCInst &MI, unsigned OpNo,
                     SmallVectorImpl<MCFixup> &Fixups,
                     const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);
  if (MO.isImm()) {
    unsigned Value = MO.getImm();
    return Value >> 1;
  }

  return 0;
}

unsigned LoongArchMCCodeEmitter::
getSImm10Lsl2Encoding(const MCInst &MI, unsigned OpNo,
                     SmallVectorImpl<MCFixup> &Fixups,
                     const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);
  if (MO.isImm()) {
    unsigned Value = MO.getImm();
    return Value >> 2;
  }

  return 0;
}

unsigned LoongArchMCCodeEmitter::
getSImm9Lsl3Encoding(const MCInst &MI, unsigned OpNo,
                     SmallVectorImpl<MCFixup> &Fixups,
                     const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);
  if (MO.isImm()) {
    unsigned Value = MO.getImm();
    return Value >> 3;
  }

  return 0;
}

unsigned LoongArchMCCodeEmitter::
getSImm8Lsl1Encoding(const MCInst &MI, unsigned OpNo,
                     SmallVectorImpl<MCFixup> &Fixups,
                     const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);
  if (MO.isImm()) {
    unsigned Value = MO.getImm();
    return Value >> 1;
  }

  return 0;
}

unsigned LoongArchMCCodeEmitter::
getSImm8Lsl2Encoding(const MCInst &MI, unsigned OpNo,
                     SmallVectorImpl<MCFixup> &Fixups,
                     const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);
  if (MO.isImm()) {
    unsigned Value = MO.getImm();
    return Value >> 2;
  }

  return 0;
}

unsigned LoongArchMCCodeEmitter::
getSImm8Lsl3Encoding(const MCInst &MI, unsigned OpNo,
                     SmallVectorImpl<MCFixup> &Fixups,
                     const MCSubtargetInfo &STI) const {
  const MCOperand &MO = MI.getOperand(OpNo);
  if (MO.isImm()) {
    unsigned Value = MO.getImm();
    return Value >> 3;
  }

  return 0;
}

unsigned LoongArchMCCodeEmitter::
getExprOpValue(const MCInst &MI, const MCExpr *Expr,
               SmallVectorImpl<MCFixup> &Fixups,
               const MCSubtargetInfo &STI) const {
  int64_t Res;

  if (Expr->evaluateAsAbsolute(Res))
    return Res;

  MCExpr::ExprKind Kind = Expr->getKind();
  if (Kind == MCExpr::Constant) {
    return cast<MCConstantExpr>(Expr)->getValue();
  }

  if (Kind == MCExpr::Binary) {
    unsigned Res = getExprOpValue(MI, cast<MCBinaryExpr>(Expr)->getLHS(), Fixups, STI);
    Res += getExprOpValue(MI, cast<MCBinaryExpr>(Expr)->getRHS(), Fixups, STI);
    return Res;
  }

  if (Kind == MCExpr::Target) {
    int64_t Value = 0x0;
    const LoongArchMCExpr *LoongArchExpr = cast<LoongArchMCExpr>(Expr);
    const MCExpr *BinExpr = nullptr;
    const MCExpr *GOTExpr = nullptr;
    const MCSymbol *GOTSymbol = Ctx.getOrCreateSymbol(StringRef("_GLOBAL_OFFSET_TABLE_"));

    LoongArch::Fixups FixupKind = LoongArch::Fixups(0);
    switch (LoongArchExpr->getKind()) {
    case LoongArchMCExpr::MEK_None:
    case LoongArchMCExpr::MEK_Special:
      llvm_unreachable("Unhandled fixup kind!");
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      break;
    case LoongArchMCExpr::MEK_PLT:
      Value = 0x0;
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PLT_PCREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      if (MI.getOpcode() == LoongArch::JIRL)
        Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
              MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_10_16_S2)));
      else // B or BL
        Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
              MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_0_10_10_16_S2)));
      break;
    case LoongArchMCExpr::MEK_CALL_HI:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PLT_PCREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));

      Value = 0x20000;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0x12;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));

      break;
    case LoongArchMCExpr::MEK_CALL_LO:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PLT_PCREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));

      Value = 0x4;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x20004;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0x12;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x12;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SUB)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_10_16_S2)));
      break;
    case LoongArchMCExpr::MEK_GOT_HI:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x800;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));

      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_GPREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_GOT_LO:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x4;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_GPREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x804;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_GPREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SUB)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_10_12)));
      break;
    case LoongArchMCExpr::MEK_GOT_RRHI:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Fixups.push_back(MCFixup::create(0, GOTExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_GPREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x80000000;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_GPREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SUB)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x2c;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_GOT_RRLO:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x4;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_GPREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x80000004;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_GPREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SUB)));
      Value = 0xfff;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_AND)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_U_10_12)));
      break;
    case LoongArchMCExpr::MEK_GOT_RRHIGHER:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x80000008;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_GPREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x2c;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_GOT_RRHIGHEST:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x8000000c;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_GPREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0x34;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_10_12)));
      break;
    case LoongArchMCExpr::MEK_ABS_HI:
      FixupKind = LoongArch::fixup_LARCH_MARK_LA;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x2c;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_ABS_LO:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0xfff;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_AND)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_U_10_12)));
      break;
    case LoongArchMCExpr::MEK_ABS_HIGHER:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x2c;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_ABS_HIGHEST:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x34;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_10_12)));
      break;
    case LoongArchMCExpr::MEK_PCREL_HI:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x800;
      BinExpr = MCBinaryExpr::createAdd(Expr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_PCREL_LO:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x4;
      BinExpr = MCBinaryExpr::createAdd(Expr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x804;
      BinExpr = MCBinaryExpr::createAdd(Expr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SUB)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_10_12)));
      break;
    case LoongArchMCExpr::MEK_PCREL_RRHI:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x80000000;
      BinExpr = MCBinaryExpr::createAdd(LoongArchExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SUB)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x2c;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_PCREL_RRLO:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x4;
      BinExpr = MCBinaryExpr::createAdd(LoongArchExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x80000004;
      BinExpr = MCBinaryExpr::createAdd(LoongArchExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SUB)));
      Value = 0xfff;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_AND)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_U_10_12)));
      break;
    case LoongArchMCExpr::MEK_PCREL_RRHIGHER:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x80000008;
      BinExpr = MCBinaryExpr::createAdd(LoongArchExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x2c;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_PCREL_RRHIGHEST:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x8000000c;
      BinExpr = MCBinaryExpr::createAdd(LoongArchExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      Value = 0x34;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_10_12)));
      break;
    case LoongArchMCExpr::MEK_TLSGD_HI:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x800;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GD;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_TLSGD_LO:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x4;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GD;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x804;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GD;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SUB)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_10_12)));
      break;
    case LoongArchMCExpr::MEK_TLSGD_RRHI:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Fixups.push_back(MCFixup::create(0, GOTExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GD;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x80000000;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GD;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SUB)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x2c;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_TLSGD_RRLO:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x4;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GD;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x80000004;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GD;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SUB)));
      Value = 0xfff;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_AND)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_U_10_12)));
      break;
    case LoongArchMCExpr::MEK_TLSGD_RRHIGHER:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x80000008;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GD;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x2c;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_TLSGD_RRHIGHEST:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x8000000c;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GD;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0x34;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_10_12)));
      break;
    case LoongArchMCExpr::MEK_TLSIE_HI:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x800;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GOT;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_TLSIE_LO:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x4;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GOT;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x804;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GOT;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SUB)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_10_12)));
      break;
    case LoongArchMCExpr::MEK_TLSIE_RRHI:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Fixups.push_back(MCFixup::create(0, GOTExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GOT;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x80000000;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GOT;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SUB)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x2c;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_TLSIE_RRLO:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x4;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GOT;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      Value = 0x80000004;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GOT;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SUB)));
      Value = 0xfff;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_AND)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_U_10_12)));
      break;
    case LoongArchMCExpr::MEK_TLSIE_RRHIGHER:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x80000008;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GOT;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x2c;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_TLSIE_RRHIGHEST:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_PCREL;
      GOTExpr = MCSymbolRefExpr::create(GOTSymbol,
                                        MCSymbolRefExpr::VK_None, Ctx);
      Value = 0x8000000c;
      BinExpr = MCBinaryExpr::createAdd(GOTExpr, MCConstantExpr::create(Value, Ctx), Ctx);
      Fixups.push_back(MCFixup::create(0, BinExpr, MCFixupKind(FixupKind)));
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_GOT;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_ADD)));
      Value = 0x34;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_10_12)));
      break;
    case LoongArchMCExpr::MEK_TLSLE_HI:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_TPREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));

      Value = 0x20;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x2c;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_TLSLE_LO:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_TPREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0xfff;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_AND)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_U_10_12)));
      break;
    case LoongArchMCExpr::MEK_TLSLE_HIGHER:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_TPREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0xc;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SL)));
      Value = 0x2c;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_5_20)));
      break;
    case LoongArchMCExpr::MEK_TLSLE_HIGHEST:
      FixupKind = LoongArch::fixup_LARCH_SOP_PUSH_TLS_TPREL;
      Fixups.push_back(MCFixup::create(0, LoongArchExpr, MCFixupKind(FixupKind)));
      Value = 0x34;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_PUSH_ABSOLUTE)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_SR)));
      Value = 0x0;
      Fixups.push_back(MCFixup::create(0, MCConstantExpr::create(Value, Ctx),
            MCFixupKind(LoongArch::fixup_LARCH_SOP_POP_32_S_10_12)));
      break;
    }
    return 0;
  }

  if (Kind == MCExpr::SymbolRef) {
    LoongArch::Fixups FixupKind = LoongArch::Fixups(0);

    switch(cast<MCSymbolRefExpr>(Expr)->getKind()) {
    default: llvm_unreachable("Unknown fixup kind!");
      break;
    }
    Fixups.push_back(MCFixup::create(0, Expr, MCFixupKind(FixupKind)));
    return 0;
  }
  return 0;
}

/// getMachineOpValue - Return binary encoding of operand. If the machine
/// operand requires relocation, record the relocation and return zero.
unsigned LoongArchMCCodeEmitter::
getMachineOpValue(const MCInst &MI, const MCOperand &MO,
                  SmallVectorImpl<MCFixup> &Fixups,
                  const MCSubtargetInfo &STI) const {
  if (MO.isReg()) {
    unsigned Reg = MO.getReg();
    unsigned RegNo = Ctx.getRegisterInfo()->getEncodingValue(Reg);
    return RegNo;
  } else if (MO.isImm()) {
    return static_cast<unsigned>(MO.getImm());
  }
  // MO must be an Expr.
  assert(MO.isExpr());
  return getExprOpValue(MI, MO.getExpr(),Fixups, STI);
}

/// Return binary encoding of memory related operand.
/// If the offset operand requires relocation, record the relocation.
template <unsigned ShiftAmount>
unsigned LoongArchMCCodeEmitter::getMemEncoding(const MCInst &MI, unsigned OpNo,
                                                SmallVectorImpl<MCFixup> &Fixups,
                                                const MCSubtargetInfo &STI) const {
  // Base register is encoded in bits 16-12, offset is encoded in bits 11-0.
  assert(MI.getOperand(OpNo).isReg());
  unsigned RegBits = getMachineOpValue(MI, MI.getOperand(OpNo),Fixups, STI) << 12;
  unsigned OffBits = getMachineOpValue(MI, MI.getOperand(OpNo+1), Fixups, STI);

  // Apply the scale factor if there is one.
  OffBits >>= ShiftAmount;

  return (OffBits & 0xFFF) | RegBits;
}

unsigned LoongArchMCCodeEmitter::getMemEncoding10l2(const MCInst &MI, unsigned OpNo,
                                                SmallVectorImpl<MCFixup> &Fixups,
                                                const MCSubtargetInfo &STI) const {
  // Base register is encoded in bits 16-12, offset is encoded in bits 11-0.
  assert(MI.getOperand(OpNo).isReg());
  unsigned RegBits = getMachineOpValue(MI, MI.getOperand(OpNo),Fixups, STI) << 10;
  unsigned OffBits = getMachineOpValue(MI, MI.getOperand(OpNo+1), Fixups, STI);

  // Apply the scale factor if there is one.
  OffBits >>= 2;

  return (OffBits & 0x3FF) | RegBits;
}

unsigned LoongArchMCCodeEmitter::getMemEncoding11l1(const MCInst &MI, unsigned OpNo,
                                                SmallVectorImpl<MCFixup> &Fixups,
                                                const MCSubtargetInfo &STI) const {
  // Base register is encoded in bits 16-12, offset is encoded in bits 11-0.
  assert(MI.getOperand(OpNo).isReg());
  unsigned RegBits = getMachineOpValue(MI, MI.getOperand(OpNo),Fixups, STI) << 11;
  unsigned OffBits = getMachineOpValue(MI, MI.getOperand(OpNo+1), Fixups, STI);

  // Apply the scale factor if there is one.
  OffBits >>= 1;

  return (OffBits & 0x7FF) | RegBits;
}

unsigned LoongArchMCCodeEmitter::getMemEncoding9l3(const MCInst &MI, unsigned OpNo,
                                                SmallVectorImpl<MCFixup> &Fixups,
                                                const MCSubtargetInfo &STI) const {
  // Base register is encoded in bits 16-12, offset is encoded in bits 11-0.
  assert(MI.getOperand(OpNo).isReg());
  unsigned RegBits = getMachineOpValue(MI, MI.getOperand(OpNo),Fixups, STI) << 9;
  unsigned OffBits = getMachineOpValue(MI, MI.getOperand(OpNo+1), Fixups, STI);

  // Apply the scale factor if there is one.
  OffBits >>= 3;

  return (OffBits & 0x1FF) | RegBits;
}

/// Return binary encoding of simm14 memory related operand. Such as LL/SC instructions.
/// If the offset operand requires relocation, record the relocation.
template <unsigned ShiftAmount>
unsigned LoongArchMCCodeEmitter::getSimm14MemEncoding(const MCInst &MI, unsigned OpNo,
                                                      SmallVectorImpl<MCFixup> &Fixups,
                                                      const MCSubtargetInfo &STI) const {
  // Base register is encoded in bits 18-14, offset is encoded in bits 13-0.
  assert(MI.getOperand(OpNo).isReg());
  unsigned RegBits = getMachineOpValue(MI, MI.getOperand(OpNo),Fixups, STI) << 14;
  unsigned OffBits = getMachineOpValue(MI, MI.getOperand(OpNo+1), Fixups, STI);

  // Apply the scale factor if there is one.
  OffBits >>= ShiftAmount;

  return (OffBits & 0x3FFF) | RegBits;
}

unsigned
LoongArchMCCodeEmitter::getFCMPEncoding(const MCInst &MI, unsigned OpNo,
                                       SmallVectorImpl<MCFixup> &Fixups,
                                       const MCSubtargetInfo &STI) const {
  const MCOperand& MO = MI.getOperand(OpNo);
  switch((LoongArch::CondCode)MO.getImm()){
  case LoongArch::FCOND_T:
    return 0x0;
  case LoongArch::FCOND_OR:
    return 0x8;
  case LoongArch::FCOND_UNE:
    return 0x4;
  case LoongArch::FCOND_ONE:
    return 0xC;
  case LoongArch::FCOND_UGE:
    return 0x2;
  case LoongArch::FCOND_OGE:
    return 0xA;
  case LoongArch::FCOND_UGT:
    return 0x6;
  case LoongArch::FCOND_OGT:
    return 0xE;
  case LoongArch::FCOND_ST:
    return 0x1;
  case LoongArch::FCOND_GLE:
    return 0x9;
  case LoongArch::FCOND_GL:
    return 0xD;
  case LoongArch::FCOND_NLT:
    return 0x3;
  case LoongArch::FCOND_GE:
    return 0xB;
  case LoongArch::FCOND_NLE:
    return 0x7;
  case LoongArch::FCOND_GT:
    return 0xF;
  default:
    return MO.getImm();
  }
}

template <unsigned Bits, int Offset>
unsigned
LoongArchMCCodeEmitter::getUImmWithOffsetEncoding(const MCInst &MI, unsigned OpNo,
                                             SmallVectorImpl<MCFixup> &Fixups,
                                             const MCSubtargetInfo &STI) const {
  assert(MI.getOperand(OpNo).isImm());
  unsigned Value = getMachineOpValue(MI, MI.getOperand(OpNo), Fixups, STI);
  Value -= Offset;
  return Value;
}

#include "LoongArchGenMCCodeEmitter.inc"
