//===- LoongArchMCInstLower.cpp - Convert LoongArch MachineInstr to MCInst ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains code to lower LoongArch MachineInstrs to their corresponding
// MCInst records.
//
//===----------------------------------------------------------------------===//

#include "LoongArchMCInstLower.h"
#include "MCTargetDesc/LoongArchBaseInfo.h"
#include "MCTargetDesc/LoongArchMCExpr.h"
#include "LoongArchAsmPrinter.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Support/ErrorHandling.h"
#include <cassert>

using namespace llvm;

LoongArchMCInstLower::LoongArchMCInstLower(LoongArchAsmPrinter &asmprinter)
  : AsmPrinter(asmprinter) {}

void LoongArchMCInstLower::Initialize(MCContext *C) {
  Ctx = C;
}

MCOperand LoongArchMCInstLower::LowerSymbolOperand(const MachineOperand &MO,
                                              MachineOperandType MOTy,
                                              unsigned Offset) const {
  MCSymbolRefExpr::VariantKind Kind = MCSymbolRefExpr::VK_None;
  LoongArchMCExpr::LoongArchExprKind TargetKind = LoongArchMCExpr::MEK_None;
  const MCSymbol *Symbol;

  switch(MO.getTargetFlags()) {
  default:
    llvm_unreachable("Invalid target flag!");
  case LoongArchII::MO_NO_FLAG:
    break;
  case LoongArchII::MO_GOT_HI:
    TargetKind = LoongArchMCExpr::MEK_GOT_HI;
    break;
  case LoongArchII::MO_GOT_LO:
    TargetKind = LoongArchMCExpr::MEK_GOT_LO;
    break;
  case LoongArchII::MO_GOT_RRHI:
    TargetKind = LoongArchMCExpr::MEK_GOT_RRHI;
    break;
  case LoongArchII::MO_GOT_RRHIGHER:
    TargetKind = LoongArchMCExpr::MEK_GOT_RRHIGHER;
    break;
  case LoongArchII::MO_GOT_RRHIGHEST:
    TargetKind = LoongArchMCExpr::MEK_GOT_RRHIGHEST;
    break;
  case LoongArchII::MO_GOT_RRLO:
    TargetKind = LoongArchMCExpr::MEK_GOT_RRLO;
    break;
  case LoongArchII::MO_PCREL_HI:
    TargetKind = LoongArchMCExpr::MEK_PCREL_HI;
    break;
  case LoongArchII::MO_PCREL_LO:
    TargetKind = LoongArchMCExpr::MEK_PCREL_LO;
    break;
  case LoongArchII::MO_PCREL_RRHI:
    TargetKind = LoongArchMCExpr::MEK_PCREL_RRHI;
    break;
  case LoongArchII::MO_PCREL_RRHIGHER:
    TargetKind = LoongArchMCExpr::MEK_PCREL_RRHIGHER;
    break;
  case LoongArchII::MO_PCREL_RRHIGHEST:
    TargetKind = LoongArchMCExpr::MEK_PCREL_RRHIGHEST;
    break;
  case LoongArchII::MO_PCREL_RRLO:
    TargetKind = LoongArchMCExpr::MEK_PCREL_RRLO;
    break;
  case LoongArchII::MO_TLSIE_HI:
    TargetKind = LoongArchMCExpr::MEK_TLSIE_HI;
    break;
  case LoongArchII::MO_TLSIE_LO:
    TargetKind = LoongArchMCExpr::MEK_TLSIE_LO;
    break;
  case LoongArchII::MO_TLSIE_RRHI:
    TargetKind = LoongArchMCExpr::MEK_TLSIE_RRHI;
    break;
  case LoongArchII::MO_TLSIE_RRHIGHER:
    TargetKind = LoongArchMCExpr::MEK_TLSIE_RRHIGHER;
    break;
  case LoongArchII::MO_TLSIE_RRHIGHEST:
    TargetKind = LoongArchMCExpr::MEK_TLSIE_RRHIGHEST;
    break;
  case LoongArchII::MO_TLSIE_RRLO:
    TargetKind = LoongArchMCExpr::MEK_TLSIE_RRLO;
    break;
  case LoongArchII::MO_TLSLE_HI:
    TargetKind = LoongArchMCExpr::MEK_TLSLE_HI;
    break;
  case LoongArchII::MO_TLSLE_HIGHER:
    TargetKind = LoongArchMCExpr::MEK_TLSLE_HIGHER;
    break;
  case LoongArchII::MO_TLSLE_HIGHEST:
    TargetKind = LoongArchMCExpr::MEK_TLSLE_HIGHEST;
    break;
  case LoongArchII::MO_TLSLE_LO:
    TargetKind = LoongArchMCExpr::MEK_TLSLE_LO;
    break;
  case LoongArchII::MO_TLSGD_HI:
    TargetKind = LoongArchMCExpr::MEK_TLSGD_HI;
    break;
  case LoongArchII::MO_TLSGD_LO:
    TargetKind = LoongArchMCExpr::MEK_TLSGD_LO;
    break;
  case LoongArchII::MO_TLSGD_RRHI:
    TargetKind = LoongArchMCExpr::MEK_TLSGD_RRHI;
    break;
  case LoongArchII::MO_TLSGD_RRHIGHER:
    TargetKind = LoongArchMCExpr::MEK_TLSGD_RRHIGHER;
    break;
  case LoongArchII::MO_TLSGD_RRHIGHEST:
    TargetKind = LoongArchMCExpr::MEK_TLSGD_RRHIGHEST;
    break;
  case LoongArchII::MO_TLSGD_RRLO:
    TargetKind = LoongArchMCExpr::MEK_TLSGD_RRLO;
    break;
  case LoongArchII::MO_ABS_HI:
    TargetKind = LoongArchMCExpr::MEK_ABS_HI;
    break;
  case LoongArchII::MO_ABS_HIGHER:
    TargetKind = LoongArchMCExpr::MEK_ABS_HIGHER;
    break;
  case LoongArchII::MO_ABS_HIGHEST:
    TargetKind = LoongArchMCExpr::MEK_ABS_HIGHEST;
    break;
  case LoongArchII::MO_ABS_LO:
    TargetKind = LoongArchMCExpr::MEK_ABS_LO;
    break;
  case LoongArchII::MO_CALL_HI:
    TargetKind = LoongArchMCExpr::MEK_CALL_HI;
    break;
  case LoongArchII::MO_CALL_LO:
    TargetKind = LoongArchMCExpr::MEK_CALL_LO;
    break;
  }

  switch (MOTy) {
  case MachineOperand::MO_MachineBasicBlock:
    Symbol = MO.getMBB()->getSymbol();
    break;

  case MachineOperand::MO_GlobalAddress:
    Symbol = AsmPrinter.getSymbol(MO.getGlobal());
    Offset += MO.getOffset();
    break;

  case MachineOperand::MO_BlockAddress:
    Symbol = AsmPrinter.GetBlockAddressSymbol(MO.getBlockAddress());
    Offset += MO.getOffset();
    break;

  case MachineOperand::MO_ExternalSymbol:
    Symbol = AsmPrinter.GetExternalSymbolSymbol(MO.getSymbolName());
    Offset += MO.getOffset();
    break;

  case MachineOperand::MO_MCSymbol:
    Symbol = MO.getMCSymbol();
    Offset += MO.getOffset();
    break;

  case MachineOperand::MO_JumpTableIndex:
    Symbol = AsmPrinter.GetJTISymbol(MO.getIndex());
    break;

  case MachineOperand::MO_ConstantPoolIndex:
    Symbol = AsmPrinter.GetCPISymbol(MO.getIndex());
    Offset += MO.getOffset();
    break;

  default:
    llvm_unreachable("<unknown operand type>");
  }

  const MCExpr *Expr = MCSymbolRefExpr::create(Symbol, Kind, *Ctx);

  if (Offset) {
    // Assume offset is never negative.
    assert(Offset > 0);

    Expr = MCBinaryExpr::createAdd(Expr, MCConstantExpr::create(Offset, *Ctx),
                                   *Ctx);
  }

  if (TargetKind != LoongArchMCExpr::MEK_None)
    Expr = LoongArchMCExpr::create(TargetKind, Expr, *Ctx);

  return MCOperand::createExpr(Expr);
}

MCOperand LoongArchMCInstLower::LowerOperand(const MachineOperand &MO,
                                        unsigned offset) const {
  MachineOperandType MOTy = MO.getType();

  switch (MOTy) {
  default: llvm_unreachable("unknown operand type");
  case MachineOperand::MO_Register:
    // Ignore all implicit register operands.
    if (MO.isImplicit()) break;
    return MCOperand::createReg(MO.getReg());
  case MachineOperand::MO_Immediate:
    return MCOperand::createImm(MO.getImm() + offset);
  case MachineOperand::MO_MachineBasicBlock:
  case MachineOperand::MO_GlobalAddress:
  case MachineOperand::MO_ExternalSymbol:
  case MachineOperand::MO_MCSymbol:
  case MachineOperand::MO_JumpTableIndex:
  case MachineOperand::MO_ConstantPoolIndex:
  case MachineOperand::MO_BlockAddress:
    return LowerSymbolOperand(MO, MOTy, offset);
  case MachineOperand::MO_RegisterMask:
    break;
 }

  return MCOperand();
}

MCOperand LoongArchMCInstLower::createSub(MachineBasicBlock *BB1,
                                     MachineBasicBlock *BB2,
                                     LoongArchMCExpr::LoongArchExprKind Kind) const {
  const MCSymbolRefExpr *Sym1 = MCSymbolRefExpr::create(BB1->getSymbol(), *Ctx);
  const MCSymbolRefExpr *Sym2 = MCSymbolRefExpr::create(BB2->getSymbol(), *Ctx);
  const MCBinaryExpr *Sub = MCBinaryExpr::createSub(Sym1, Sym2, *Ctx);

  return MCOperand::createExpr(LoongArchMCExpr::create(Kind, Sub, *Ctx));
}

void LoongArchMCInstLower::lowerLongBranchADDI(const MachineInstr *MI,
                                           MCInst &OutMI, int Opcode) const {
  OutMI.setOpcode(Opcode);

  LoongArchMCExpr::LoongArchExprKind Kind;
  unsigned TargetFlags = MI->getOperand(2).getTargetFlags();
  switch (TargetFlags) {
  case LoongArchII::MO_ABS_HIGHEST:
    Kind = LoongArchMCExpr::MEK_ABS_HIGHEST;
    break;
  case LoongArchII::MO_ABS_HIGHER:
    Kind = LoongArchMCExpr::MEK_ABS_HIGHER;
    break;
  case LoongArchII::MO_ABS_HI:
    Kind = LoongArchMCExpr::MEK_ABS_HI;
    break;
  case LoongArchII::MO_ABS_LO:
    Kind = LoongArchMCExpr::MEK_ABS_LO;
    break;
  default:
    report_fatal_error("Unexpected flags for lowerLongBranchADDI");
  }

  // Lower two register operands.
  for (unsigned I = 0, E = 2; I != E; ++I) {
    const MachineOperand &MO = MI->getOperand(I);
    OutMI.addOperand(LowerOperand(MO));
  }

  if (MI->getNumOperands() == 3) {
    // Lower register operand.
    const MCExpr *Expr =
        MCSymbolRefExpr::create(MI->getOperand(2).getMBB()->getSymbol(), *Ctx);
    const LoongArchMCExpr *LoongArchExpr = LoongArchMCExpr::create(Kind, Expr, *Ctx);
    OutMI.addOperand(MCOperand::createExpr(LoongArchExpr));
  } else if (MI->getNumOperands() == 4) {
    // Create %lo($tgt-$baltgt) or %hi($tgt-$baltgt).
    OutMI.addOperand(createSub(MI->getOperand(2).getMBB(),
                               MI->getOperand(3).getMBB(), Kind));
  }
}

void LoongArchMCInstLower::lowerLongBranchPCADDU12I(const MachineInstr *MI,
                                           MCInst &OutMI, int Opcode) const {
  OutMI.setOpcode(Opcode);

  LoongArchMCExpr::LoongArchExprKind Kind;
  unsigned TargetFlags = MI->getOperand(1).getTargetFlags();
  switch (TargetFlags) {
  case LoongArchII::MO_PCREL_HI:
    Kind = LoongArchMCExpr::MEK_PCREL_HI;
    break;
  case LoongArchII::MO_PCREL_LO:
    Kind = LoongArchMCExpr::MEK_PCREL_LO;
    break;
  default:
    report_fatal_error("Unexpected flags for lowerLongBranchADDI");
  }

  // Lower one register operands.
  const MachineOperand &MO = MI->getOperand(0);
  OutMI.addOperand(LowerOperand(MO));

  const MCExpr *Expr =
    MCSymbolRefExpr::create(MI->getOperand(1).getMBB()->getSymbol(), *Ctx);
  const LoongArchMCExpr *LoongArchExpr = LoongArchMCExpr::create(Kind, Expr, *Ctx);
  OutMI.addOperand(MCOperand::createExpr(LoongArchExpr));
}
bool LoongArchMCInstLower::lowerLongBranch(const MachineInstr *MI,
                                      MCInst &OutMI) const {
  switch (MI->getOpcode()) {
  default:
    return false;
  case LoongArch::LONG_BRANCH_ADDIW:
  case LoongArch::LONG_BRANCH_ADDIW2Op:
    lowerLongBranchADDI(MI, OutMI, LoongArch::ADDI_W);
    return true;
  case LoongArch::LONG_BRANCH_ADDID:
  case LoongArch::LONG_BRANCH_ADDID2Op:
    lowerLongBranchADDI(MI, OutMI, LoongArch::ADDI_D);
    return true;
  case LoongArch::LONG_BRANCH_PCADDU12I:
    lowerLongBranchPCADDU12I(MI, OutMI, LoongArch::PCADDU12I);
    return true;
  }
}

void LoongArchMCInstLower::Lower(const MachineInstr *MI, MCInst &OutMI) const {
  if (lowerLongBranch(MI, OutMI))
    return;

  OutMI.setOpcode(MI->getOpcode());

  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    const MachineOperand &MO = MI->getOperand(i);
    MCOperand MCOp = LowerOperand(MO);

    if (MCOp.isValid())
      OutMI.addOperand(MCOp);
  }
}
