//===-- LoongArchInstPrinter.cpp - Convert LoongArch MCInst to assembly syntax ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This class prints an LoongArch MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#include "LoongArchInstPrinter.h"
#include "MCTargetDesc/LoongArchMCExpr.h"
#include "LoongArchInstrInfo.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

#define PRINT_ALIAS_INSTR
#include "LoongArchGenAsmWriter.inc"

template<unsigned R>
static bool isReg(const MCInst &MI, unsigned OpNo) {
  assert(MI.getOperand(OpNo).isReg() && "Register operand expected.");
  return MI.getOperand(OpNo).getReg() == R;
}

const char* LoongArch::LoongArchFCCToString(LoongArch::CondCode CC) {
  switch (CC) {
  case FCOND_T:
  case FCOND_F:   return "caf";
  case FCOND_OR:
  case FCOND_UN:  return "cun";
  case FCOND_UNE:
  case FCOND_OEQ: return "ceq";
  case FCOND_ONE:
  case FCOND_UEQ: return "cueq";
  case FCOND_UGE:
  case FCOND_OLT: return "clt";
  case FCOND_OGE:
  case FCOND_ULT: return "cult";
  case FCOND_UGT:
  case FCOND_OLE: return "cle";
  case FCOND_OGT:
  case FCOND_ULE: return "cule";
  case FCOND_ST:
  case FCOND_SF:  return "saf";
  case FCOND_GLE:
  case FCOND_NGLE:return "sun";
  case FCOND_SEQ: return "seq";
  case FCOND_SNE: return "sne";
  case FCOND_GL:
  case FCOND_NGL: return "sueq";
  case FCOND_NLT:
  case FCOND_LT:  return "slt";
  case FCOND_GE:
  case FCOND_NGE: return "sult";
  case FCOND_NLE:
  case FCOND_LE:  return "sle";
  case FCOND_GT:
  case FCOND_NGT: return "sule";
  case FCOND_CNE:  return "cne";
  case FCOND_COR:  return "cor";
  case FCOND_SOR:  return "sor";
  case FCOND_CUNE:  return "cune";
  case FCOND_SUNE:  return "sune";
  }
  llvm_unreachable("Impossible condition code!");
}

void LoongArchInstPrinter::printRegName(raw_ostream &OS, unsigned RegNo) const {
  OS << '$' << StringRef(getRegisterName(RegNo)).lower();
}

void LoongArchInstPrinter::printInst(const MCInst *MI, uint64_t Address,
                                     StringRef Annot,
                                     const MCSubtargetInfo &STI,
                                     raw_ostream &O) {
  switch (MI->getOpcode()) {
  default:
    break;
  case LoongArch::PCADDU12I_ri:
  case LoongArch::PCADDU12I_rii:
  case LoongArch::LU12I_W_ri:
    printLoadAddr(MI, O);
    return;
  case LoongArch::ADD_D_rrr:
  case LoongArch::LDX_D_rrr:
  case LoongArch::ADDI_D_rri:
  case LoongArch::ADDI_D_rrii:
  case LoongArch::LD_D_rri:
  case LoongArch::LD_D_rrii:
  case LoongArch::ORI_rri:
  case LoongArch::ORI_rrii:
  case LoongArch::LU32I_D_ri:
  case LoongArch::LU32I_D_rii:
  case LoongArch::LU52I_D_rri:
  case LoongArch::LU52I_D_rrii:
    O << "\t# la expanded slot";
    return;
  }

  // Try to print any aliases first.
  if (!printAliasInstr(MI, Address, O) && !printAlias(*MI, O))
    printInstruction(MI, Address, O);
  printAnnotation(O, Annot);
}

void LoongArchInstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                   raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNo);
  if (Op.isReg()) {
    printRegName(O, Op.getReg());
    return;
  }

  if (Op.isImm()) {
    O << formatImm(Op.getImm());
    return;
  }

  assert(Op.isExpr() && "unknown operand kind in printOperand");
  Op.getExpr()->print(O, &MAI, true);
}

template <unsigned Bits, unsigned Offset>
void LoongArchInstPrinter::printUImm(const MCInst *MI, int opNum, raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(opNum);
  if (MO.isImm()) {
    uint64_t Imm = MO.getImm();
    Imm -= Offset;
    Imm &= (1 << Bits) - 1;
    Imm += Offset;
    O << formatImm(Imm);
    return;
  }

  printOperand(MI, opNum, O);
}

void LoongArchInstPrinter::
printMemOperand(const MCInst *MI, int opNum, raw_ostream &O) {
  // Load/Store memory operands -- $reg, imm
  printOperand(MI, opNum, O);
  O << ", ";
  printOperand(MI, opNum+1, O);
}

void LoongArchInstPrinter::
printMemOperandEA(const MCInst *MI, int opNum, raw_ostream &O) {
  // when using stack locations for not load/store instructions
  // print the same way as all normal 3 operand instructions.
  printOperand(MI, opNum, O);
  O << ", ";
  printOperand(MI, opNum+1, O);
}

void LoongArchInstPrinter::
printFCCOperand(const MCInst *MI, int opNum, raw_ostream &O) {
  const MCOperand& MO = MI->getOperand(opNum);
  O << LoongArchFCCToString((LoongArch::CondCode)MO.getImm());
}

bool LoongArchInstPrinter::printAlias(const char *Str, const MCInst &MI,
                                 unsigned OpNo, raw_ostream &OS) {
  OS << "\t" << Str << "\t";
  if(MI.getOpcode() == LoongArch::JIRL) {
    printOperand(&MI, OpNo, OS);
    OS << "@plt";
  }else
    printOperand(&MI, OpNo, OS);
  return true;
}

bool LoongArchInstPrinter::printAlias(const char *Str, const MCInst &MI,
                                 unsigned OpNo0, unsigned OpNo1,
                                 raw_ostream &OS) {
  printAlias(Str, MI, OpNo0, OS);
  OS << ", ";
  printOperand(&MI, OpNo1, OS);
  return true;
}

bool LoongArchInstPrinter::printAlias(const MCInst &MI, raw_ostream &OS) {
  switch (MI.getOpcode()) {
    case LoongArch::OR:
      // or $r0, $r1, $zero => move $r0, $r1
      return isReg<LoongArch::ZERO>(MI, 2) && printAlias("move", MI, 0, 1, OS);
  default: return false;
  }
}

void LoongArchInstPrinter::
printRegisterList(const MCInst *MI, int opNum, raw_ostream &O) {
  // - 2 because register List is always first operand of instruction and it is
  // always followed by memory operand (base + offset).
  for (int i = opNum, e = MI->getNumOperands() - 2; i != e; ++i) {
    if (i != opNum)
      O << ", ";
    printRegName(O, MI->getOperand(i).getReg());
  }
}

void LoongArchInstPrinter::
printLoadAddr(const MCInst *MI, raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(1);
  const MCExpr *Expr = Op.getExpr();
  const LoongArchMCExpr *LoongArchExpr = cast<LoongArchMCExpr>(Expr);
  switch (LoongArchExpr->getKind()) {
    default:
      llvm_unreachable("invalid handled!");
      return;
    case LoongArchMCExpr::MEK_ABS_HI:
      O << "\tla.abs\t";
      break;
    case LoongArchMCExpr::MEK_GOT_HI:
      O << "\tla.got\t";
      break;
    case LoongArchMCExpr::MEK_PCREL_HI:
      O << "\tla.pcrel\t";
      break;
    case LoongArchMCExpr::MEK_TLSGD_HI:
      O << "\tla.tls.gd\t";
      break;
    case LoongArchMCExpr::MEK_TLSIE_HI:
      O << "\tla.tls.ie\t";
      break;
    case LoongArchMCExpr::MEK_TLSLE_HI:
      O << "\tla.tls.le\t";
      break;
  }
  printRegName(O, MI->getOperand(0).getReg());
  O << ", ";
  Expr->print(O, nullptr);
  return;
}
