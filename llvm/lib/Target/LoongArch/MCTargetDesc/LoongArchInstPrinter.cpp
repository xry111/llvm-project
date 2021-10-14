//===-- LoongArchInstPrinter.cpp - Convert LoongArch MCInst to asm syntax -===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This class prints an LoongArch MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#include "LoongArchInstPrinter.h"
#include "LoongArchBaseInfo.h"
#include "LoongArchMCExpr.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

// Include the auto-generated portion of the assembly writer.
#define PRINT_ALIAS_INSTR
#include "LoongArchGenAsmWriter.inc"

static cl::opt<bool>
    ArchRegNames("loongarch-arch-reg-names",
                 cl::desc("Print architectural register names rather than the "
                          "ABI names (such as r3 instead of sp)"),
                 cl::init(false), cl::Hidden);

// The command-line flags above are used by llvm-mc and llc. They can be used by
// `llvm-objdump`, but we override their values here to handle options passed to
// `llvm-objdump` with `-M` (which matches GNU objdump). There did not seem to
// be an easier way to allow these options in all these tools, without doing it
// this way.
bool LoongArchInstPrinter::applyTargetSpecificCLOption(StringRef Opt) {
  if (Opt == "numeric") {
    ArchRegNames = true;
    return true;
  }

  return false;
}

void LoongArchInstPrinter::printInst(const MCInst *MI, uint64_t Address,
                                     StringRef Annot,
                                     const MCSubtargetInfo &STI,
                                     raw_ostream &O) {
  bool Res = false;
  const MCInst *NewMI = MI;
  MCInst UncompressedMI;
  if (Res)
    NewMI = const_cast<MCInst *>(&UncompressedMI);
  if (!printAliasInstr(NewMI, Address, STI, O))
    printInstruction(NewMI, Address, STI, O);
  printAnnotation(O, Annot);
}

void LoongArchInstPrinter::printRegName(raw_ostream &O, unsigned RegNo) const {
  O << '$' << getRegisterName(RegNo);
}

void LoongArchInstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                        const MCSubtargetInfo &STI,
                                        raw_ostream &O, const char *Modifier) {
  assert((Modifier == 0 || Modifier[0] == 0) && "No modifiers supported");
  const MCOperand &MO = MI->getOperand(OpNo);

  if (MO.isReg()) {
    printRegName(O, MO.getReg());
    return;
  }

  if (MO.isImm()) {
    O << MO.getImm();
    return;
  }

  assert(MO.isExpr() && "Unknown operand kind in printOperand");
  MO.getExpr()->print(O, &MAI);
}

void LoongArchInstPrinter::printBranchOperand(const MCInst *MI,
                                              uint64_t Address, unsigned OpNo,
                                              const MCSubtargetInfo &STI,
                                              raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(OpNo);
  if (!MO.isImm())
    return printOperand(MI, OpNo, STI, O);

  if (PrintBranchImmAsAddress) {
    uint64_t Target = Address + MO.getImm();
    if (!STI.hasFeature(LoongArch::Feature64Bit))
      Target &= 0xffffffff;
    O << formatHex(Target);
  } else {
    O << MO.getImm();
  }
}

void LoongArchInstPrinter::printCSRSystemRegister(const MCInst *MI,
                                                  unsigned OpNo,
                                                  const MCSubtargetInfo &STI,
                                                  raw_ostream &O) {
  unsigned Imm = MI->getOperand(OpNo).getImm();
  auto SysReg = LoongArchSysReg::lookupSysRegByEncoding(Imm);
  if (SysReg && SysReg->haveRequiredFeatures(STI.getFeatureBits()))
    O << SysReg->Name;
  else
    O << Imm;
}

void LoongArchInstPrinter::printFenceArg(const MCInst *MI, unsigned OpNo,
                                         const MCSubtargetInfo &STI,
                                         raw_ostream &O) {
  unsigned FenceArg = MI->getOperand(OpNo).getImm();
  assert(((FenceArg >> 4) == 0) && "Invalid immediate in printFenceArg");

  if ((FenceArg & LoongArchFenceField::I) != 0)
    O << 'i';
  if ((FenceArg & LoongArchFenceField::O) != 0)
    O << 'o';
  if ((FenceArg & LoongArchFenceField::R) != 0)
    O << 'r';
  if ((FenceArg & LoongArchFenceField::W) != 0)
    O << 'w';
  if (FenceArg == 0)
    O << "unknown";
}

void LoongArchInstPrinter::printFRMArg(const MCInst *MI, unsigned OpNo,
                                       const MCSubtargetInfo &STI,
                                       raw_ostream &O) {
  auto FRMArg = static_cast<LoongArchFPRndMode::RoundingMode>(
      MI->getOperand(OpNo).getImm());
  O << LoongArchFPRndMode::roundingModeToString(FRMArg);
}

void LoongArchInstPrinter::printAtomicMemOp(const MCInst *MI, unsigned OpNo,
                                            const MCSubtargetInfo &STI,
                                            raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(OpNo);

  assert(MO.isReg() && "printAtomicMemOp can only print register operands");
  O << "(";
  printRegName(O, MO.getReg());
  O << ")";
}

void LoongArchInstPrinter::printVTypeI(const MCInst *MI, unsigned OpNo,
                                       const MCSubtargetInfo &STI,
                                       raw_ostream &O) {
  unsigned Imm = MI->getOperand(OpNo).getImm();
  LoongArchVType::printVType(Imm, O);
}

void LoongArchInstPrinter::printVMaskReg(const MCInst *MI, unsigned OpNo,
                                         const MCSubtargetInfo &STI,
                                         raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(OpNo);

  assert(MO.isReg() && "printVMaskReg can only print register operands");
  if (MO.getReg() == LoongArch::NoRegister)
    return;
  O << ", ";
  printRegName(O, MO.getReg());
  O << ".t";
}

const char *LoongArchInstPrinter::getRegisterName(unsigned RegNo) {
  return getRegisterName(RegNo, ArchRegNames ? LoongArch::NoRegAltName
                                             : LoongArch::ABIRegAltName);
}
