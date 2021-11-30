//===- LoongArchAsmPrinter.cpp - LoongArch LLVM Assembly Printer --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a printer that converts from our internal representation
// of machine-dependent LLVM code to GAS-format LoongArch assembly language.
//
//===----------------------------------------------------------------------===//

#include "LoongArchAsmPrinter.h"
#include "MCTargetDesc/LoongArchInstPrinter.h"
#include "MCTargetDesc/LoongArchABIInfo.h"
#include "MCTargetDesc/LoongArchBaseInfo.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "LoongArch.h"
#include "LoongArchMCInstLower.h"
#include "LoongArchMachineFunction.h"
#include "LoongArchSubtarget.h"
#include "LoongArchTargetMachine.h"
#include "LoongArchTargetStreamer.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/ADT/Twine.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Instructions.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstBuilder.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include <cassert>
#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "loongarch-asm-printer"

LoongArchTargetStreamer &LoongArchAsmPrinter::getTargetStreamer() const {
  return static_cast<LoongArchTargetStreamer &>(*OutStreamer->getTargetStreamer());
}

bool LoongArchAsmPrinter::runOnMachineFunction(MachineFunction &MF) {
  Subtarget = &MF.getSubtarget<LoongArchSubtarget>();

  LoongArchFI = MF.getInfo<LoongArchFunctionInfo>();
  MCP = MF.getConstantPool();

  AsmPrinter::runOnMachineFunction(MF);

  emitXRayTable();

  return true;
}

bool LoongArchAsmPrinter::lowerOperand(const MachineOperand &MO, MCOperand &MCOp) {
  MCOp = MCInstLowering.LowerOperand(MO);
  return MCOp.isValid();
}

#include "LoongArchGenMCPseudoLowering.inc"

// Lower PseudoReturn/PseudoIndirectBranch/PseudoIndirectBranch64 to
// JIRL as appropriate for the target.
void LoongArchAsmPrinter::emitPseudoIndirectBranch(MCStreamer &OutStreamer,
                                              const MachineInstr *MI) {
  bool HasLinkReg = false;
  MCInst TmpInst0;
  TmpInst0.setOpcode(LoongArch::JIRL);
  HasLinkReg = true;

  MCOperand MCOp;

  if (HasLinkReg) {
    unsigned ZeroReg = Subtarget->is64Bit() ? LoongArch::ZERO_64 : LoongArch::ZERO;
    TmpInst0.addOperand(MCOperand::createReg(ZeroReg));
  }

  lowerOperand(MI->getOperand(0), MCOp);
  TmpInst0.addOperand(MCOp);

  TmpInst0.addOperand(MCOperand::createImm(0));

  EmitToStreamer(OutStreamer, TmpInst0);
}

void LoongArchAsmPrinter::emitInstruction(const MachineInstr *MI) {
  LoongArchTargetStreamer &TS = getTargetStreamer();
  unsigned Opc = MI->getOpcode();
  TS.forbidModuleDirective();

  if (MI->isDebugValue()) {
    SmallString<128> Str;
    raw_svector_ostream OS(Str);

    PrintDebugValueComment(MI, OS);
    return;
  }
  if (MI->isDebugLabel())
    return;
  // If we just ended a constant pool, mark it as such.
  OutStreamer->emitDataRegion(MCDR_DataRegionEnd);
  InConstantPool = false;

  switch (Opc) {
  case LoongArch::PATCHABLE_FUNCTION_ENTER:
    LowerPATCHABLE_FUNCTION_ENTER(*MI);
    return;
  case LoongArch::PATCHABLE_FUNCTION_EXIT:
    LowerPATCHABLE_FUNCTION_EXIT(*MI);
    return;
  case LoongArch::PATCHABLE_TAIL_CALL:
    LowerPATCHABLE_TAIL_CALL(*MI);
    return;
  }
  MachineBasicBlock::const_instr_iterator I = MI->getIterator();
  MachineBasicBlock::const_instr_iterator E = MI->getParent()->instr_end();

  do {
    // Do any auto-generated pseudo lowerings.
    if (emitPseudoExpansionLowering(*OutStreamer, &*I))
      continue;
    if (I->getOpcode() == LoongArch::PseudoReturn ||
        I->getOpcode() == LoongArch::PseudoReturn64){
      emitPseudoIndirectBranch(*OutStreamer, &*I);
      continue;
    }

    // Some instructions are marked as pseudo right now which
    // would make the test fail for the wrong reason but
    // that will be fixed soon. We need this here because we are
    // removing another test for this situation downstream in the
    // callchain.
    //
    if (I->isPseudo()
        && !isLongBranchPseudo(I->getOpcode()))
      llvm_unreachable("Pseudo opcode found in EmitInstruction()");

    MCInst TmpInst0;
    MCInstLowering.Lower(&*I, TmpInst0);
    EmitToStreamer(*OutStreamer, TmpInst0);
  } while ((++I != E) && I->isInsideBundle());
}

//===----------------------------------------------------------------------===//
//
//  LoongArch Asm Directives
//
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Set directives
//===----------------------------------------------------------------------===//

/// Emit Set directives.
const char *LoongArchAsmPrinter::getCurrentABIString() const {
  switch (static_cast<LoongArchTargetMachine &>(TM).getABI().GetEnumValue()) {
  case LoongArchABIInfo::ABI::LP32:  return "abilp32";
  case LoongArchABIInfo::ABI::LPX32:  return "abilpx32";
  case LoongArchABIInfo::ABI::LP64:  return "abilp64";
  default: llvm_unreachable("Unknown LoongArch ABI");
  }
}

void LoongArchAsmPrinter::emitFunctionEntryLabel() {

  OutStreamer->emitLabel(CurrentFnSym);

}

/// EmitFunctionBodyStart - Targets can override this to emit stuff before
/// the first basic block in the function.
void LoongArchAsmPrinter::emitFunctionBodyStart() {

  MCInstLowering.Initialize(&MF->getContext());
}

/// EmitFunctionBodyEnd - Targets can override this to emit stuff after
/// the last basic block in the function.
void LoongArchAsmPrinter::emitFunctionBodyEnd() {

  // Make sure to terminate any constant pools that were at the end
  // of the function.
  if (!InConstantPool)
    return;
  InConstantPool = false;
  OutStreamer->emitDataRegion(MCDR_DataRegionEnd);
}

void LoongArchAsmPrinter::emitBasicBlockEnd(const MachineBasicBlock &MBB) {
  AsmPrinter::emitBasicBlockEnd(MBB);
}

/// isBlockOnlyReachableByFallthough - Return true if the basic block has
/// exactly one predecessor and the control transfer mechanism between
/// the predecessor and this block is a fall-through.
bool LoongArchAsmPrinter::isBlockOnlyReachableByFallthrough(const MachineBasicBlock*
                                                       MBB) const {
  // The predecessor has to be immediately before this block.
  const MachineBasicBlock *Pred = *MBB->pred_begin();

  // If the predecessor is a switch statement, assume a jump table
  // implementation, so it is not a fall through.
  if (const BasicBlock *bb = Pred->getBasicBlock())
    if (isa<SwitchInst>(bb->getTerminator()))
      return false;

  // Check default implementation
  return AsmPrinter::isBlockOnlyReachableByFallthrough(MBB);
}

// Print out an operand for an inline asm expression.
bool LoongArchAsmPrinter::PrintAsmOperand(const MachineInstr *MI,
    unsigned OpNum, const char *ExtraCode, raw_ostream &O) {
  // Does this asm operand have a single letter operand modifier?
  if (ExtraCode && ExtraCode[0]) {
    if (ExtraCode[1] != 0) return true; // Unknown modifier.

    const MachineOperand &MO = MI->getOperand(OpNum);
    switch (ExtraCode[0]) {
    default:
      // See if this is a generic print operand
      return AsmPrinter::PrintAsmOperand(MI,OpNum,ExtraCode,O);
    case 'X': // hex const int
      if ((MO.getType()) != MachineOperand::MO_Immediate)
        return true;
      O << "0x" << Twine::utohexstr(MO.getImm());
      return false;
    case 'x': // hex const int (low 16 bits)
      if ((MO.getType()) != MachineOperand::MO_Immediate)
        return true;
      O << "0x" << Twine::utohexstr(MO.getImm() & 0xffff);
      return false;
    case 'd': // decimal const int
      if ((MO.getType()) != MachineOperand::MO_Immediate)
        return true;
      O << MO.getImm();
      return false;
    case 'm': // decimal const int minus 1
      if ((MO.getType()) != MachineOperand::MO_Immediate)
        return true;
      O << MO.getImm() - 1;
      return false;
    case 'y': // exact log2
      if ((MO.getType()) != MachineOperand::MO_Immediate)
        return true;
      if (!isPowerOf2_64(MO.getImm()))
        return true;
      O << Log2_64(MO.getImm());
      return false;
    case 'z':
      // $0 if zero, regular printing otherwise
      if (MO.getType() == MachineOperand::MO_Immediate && MO.getImm() == 0) {
        O << "$0";
        return false;
      }
      // If not, call printOperand as normal.
      break;
    case 'D': // Second part of a double word register operand
    case 'L': // Low order register of a double word register operand
    case 'M': // High order register of a double word register operand
    {
      if (OpNum == 0)
        return true;
      const MachineOperand &FlagsOP = MI->getOperand(OpNum - 1);
      if (!FlagsOP.isImm())
        return true;
      unsigned Flags = FlagsOP.getImm();
      unsigned NumVals = InlineAsm::getNumOperandRegisters(Flags);
      // Number of registers represented by this operand. We are looking
      // for 2 for 32 bit mode and 1 for 64 bit mode.
      if (NumVals != 2) {
        if (Subtarget->is64Bit() && NumVals == 1 && MO.isReg()) {
          unsigned Reg = MO.getReg();
          O << '$' << LoongArchInstPrinter::getRegisterName(Reg);
          return false;
        }
        return true;
      }

      unsigned RegOp = OpNum;
      if (!Subtarget->is64Bit()){
        // Endianness reverses which register holds the high or low value
        // between M and L.
        switch(ExtraCode[0]) {
        case 'M':
          RegOp = OpNum + 1;
          break;
        case 'L':
          RegOp = OpNum;
          break;
        case 'D': // Always the second part
          RegOp = OpNum + 1;
        }
        if (RegOp >= MI->getNumOperands())
          return true;
        const MachineOperand &MO = MI->getOperand(RegOp);
        if (!MO.isReg())
          return true;
        unsigned Reg = MO.getReg();
        O << '$' << LoongArchInstPrinter::getRegisterName(Reg);
        return false;
      }
      break;
    }
    }
  }

  printOperand(MI, OpNum, O);
  return false;
}

bool LoongArchAsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                                unsigned OpNum,
                                                const char *ExtraCode,
                                                raw_ostream &O) {
  assert(OpNum + 1 < MI->getNumOperands() && "Insufficient operands");
  const MachineOperand &BaseMO = MI->getOperand(OpNum);
  const MachineOperand &OffsetMO = MI->getOperand(OpNum + 1);
  assert(BaseMO.isReg() && "Unexpected base pointer for inline asm memory operand.");
  assert(OffsetMO.isImm() && "Unexpected offset for inline asm memory operand.");
  int Offset = OffsetMO.getImm();

  // Currently we are expecting either no ExtraCode or 'D','M','L'.
  if (ExtraCode) {
    switch (ExtraCode[0]) {
    case 'D':
    case 'M':
      Offset += 4;
      break;
    case 'L':
      break;
    default:
      return true; // Unknown modifier.
    }
  }

  O << "$" << LoongArchInstPrinter::getRegisterName(BaseMO.getReg()) << ", " << Offset;

  return false;
}

void LoongArchAsmPrinter::printOperand(const MachineInstr *MI, int opNum,
                                  raw_ostream &O) {
  const MachineOperand &MO = MI->getOperand(opNum);

  switch (MO.getType()) {
    case MachineOperand::MO_Register:
      O << '$'
        << StringRef(LoongArchInstPrinter::getRegisterName(MO.getReg())).lower();
      break;

    case MachineOperand::MO_Immediate:
      O << MO.getImm();
      break;

    case MachineOperand::MO_MachineBasicBlock:
      MO.getMBB()->getSymbol()->print(O, MAI);
      return;

    case MachineOperand::MO_GlobalAddress:
      getSymbol(MO.getGlobal())->print(O, MAI);
      break;

    case MachineOperand::MO_BlockAddress: {
      MCSymbol *BA = GetBlockAddressSymbol(MO.getBlockAddress());
      O << BA->getName();
      break;
    }

    case MachineOperand::MO_ConstantPoolIndex:
      O << getDataLayout().getPrivateGlobalPrefix() << "CPI"
        << getFunctionNumber() << "_" << MO.getIndex();
      if (MO.getOffset())
        O << "+" << MO.getOffset();
      break;

    default:
      llvm_unreachable("<unknown operand type>");
  }
}

void LoongArchAsmPrinter::
printMemOperand(const MachineInstr *MI, int opNum, raw_ostream &O) {
  // Load/Store memory operands -- imm($reg)
  // If PIC target the target is loaded as the
  // pattern lw $25,%call16($28)

  printOperand(MI, opNum+1, O);
  O << "(";
  printOperand(MI, opNum, O);
  O << ")";
}

void LoongArchAsmPrinter::
printMemOperandEA(const MachineInstr *MI, int opNum, raw_ostream &O) {
  // when using stack locations for not load/store instructions
  // print the same way as all normal 3 operand instructions.
  printOperand(MI, opNum, O);
  O << ", ";
  printOperand(MI, opNum+1, O);
}

void LoongArchAsmPrinter::
printRegisterList(const MachineInstr *MI, int opNum, raw_ostream &O) {
  for (int i = opNum, e = MI->getNumOperands(); i != e; ++i) {
    if (i != opNum) O << ", ";
    printOperand(MI, i, O);
  }
}

void LoongArchAsmPrinter::emitStartOfAsmFile(Module &M) {
  LoongArchTargetStreamer &TS = getTargetStreamer();

  // LoongArchTargetStreamer has an initialization order problem when emitting an
  // object file directly (see LoongArchTargetELFStreamer for full details). Work
  // around it by re-initializing the PIC state here.
  TS.setPic(OutContext.getObjectFileInfo()->isPositionIndependent());

  // Compute LoongArch architecture attributes based on the default subtarget
  // that we'd have constructed. Module level directives aren't LTO
  // clean anyhow.
  // FIXME: For ifunc related functions we could iterate over and look
  // for a feature string that doesn't match the default one.
  const Triple &TT = TM.getTargetTriple();
  StringRef CPU = LoongArch_MC::selectLoongArchCPU(TT, TM.getTargetCPU());
  StringRef FS = TM.getTargetFeatureString();
  const LoongArchTargetMachine &MTM = static_cast<const LoongArchTargetMachine &>(TM);
  const LoongArchSubtarget STI(TT, CPU, FS, MTM, None);

  const LoongArchABIInfo &ABI = MTM.getABI();

  // Tell the assembler which ABI we are using
  std::string SectionName = std::string(".mdebug.") + getCurrentABIString();
  OutStreamer->SwitchSection(
      OutContext.getELFSection(SectionName, ELF::SHT_PROGBITS, 0));

  // TODO: handle O64 ABI

  TS.updateABIInfo(STI);

  // We should always emit a '.module fp=...' but binutils 2.31 does not accept
  // it. We therefore emit it when it contradicts the ABI defaults (-mfp64)
  // and omit it otherwise.
  if ((ABI.IsLP32() && (STI.isFP64bit())) ||
      STI.useSoftFloat())
    TS.emitDirectiveModuleFP();
}

void LoongArchAsmPrinter::emitInlineAsmStart() const {

  OutStreamer->AddBlankLine();
}

void LoongArchAsmPrinter::emitInlineAsmEnd(const MCSubtargetInfo &StartInfo,
                                      const MCSubtargetInfo *EndInfo) const {
  OutStreamer->AddBlankLine();
}

void LoongArchAsmPrinter::EmitInstrReg(const MCSubtargetInfo &STI, unsigned Opcode,
                                  unsigned Reg) {
  MCInst I;
  I.setOpcode(Opcode);
  I.addOperand(MCOperand::createReg(Reg));
  OutStreamer->emitInstruction(I, STI);
}

void LoongArchAsmPrinter::EmitInstrRegReg(const MCSubtargetInfo &STI,
                                     unsigned Opcode, unsigned Reg1,
                                     unsigned Reg2) {
  MCInst I;
  //
  // Because of the current td files for LoongArch32, the operands for MTC1
  // appear backwards from their normal assembly order. It's not a trivial
  // change to fix this in the td file so we adjust for it here.
  //
  if (Opcode == LoongArch::MOVGR2FR_W) {
    unsigned Temp = Reg1;
    Reg1 = Reg2;
    Reg2 = Temp;
  }
  I.setOpcode(Opcode);
  I.addOperand(MCOperand::createReg(Reg1));
  I.addOperand(MCOperand::createReg(Reg2));
  OutStreamer->emitInstruction(I, STI);
}

void LoongArchAsmPrinter::EmitInstrRegRegReg(const MCSubtargetInfo &STI,
                                        unsigned Opcode, unsigned Reg1,
                                        unsigned Reg2, unsigned Reg3) {
  MCInst I;
  I.setOpcode(Opcode);
  I.addOperand(MCOperand::createReg(Reg1));
  I.addOperand(MCOperand::createReg(Reg2));
  I.addOperand(MCOperand::createReg(Reg3));
  OutStreamer->emitInstruction(I, STI);
}

void LoongArchAsmPrinter::EmitMovFPIntPair(const MCSubtargetInfo &STI,
                                      unsigned MovOpc, unsigned Reg1,
                                      unsigned Reg2, unsigned FPReg1,
                                      unsigned FPReg2, bool LE) {
  if (!LE) {
    unsigned temp = Reg1;
    Reg1 = Reg2;
    Reg2 = temp;
  }
  EmitInstrRegReg(STI, MovOpc, Reg1, FPReg1);
  EmitInstrRegReg(STI, MovOpc, Reg2, FPReg2);
}

void LoongArchAsmPrinter::emitEndOfAsmFile(Module &M) {
  // return to the text section
  OutStreamer->SwitchSection(OutContext.getObjectFileInfo()->getTextSection());
}

void LoongArchAsmPrinter::EmitSled(const MachineInstr &MI, SledKind Kind) {
// Now this is unimplemented.
}

void LoongArchAsmPrinter::LowerPATCHABLE_FUNCTION_ENTER(const MachineInstr &MI) {
  EmitSled(MI, SledKind::FUNCTION_ENTER);
}

void LoongArchAsmPrinter::LowerPATCHABLE_FUNCTION_EXIT(const MachineInstr &MI) {
  EmitSled(MI, SledKind::FUNCTION_EXIT);
}

void LoongArchAsmPrinter::LowerPATCHABLE_TAIL_CALL(const MachineInstr &MI) {
  EmitSled(MI, SledKind::TAIL_CALL);
}

void LoongArchAsmPrinter::PrintDebugValueComment(const MachineInstr *MI,
                                           raw_ostream &OS) {
  // TODO: implement
}

bool LoongArchAsmPrinter::isLongBranchPseudo(int Opcode) const {
    return (Opcode == LoongArch::LONG_BRANCH_ADDIW
          || Opcode == LoongArch::LONG_BRANCH_ADDIW2Op
          || Opcode == LoongArch::LONG_BRANCH_ADDID
          || Opcode == LoongArch::LONG_BRANCH_ADDID2Op
          || Opcode == LoongArch::LONG_BRANCH_PCADDU12I);
}

// Force static initialization.
extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeLoongArchAsmPrinter() {
  RegisterAsmPrinter<LoongArchAsmPrinter> X(getTheLoongArch32Target());
  RegisterAsmPrinter<LoongArchAsmPrinter> A(getTheLoongArch64Target());
}
