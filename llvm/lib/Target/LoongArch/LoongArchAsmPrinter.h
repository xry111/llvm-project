//===- LoongArchAsmPrinter.h - LoongArch LLVM Assembly Printer -----------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// LoongArch Assembly printer class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_LOONGARCHASMPRINTER_H
#define LLVM_LIB_TARGET_LOONGARCH_LOONGARCHASMPRINTER_H

#include "LoongArchMCInstLower.h"
#include "LoongArchSubtarget.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/Compiler.h"
#include <algorithm>
#include <map>
#include <memory>

namespace llvm {

class MCOperand;
class MCSubtargetInfo;
class MCSymbol;
class MachineBasicBlock;
class MachineConstantPool;
class MachineFunction;
class MachineInstr;
class MachineOperand;
class LoongArchFunctionInfo;
class LoongArchTargetStreamer;
class Module;
class raw_ostream;
class TargetMachine;

class LLVM_LIBRARY_VISIBILITY LoongArchAsmPrinter : public AsmPrinter {
  LoongArchTargetStreamer &getTargetStreamer() const;

  void EmitInstrWithMacroNoAT(const MachineInstr *MI);

  //===------------------------------------------------------------------===//
  // XRay implementation
  //===------------------------------------------------------------------===//

public:
  // XRay-specific lowering for LoongArch.
  void LowerPATCHABLE_FUNCTION_ENTER(const MachineInstr &MI);
  void LowerPATCHABLE_FUNCTION_EXIT(const MachineInstr &MI);
  void LowerPATCHABLE_TAIL_CALL(const MachineInstr &MI);

private:
  /// MCP - Keep a pointer to constantpool entries of the current
  /// MachineFunction.
  const MachineConstantPool *MCP = nullptr;

  /// InConstantPool - Maintain state when emitting a sequence of constant
  /// pool entries so we can properly mark them as data regions.
  bool InConstantPool = false;

  void EmitSled(const MachineInstr &MI, SledKind Kind);

  // tblgen'erated function.
  bool emitPseudoExpansionLowering(MCStreamer &OutStreamer,
                                   const MachineInstr *MI);

  // Emit PseudoReturn, PseudoReturn64, PseudoIndirectBranch,
  // and PseudoIndirectBranch64 as a JIRL as appropriate
  // for the target.
  void emitPseudoIndirectBranch(MCStreamer &OutStreamer,
                                const MachineInstr *MI);

  // lowerOperand - Convert a MachineOperand into the equivalent MCOperand.
  bool lowerOperand(const MachineOperand &MO, MCOperand &MCOp);

  void emitInlineAsmStart() const override;

  void emitInlineAsmEnd(const MCSubtargetInfo &StartInfo,
                        const MCSubtargetInfo *EndInfo) const override;

  void EmitInstrReg(const MCSubtargetInfo &STI, unsigned Opcode, unsigned Reg);

  void EmitInstrRegReg(const MCSubtargetInfo &STI, unsigned Opcode,
                       unsigned Reg1, unsigned Reg2);

  void EmitInstrRegRegReg(const MCSubtargetInfo &STI, unsigned Opcode,
                          unsigned Reg1, unsigned Reg2, unsigned Reg3);

  void EmitMovFPIntPair(const MCSubtargetInfo &STI, unsigned MovOpc,
                        unsigned Reg1, unsigned Reg2, unsigned FPReg1,
                        unsigned FPReg2, bool LE);

  bool isLongBranchPseudo(int Opcode) const;

public:
  const LoongArchSubtarget *Subtarget;
  const LoongArchFunctionInfo *LoongArchFI;
  LoongArchMCInstLower MCInstLowering;

  explicit LoongArchAsmPrinter(TargetMachine &TM,
                          std::unique_ptr<MCStreamer> Streamer)
      : AsmPrinter(TM, std::move(Streamer)), MCInstLowering(*this) {}

  StringRef getPassName() const override { return "LoongArch Assembly Printer"; }

  bool runOnMachineFunction(MachineFunction &MF) override;

  void emitInstruction(const MachineInstr *MI) override;
  const char *getCurrentABIString() const;
  void emitFunctionEntryLabel() override;
  void emitFunctionBodyStart() override;
  void emitFunctionBodyEnd() override;
  void emitBasicBlockEnd(const MachineBasicBlock &MBB) override;
  bool isBlockOnlyReachableByFallthrough(
                                   const MachineBasicBlock* MBB) const override;
  bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                       const char *ExtraCode, raw_ostream &O) override;
  bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNum,
                             const char *ExtraCode, raw_ostream &O) override;
  void printOperand(const MachineInstr *MI, int opNum, raw_ostream &O);
  void printMemOperand(const MachineInstr *MI, int opNum, raw_ostream &O);
  void printMemOperandEA(const MachineInstr *MI, int opNum, raw_ostream &O);
  void printRegisterList(const MachineInstr *MI, int opNum, raw_ostream &O);
  void emitStartOfAsmFile(Module &M) override;
  void emitEndOfAsmFile(Module &M) override;
  void PrintDebugValueComment(const MachineInstr *MI, raw_ostream &OS);
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_LOONGARCH_LOONGARCHASMPRINTER_H
