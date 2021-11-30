//===- LoongArchMachineFunctionInfo.h - Private data used for LoongArch ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the LoongArch specific subclass of MachineFunctionInfo.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_LOONGARCHMACHINEFUNCTION_H
#define LLVM_LIB_TARGET_LOONGARCH_LOONGARCHMACHINEFUNCTION_H

#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include <map>

namespace llvm {

/// LoongArchFunctionInfo - This class is derived from MachineFunction private
/// LoongArch target-specific information for each MachineFunction.
class LoongArchFunctionInfo : public MachineFunctionInfo {
public:
  LoongArchFunctionInfo(MachineFunction &MF) : MF(MF) {}

  ~LoongArchFunctionInfo() override;

  unsigned getSRetReturnReg() const { return SRetReturnReg; }
  void setSRetReturnReg(unsigned Reg) { SRetReturnReg = Reg; }

  int getVarArgsFrameIndex() const { return VarArgsFrameIndex; }
  void setVarArgsFrameIndex(int Index) { VarArgsFrameIndex = Index; }

  bool hasByvalArg() const { return HasByvalArg; }
  void setFormalArgInfo(unsigned Size, bool HasByval) {
    IncomingArgSize = Size;
    HasByvalArg = HasByval;
  }

  unsigned getIncomingArgSize() const { return IncomingArgSize; }

  bool callsEhReturn() const { return CallsEhReturn; }
  void setCallsEhReturn() { CallsEhReturn = true; }

  void createEhDataRegsFI();
  int getEhDataRegFI(unsigned Reg) const { return EhDataRegFI[Reg]; }
  bool isEhDataRegFI(int FI) const;

  /// Create a MachinePointerInfo that has an ExternalSymbolPseudoSourceValue
  /// object representing a GOT entry for an external function.
  MachinePointerInfo callPtrInfo(const char *ES);

  /// Create a MachinePointerInfo that has a GlobalValuePseudoSourceValue object
  /// representing a GOT entry for a global function.
  MachinePointerInfo callPtrInfo(const GlobalValue *GV);

  void setSaveS2() { SaveS2 = true; }
  bool hasSaveS2() const { return SaveS2; }

  int getMoveF64ViaSpillFI(const TargetRegisterClass *RC);

private:
  virtual void anchor();

  MachineFunction& MF;

  /// SRetReturnReg - Some subtargets require that sret lowering includes
  /// returning the value of the returned struct in a register. This field
  /// holds the virtual register into which the sret argument is passed.
  unsigned SRetReturnReg = 0;

  /// VarArgsFrameIndex - FrameIndex for start of varargs area.
  int VarArgsFrameIndex = 0;

  /// True if function has a byval argument.
  bool HasByvalArg;

  /// Size of incoming argument area.
  unsigned IncomingArgSize;

  /// CallsEhReturn - Whether the function calls llvm.eh.return.
  bool CallsEhReturn = false;

  /// Frame objects for spilling eh data registers.
  int EhDataRegFI[4];

  // saveS2
  bool SaveS2 = false;

  /// FrameIndex for expanding BuildPairF64 nodes to spill and reload when the
  /// LP32 FPXX ABI is enabled. -1 is used to denote invalid index.
  int MoveF64ViaSpillFI = -1;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_LOONGARCH_LOONGARCHMACHINEFUNCTION_H
