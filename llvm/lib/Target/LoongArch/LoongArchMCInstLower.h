//===- LoongArchMCInstLower.h - Lower MachineInstr to MCInst --------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_LOONGARCHMCINSTLOWER_H
#define LLVM_LIB_TARGET_LOONGARCH_LOONGARCHMCINSTLOWER_H

#include "MCTargetDesc/LoongArchMCExpr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/Support/Compiler.h"

namespace llvm {

class MachineBasicBlock;
class MachineInstr;
class MCContext;
class MCInst;
class MCOperand;
class LoongArchAsmPrinter;

/// LoongArchMCInstLower - This class is used to lower an MachineInstr into an
///                   MCInst.
class LLVM_LIBRARY_VISIBILITY LoongArchMCInstLower {
  using MachineOperandType = MachineOperand::MachineOperandType;

  MCContext *Ctx;
  LoongArchAsmPrinter &AsmPrinter;

public:
  LoongArchMCInstLower(LoongArchAsmPrinter &asmprinter);

  void Initialize(MCContext *C);
  void Lower(const MachineInstr *MI, MCInst &OutMI) const;
  MCOperand LowerOperand(const MachineOperand& MO, unsigned offset = 0) const;

private:
  MCOperand LowerSymbolOperand(const MachineOperand &MO,
                               MachineOperandType MOTy, unsigned Offset) const;
  MCOperand createSub(MachineBasicBlock *BB1, MachineBasicBlock *BB2,
                      LoongArchMCExpr::LoongArchExprKind Kind) const;
  void lowerLongBranchLUi(const MachineInstr *MI, MCInst &OutMI) const;
  void lowerLongBranchADDI(const MachineInstr *MI, MCInst &OutMI,
                           int Opcode) const;
  void lowerLongBranchPCADDU12I(const MachineInstr *MI, MCInst &OutMI,
                            int Opcode) const;
  bool lowerLongBranch(const MachineInstr *MI, MCInst &OutMI) const;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_LOONGARCH_LOONGARCHMCINSTLOWER_H
