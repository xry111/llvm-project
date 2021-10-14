//===-- LoongArch.h - Top-level interface for LoongArch -----------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in the LLVM
// RISC-V back-end.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LoongArch_LoongArch_H
#define LLVM_LIB_TARGET_LoongArch_LoongArch_H

#include "MCTargetDesc/LoongArchBaseInfo.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class LoongArchRegisterBankInfo;
class LoongArchSubtarget;
class LoongArchTargetMachine;
class AsmPrinter;
class FunctionPass;
class InstructionSelector;
class MCInst;
class MCOperand;
class MachineInstr;
class MachineOperand;
class PassRegistry;

void LowerLoongArchMachineInstrToMCInst(const MachineInstr *MI, MCInst &OutMI,
                                    const AsmPrinter &AP);
bool LowerLoongArchMachineOperandToMCOperand(const MachineOperand &MO,
                                         MCOperand &MCOp, const AsmPrinter &AP);

FunctionPass *createLoongArchISelDag(LoongArchTargetMachine &TM);

FunctionPass *createLoongArchExpandPseudoPass();
void initializeLoongArchExpandPseudoPass(PassRegistry &);

FunctionPass *createLoongArchExpandAtomicPseudoPass();
void initializeLoongArchExpandAtomicPseudoPass(PassRegistry &);

InstructionSelector *createLoongArchInstructionSelector(const LoongArchTargetMachine &,
                                                    LoongArchSubtarget &,
                                                    LoongArchRegisterBankInfo &);
}

#endif
