//===-- LoongArch.h - Top-level interface for LoongArch representation ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in
// the LLVM LoongArch back-end.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_LOONGARCH_H
#define LLVM_LIB_TARGET_LOONGARCH_LOONGARCH_H

#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
  class LoongArchTargetMachine;
  class ModulePass;
  class FunctionPass;
  class LoongArchSubtarget;
  class LoongArchTargetMachine;
  class InstructionSelector;
  class PassRegistry;

  FunctionPass *createLoongArchModuleISelDagPass();
  FunctionPass *createLoongArchOptimizePICCallPass();
  FunctionPass *createLoongArchBranchExpansion();
  FunctionPass *createLoongArchExpandPseudoPass();

  void initializeLoongArchBranchExpansionPass(PassRegistry &);
} // end namespace llvm;

#endif
