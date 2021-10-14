//===- LoongArchTargetTransformInfo.h - RISC-V specific TTI ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file defines a TargetTransformInfo::Concept conforming object specific
/// to the RISC-V target machine. It uses the target's detailed information to
/// provide more precise answers to certain TTI queries, while letting the
/// target independent and default TTI implementations handle the rest.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LoongArch_LoongArchTARGETTRANSFORMINFO_H
#define LLVM_LIB_TARGET_LoongArch_LoongArchTARGETTRANSFORMINFO_H

#include "LoongArchSubtarget.h"
#include "LoongArchTargetMachine.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/CodeGen/BasicTTIImpl.h"
#include "llvm/IR/Function.h"

namespace llvm {

class LoongArchTTIImpl : public BasicTTIImplBase<LoongArchTTIImpl> {
  using BaseT = BasicTTIImplBase<LoongArchTTIImpl>;
  using TTI = TargetTransformInfo;

  friend BaseT;

  const LoongArchSubtarget *ST;
  const LoongArchTargetLowering *TLI;

  const LoongArchSubtarget *getST() const { return ST; }
  const LoongArchTargetLowering *getTLI() const { return TLI; }

public:
  explicit LoongArchTTIImpl(const LoongArchTargetMachine *TM, const Function &F)
      : BaseT(TM, F.getParent()->getDataLayout()), ST(TM->getSubtargetImpl(F)),
        TLI(ST->getTargetLowering()) {}

  int getIntImmCost(const APInt &Imm, Type *Ty, TTI::TargetCostKind CostKind);
  int getIntImmCostInst(unsigned Opcode, unsigned Idx, const APInt &Imm,
                        Type *Ty, TTI::TargetCostKind CostKind,
                        Instruction *Inst = nullptr);
  int getIntImmCostIntrin(Intrinsic::ID IID, unsigned Idx, const APInt &Imm,
                          Type *Ty, TTI::TargetCostKind CostKind);
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_LoongArch_LoongArchTARGETTRANSFORMINFO_H
