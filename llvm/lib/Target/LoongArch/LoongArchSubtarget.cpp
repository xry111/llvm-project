//===-- LoongArchSubtarget.cpp - LoongArch Subtarget Information ----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the LoongArch specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "LoongArchSubtarget.h"
#include "LoongArch.h"
#include "LoongArchCallLowering.h"
#include "LoongArchFrameLowering.h"
#include "LoongArchLegalizerInfo.h"
#include "LoongArchRegisterBankInfo.h"
#include "LoongArchTargetMachine.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

#define DEBUG_TYPE "loongarch-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "LoongArchGenSubtargetInfo.inc"

void LoongArchSubtarget::anchor() {}

LoongArchSubtarget &LoongArchSubtarget::initializeSubtargetDependencies(
    const Triple &TT, StringRef CPU, StringRef TuneCPU, StringRef FS,
    StringRef ABIName) {
  // Determine default and user-specified characteristics
  bool Is64Bit = TT.isArch64Bit();
  std::string CPUName = std::string(CPU);
  std::string TuneCPUName = std::string(TuneCPU);
  if (CPUName.empty())
    CPUName = Is64Bit ? "loongarch64" : "loongarch32";
  if (TuneCPUName.empty())
    TuneCPUName = CPUName;
  ParseSubtargetFeatures(CPUName, TuneCPUName, FS);
  if (Is64Bit) {
    XLenVT = MVT::i64;
    XLen = 64;
    ADD = LoongArch::ADD_D;
    SUB = LoongArch::SUB_D;
    SLL = LoongArch::SLL_D;
    SRA = LoongArch::SRA_D;
    ADDI = LoongArch::ADDI_D;
    SLLI = LoongArch::SLLI_D;
    SRLI = LoongArch::SRLI_D;
  }

  TargetABI = LoongArchABI::computeTargetABI(TT, getFeatureBits(), ABIName);
  LoongArchFeatures::validate(TT, getFeatureBits());
  return *this;
}

LoongArchSubtarget::LoongArchSubtarget(const Triple &TT, StringRef CPU,
                                       StringRef TuneCPU, StringRef FS,
                                       StringRef ABIName,
                                       const TargetMachine &TM)
    : LoongArchGenSubtargetInfo(TT, CPU, TuneCPU, FS),
      UserReservedRegister(LoongArch::NUM_TARGET_REGS),
      FrameLowering(
          initializeSubtargetDependencies(TT, CPU, TuneCPU, FS, ABIName)),
      InstrInfo(*this), RegInfo(getHwMode()), TLInfo(TM, *this) {
  CallLoweringInfo.reset(new LoongArchCallLowering(*getTargetLowering()));
  Legalizer.reset(new LoongArchLegalizerInfo(*this));

  auto *RBI = new LoongArchRegisterBankInfo(*getRegisterInfo());
  RegBankInfo.reset(RBI);
  InstSelector.reset(createLoongArchInstructionSelector(
      *static_cast<const LoongArchTargetMachine *>(&TM), *this, *RBI));
}

const CallLowering *LoongArchSubtarget::getCallLowering() const {
  return CallLoweringInfo.get();
}

InstructionSelector *LoongArchSubtarget::getInstructionSelector() const {
  return InstSelector.get();
}

const LegalizerInfo *LoongArchSubtarget::getLegalizerInfo() const {
  return Legalizer.get();
}

const RegisterBankInfo *LoongArchSubtarget::getRegBankInfo() const {
  return RegBankInfo.get();
}
