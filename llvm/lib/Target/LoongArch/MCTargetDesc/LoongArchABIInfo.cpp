//===---- LoongArchABIInfo.cpp - Information about LoongArch ABI's ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "LoongArchABIInfo.h"
#include "LoongArchRegisterInfo.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCTargetOptions.h"

using namespace llvm;

namespace {
static const MCPhysReg LP32IntRegs[4] = {LoongArch::A0, LoongArch::A1, LoongArch::A2, LoongArch::A3};

static const MCPhysReg LoongArch64IntRegs[8] = {
    LoongArch::A0_64, LoongArch::A1_64, LoongArch::A2_64, LoongArch::A3_64,
    LoongArch::A4_64, LoongArch::A5_64, LoongArch::A6_64, LoongArch::A7_64};
}

ArrayRef<MCPhysReg> LoongArchABIInfo::GetByValArgRegs() const {
  if (IsLP32())
    return makeArrayRef(LP32IntRegs);
  if (IsLPX32() || IsLP64())
    return makeArrayRef(LoongArch64IntRegs);
  llvm_unreachable("Unhandled ABI");
}

ArrayRef<MCPhysReg> LoongArchABIInfo::GetVarArgRegs() const {
  if (IsLP32())
    return makeArrayRef(LP32IntRegs);
  if (IsLPX32() || IsLP64())
    return makeArrayRef(LoongArch64IntRegs);
  llvm_unreachable("Unhandled ABI");
}

unsigned LoongArchABIInfo::GetCalleeAllocdArgSizeInBytes(CallingConv::ID CC) const {
  if (IsLP32())
    return CC != CallingConv::Fast ? 16 : 0;
  if (IsLPX32() || IsLP64())
    return 0;
  llvm_unreachable("Unhandled ABI");
}

LoongArchABIInfo LoongArchABIInfo::computeTargetABI(const Triple &TT, StringRef CPU,
                                          const MCTargetOptions &Options) {
  if (Options.getABIName().startswith("lp32"))
    return LoongArchABIInfo::LP32();
  if (Options.getABIName().startswith("lpx32"))
    return LoongArchABIInfo::LPX32();
  if (Options.getABIName().startswith("lp64"))
    return LoongArchABIInfo::LP64();
  assert(Options.getABIName().empty() && "Unknown ABI option for LoongArch");

  if (TT.isLoongArch64())
    return LoongArchABIInfo::LP64();
  return LoongArchABIInfo::LP32();
}

unsigned LoongArchABIInfo::GetStackPtr() const {
  return ArePtrs64bit() ? LoongArch::SP_64 : LoongArch::SP;
}

unsigned LoongArchABIInfo::GetFramePtr() const {
  return ArePtrs64bit() ? LoongArch::FP_64 : LoongArch::FP;
}

unsigned LoongArchABIInfo::GetBasePtr() const {
  return ArePtrs64bit() ? LoongArch::S7_64 : LoongArch::S7;
}

unsigned LoongArchABIInfo::GetNullPtr() const {
  return ArePtrs64bit() ? LoongArch::ZERO_64 : LoongArch::ZERO;
}

unsigned LoongArchABIInfo::GetZeroReg() const {
  return AreGprs64bit() ? LoongArch::ZERO_64 : LoongArch::ZERO;
}

unsigned LoongArchABIInfo::GetPtrAddOp() const {
  return ArePtrs64bit() ? LoongArch::ADD_D : LoongArch::ADD_W;
}

unsigned LoongArchABIInfo::GetPtrAddiOp() const {
  return ArePtrs64bit() ? LoongArch::ADDI_D : LoongArch::ADDI_W;
}

unsigned LoongArchABIInfo::GetPtrSubOp() const {
  return ArePtrs64bit() ? LoongArch::SUB_D : LoongArch::SUB_W;
}

unsigned LoongArchABIInfo::GetPtrAndOp() const {
  return ArePtrs64bit() ? LoongArch::AND : LoongArch::AND32;
}

unsigned LoongArchABIInfo::GetGPRMoveOp() const {
  return ArePtrs64bit() ? LoongArch::OR : LoongArch::OR32;
}

unsigned LoongArchABIInfo::GetEhDataReg(unsigned I) const {
  static const unsigned EhDataReg[] = {
    LoongArch::A0, LoongArch::A1, LoongArch::A2, LoongArch::A3
  };
  static const unsigned EhDataReg64[] = {
    LoongArch::A0_64, LoongArch::A1_64, LoongArch::A2_64, LoongArch::A3_64
  };

  return IsLP64() ? EhDataReg64[I] : EhDataReg[I];
}

