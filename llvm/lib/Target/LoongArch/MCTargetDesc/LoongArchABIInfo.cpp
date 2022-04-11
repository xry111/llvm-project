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

static const MCPhysReg LoongArch64IntRegs[8] = {
    LoongArch::A0_64, LoongArch::A1_64, LoongArch::A2_64, LoongArch::A3_64,
    LoongArch::A4_64, LoongArch::A5_64, LoongArch::A6_64, LoongArch::A7_64};
}

ArrayRef<MCPhysReg> LoongArchABIInfo::GetByValArgRegs() const {
  if (IsILP32D() || IsILP32F() || IsILP32S())
    // TODO
    llvm_unreachable("Unimplemented ABI");
  if (IsLP64D() || IsLP64S() || IsLP64F())
    return makeArrayRef(LoongArch64IntRegs);
  llvm_unreachable("Unhandled ABI");
}

ArrayRef<MCPhysReg> LoongArchABIInfo::GetVarArgRegs() const {
  if (IsILP32D() || IsILP32F() || IsILP32S())
    // TODO
    llvm_unreachable("Unimplemented ABI");
  if (IsLP64D() || IsLP64S() || IsLP64F())
    return makeArrayRef(LoongArch64IntRegs);
  llvm_unreachable("Unhandled ABI");
}

unsigned LoongArchABIInfo::GetCalleeAllocdArgSizeInBytes(CallingConv::ID CC) const {
  if (IsILP32D() || IsILP32F() || IsILP32S())
    // TODO
    llvm_unreachable("Unimplemented ABI");
  if (IsLP64D() || IsLP64S() || IsLP64F())
    return 0;
  llvm_unreachable("Unhandled ABI");
}

LoongArchABIInfo LoongArchABIInfo::computeTargetABI(const Triple &TT, StringRef CPU,
                                          const MCTargetOptions &Options) {
  if (Options.getABIName().startswith("ilp32d"))
    return LoongArchABIInfo::ILP32D();
  if (Options.getABIName().startswith("ilp32f"))
    return LoongArchABIInfo::ILP32F();
  if (Options.getABIName().startswith("ilp32s"))
    return LoongArchABIInfo::ILP32S();
  if (Options.getABIName().startswith("lp64d"))
    return LoongArchABIInfo::LP64D();
  if (Options.getABIName().startswith("lp64s"))
    return LoongArchABIInfo::LP64S();
  if (Options.getABIName().startswith("lp64f"))
    return LoongArchABIInfo::LP64F();
  assert(Options.getABIName().empty() && "Unknown ABI option for LoongArch");

  if (TT.isLoongArch64())
    return LoongArchABIInfo::LP64D();
  return LoongArchABIInfo::ILP32D();
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

  return (IsLP64D() || IsLP64S() || IsLP64F()) ? EhDataReg64[I] : EhDataReg[I];
}

