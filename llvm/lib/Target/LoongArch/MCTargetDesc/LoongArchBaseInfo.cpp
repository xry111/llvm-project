//===-- LoongArchBaseInfo.cpp - Top level definitions for LoongArch MC ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains small standalone enum definitions for the LoongArch target
// useful for the compiler back-end and the MC libraries.
//
//===----------------------------------------------------------------------===//

#include "LoongArchBaseInfo.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm {
namespace LoongArchSysReg {
#define GET_SysRegsList_IMPL
#include "LoongArchGenSearchableTables.inc"
} // namespace LoongArchSysReg

namespace LoongArchABI {
ABI computeTargetABI(const Triple &TT, FeatureBitset FeatureBits,
                     StringRef ABIName) {
  auto TargetABI = getTargetABI(ABIName);
  bool IsLA64 = TT.isArch64Bit();

  if (!ABIName.empty() && TargetABI == ABI_Unknown) {
    errs()
        << "'" << ABIName
        << "' is not a recognized ABI for this target (ignoring target-abi)\n";
  } else if (ABIName.startswith("ilp32") && IsLA64) {
    errs() << "32-bit ABIs are not supported for 64-bit targets (ignoring "
              "target-abi)\n";
    TargetABI = ABI_Unknown;
  } else if (ABIName.startswith("lp64") && !IsLA64) {
    errs() << "64-bit ABIs are not supported for 32-bit targets (ignoring "
              "target-abi)\n";
    TargetABI = ABI_Unknown;
  }

  if (TargetABI != ABI_Unknown)
    return TargetABI;

  // Guess ABI based on GR len and FPU features.
  if (FeatureBits[LoongArch::FeatureStdExtD])
    return IsLA64 ? ABI_LP64D : ABI_ILP32D;

  if (FeatureBits[LoongArch::FeatureStdExtF])
    return IsLA64 ? ABI_LP64F : ABI_ILP32F;

  return IsLA64 ? ABI_LP64S : ABI_ILP32S;
}

ABI getTargetABI(StringRef ABIName) {
  auto TargetABI = StringSwitch<ABI>(ABIName)
                       .Case("ilp32s", ABI_ILP32S)
                       .Case("ilp32f", ABI_ILP32F)
                       .Case("ilp32d", ABI_ILP32D)
                       .Case("lp64s", ABI_LP64S)
                       .Case("lp64f", ABI_LP64F)
                       .Case("lp64d", ABI_LP64D)
                       .Default(ABI_Unknown);
  return TargetABI;
}

// To avoid the BP value clobbered by a function call, we need to choose a
// callee saved register to save the value. We choose s0 as bp.
MCRegister getBPReg() { return LoongArch::R23; }

// Returns the register holding shadow call stack pointer.
MCRegister getSCSPReg() { return LoongArch::R24; }

} // namespace LoongArchABI

namespace LoongArchFeatures {

void validate(const Triple &TT, const FeatureBitset &FeatureBits) {}

} // namespace LoongArchFeatures

namespace LoongArchVPseudosTable {

#define GET_LoongArchVPseudosTable_IMPL
#include "LoongArchGenSearchableTables.inc"

} // namespace LoongArchVPseudosTable

void LoongArchVType::printVType(unsigned VType, raw_ostream &OS) {
  LoongArchVSEW VSEW = getVSEW(VType);
  LoongArchVLMUL VLMUL = getVLMUL(VType);

  unsigned Sew = 1 << (static_cast<unsigned>(VSEW) + 3);
  OS << "e" << Sew;

  switch (VLMUL) {
  case LoongArchVLMUL::LMUL_RESERVED:
    llvm_unreachable("Unexpected LMUL value!");
  case LoongArchVLMUL::LMUL_1:
  case LoongArchVLMUL::LMUL_2:
  case LoongArchVLMUL::LMUL_4:
  case LoongArchVLMUL::LMUL_8: {
    unsigned LMul = 1 << static_cast<unsigned>(VLMUL);
    OS << ",m" << LMul;
    break;
  }
  case LoongArchVLMUL::LMUL_F2:
  case LoongArchVLMUL::LMUL_F4:
  case LoongArchVLMUL::LMUL_F8: {
    unsigned LMul = 1 << (8 - static_cast<unsigned>(VLMUL));
    OS << ",mf" << LMul;
    break;
  }
  }

  if (isTailAgnostic(VType))
    OS << ",ta";
  else
    OS << ",tu";

  if (isMaskAgnostic(VType))
    OS << ",ma";
  else
    OS << ",mu";
}

} // namespace llvm
