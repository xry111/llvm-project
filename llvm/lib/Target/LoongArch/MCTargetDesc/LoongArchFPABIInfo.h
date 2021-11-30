//===--- LoongArchFPABIInfo.h - Information about LoongArch Folating Point ABI's ---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHFPABIINFO_H
#define LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHFPABIINFO_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"
#include <cstdint>

namespace llvm {

class MCStreamer;

struct LoongArchFPABIInfo {
  // Internal representation of the fp_abi related values used in .module.
  enum class FpABIKind { ANY, S32, S64, SOFT };

protected:
  // The floating-point ABI.
  FpABIKind FpABI = FpABIKind::ANY;

public:
  LoongArchFPABIInfo() = default;

  FpABIKind getFpABI() { return FpABI; }
  void setFpABI(FpABIKind Value, bool IsABI32Bit) {
    FpABI = Value;
  }

  StringRef getFpABIString(FpABIKind Value);

  template <class PredicateLibrary>
  void setFpAbiFromPredicates(const PredicateLibrary &P) {
    FpABI = FpABIKind::ANY;
    if (P.useSoftFloat())
      FpABI = FpABIKind::SOFT;
    else if (P.isABI_LPX32() || P.isABI_LP64())
      FpABI = FpABIKind::S64;
    else if (P.isABI_LP32()) {
      if (P.isFP64bit())
        FpABI = FpABIKind::S64;
      else
        FpABI = FpABIKind::S32;
    }
  }

  template <class PredicateLibrary>
  void setAllFromPredicates(const PredicateLibrary &P) {
    setFpAbiFromPredicates(P);
  }
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHFPABIINFO_H
