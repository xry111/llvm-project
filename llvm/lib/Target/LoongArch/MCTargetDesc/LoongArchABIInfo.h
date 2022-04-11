//===---- LoongArchABIInfo.h - Information about LoongArch ABI's --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHABIINFO_H
#define LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHABIINFO_H

#include "llvm/ADT/Triple.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/MC/MCRegisterInfo.h"

namespace llvm {

template <typename T> class ArrayRef;
class MCTargetOptions;
class StringRef;
class TargetRegisterClass;

class LoongArchABIInfo {
public:
  enum class ABI { Unknown, ILP32D, ILP32F, ILP32S, LP64D, LP64F, LP64S };

protected:
  ABI ThisABI;

public:
  LoongArchABIInfo(ABI ThisABI) : ThisABI(ThisABI) {}

  static LoongArchABIInfo Unknown() { return LoongArchABIInfo(ABI::Unknown); }
  static LoongArchABIInfo ILP32D() { return LoongArchABIInfo(ABI::ILP32D); }
  static LoongArchABIInfo ILP32F() { return LoongArchABIInfo(ABI::ILP32F); }
  static LoongArchABIInfo ILP32S() { return LoongArchABIInfo(ABI::ILP32S); }
  static LoongArchABIInfo LP64D() { return LoongArchABIInfo(ABI::LP64D); }
  static LoongArchABIInfo LP64S() { return LoongArchABIInfo(ABI::LP64S); }
  static LoongArchABIInfo LP64F() { return LoongArchABIInfo(ABI::LP64F); }
  static LoongArchABIInfo computeTargetABI(const Triple &TT, StringRef CPU,
                                      const MCTargetOptions &Options);

  bool IsKnown() const { return ThisABI != ABI::Unknown; }
  bool IsILP32D() const { return ThisABI == ABI::ILP32D; }
  bool IsILP32F() const { return ThisABI == ABI::ILP32F; }
  bool IsILP32S() const { return ThisABI == ABI::ILP32S; }
  bool IsLP64D() const { return ThisABI == ABI::LP64D; }
  bool IsLP64S() const { return ThisABI == ABI::LP64S; }
  bool IsLP64F() const { return ThisABI == ABI::LP64F; }
  ABI GetEnumValue() const { return ThisABI; }

  /// The registers to use for byval arguments.
  ArrayRef<MCPhysReg> GetByValArgRegs() const;

  /// The registers to use for the variable argument list.
  ArrayRef<MCPhysReg> GetVarArgRegs() const;

  /// Obtain the size of the area allocated by the callee for arguments.
  /// CallingConv::FastCall affects the value for 32-bit ABI.
  unsigned GetCalleeAllocdArgSizeInBytes(CallingConv::ID CC) const;

  /// Ordering of ABI's
  /// LoongArchGenSubtargetInfo.inc will use this to resolve conflicts when given
  /// multiple ABI options.
  bool operator<(const LoongArchABIInfo Other) const {
    return ThisABI < Other.GetEnumValue();
  }

  unsigned GetStackPtr() const;
  unsigned GetFramePtr() const;
  unsigned GetBasePtr() const;
  unsigned GetNullPtr() const;
  unsigned GetZeroReg() const;
  unsigned GetPtrAddOp() const;
  unsigned GetPtrAddiOp() const;
  unsigned GetPtrSubOp() const;
  unsigned GetPtrAndOp() const;
  unsigned GetGPRMoveOp() const;
  inline bool ArePtrs64bit() const {
    return IsLP64D() || IsLP64S() || IsLP64F();
  }
  inline bool AreGprs64bit() const {
    return IsLP64D() || IsLP64S() || IsLP64F();
  }

  unsigned GetEhDataReg(unsigned I) const;
};
}

#endif
