//===-- LoongArchBaseInfo.h - Top level definitions for LoongArch MC ----*- C++ -*-===//
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
#ifndef LLVM_LIB_TARGET_LoongArch_MCTARGETDESC_LoongArchBASEINFO_H
#define LLVM_LIB_TARGET_LoongArch_MCTARGETDESC_LoongArchBASEINFO_H

#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Support/MachineValueType.h"

namespace llvm {

// LoongArchII - This namespace holds all of the target specific flags that
// instruction info tracks. All definitions must match LoongArchInstrFormats.td.
namespace LoongArchII {
enum {
  InstFormatPseudo = 0,
  InstFormatR = 1,
  InstFormatR4 = 2,
  InstFormatI = 3,
  InstFormatS = 4,
  InstFormatB = 5,
  InstFormatU = 6,
  InstFormatJ = 7,
  InstFormatCR = 8,
  InstFormatCI = 9,
  InstFormatCSS = 10,
  InstFormatCIW = 11,
  InstFormatCL = 12,
  InstFormatCS = 13,
  InstFormatCA = 14,
  InstFormatCB = 15,
  InstFormatCJ = 16,
  InstFormatOther = 17,

  InstFormatMask = 31,

  ConstraintShift = 5,
  ConstraintMask = 0b111 << ConstraintShift,

  VLMulShift = ConstraintShift + 3,
  VLMulMask = 0b111 << VLMulShift,

  // Do we need to add a dummy mask op when converting RVV Pseudo to MCInst.
  HasDummyMaskOpShift = VLMulShift + 3,
  HasDummyMaskOpMask = 1 << HasDummyMaskOpShift,

  // Does this instruction only update element 0 the destination register.
  WritesElement0Shift = HasDummyMaskOpShift + 1,
  WritesElement0Mask = 1 << WritesElement0Shift,

  // Does this instruction have a merge operand that must be removed when
  // converting to MCInst. It will be the first explicit use operand. Used by
  // RVV Pseudos.
  HasMergeOpShift = WritesElement0Shift + 1,
  HasMergeOpMask = 1 << HasMergeOpShift,

  // Does this instruction have a SEW operand. It will be the last explicit
  // operand. Used by RVV Pseudos.
  HasSEWOpShift = HasMergeOpShift + 1,
  HasSEWOpMask = 1 << HasSEWOpShift,

  // Does this instruction have a VL operand. It will be the second to last
  // explicit operand. Used by RVV Pseudos.
  HasVLOpShift = HasSEWOpShift + 1,
  HasVLOpMask = 1 << HasVLOpShift,
};

// Match with the definitions in LoongArchInstrFormatsV.td
enum RVVConstraintType {
  NoConstraint = 0,
  VS2Constraint = 0b001,
  VS1Constraint = 0b010,
  VMConstraint = 0b100,
};

// RISC-V Specific Machine Operand Flags
enum {
  MO_None = 0,
  MO_CALL = 1,
  MO_PLT = 2,

  // Used to differentiate between target-specific "direct" flags and "bitmask"
  // flags. A machine operand can only have one "direct" flag, but can have
  // multiple "bitmask" flags.
  MO_DIRECT_FLAG_MASK = 3
};
} // namespace LoongArchII

namespace LoongArchOp {
enum OperandType : unsigned {
  OPERAND_FIRST_LoongArch_IMM = MCOI::OPERAND_FIRST_TARGET,
  OPERAND_UIMM2 = OPERAND_FIRST_LoongArch_IMM,
  OPERAND_UIMM2_ALSL,
  OPERAND_UIMM4,
  OPERAND_UIMM5,
  OPERAND_UIMM6,
  OPERAND_UIMM12,
  OPERAND_SIMM12,
  OPERAND_SIMM14,
  OPERAND_UIMM15,
  OPERAND_SIMM16,
  OPERAND_UIMM20,
  OPERAND_SIMM20,
  OPERAND_UIMMLOG2XLEN,
  OPERAND_LAST_LoongArch_IMM = OPERAND_UIMMLOG2XLEN
};
} // namespace LoongArchOp

// Describes the predecessor/successor bits used in the FENCE instruction.
namespace LoongArchFenceField {
enum FenceField {
  I = 8,
  O = 4,
  R = 2,
  W = 1
};
}

// Describes the supported floating point rounding mode encodings.
namespace LoongArchFPRndMode {
enum RoundingMode {
  RNE = 0,
  RTZ = 1,
  RDN = 2,
  RUP = 3,
  RMM = 4,
  DYN = 7,
  Invalid
};

inline static StringRef roundingModeToString(RoundingMode RndMode) {
  switch (RndMode) {
  default:
    llvm_unreachable("Unknown floating point rounding mode");
  case LoongArchFPRndMode::RNE:
    return "rne";
  case LoongArchFPRndMode::RTZ:
    return "rtz";
  case LoongArchFPRndMode::RDN:
    return "rdn";
  case LoongArchFPRndMode::RUP:
    return "rup";
  case LoongArchFPRndMode::RMM:
    return "rmm";
  case LoongArchFPRndMode::DYN:
    return "dyn";
  }
}

inline static RoundingMode stringToRoundingMode(StringRef Str) {
  return StringSwitch<RoundingMode>(Str)
      .Case("rne", LoongArchFPRndMode::RNE)
      .Case("rtz", LoongArchFPRndMode::RTZ)
      .Case("rdn", LoongArchFPRndMode::RDN)
      .Case("rup", LoongArchFPRndMode::RUP)
      .Case("rmm", LoongArchFPRndMode::RMM)
      .Case("dyn", LoongArchFPRndMode::DYN)
      .Default(LoongArchFPRndMode::Invalid);
}

inline static bool isValidRoundingMode(unsigned Mode) {
  switch (Mode) {
  default:
    return false;
  case LoongArchFPRndMode::RNE:
  case LoongArchFPRndMode::RTZ:
  case LoongArchFPRndMode::RDN:
  case LoongArchFPRndMode::RUP:
  case LoongArchFPRndMode::RMM:
  case LoongArchFPRndMode::DYN:
    return true;
  }
}
} // namespace LoongArchFPRndMode

namespace LoongArchSysReg {
struct SysReg {
  const char *Name;
  unsigned Encoding;
  const char *AltName;
  // FIXME: add these additional fields when needed.
  // Privilege Access: Read, Write, Read-Only.
  // unsigned ReadWrite;
  // Privilege Mode: User, System or Machine.
  // unsigned Mode;
  // Check field name.
  // unsigned Extra;
  // Register number without the privilege bits.
  // unsigned Number;
  FeatureBitset FeaturesRequired;
  bool isLA32Only;

  bool haveRequiredFeatures(FeatureBitset ActiveFeatures) const {
    // Not in 32-bit mode.
    if (isLA32Only && ActiveFeatures[LoongArch::Feature64Bit])
      return false;
    // No required feature associated with the system register.
    if (FeaturesRequired.none())
      return true;
    return (FeaturesRequired & ActiveFeatures) == FeaturesRequired;
  }
};

#define GET_SysRegsList_DECL
#include "LoongArchGenSearchableTables.inc"
} // end namespace LoongArchSysReg

namespace LoongArchABI {

enum ABI {
  ABI_ILP32S,
  ABI_ILP32F,
  ABI_ILP32D,
  ABI_LP64S,
  ABI_LP64F,
  ABI_LP64D,
  ABI_Unknown
};

// Returns the target ABI, or else a StringError if the requested ABIName is
// not supported for the given TT and FeatureBits combination.
ABI computeTargetABI(const Triple &TT, FeatureBitset FeatureBits,
                     StringRef ABIName);

ABI getTargetABI(StringRef ABIName);

// Returns the register used to hold the stack pointer after realignment.
MCRegister getBPReg();

// Returns the register holding shadow call stack pointer.
MCRegister getSCSPReg();

} // namespace LoongArchABI

namespace LoongArchFeatures {

// Validates if the given combination of features are valid for the target
// triple. Exits with report_fatal_error if not.
void validate(const Triple &TT, const FeatureBitset &FeatureBits);

} // namespace LoongArchFeatures

namespace LoongArchVMVTs {

constexpr MVT vint8mf8_t = MVT::nxv1i8;
constexpr MVT vint8mf4_t = MVT::nxv2i8;
constexpr MVT vint8mf2_t = MVT::nxv4i8;
constexpr MVT vint8m1_t = MVT::nxv8i8;
constexpr MVT vint8m2_t = MVT::nxv16i8;
constexpr MVT vint8m4_t = MVT::nxv32i8;
constexpr MVT vint8m8_t = MVT::nxv64i8;

constexpr MVT vint16mf4_t = MVT::nxv1i16;
constexpr MVT vint16mf2_t = MVT::nxv2i16;
constexpr MVT vint16m1_t = MVT::nxv4i16;
constexpr MVT vint16m2_t = MVT::nxv8i16;
constexpr MVT vint16m4_t = MVT::nxv16i16;
constexpr MVT vint16m8_t = MVT::nxv32i16;

constexpr MVT vint32mf2_t = MVT::nxv1i32;
constexpr MVT vint32m1_t = MVT::nxv2i32;
constexpr MVT vint32m2_t = MVT::nxv4i32;
constexpr MVT vint32m4_t = MVT::nxv8i32;
constexpr MVT vint32m8_t = MVT::nxv16i32;

constexpr MVT vint64m1_t = MVT::nxv1i64;
constexpr MVT vint64m2_t = MVT::nxv2i64;
constexpr MVT vint64m4_t = MVT::nxv4i64;
constexpr MVT vint64m8_t = MVT::nxv8i64;

constexpr MVT vfloat16mf4_t = MVT::nxv1f16;
constexpr MVT vfloat16mf2_t = MVT::nxv2f16;
constexpr MVT vfloat16m1_t = MVT::nxv4f16;
constexpr MVT vfloat16m2_t = MVT::nxv8f16;
constexpr MVT vfloat16m4_t = MVT::nxv16f16;
constexpr MVT vfloat16m8_t = MVT::nxv32f16;

constexpr MVT vfloat32mf2_t = MVT::nxv1f32;
constexpr MVT vfloat32m1_t = MVT::nxv2f32;
constexpr MVT vfloat32m2_t = MVT::nxv4f32;
constexpr MVT vfloat32m4_t = MVT::nxv8f32;
constexpr MVT vfloat32m8_t = MVT::nxv16f32;

constexpr MVT vfloat64m1_t = MVT::nxv1f64;
constexpr MVT vfloat64m2_t = MVT::nxv2f64;
constexpr MVT vfloat64m4_t = MVT::nxv4f64;
constexpr MVT vfloat64m8_t = MVT::nxv8f64;

constexpr MVT vbool1_t = MVT::nxv64i1;
constexpr MVT vbool2_t = MVT::nxv32i1;
constexpr MVT vbool4_t = MVT::nxv16i1;
constexpr MVT vbool8_t = MVT::nxv8i1;
constexpr MVT vbool16_t = MVT::nxv4i1;
constexpr MVT vbool32_t = MVT::nxv2i1;
constexpr MVT vbool64_t = MVT::nxv1i1;

} // namespace LoongArchVMVTs

enum class LoongArchVSEW {
  SEW_8 = 0,
  SEW_16,
  SEW_32,
  SEW_64,
  SEW_128,
  SEW_256,
  SEW_512,
  SEW_1024,
};

enum class LoongArchVLMUL {
  LMUL_1 = 0,
  LMUL_2,
  LMUL_4,
  LMUL_8,
  LMUL_RESERVED,
  LMUL_F8,
  LMUL_F4,
  LMUL_F2
};

namespace LoongArchVType {
// Is this a SEW value that can be encoded into the VTYPE format.
inline static bool isValidSEW(unsigned SEW) {
  return isPowerOf2_32(SEW) && SEW >= 8 && SEW <= 1024;
}

// Is this a LMUL value that can be encoded into the VTYPE format.
inline static bool isValidLMUL(unsigned LMUL, bool Fractional) {
  return isPowerOf2_32(LMUL) && LMUL <= 8 && (!Fractional || LMUL != 1);
}

// Encode VTYPE into the binary format used by the the VSETVLI instruction which
// is used by our MC layer representation.
//
// Bits | Name       | Description
// -----+------------+------------------------------------------------
// 7    | vma        | Vector mask agnostic
// 6    | vta        | Vector tail agnostic
// 5:3  | vsew[2:0]  | Standard element width (SEW) setting
// 2:0  | vlmul[2:0] | Vector register group multiplier (LMUL) setting
inline static unsigned encodeVTYPE(LoongArchVLMUL VLMUL, LoongArchVSEW VSEW,
                                   bool TailAgnostic, bool MaskAgnostic) {
  unsigned VLMULBits = static_cast<unsigned>(VLMUL);
  unsigned VSEWBits = static_cast<unsigned>(VSEW);
  unsigned VTypeI = (VSEWBits << 3) | (VLMULBits & 0x7);
  if (TailAgnostic)
    VTypeI |= 0x40;
  if (MaskAgnostic)
    VTypeI |= 0x80;

  return VTypeI;
}

inline static LoongArchVLMUL getVLMUL(unsigned VType) {
  unsigned VLMUL = VType & 0x7;
  return static_cast<LoongArchVLMUL>(VLMUL);
}

inline static LoongArchVSEW getVSEW(unsigned VType) {
  unsigned VSEW = (VType >> 3) & 0x7;
  return static_cast<LoongArchVSEW>(VSEW);
}

inline static bool isTailAgnostic(unsigned VType) { return VType & 0x40; }

inline static bool isMaskAgnostic(unsigned VType) { return VType & 0x80; }

void printVType(unsigned VType, raw_ostream &OS);

} // namespace LoongArchVType

namespace LoongArchVPseudosTable {

struct PseudoInfo {
#include "MCTargetDesc/LoongArchBaseInfo.h"
  uint16_t Pseudo;
  uint16_t BaseInstr;
};

using namespace LoongArch;

#define GET_LoongArchVPseudosTable_DECL
#include "LoongArchGenSearchableTables.inc"

} // end namespace LoongArchVPseudosTable

} // namespace llvm

#endif
