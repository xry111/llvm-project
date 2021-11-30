//===-- LoongArchBaseInfo.h - Top level definitions for LoongArch MC ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains small standalone helper functions and enum definitions for
// the LoongArch target useful for the compiler back-end and the MC libraries.
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHBASEINFO_H
#define LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHBASEINFO_H

#include "LoongArchFixupKinds.h"
#include "LoongArchMCTargetDesc.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/Support/DataTypes.h"
#include "llvm/Support/ErrorHandling.h"

namespace llvm {

/// LoongArchII - This namespace holds all of the target specific flags that
/// instruction info tracks.
///
namespace LoongArchII {
  /// Target Operand Flag enum.
  enum TOF {
    //===------------------------------------------------------------------===//
    // LoongArch Specific MachineOperand flags.

    MO_NO_FLAG,

    /// MO_ABS_XXX - Represents the hi or low part of an absolute symbol
    /// address.
    MO_ABS_HI,
    MO_ABS_LO,
    MO_ABS_HIGHER,
    MO_ABS_HIGHEST,

    /// MO_PCREL_XXX - Represents the hi or low part of an pc relative symbol
    /// address.
    MO_PCREL_HI,
    MO_PCREL_LO,
    // with tmp reg
    MO_PCREL_RRHI,
    MO_PCREL_RRLO,
    MO_PCREL_RRHIGHER,
    MO_PCREL_RRHIGHEST,

    // LArch Tls gd and ld
    MO_TLSGD_HI,
    MO_TLSGD_LO,
    // with tmp reg
    MO_TLSGD_RRHI,
    MO_TLSGD_RRLO,
    MO_TLSGD_RRHIGHER,
    MO_TLSGD_RRHIGHEST,

    // LArch thread tprel (ie/le)
    // LArch Tls ie
    MO_TLSIE_HI,
    MO_TLSIE_LO,
    // with tmp reg
    MO_TLSIE_RRHI,
    MO_TLSIE_RRLO,
    MO_TLSIE_RRHIGHER,
    MO_TLSIE_RRHIGHEST,
    // LArch Tls le
    MO_TLSLE_HI,
    MO_TLSLE_LO,
    MO_TLSLE_HIGHER,
    MO_TLSLE_HIGHEST,

    // Loongarch got
    MO_GOT_HI,
    MO_GOT_LO,
    // with tmp reg
    MO_GOT_RRHI,
    MO_GOT_RRLO,
    MO_GOT_RRHIGHER,
    MO_GOT_RRHIGHEST,

    MO_CALL_HI,
    MO_CALL_LO,
  };

  enum {
    //===------------------------------------------------------------------===//
    // Instruction encodings.  These are the standard/most common forms for
    // LoongArch instructions.
    //

    // Pseudo - This represents an instruction that is a pseudo instruction
    // or one that has not been implemented yet.  It is illegal to code generate
    // it, but tolerated for intermediate implementation stages.
    Pseudo   = 0,

    /// FrmR - This form is for instructions of the format R.
    FrmR  = 1,
    /// FrmI - This form is for instructions of the format I.
    FrmI  = 2,
    /// FrmJ - This form is for instructions of the format J.
    FrmJ  = 3,
    /// FrmFR - This form is for instructions of the format FR.
    FrmFR = 4,
    /// FrmFI - This form is for instructions of the format FI.
    FrmFI = 5,
    /// FrmOther - This form is for instructions that have no specific format.
    FrmOther = 6,

    FormMask = 15,
    /// IsCTI - Instruction is a Control Transfer Instruction.
    IsCTI = 1 << 4,
    /// HasForbiddenSlot - Instruction has a forbidden slot.
    HasForbiddenSlot = 1 << 5,
    /// IsPCRelativeLoad - A Load instruction with implicit source register
    ///                    ($pc) with explicit offset and destination register
    IsPCRelativeLoad = 1 << 6,
    /// HasFCCRegOperand - Instruction uses an $fcc<x> register.
    HasFCCRegOperand = 1 << 7

  };
}
}

#endif
