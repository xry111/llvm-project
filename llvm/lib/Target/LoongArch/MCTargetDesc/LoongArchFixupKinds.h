//===-- LoongArchFixupKinds.h - LoongArch Specific Fixup Entries ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHFIXUPKINDS_H
#define LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm {
namespace LoongArch {
  // Although most of the current fixup types reflect a unique relocation
  // one can have multiple fixup types for a given relocation and thus need
  // to be uniquely named.
  //
  // This table *must* be in the same order of
  // MCFixupKindInfo Infos[LoongArch::NumTargetFixupKinds]
  // in LoongArchAsmBackend.cpp.
  //
  enum Fixups {
    // R_LARCH_NONE.
    fixup_LARCH_NONE = FirstTargetFixupKind,

    // reloc_hint
    fixup_LARCH_SOP_PUSH_ABSOLUTE,
    fixup_LARCH_SOP_PUSH_PCREL,
    fixup_LARCH_SOP_PUSH_GPREL,
    fixup_LARCH_SOP_PUSH_TLS_TPREL,
    fixup_LARCH_SOP_PUSH_TLS_GOT,
    fixup_LARCH_SOP_PUSH_TLS_GD,
    fixup_LARCH_SOP_PUSH_PLT_PCREL,
    // fixup methods
    fixup_LARCH_32,
    fixup_LARCH_64,
    fixup_LARCH_RELATIVE,
    fixup_LARCH_COPY,
    fixup_LARCH_JUMP_SLOT,
    fixup_LARCH_TLS_DTPMOD32,
    fixup_LARCH_TLS_DTPMOD64,
    fixup_LARCH_TLS_DTPREL32,
    fixup_LARCH_TLS_DTPREL64,
    fixup_LARCH_TLS_TPREL32,
    fixup_LARCH_TLS_TPREL64,
    fixup_LARCH_IRELATIVE,
    fixup_LARCH_MARK_LA,
    fixup_LARCH_MARK_PCREL,
    fixup_LARCH_SOP_PUSH_DUP,
    fixup_LARCH_SOP_ASSERT,
    fixup_LARCH_SOP_NOT,
    fixup_LARCH_SOP_SUB,
    fixup_LARCH_SOP_SL,
    fixup_LARCH_SOP_SR,
    fixup_LARCH_SOP_ADD,
    fixup_LARCH_SOP_AND,
    fixup_LARCH_SOP_IF_ELSE,
    fixup_LARCH_SOP_POP_32_S_10_5,
    fixup_LARCH_SOP_POP_32_U_10_12,
    fixup_LARCH_SOP_POP_32_S_10_12,
    fixup_LARCH_SOP_POP_32_S_10_16,
    fixup_LARCH_SOP_POP_32_S_10_16_S2,
    fixup_LARCH_SOP_POP_32_S_5_20,
    fixup_LARCH_SOP_POP_32_S_0_5_10_16_S2,
    fixup_LARCH_SOP_POP_32_S_0_10_10_16_S2,
    fixup_LARCH_SOP_POP_32_U,
    fixup_LARCH_ADD8,
    fixup_LARCH_ADD16,
    fixup_LARCH_ADD24,
    fixup_LARCH_ADD32,
    fixup_LARCH_ADD64,
    fixup_LARCH_SUB8,
    fixup_LARCH_SUB16,
    fixup_LARCH_SUB24,
    fixup_LARCH_SUB32,
    fixup_LARCH_SUB64,
    fixup_LARCH_GNU_VTINHERIT,
    fixup_LARCH_GNU_VTENTRY,

    // Marker
    LastTargetFixupKind,
    NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
  };
} // namespace LoongArch
} // namespace llvm


#endif
