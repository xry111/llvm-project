//===-- LoongArchFixupKinds.h - LoongArch Specific Fixup Entries --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LoongArch_MCTARGETDESC_LoongArchFIXUPKINDS_H
#define LLVM_LIB_TARGET_LoongArch_MCTARGETDESC_LoongArchFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

#undef LoongArch

namespace llvm {
namespace LoongArch {
enum Fixups {
  fixup_larch_sop_push_pcrel = FirstTargetFixupKind,
  fixup_larch_sop_push_absolute,
  fixup_larch_sop_push_gprel,
  fixup_larch_sop_push_tls_tprel,
  fixup_larch_sop_push_tls_got,
  fixup_larch_sop_push_tls_gd,
  fixup_larch_sop_push_plt_pcrel,
  fixup_larch_sop_sub,
  fixup_larch_sop_sl,
  fixup_larch_sop_sr,
  fixup_larch_sop_add,
  fixup_larch_sop_and,
  fixup_larch_sop_pop_32_u_10_12,
  fixup_larch_sop_pop_32_s_10_12,
  fixup_larch_sop_pop_32_s_10_16_s2,
  fixup_larch_sop_pop_32_s_5_20,
  fixup_larch_sop_pop_32_s_0_5_10_16_s2,
  fixup_larch_sop_pop_32_s_0_10_10_16_s2,
  fixup_larch_add_8,
  fixup_larch_add_16,
  fixup_larch_add_32,
  fixup_larch_add_64,
  fixup_larch_sub_8,
  fixup_larch_sub_16,
  fixup_larch_sub_32,
  fixup_larch_sub_64,
  fixup_larch_invalid,
  NumTargetFixupKinds = fixup_larch_invalid - FirstTargetFixupKind
};
} // end namespace LoongArch
} // end namespace llvm

#endif
