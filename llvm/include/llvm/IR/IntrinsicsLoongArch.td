//===- IntrinsicsLoongArch.td - Defines LoongArch intrinsics ---------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines all of the LoongArch-specific intrinsics.
//
//===----------------------------------------------------------------------===//

let TargetPrefix = "loongarch" in {  // All intrinsics start with "llvm.loongarch.".

//===----------------------------------------------------------------------===//
// LoongArch BASE

def int_loongarch_cpucfg : GCCBuiltin<"__builtin_loongarch_cpucfg">,
  Intrinsic<[llvm_i32_ty], [llvm_i32_ty], []>;

def int_loongarch_csrrd : GCCBuiltin<"__builtin_loongarch_csrrd">,
  Intrinsic<[llvm_i32_ty], [llvm_i32_ty], []>;

def int_loongarch_dcsrrd : GCCBuiltin<"__builtin_loongarch_dcsrrd">,
  Intrinsic<[llvm_i64_ty], [llvm_i64_ty], []>;

def int_loongarch_csrwr : GCCBuiltin<"__builtin_loongarch_csrwr">,
  Intrinsic<[llvm_i32_ty], [llvm_i32_ty, llvm_i32_ty], []>;

def int_loongarch_dcsrwr : GCCBuiltin<"__builtin_loongarch_dcsrwr">,
  Intrinsic<[llvm_i64_ty], [llvm_i64_ty, llvm_i64_ty], []>;

def int_loongarch_csrxchg : GCCBuiltin<"__builtin_loongarch_csrxchg">,
  Intrinsic<[llvm_i32_ty], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty], []>;

def int_loongarch_dcsrxchg : GCCBuiltin<"__builtin_loongarch_dcsrxchg">,
  Intrinsic<[llvm_i64_ty], [llvm_i64_ty, llvm_i64_ty, llvm_i64_ty], []>;

def int_loongarch_iocsrrd_b : GCCBuiltin<"__builtin_loongarch_iocsrrd_b">,
  Intrinsic<[llvm_i32_ty], [llvm_i32_ty], []>;

def int_loongarch_iocsrrd_h : GCCBuiltin<"__builtin_loongarch_iocsrrd_h">,
  Intrinsic<[llvm_i32_ty], [llvm_i32_ty], []>;

def int_loongarch_iocsrrd_w : GCCBuiltin<"__builtin_loongarch_iocsrrd_w">,
  Intrinsic<[llvm_i32_ty], [llvm_i32_ty], []>;

def int_loongarch_iocsrrd_d : GCCBuiltin<"__builtin_loongarch_iocsrrd_d">,
  Intrinsic<[llvm_i64_ty], [llvm_i32_ty], []>;

def int_loongarch_iocsrwr_b : GCCBuiltin<"__builtin_loongarch_iocsrwr_b">,
  Intrinsic<[], [llvm_i32_ty, llvm_i32_ty], []>;

def int_loongarch_iocsrwr_h : GCCBuiltin<"__builtin_loongarch_iocsrwr_h">,
  Intrinsic<[], [llvm_i32_ty, llvm_i32_ty], []>;

def int_loongarch_iocsrwr_w : GCCBuiltin<"__builtin_loongarch_iocsrwr_w">,
  Intrinsic<[], [llvm_i32_ty, llvm_i32_ty], []>;

def int_loongarch_iocsrwr_d : GCCBuiltin<"__builtin_loongarch_iocsrwr_d">,
  Intrinsic<[], [llvm_i64_ty, llvm_i32_ty], []>;

def int_loongarch_cacop : GCCBuiltin<"__builtin_loongarch_cacop">,
  Intrinsic<[], [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty], []>;

def int_loongarch_dcacop : GCCBuiltin<"__builtin_loongarch_dcacop">,
  Intrinsic<[], [llvm_i32_ty, llvm_i64_ty, llvm_i64_ty], []>;

def int_loongarch_crc_w_b_w : GCCBuiltin<"__builtin_loongarch_crc_w_b_w">,
  Intrinsic<[llvm_i32_ty], [llvm_i32_ty, llvm_i32_ty], []>;

def int_loongarch_crc_w_h_w : GCCBuiltin<"__builtin_loongarch_crc_w_h_w">,
  Intrinsic<[llvm_i32_ty], [llvm_i32_ty, llvm_i32_ty], []>;

def int_loongarch_crc_w_w_w : GCCBuiltin<"__builtin_loongarch_crc_w_w_w">,
  Intrinsic<[llvm_i32_ty], [llvm_i32_ty, llvm_i32_ty], []>;

def int_loongarch_crc_w_d_w : GCCBuiltin<"__builtin_loongarch_crc_w_d_w">,
  Intrinsic<[llvm_i32_ty], [llvm_i64_ty, llvm_i32_ty], []>;

def int_loongarch_crcc_w_b_w : GCCBuiltin<"__builtin_loongarch_crcc_w_b_w">,
  Intrinsic<[llvm_i32_ty], [llvm_i32_ty, llvm_i32_ty], []>;

def int_loongarch_crcc_w_h_w : GCCBuiltin<"__builtin_loongarch_crcc_w_h_w">,
  Intrinsic<[llvm_i32_ty], [llvm_i32_ty, llvm_i32_ty], []>;

def int_loongarch_crcc_w_w_w : GCCBuiltin<"__builtin_loongarch_crcc_w_w_w">,
  Intrinsic<[llvm_i32_ty], [llvm_i32_ty, llvm_i32_ty], []>;

def int_loongarch_crcc_w_d_w : GCCBuiltin<"__builtin_loongarch_crcc_w_d_w">,
  Intrinsic<[llvm_i32_ty], [llvm_i64_ty, llvm_i32_ty], []>;

def int_loongarch_tlbclr : GCCBuiltin<"__builtin_loongarch_tlbclr">,
  Intrinsic<[], [], []>;

def int_loongarch_tlbflush : GCCBuiltin<"__builtin_loongarch_tlbflush">,
  Intrinsic<[], [], []>;

def int_loongarch_tlbfill : GCCBuiltin<"__builtin_loongarch_tlbfill">,
  Intrinsic<[], [], []>;

def int_loongarch_tlbrd : GCCBuiltin<"__builtin_loongarch_tlbrd">,
  Intrinsic<[], [], []>;

def int_loongarch_tlbwr : GCCBuiltin<"__builtin_loongarch_tlbwr">,
  Intrinsic<[], [], []>;

def int_loongarch_tlbsrch : GCCBuiltin<"__builtin_loongarch_tlbsrch">,
  Intrinsic<[], [], []>;

def int_loongarch_syscall : GCCBuiltin<"__builtin_loongarch_syscall">,
  Intrinsic<[], [llvm_i64_ty], []>;

def int_loongarch_break : GCCBuiltin<"__builtin_loongarch_break">,
  Intrinsic<[], [llvm_i64_ty], []>;

def int_loongarch_asrtle_d : GCCBuiltin<"__builtin_loongarch_asrtle_d">,
  Intrinsic<[], [llvm_i64_ty, llvm_i64_ty], []>;

def int_loongarch_asrtgt_d : GCCBuiltin<"__builtin_loongarch_asrtgt_d">,
  Intrinsic<[], [llvm_i64_ty, llvm_i64_ty], []>;

def int_loongarch_dbar : GCCBuiltin<"__builtin_loongarch_dbar">,
  Intrinsic<[], [llvm_i64_ty], []>;

def int_loongarch_ibar : GCCBuiltin<"__builtin_loongarch_ibar">,
  Intrinsic<[], [llvm_i64_ty], []>;

}
