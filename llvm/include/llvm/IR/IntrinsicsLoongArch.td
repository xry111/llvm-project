//===- IntrinsicsLoongArch.td - Defines LoongArch intrinsics -------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines all of the LoongArch-specific intrinsics.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Atomics

// Atomic Intrinsics have multiple versions for different access widths, which
// all follow one of the following signatures (depending on how many arguments
// they require). We carefully instantiate only specific versions of these for
// specific integer widths, rather than using `llvm_anyint_ty`.
//
// In fact, as these intrinsics take `llvm_anyptr_ty`, the given names are the
// canonical names, and the intrinsics used in the code will have a name
// suffixed with the pointer type they are specialised for (denoted `<p>` in the
// names below), in order to avoid type conflicts.

let TargetPrefix = "loongarch" in {

  // T @llvm.<name>.T.<p>(any*, T, T, T imm);
  class LAMaskedAtomicRMWFourArg<LLVMType itype>
      : Intrinsic<[itype], [llvm_anyptr_ty, itype, itype, itype],
                  [IntrArgMemOnly, NoCapture<ArgIndex<0>>, ImmArg<ArgIndex<3>>]>;
  // T @llvm.<name>.T.<p>(any*, T, T, T, T imm);
  class LAMaskedAtomicRMWFiveArg<LLVMType itype>
      : Intrinsic<[itype], [llvm_anyptr_ty, itype, itype, itype, itype],
                  [IntrArgMemOnly, NoCapture<ArgIndex<0>>, ImmArg<ArgIndex<4>>]>;

  // We define 32-bit and 64-bit variants of the above, where T stands for i32
  // or i64 respectively:
  multiclass LAMaskedAtomicRMWFourArgIntrinsics {
    // i32 @llvm.<name>.i32.<p>(any*, i32, i32, i32 imm);
    def _i32 : LAMaskedAtomicRMWFourArg<llvm_i32_ty>;
    // i64 @llvm.<name>.i32.<p>(any*, i64, i64, i64 imm);
    def _i64 : LAMaskedAtomicRMWFourArg<llvm_i64_ty>;
  }

  multiclass LAMaskedAtomicRMWFiveArgIntrinsics {
    // i32 @llvm.<name>.i32.<p>(any*, i32, i32, i32, i32 imm);
    def _i32 : LAMaskedAtomicRMWFiveArg<llvm_i32_ty>;
    // i64 @llvm.<name>.i64.<p>(any*, i64, i64, i64, i64 imm);
    def _i64 : LAMaskedAtomicRMWFiveArg<llvm_i64_ty>;
  }

  // @llvm.loongarch.masked.atomicrmw.*.{i32,i64}.<p>(...)
  defm int_loongarch_masked_atomicrmw_xchg : LAMaskedAtomicRMWFourArgIntrinsics;
  defm int_loongarch_masked_atomicrmw_add : LAMaskedAtomicRMWFourArgIntrinsics;
  defm int_loongarch_masked_atomicrmw_sub : LAMaskedAtomicRMWFourArgIntrinsics;
  defm int_loongarch_masked_atomicrmw_nand : LAMaskedAtomicRMWFourArgIntrinsics;
  // Signed min and max need an extra operand to do sign extension with.
  defm int_loongarch_masked_atomicrmw_max : LAMaskedAtomicRMWFiveArgIntrinsics;
  defm int_loongarch_masked_atomicrmw_min : LAMaskedAtomicRMWFiveArgIntrinsics;
  // Unsigned min and max don't need the extra operand.
  defm int_loongarch_masked_atomicrmw_umax : LAMaskedAtomicRMWFourArgIntrinsics;
  defm int_loongarch_masked_atomicrmw_umin : LAMaskedAtomicRMWFourArgIntrinsics;

  // @llvm.loongarch.masked.cmpxchg.{i32,i64}.<p>(...)
  defm int_loongarch_masked_cmpxchg : LAMaskedAtomicRMWFiveArgIntrinsics;

} // TargetPrefix = "loongarch"
