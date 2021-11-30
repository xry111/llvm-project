//===-- LoongArchCallingConv.td - Calling Conventions for LoongArch --*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This describes the calling conventions for LoongArch architecture.
//===----------------------------------------------------------------------===//

/// CCIfSubtarget - Match if the current subtarget has a feature F.
class CCIfSubtarget<string F, CCAction A, string Invert = "">
    : CCIf<!strconcat(Invert,
                      "static_cast<const LoongArchSubtarget&>"
			"(State.getMachineFunction().getSubtarget()).",
                      F),
           A>;

// The inverse of CCIfSubtarget
class CCIfSubtargetNot<string F, CCAction A> : CCIfSubtarget<F, A, "!">;

/// Match if the original argument (before lowering) was a float.
/// For example, this is true for i32's that were lowered from soft-float.
class CCIfOrigArgWasNotFloat<CCAction A>
    : CCIf<"!static_cast<LoongArchCCState *>(&State)->WasOriginalArgFloat(ValNo)",
           A>;

/// Match if the original argument (before lowering) was a 128-bit float (i.e.
/// long double).
class CCIfOrigArgWasF128<CCAction A>
    : CCIf<"static_cast<LoongArchCCState *>(&State)->WasOriginalArgF128(ValNo)", A>;

/// Match if this specific argument is a vararg.
/// This is slightly different fro CCIfIsVarArg which matches if any argument is
/// a vararg.
class CCIfArgIsVarArg<CCAction A>
    : CCIf<"!static_cast<LoongArchCCState *>(&State)->IsCallOperandFixed(ValNo)", A>;

/// Match if the return was a floating point vector.
class CCIfOrigArgWasNotVectorFloat<CCAction A>
    : CCIf<"!static_cast<LoongArchCCState *>(&State)"
                "->WasOriginalRetVectorFloat(ValNo)", A>;

/// Match if the special calling conv is the specified value.
class CCIfSpecialCallingConv<string CC, CCAction A>
    : CCIf<"static_cast<LoongArchCCState *>(&State)->getSpecialCallingConv() == "
               "LoongArchCCState::" # CC, A>;

// For soft-float, f128 values are returned in A0_64 rather than V1_64.
def RetCC_F128SoftFloat : CallingConv<[
  CCAssignToReg<[A0_64, A1_64]>
]>;

//
// For hard-float, f128 values are returned as a pair of f64's rather than a
// pair of i64's.
def RetCC_F128HardFloat : CallingConv<[
  //CCBitConvertToType<f64>,

  // Contrary to the ABI documentation, a struct containing a long double is
  // returned in $f0, and $f1 instead of the usual $f0, and $f2. This is to
  // match the de facto ABI as implemented by GCC.
  CCIfInReg<CCAssignToReg<[A0_64, A1_64]>>,

  CCAssignToReg<[A0_64, A1_64]>
]>;

// Handle F128 specially since we can't identify the original type during the
// tablegen-erated code.
def RetCC_F128 : CallingConv<[
  CCIfSubtarget<"useSoftFloat()",
      CCIfType<[i64], CCDelegateTo<RetCC_F128SoftFloat>>>,
  CCIfSubtargetNot<"useSoftFloat()",
      CCIfType<[i64], CCDelegateTo<RetCC_F128HardFloat>>>
]>;

//===----------------------------------------------------------------------===//
// LoongArch LP32 Calling Convention
//===----------------------------------------------------------------------===//

def CC_LoongArchLP32 : CallingConv<[
  // Promote i8/i16 arguments to i32.
  CCIfType<[i1, i8, i16], CCPromoteToType<i32>>,

  // Integer values get stored in stack slots that are 4 bytes in
  // size and 4-byte aligned.
  CCIfType<[i32, f32], CCAssignToStack<4, 4>>,

  // Integer values get stored in stack slots that are 8 bytes in
  // size and 8-byte aligned.
  CCIfType<[f64], CCAssignToStack<8, 8>>
]>;

// Only the return rules are defined here for LP32. The rules for argument
// passing are defined in LoongArchISelLowering.cpp.
def RetCC_LoongArchLP32 : CallingConv<[
  // Promote i1/i8/i16 return values to i32.
  CCIfType<[i1, i8, i16], CCPromoteToType<i32>>,

  // i32 are returned in registers V0, V1, A0, A1, unless the original return
  // type was a vector of floats.
  CCIfOrigArgWasNotVectorFloat<CCIfType<[i32],
                                        CCAssignToReg<[A0, A1]>>>,

  // f32 are returned in registers F0, F2
  CCIfType<[f32], CCAssignToReg<[F0, F1]>>,

  // f64 arguments are returned in F0_64 and F2_64 in FP64bit mode or
  // in F0 and F1 in FP32bit mode.
  CCIfType<[f64], CCIfSubtarget<"isFP64bit()", CCAssignToReg<[F0_64, F1_64]>>>
]>;

def CC_LoongArchLP32_FP32 : CustomCallingConv;
def CC_LoongArchLP32_FP64 : CustomCallingConv;
def CC_LoongArch_F128 : CustomCallingConv;

def CC_LoongArchLP32_FP : CallingConv<[
  CCIfSubtargetNot<"isFP64bit()", CCDelegateTo<CC_LoongArchLP32_FP32>>,
  CCIfSubtarget<"isFP64bit()", CCDelegateTo<CC_LoongArchLP32_FP64>>
]>;

//===----------------------------------------------------------------------===//
// LoongArch LPX32/LP64 Calling Convention
//===----------------------------------------------------------------------===//

def CC_LoongArchLP64LPX32_SoftFloat : CallingConv<[
  CCAssignToReg<[A0, A1, A2, A3,
                 A4, A5, A6, A7]>,
  CCAssignToStack<4, 8>
]>;

def CC_LoongArchLP64LPX32 : CallingConv<[

  // All integers (except soft-float integers) are promoted to 64-bit.
  CCIfType<[i8, i16, i32], CCIfOrigArgWasNotFloat<CCPromoteToType<i64>>>,

  // The only i32's we have left are soft-float arguments.
  CCIfSubtarget<"useSoftFloat()", CCIfType<[i32], CCDelegateTo<CC_LoongArchLP64LPX32_SoftFloat>>>,

  // Integer arguments are passed in integer registers.
  //CCIfType<[i64], CCAssignToRegWithShadow<[A0_64, A1_64, A2_64, A3_64,
  //                                         A4_64, A5_64, A6_64, A7_64],
  //                                        [F0_64, F1_64, F2_64, F3_64,
  //                                         F4_64, F5_64, F6_64, F7_64]>>,
  CCIfType<[i64], CCAssignToReg<[A0_64, A1_64, A2_64, A3_64,
                                 A4_64, A5_64, A6_64, A7_64]>>,

  // f32 arguments are passed in single precision FP registers.
  CCIfType<[f32], CCAssignToReg<[F0, F1, F2, F3,
                                 F4, F5, F6, F7]>>,

  // f64 arguments are passed in double precision FP registers.
  CCIfType<[f64], CCAssignToReg<[F0_64, F1_64, F2_64, F3_64,
                                 F4_64, F5_64, F6_64, F7_64]>>,

  // others f32 arguments are passed in single precision FP registers.
  CCIfType<[f32], CCAssignToReg<[A0, A1, A2, A3, A4, A5, A6, A7]>>,

  // others f64 arguments are passed in double precision FP registers.
  CCIfType<[f64], CCAssignToReg<[A0_64, A1_64, A2_64, A3_64,
                                 A4_64, A5_64, A6_64, A7_64]>>,

  // All stack parameter slots become 64-bit doublewords and are 8-byte aligned.
  CCIfType<[f32], CCAssignToStack<4, 8>>,
  CCIfType<[i64, f64], CCAssignToStack<8, 8>>,
  CCIfType<[v16i8, v8i16, v4i32, v2i64, v4f32, v2f64],
         CCAssignToStack<16, 16>>,
  CCIfType<[v32i8, v16i16, v8i32, v4i64, v8f32, v4f64],
         CCAssignToStack<32, 32>>
]>;

// LPX32/LP64 variable arguments.
// All arguments are passed in integer registers.
def CC_LoongArchLP64LPX32_VarArg : CallingConv<[
  // All integers are promoted to 64-bit.
  CCIfType<[i8, i16, i32], CCPromoteToType<i64>>,

  CCIfType<[f32], CCAssignToReg<[A0, A1, A2, A3, A4, A5, A6, A7]>>,

  CCIfType<[i64], CCIfOrigArgWasF128<CCDelegateTo<CC_LoongArch_F128>>>,

  CCIfType<[i64, f64], CCAssignToReg<[A0_64, A1_64, A2_64, A3_64,
                                      A4_64, A5_64, A6_64, A7_64]>>,

  // All stack parameter slots become 64-bit doublewords and are 8-byte aligned.
  CCIfType<[f32], CCAssignToStack<4, 8>>,
  CCIfType<[i64, f64], CCAssignToStack<8, 8>>
]>;

def RetCC_LoongArchLP64LPX32 : CallingConv<[
  // f128 needs to be handled similarly to f32 and f64. However, f128 is not
  // legal and is lowered to i128 which is further lowered to a pair of i64's.
  // This presents us with a problem for the calling convention since hard-float
  // still needs to pass them in FPU registers, and soft-float needs to use $v0,
  // and $a0 instead of the usual $v0, and $v1. We therefore resort to a
  // pre-analyze (see PreAnalyzeReturnForF128()) step to pass information on
  // whether the result was originally an f128 into the tablegen-erated code.
  //
  // f128 should only occur for the LP64 ABI where long double is 128-bit. On
  // LPX32, long double is equivalent to double.
  CCIfType<[i64], CCIfOrigArgWasF128<CCDelegateTo<RetCC_F128>>>,

  CCIfType<[i8, i16, i32, i64], CCIfInReg<CCPromoteToType<i64>>>,

  // i64 are returned in registers V0_64, V1_64
  CCIfType<[i64], CCAssignToReg<[A0_64, A1_64]>>,

  // f32 are returned in registers F0, F2
  CCIfType<[f32], CCAssignToReg<[F0, F1]>>,

  // f64 are returned in registers D0, D2
  CCIfType<[f64], CCAssignToReg<[F0_64, F1_64]>>
]>;

//===----------------------------------------------------------------------===//
// LoongArch FastCC Calling Convention
//===----------------------------------------------------------------------===//
//LP32 has been removed because of not support

def CC_LoongArchLP64LPX32_FastCC : CallingConv<[
  // Integer arguments are passed in integer registers.
  CCIfType<[i64], CCAssignToReg<[A0_64, A1_64, A2_64, A3_64, T0_64, T1_64,
                                 T2_64, T3_64, T4_64, T5_64, T6_64, T7_64,
                                 T8_64]>>,

  // f64 arguments are passed in double-precision floating pointer registers.
  CCIfType<[f64], CCAssignToReg<[F0_64, F1_64, F2_64, F3_64, F4_64, F5_64,
                                 F6_64, F7_64, F8_64, F9_64, F10_64, F11_64,
                                 F12_64, F13_64, F14_64, F15_64, F16_64, F17_64,
                                 F18_64, F19]>>,

  // Stack parameter slots for i64 and f64 are 64-bit doublewords and
  // 8-byte aligned.
  CCIfType<[i64, f64], CCAssignToStack<8, 8>>
]>;

def CC_LoongArch_FastCC : CallingConv<[
  // Handles byval parameters.
  CCIfByVal<CCPassByVal<4, 4>>,

  // Promote i8/i16 arguments to i32.
  CCIfType<[i8, i16], CCPromoteToType<i32>>,

  // Integer arguments are passed in integer registers. All scratch registers,
  // except for V0 and T9, are available to be used as argument registers.
  CCIfType<[i32], CCAssignToReg<[A0, A1, A2, A3, T0, T1, T2, T3, T4, T5, T6, T7, T8]>>,

  // Stack parameter slots for i32 and f32 are 32-bit words and 4-byte aligned.
  CCIfType<[i32, f32], CCAssignToStack<4, 4>>,

//  CCIfSubtarget<"isABI_LP32()", CCDelegateTo<CC_LoongArchLP32_FastCC>>,
  CCDelegateTo<CC_LoongArchLP64LPX32_FastCC>
]>;

//===----------------------------------------------------------------------===//
// LoongArch Calling Convention Dispatch
//===----------------------------------------------------------------------===//

def RetCC_LoongArch : CallingConv<[
  CCIfSubtarget<"isABI_LPX32()", CCDelegateTo<RetCC_LoongArchLP64LPX32>>,
  CCIfSubtarget<"isABI_LP64()", CCDelegateTo<RetCC_LoongArchLP64LPX32>>,
  CCDelegateTo<RetCC_LoongArchLP32>
]>;

def CC_LoongArch_ByVal : CallingConv<[
  CCIfSubtarget<"isABI_LP32()", CCIfByVal<CCPassByVal<4, 4>>>,
  CCIfByVal<CCPassByVal<8, 8>>
]>;

def CC_LoongArch_FixedArg : CallingConv<[
  CCIfByVal<CCDelegateTo<CC_LoongArch_ByVal>>,
  //CCIfByVal<CCIfType<[i64],  CCAssignToReg<[A0_64,  A1_64,  A2_64,  A3_64,
  //                                          A4_64,  A5_64,  A6_64,  A7_64]>>>,

  // f128 needs to be handled similarly to f32 and f64 on hard-float. However,
  // f128 is not legal and is lowered to i128 which is further lowered to a pair
  // of i64's.
  // This presents us with a problem for the calling convention since hard-float
  // still needs to pass them in FPU registers. We therefore resort to a
  // pre-analyze (see PreAnalyzeFormalArgsForF128()) step to pass information on
  // whether the argument was originally an f128 into the tablegen-erated code.
  //
  // f128 should only occur for the LP64 ABI where long double is 128-bit. On
  // LPX32, long double is equivalent to double.
  CCIfType<[i64],
      CCIfSubtargetNot<"useSoftFloat()",
          CCIfOrigArgWasF128<CCBitConvertToType<i64>>>>,

  CCIfCC<"CallingConv::Fast", CCDelegateTo<CC_LoongArch_FastCC>>,

  CCIfSubtarget<"isABI_LP32()", CCDelegateTo<CC_LoongArchLP32_FP>>,
  CCDelegateTo<CC_LoongArchLP64LPX32>
]>;

def CC_LoongArch_VarArg : CallingConv<[
  CCIfByVal<CCDelegateTo<CC_LoongArch_ByVal>>,

  CCIfSubtarget<"isABI_LP32()", CCDelegateTo<CC_LoongArchLP32_FP>>,
  CCDelegateTo<CC_LoongArchLP64LPX32_VarArg>
]>;

def CC_LoongArch : CallingConv<[
  CCIfVarArg<CCIfArgIsVarArg<CCDelegateTo<CC_LoongArch_VarArg>>>,
  CCDelegateTo<CC_LoongArch_FixedArg>
]>;

//===----------------------------------------------------------------------===//
// Callee-saved register lists.
//===----------------------------------------------------------------------===//

def CSR_SingleFloatOnly : CalleeSavedRegs<(add (sequence "F%u", 31, 24), RA, FP,
                                               (sequence "S%u", 8, 0))>;

//def CSR_LP32_FPXX : CalleeSavedRegs<(add (sequence "D%u", 15, 10), RA, FP,
//                                        (sequence "S%u", 8, 0))> {
//  let OtherPreserved = (add (decimate (sequence "F%u", 30, 20), 2));
//}

def CSR_LP32 : CalleeSavedRegs<(add (sequence "F%u_64", 31, 24), RA, FP,
                                   (sequence "S%u", 8, 0))>;

//def CSR_LP32_FP64 :
//  CalleeSavedRegs<(add (decimate (sequence "D%u_64", 30, 20), 2), RA, FP,
//                       (sequence "S%u", 8, 0))>;

def CSR_LPX32 : CalleeSavedRegs<(add F20_64, F22_64, F24_64, F26_64, F28_64,
                                   F30_64, RA_64, FP_64,
                                   (sequence "S%u_64", 8, 0))>;

//def CSR_LP64 : CalleeSavedRegs<(add (sequence "D%u_64", 31, 24), RA_64, SP_64, FP_64,
def CSR_LP64 : CalleeSavedRegs<(add (sequence "F%u_64", 31, 24), RA_64, FP_64,
                                   (sequence "S%u_64", 8, 0))>;
