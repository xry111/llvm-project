//===-- LoongArchInstrInfoF.td - RISC-V 'F' instructions -------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file describes the RISC-V instructions from the standard 'F',
// Single-Precision Floating-Point instruction set extension.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// RISC-V specific DAG Nodes.
//===----------------------------------------------------------------------===//

def SDT_LoongArchFMV_W_X_LA64
    : SDTypeProfile<1, 1, [SDTCisVT<0, f32>, SDTCisVT<1, i64>]>;
def SDT_LoongArchFMV_X_ANYEXTW_LA64
    : SDTypeProfile<1, 1, [SDTCisVT<0, i64>, SDTCisVT<1, f32>]>;

def larch_fmv_w_x_rv64
    : SDNode<"LoongArchISD::FMV_W_X_LA64", SDT_LoongArchFMV_W_X_LA64>;
def larch_fmv_x_anyextw_rv64
    : SDNode<"LoongArchISD::FMV_X_ANYEXTW_LA64", SDT_LoongArchFMV_X_ANYEXTW_LA64>;

//===----------------------------------------------------------------------===//
// Instruction class templates
//===----------------------------------------------------------------------===//

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
class LA_FPALUS_rr<bits<7> funct7, string opcodestr>
    : LAInst3R<0b0000000100, funct7, (outs FPR32:$rd),
               (ins FPR32:$rj, FPR32:$rk), opcodestr, "$rd, $rj, $rk">,
	  Sched<[]>;

class LA_FCMPS<bits<5> cond, string condstr>
    : LAInstFcmp<0b01, cond, (outs CFR:$cd), (ins FPR32:$fj, FPR32:$fk),
                 "fcmp."#condstr#".s", "$cd, $fj, $fk">,
      Sched<[]>;

class LA_BranchCC_ci21<bits<1> funct1, string opcodestr>
    : LAInstBC<funct1, (outs),
               (ins CFR:$cj, off23_lsb00:$imm21),
               opcodestr, "$cj, $imm21">,
      Sched<[]> {
  let isBranch = 1;
  let isTerminator = 1;
}
class LA_FPUnaryOp_r<bits<10> funct10, RegisterClass rdty,
                     RegisterClass rsty, string opcodestr>
    : LAInst2R_F<funct10, (outs rdty:$fd), (ins rsty:$fj),
                 opcodestr, "$fd, $fj">;
class LA_FPFMAS_rrr<bits<4> funct4, string opcodestr>
    : LAInst4R<funct4, (outs FPR32:$fd),
               (ins FPR32:$fj, FPR32:$fk, FPR32:$fa),
               opcodestr, "$fd, $fj, $fk, $fa">,
      Sched<[]>;
}

//===----------------------------------------------------------------------===//
// Instructions
//===----------------------------------------------------------------------===//

let Predicates = [HasStdExtF] in {
def FADD_S       : LA_FPALUS_rr<0b0000001, "fadd.s">;
def FSUB_S       : LA_FPALUS_rr<0b0000101, "fsub.s">;
def FMUL_S       : LA_FPALUS_rr<0b0001001, "fmul.s">;
def FDIV_S       : LA_FPALUS_rr<0b0001101, "fdiv.s">;
def FMAX_S       : LA_FPALUS_rr<0b0010001, "fmax.s">;
def FMIN_S       : LA_FPALUS_rr<0b0010101, "fmin.s">;
def FMAXA_S      : LA_FPALUS_rr<0b0011001, "fmaxa.s">;
def FMINA_S      : LA_FPALUS_rr<0b0011101, "fmina.s">;
def FSCALEB_S    : LA_FPALUS_rr<0b0100001, "fscaleb.s">;
def FCOPYSIGN_S  : LA_FPALUS_rr<0b0100101, "fcopysign.s">;

def FCMP_CAF_S   : LA_FCMPS<0x00, "caf">;
def FCMP_CLT_S   : LA_FCMPS<0x02, "clt">;
def FCMP_CEQ_S   : LA_FCMPS<0x04, "ceq">;
def FCMP_CLE_S   : LA_FCMPS<0x06, "cle">;
def FCMP_CUN_S   : LA_FCMPS<0x08, "cun">;
def FCMP_CULT_S  : LA_FCMPS<0x0A, "cult">;
def FCMP_CUEQ_S  : LA_FCMPS<0x0C, "cueq">;
def FCMP_CULE_S  : LA_FCMPS<0x0E, "cule">;
def FCMP_CNE_S   : LA_FCMPS<0x10, "cne">;
def FCMP_COR_S   : LA_FCMPS<0x14, "cor">;
def FCMP_CUNE_S  : LA_FCMPS<0x18, "cune">;

def FCMP_SAF_S   : LA_FCMPS<0x01, "saf">;
def FCMP_SLT_S   : LA_FCMPS<0x03, "slt">;
def FCMP_SEQ_S   : LA_FCMPS<0x05, "seq">;
def FCMP_SLE_S   : LA_FCMPS<0x07, "sle">;
def FCMP_SUN_S   : LA_FCMPS<0x09, "sun">;
def FCMP_SULT_S  : LA_FCMPS<0x0B, "sult">;
def FCMP_SUEQ_S  : LA_FCMPS<0x0D, "sueq">;
def FCMP_SULE_S  : LA_FCMPS<0x0F, "sule">;
def FCMP_SNE_S   : LA_FCMPS<0x11, "sne">;
def FCMP_SOR_S   : LA_FCMPS<0x15, "sor">;
def FCMP_SUNE_S  : LA_FCMPS<0x19, "sune">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
def FSEL : LAInstFsel<(outs FPR64:$fd),
                      (ins FPR64:$fj, FPR64:$fk, CFR:$ca),
                      "fsel", "$fd, $fj, $fk, $ca">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isCodeGenOnly = 1 in
def FSEL_S : LAInstFsel<(outs FPR32:$fd),
                        (ins FPR32:$fj, FPR32:$fk, CFR:$ca),
                        "fsel", "$fd, $fj, $fk, $ca">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
def MOVCF2GR     : LAInstCR<0b11, (outs GPR:$rd), (ins CFR:$cj),
                            "movcf2gr", "$rd, $cj">;
def MOVGR2CF     : LAInstRC<0b10, (outs CFR:$cd), (ins GPR:$rj),
                            "movcf2gr", "$cd, $rj">;
}

def BCEQZ        : LA_BranchCC_ci21<0b0, "bceqz">;
def BCNEZ        : LA_BranchCC_ci21<0b1, "bcnez">;

def FABS_S       : LA_FPUnaryOp_r<0b0100000001, FPR32, FPR32, "fabs.s">;
def FNEG_S       : LA_FPUnaryOp_r<0b0100000101, FPR32, FPR32, "fneg.s">;
def FLOGB_S      : LA_FPUnaryOp_r<0b0100001001, FPR32, FPR32, "flogb.s">;
def FCLASS_S     : LA_FPUnaryOp_r<0b0100001101, FPR32, FPR32, "fclass.s">;
def FSQRT_S      : LA_FPUnaryOp_r<0b0100010001, FPR32, FPR32, "fsqrt.s">;
def FRECIP_S     : LA_FPUnaryOp_r<0b0100010101, FPR32, FPR32, "frecip.s">;
def FRSQRT_S     : LA_FPUnaryOp_r<0b0100011001, FPR32, FPR32, "frsqrt.s">;
def FMOV_S       : LA_FPUnaryOp_r<0b0100100101, FPR32, FPR32, "fmov.s">;
def MOVGR2FR_W   : LA_FPUnaryOp_r<0b0100101001, FPR32, GPR, "movgr2fr.w">;
def MOVGR2FR_D   : LA_FPUnaryOp_r<0b0100101010, FPR64, GPR, "movgr2fr.d">;
def MOVFR2GR_S   : LA_FPUnaryOp_r<0b0100101101, GPR, FPR32, "movfr2gr.s">;
def MOVFR2GR_D   : LA_FPUnaryOp_r<0b0100101110, GPR, FPR64, "movfr2gr.d">;
def FTINTRM_W_S  : LA_FPUnaryOp_r<0b1010000001, FPR32, FPR32, "ftintrm.w.s">;
def FTINTRM_L_S  : LA_FPUnaryOp_r<0b1010001001, FPR64, FPR32, "ftintrm.l.s">;
def FTINTRP_W_S  : LA_FPUnaryOp_r<0b1010010001, FPR32, FPR32, "ftintrp.w.s">;
def FTINTRP_L_S  : LA_FPUnaryOp_r<0b1010011001, FPR64, FPR32, "ftintrp.l.s">;
def FTINTRZ_W_S  : LA_FPUnaryOp_r<0b1010100001, FPR32, FPR32, "ftintrz.w.s">;
def FTINTRZ_L_S  : LA_FPUnaryOp_r<0b1010101001, FPR64, FPR32, "ftintrz.l.s">;
def FTINTRNE_W_S : LA_FPUnaryOp_r<0b1010110001, FPR32, FPR32, "ftintrne.w.s">;
def FTINTRNE_L_S : LA_FPUnaryOp_r<0b1010111001, FPR64, FPR32, "ftintrne.l.s">;
def FTINT_W_S    : LA_FPUnaryOp_r<0b1011000001, FPR32, FPR32, "ftint.w.s">;
def FTINT_L_S    : LA_FPUnaryOp_r<0b1011001001, FPR64, FPR32, "ftint.l.s">;
def FFINT_S_W    : LA_FPUnaryOp_r<0b1101000100, FPR32, FPR32, "ffint.s.w">;
def FFINT_S_L    : LA_FPUnaryOp_r<0b1101000110, FPR32, FPR64, "ffint.s.l">;

let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in
def FLD_S : LAInst2RI12<0b001010, 0b1100,
                        (outs FPR32:$rd), (ins GPR:$rj, simm12:$imm12),
                        "fld.s", "$rd, $rj, $imm12">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in
def FST_S : LAInst2RI12<0b001010, 0b1101,
                        (outs), (ins FPR32:$rd, GPR:$rj, simm12:$imm12),
                        "fst.s", "$rd, $rj, $imm12">;

def FMADD_S      : LA_FPFMAS_rrr<0b0001, "fmadd.s">;
def FMSUB_S      : LA_FPFMAS_rrr<0b0101, "fmsub.s">;
def FNMADD_S     : LA_FPFMAS_rrr<0b1001, "fnmadd.s">;
def FNMSUB_S     : LA_FPFMAS_rrr<0b1101, "fnmsub.s">;
} // Predicates = [HasStdExtF]

//===----------------------------------------------------------------------===//
// Pseudo-instructions and codegen patterns
//===----------------------------------------------------------------------===//

/// Floating point constants
def fpimm0 : PatLeaf<(fpimm), [{ return N->isExactlyValue(+0.0); }]>;

/// Generic pattern classes
class LAPatFpr32Fpr32<SDPatternOperator OpNode, LAInst Inst>
    : Pat<(OpNode FPR32:$fj, FPR32:$fk), (Inst $fj, $fk)>;

let Predicates = [HasStdExtF] in {

/// Float constants
def : Pat<(f32 (fpimm0)), (MOVGR2FR_W R0)>;

/// Float conversion operations

// [u]int32<->float conversion patterns must be gated on IsLA32 or IsLA64, so
// are defined later.

/// Float arithmetic operations

def : LAPatFpr32Fpr32<fadd, FADD_S>;
def : LAPatFpr32Fpr32<fsub, FSUB_S>;
def : LAPatFpr32Fpr32<fmul, FMUL_S>;
def : LAPatFpr32Fpr32<fdiv, FDIV_S>;

// The LoongArch ISA spec Vol. 1 (v1.00) defines fmin and fmax to match the
// behavior of IEEE 754-2008 maxNum/minNum operations. This doesn't match
// the LLVM behaviour (see https://bugs.llvm.org/show_bug.cgi?id=27363).
def : LAPatFpr32Fpr32<fminnum_ieee, FMIN_S>;
def : LAPatFpr32Fpr32<fmaxnum_ieee, FMAX_S>;
def : Pat<(fcanonicalize FPR32:$rs), (FMAX_S $rs, $rs)>;

def : LAPatFpr32Fpr32<fcopysign, FCOPYSIGN_S>;

def : Pat<(fsqrt FPR32:$rs1), (FSQRT_S FPR32:$rs1)>;
def : Pat<(fneg FPR32:$rs1), (FNEG_S $rs1)>;
def : Pat<(fabs FPR32:$rs1), (FABS_S $rs1)>;

// fmadd: rs1 * rs2 + rs3
def : Pat<(fma FPR32:$rs1, FPR32:$rs2, FPR32:$rs3),
          (FMADD_S $rs1, $rs2, $rs3)>;

// fmsub: rs1 * rs2 - rs3
def : Pat<(fma FPR32:$rs1, FPR32:$rs2, (fneg FPR32:$rs3)),
          (FMSUB_S FPR32:$rs1, FPR32:$rs2, FPR32:$rs3)>;

// fnmsub: -rs1 * rs2 + rs3
def : Pat<(fma (fneg FPR32:$rs1), FPR32:$rs2, FPR32:$rs3),
          (FNMSUB_S FPR32:$rs1, FPR32:$rs2, FPR32:$rs3)>;

// fnmadd: -rs1 * rs2 - rs3
def : Pat<(fma (fneg FPR32:$rs1), FPR32:$rs2, (fneg FPR32:$rs3)),
          (FNMADD_S FPR32:$rs1, FPR32:$rs2, FPR32:$rs3)>;

/// Setcc

multiclass LAPatFpr32Fpr32Cmp<SDPatternOperator OpNode, LAInst Inst> {
  def : Pat<(XLenVT (OpNode FPR32:$fj, FPR32:$fk)),
            (ANDI   (MOVCF2GR (Inst $fj, $fk)), 1)>;
  def : Pat<(brcond (XLenVT (OpNode FPR32:$fj, FPR32:$fk)), bb:$imm),
            (BCNEZ  (Inst $fj, $fk), $imm)>;
  def : Pat<(select (XLenVT (OpNode FPR32:$fj, FPR32:$fk)),
                    FPR32:$tv, FPR32:$fv),
            (FSEL_S $tv, $fv, (Inst $fj, $fk))>;
  let Predicates = [HasStdExtD] in
  def : Pat<(select (XLenVT (OpNode FPR32:$fj, FPR32:$fk)),
                    FPR64:$tv, FPR64:$fv),
            (FSEL $tv, $fv, (Inst $fj, $fk))>;
}

defm : LAPatFpr32Fpr32Cmp<setoeq, FCMP_SEQ_S>;
defm : LAPatFpr32Fpr32Cmp<setolt, FCMP_SLT_S>;
defm : LAPatFpr32Fpr32Cmp<setole, FCMP_SLE_S>;
defm : LAPatFpr32Fpr32Cmp<setone, FCMP_SNE_S>;
defm : LAPatFpr32Fpr32Cmp<seto,   FCMP_SOR_S>;
defm : LAPatFpr32Fpr32Cmp<setuo,  FCMP_SUN_S>;
defm : LAPatFpr32Fpr32Cmp<setueq, FCMP_SUEQ_S>;
defm : LAPatFpr32Fpr32Cmp<setult, FCMP_SULT_S>;
defm : LAPatFpr32Fpr32Cmp<setule, FCMP_SULE_S>;
defm : LAPatFpr32Fpr32Cmp<setune, FCMP_SUNE_S>;
defm : LAPatFpr32Fpr32Cmp<seteq,  FCMP_SEQ_S>;
defm : LAPatFpr32Fpr32Cmp<setlt,  FCMP_SLT_S>;
defm : LAPatFpr32Fpr32Cmp<setle,  FCMP_SLE_S>;
defm : LAPatFpr32Fpr32Cmp<setne,  FCMP_SNE_S>;

def : Pat<(select GPR:$cond, FPR32:$tv, FPR32:$fv),
          (FSEL_S $tv, $fv, (MOVGR2CF $cond))>;

/// Loads

defm : LALdPat12<load, FLD_S>;

/// Stores

defm : LAStPat12<store, FST_S, FPR32>;

} // Predicates = [HasStdExtF]

let Predicates = [HasStdExtF, IsLA32] in {
// Moves (no conversion)
def : Pat<(bitconvert GPR:$rs1), (MOVGR2FR_W GPR:$rs1)>;
def : Pat<(bitconvert FPR32:$rs1), (MOVFR2GR_S FPR32:$rs1)>;

// float->[u]int. Round-to-zero must be used.
def : Pat<(fp_to_sint FPR32:$rs1), (MOVFR2GR_S (FTINTRZ_W_S $rs1))>;

// [u]int->float. Match GCC and default to using dynamic rounding mode.
def : Pat<(sint_to_fp GPR:$rs1), (FFINT_S_W (MOVGR2FR_W $rs1))>;
} // Predicates = [HasStdExtF, IsLA32]

let Predicates = [HasStdExtF, IsLA64] in {
def : Pat<(larch_fmv_w_x_rv64 GPR:$src), (MOVGR2FR_W GPR:$src)>;
def : Pat<(larch_fmv_x_anyextw_rv64 FPR32:$src), (MOVFR2GR_S FPR32:$src)>;

def : Pat<(fp_to_sint FPR32:$rs1), (MOVFR2GR_D (FTINTRZ_L_S $rs1))>;

def : Pat<(sint_to_fp (sexti32 GPR:$rs1)), (FFINT_S_W (MOVGR2FR_W $rs1))>;
def : Pat<(sint_to_fp GPR:$rs1), (FFINT_S_L (MOVGR2FR_D $rs1))>;
} // Predicates = [HasStdExtF, IsLA64]
