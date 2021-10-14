//===-- LoongArchInstrInfoD.td - RISC-V 'D' instructions -------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file describes the RISC-V instructions from the standard 'D',
// Double-Precision Floating-Point instruction set extension.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// RISC-V specific DAG Nodes.
//===----------------------------------------------------------------------===//

def SDT_LoongArchBuildPairF64 : SDTypeProfile<1, 2, [SDTCisVT<0, f64>,
                                                 SDTCisVT<1, i32>,
                                                 SDTCisSameAs<1, 2>]>;
def SDT_LoongArchSplitF64     : SDTypeProfile<2, 1, [SDTCisVT<0, i32>,
                                                 SDTCisVT<1, i32>,
                                                 SDTCisVT<2, f64>]>;

def LoongArchBuildPairF64 : SDNode<"LoongArchISD::BuildPairF64", SDT_LoongArchBuildPairF64>;
def LoongArchSplitF64     : SDNode<"LoongArchISD::SplitF64", SDT_LoongArchSplitF64>;

//===----------------------------------------------------------------------===//
// Instruction Class Templates
//===----------------------------------------------------------------------===//

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
class LA_FPALUD_rr<bits<7> funct7, string opcodestr>
    : LAInst3R<0b0000000100, funct7, (outs FPR64:$rd),
               (ins FPR64:$rj, FPR64:$rk), opcodestr, "$rd, $rj, $rk">,
	  Sched<[]>;

class LA_FCMPD<bits<5> cond, string condstr>
    : LAInstFcmp<0b10, cond, (outs CFR:$cd), (ins FPR64:$fj, FPR64:$fk),
                 "fcmp."#condstr#".s", "$cd, $fj, $fk">,
      Sched<[]>;
class LA_FPFMAD_rrr<bits<4> funct4, string opcodestr>
    : LAInst4R<funct4, (outs FPR64:$fd),
               (ins FPR64:$fj, FPR64:$fk, FPR64:$fa),
               opcodestr, "$fd, $fj, $fk, $fa">,
      Sched<[]>;
}

//===----------------------------------------------------------------------===//
// Instructions
//===----------------------------------------------------------------------===//

let Predicates = [HasStdExtD] in {

def FADD_D       : LA_FPALUD_rr<0b0000010, "fadd.d">;
def FSUB_D       : LA_FPALUD_rr<0b0000110, "fsub.d">;
def FMUL_D       : LA_FPALUD_rr<0b0001010, "fmul.d">;
def FDIV_D       : LA_FPALUD_rr<0b0001110, "fdiv.d">;
def FMAX_D       : LA_FPALUD_rr<0b0010010, "fmax.d">;
def FMIN_D       : LA_FPALUD_rr<0b0010110, "fmin.d">;
def FMAXA_D      : LA_FPALUD_rr<0b0011010, "fmaxa.d">;
def FMINA_D      : LA_FPALUD_rr<0b0011110, "fmina.d">;
def FSCALEB_D    : LA_FPALUD_rr<0b0100010, "fscaleb.d">;
def FCOPYSIGN_D  : LA_FPALUD_rr<0b0100110, "fcopysign.d">;

def FCMP_CAF_D   : LA_FCMPD<0x00, "caf">;
def FCMP_CLT_D   : LA_FCMPD<0x02, "clt">;
def FCMP_CEQ_D   : LA_FCMPD<0x04, "ceq">;
def FCMP_CLE_D   : LA_FCMPD<0x06, "cle">;
def FCMP_CUN_D   : LA_FCMPD<0x08, "cun">;
def FCMP_CULT_D  : LA_FCMPD<0x0A, "cult">;
def FCMP_CUEQ_D  : LA_FCMPD<0x0C, "cueq">;
def FCMP_CULE_D  : LA_FCMPD<0x0E, "cule">;
def FCMP_CNE_D   : LA_FCMPD<0x10, "cne">;
def FCMP_COR_D   : LA_FCMPD<0x14, "cor">;
def FCMP_CUNE_D  : LA_FCMPD<0x18, "cune">;

def FCMP_SAF_D   : LA_FCMPD<0x01, "saf">;
def FCMP_SLT_D   : LA_FCMPD<0x03, "slt">;
def FCMP_SEQ_D   : LA_FCMPD<0x05, "seq">;
def FCMP_SLE_D   : LA_FCMPD<0x07, "sle">;
def FCMP_SUN_D   : LA_FCMPD<0x09, "sun">;
def FCMP_SULT_D  : LA_FCMPD<0x0B, "sult">;
def FCMP_SUEQ_D  : LA_FCMPD<0x0D, "sueq">;
def FCMP_SULE_D  : LA_FCMPD<0x0F, "sule">;
def FCMP_SNE_D   : LA_FCMPD<0x11, "sne">;
def FCMP_SOR_D   : LA_FCMPD<0x15, "sor">;
def FCMP_SUNE_D  : LA_FCMPD<0x19, "sune">;

def FABS_D       : LA_FPUnaryOp_r<0b0100000010, FPR64, FPR64, "fabs.d">;
def FNEG_D       : LA_FPUnaryOp_r<0b0100000110, FPR64, FPR64, "fneg.d">;
def FLOGB_D      : LA_FPUnaryOp_r<0b0100001010, FPR64, FPR64, "flogb.d">;
def FCLASS_D     : LA_FPUnaryOp_r<0b0100001110, FPR64, FPR64, "fclass.d">;
def FSQRT_D      : LA_FPUnaryOp_r<0b0100010010, FPR64, FPR64, "fsqrt.d">;
def FRECIP_D     : LA_FPUnaryOp_r<0b0100010110, FPR64, FPR64, "frecip.d">;
def FRSQRT_D     : LA_FPUnaryOp_r<0b0100011010, FPR64, FPR64, "frsqrt.d">;
def FMOV_D       : LA_FPUnaryOp_r<0b0100100110, FPR64, FPR64, "fmov.d">;

def FCVT_S_D     : LA_FPUnaryOp_r<0b1001000110, FPR32, FPR64, "fcvt.s.d">;
def FCVT_D_S     : LA_FPUnaryOp_r<0b1001001001, FPR64, FPR32, "fcvt.d.s">;
def FTINTRM_W_D  : LA_FPUnaryOp_r<0b1010000010, FPR32, FPR64, "ftintrm.w.d">;
def FTINTRM_L_D  : LA_FPUnaryOp_r<0b1010001010, FPR64, FPR64, "ftintrm.l.d">;
def FTINTRP_W_D  : LA_FPUnaryOp_r<0b1010010010, FPR32, FPR64, "ftintrp.w.d">;
def FTINTRP_L_D  : LA_FPUnaryOp_r<0b1010011010, FPR64, FPR64, "ftintrp.l.d">;
def FTINTRZ_W_D  : LA_FPUnaryOp_r<0b1010100010, FPR32, FPR64, "ftintrz.w.d">;
def FTINTRZ_L_D  : LA_FPUnaryOp_r<0b1010101010, FPR64, FPR64, "ftintrz.l.d">;
def FTINTRNE_W_D : LA_FPUnaryOp_r<0b1010110010, FPR32, FPR64, "ftintrne.w.d">;
def FTINTRNE_L_D : LA_FPUnaryOp_r<0b1010111010, FPR64, FPR64, "ftintrne.l.d">;
def FTINT_W_D    : LA_FPUnaryOp_r<0b1011000010, FPR32, FPR64, "ftint.w.d">;
def FTINT_L_D    : LA_FPUnaryOp_r<0b1011001010, FPR64, FPR64, "ftint.l.d">;
def FFINT_D_W    : LA_FPUnaryOp_r<0b1101001000, FPR64, FPR32, "ffint.d.w">;
def FFINT_D_L    : LA_FPUnaryOp_r<0b1101001010, FPR64, FPR64, "ffint.d.l">;

let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in
def FLD_D : LAInst2RI12<0b001010, 0b1110,
                        (outs FPR64:$rd), (ins GPR:$rj, simm12:$imm12),
                        "fld.d", "$rd, $rj, $imm12">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in
def FST_D : LAInst2RI12<0b001010, 0b1111,
                        (outs), (ins FPR64:$rd, GPR:$rj, simm12:$imm12),
                        "fst.d", "$rd, $rj, $imm12">;

def FMADD_D      : LA_FPFMAD_rrr<0b0010, "fmadd.d">;
def FMSUB_D      : LA_FPFMAD_rrr<0b0110, "fmsub.d">;
def FNMADD_D     : LA_FPFMAD_rrr<0b1010, "fnmadd.d">;
def FNMSUB_D     : LA_FPFMAD_rrr<0b1110, "fnmsub.d">;

multiclass LAPatFpr64Fpr64Cmp<SDPatternOperator OpNode, LAInst Inst> {
  def : Pat<(XLenVT (OpNode FPR64:$fj, FPR64:$fk)),
            (ANDI   (MOVCF2GR (Inst $fj, $fk)), 1)>;
  def : Pat<(brcond (XLenVT (OpNode FPR64:$fj, FPR64:$fk)), bb:$imm),
            (BCNEZ  (Inst $fj, $fk), $imm)>;
  def : Pat<(select (XLenVT (OpNode FPR64:$fj, FPR64:$fk)),
                    FPR64:$tv, FPR64:$fv),
            (FSEL $tv, $fv, (Inst $fj, $fk))>;
  def : Pat<(select (XLenVT (OpNode FPR64:$fj, FPR64:$fk)),
                    FPR32:$tv, FPR32:$fv),
            (FSEL_S $tv, $fv, (Inst $fj, $fk))>;
}

defm : LAPatFpr64Fpr64Cmp<setoeq, FCMP_SEQ_D>;
defm : LAPatFpr64Fpr64Cmp<setolt, FCMP_SLT_D>;
defm : LAPatFpr64Fpr64Cmp<setole, FCMP_SLE_D>;
defm : LAPatFpr64Fpr64Cmp<setone, FCMP_SNE_D>;
defm : LAPatFpr64Fpr64Cmp<seto,   FCMP_SOR_D>;
defm : LAPatFpr64Fpr64Cmp<setuo,  FCMP_SUN_D>;
defm : LAPatFpr64Fpr64Cmp<setueq, FCMP_SUEQ_D>;
defm : LAPatFpr64Fpr64Cmp<setult, FCMP_SULT_D>;
defm : LAPatFpr64Fpr64Cmp<setule, FCMP_SULE_D>;
defm : LAPatFpr64Fpr64Cmp<setune, FCMP_SUNE_D>;
defm : LAPatFpr64Fpr64Cmp<seteq,  FCMP_SEQ_D>;
defm : LAPatFpr64Fpr64Cmp<setlt,  FCMP_SLT_D>;
defm : LAPatFpr64Fpr64Cmp<setle,  FCMP_SLE_D>;
defm : LAPatFpr64Fpr64Cmp<setne,  FCMP_SNE_D>;
} // Predicates = [HasStdExtD]

//===----------------------------------------------------------------------===//
// Pseudo-instructions and codegen patterns
//===----------------------------------------------------------------------===//

class LAPatFpr64Fpr64<SDPatternOperator OpNode, LAInst Inst>
    : Pat<(OpNode FPR64:$fj, FPR64:$fk), (Inst $fj, $fk)>;

class PatFpr64Fpr64<SDPatternOperator OpNode, RVInstR Inst>
    : Pat<(OpNode FPR64:$rs1, FPR64:$rs2), (Inst $rs1, $rs2)>;

class PatFpr64Fpr64DynFrm<SDPatternOperator OpNode, RVInstRFrm Inst>
    : Pat<(OpNode FPR64:$rs1, FPR64:$rs2), (Inst $rs1, $rs2, 0b111)>;

let Predicates = [HasStdExtD] in {

/// Float conversion operations

// f64 -> f32, f32 -> f64
def : Pat<(fpround FPR64:$rs1), (FCVT_S_D FPR64:$rs1)>;
def : Pat<(fpextend FPR32:$rs1), (FCVT_D_S FPR32:$rs1)>;

// [u]int<->double conversion patterns must be gated on IsLA32 or IsLA64, so
// are defined later.

/// Float arithmetic operations

def : LAPatFpr64Fpr64<fadd, FADD_D>;
def : LAPatFpr64Fpr64<fsub, FSUB_D>;
def : LAPatFpr64Fpr64<fmul, FMUL_D>;
def : LAPatFpr64Fpr64<fdiv, FDIV_D>;

def : Pat<(fsqrt FPR64:$rs1), (FSQRT_D FPR64:$rs1)>;
def : Pat<(fneg FPR64:$rs1), (FNEG_D $rs1)>;
def : Pat<(fabs FPR64:$rs1), (FABS_D $rs1)>;

def : LAPatFpr64Fpr64<fcopysign, FCOPYSIGN_D>;
def : Pat<(fcopysign FPR64:$rs1, FPR32:$rs2), (FCOPYSIGN_D $rs1, (FCVT_D_S $rs2))>;
def : Pat<(fcopysign FPR32:$rs1, FPR64:$rs2), (FCOPYSIGN_S $rs1, (FCVT_S_D $rs2))>;

// fmadd: rs1 * rs2 + rs3
def : Pat<(fma FPR64:$rs1, FPR64:$rs2, FPR64:$rs3),
          (FMADD_D $rs1, $rs2, $rs3)>;

// fmsub: rs1 * rs2 - rs3
def : Pat<(fma FPR64:$rs1, FPR64:$rs2, (fneg FPR64:$rs3)),
          (FMSUB_D FPR64:$rs1, FPR64:$rs2, FPR64:$rs3)>;

// fnmsub: -rs1 * rs2 + rs3
def : Pat<(fma (fneg FPR64:$rs1), FPR64:$rs2, FPR64:$rs3),
          (FNMSUB_D FPR64:$rs1, FPR64:$rs2, FPR64:$rs3)>;

// fnmadd: -rs1 * rs2 - rs3
def : Pat<(fma (fneg FPR64:$rs1), FPR64:$rs2, (fneg FPR64:$rs3)),
          (FNMADD_D FPR64:$rs1, FPR64:$rs2, FPR64:$rs3)>;

// The LoongArch ISA spec Vol. 1 (v1.00) defines fmin and fmax to match the
// behavior of IEEE 754-2008 maxNum/minNum operations. This doesn't match
// the LLVM behaviour (see https://bugs.llvm.org/show_bug.cgi?id=27363).
def : LAPatFpr64Fpr64<fminnum_ieee, FMIN_D>;
def : LAPatFpr64Fpr64<fmaxnum_ieee, FMAX_D>;
def : Pat<(fcanonicalize FPR64:$rs), (FMAX_D $rs, $rs)>;

/// Setcc

def : Pat<(select GPR:$cond, FPR64:$tv, FPR64:$fv),
          (FSEL $tv, $fv, (MOVGR2CF $cond))>;

/// Loads

defm : LALdPat12<load, FLD_D>;

/// Stores

defm : LAStPat12<store, FST_D, FPR64>;

/// Pseudo-instructions needed for the soft-float ABI with LA32D

// Moves two GPRs to an FPR.
let usesCustomInserter = 1 in
def BuildPairF64Pseudo
    : Pseudo<(outs FPR64:$dst), (ins GPR:$src1, GPR:$src2),
             [(set FPR64:$dst, (LoongArchBuildPairF64 GPR:$src1, GPR:$src2))]>;

// Moves an FPR to two GPRs.
let usesCustomInserter = 1 in
def SplitF64Pseudo
    : Pseudo<(outs GPR:$dst1, GPR:$dst2), (ins FPR64:$src),
             [(set GPR:$dst1, GPR:$dst2, (LoongArchSplitF64 FPR64:$src))]>;

} // Predicates = [HasStdExtD]

let Predicates = [HasStdExtD, IsLA32] in {
def : Pat<(f64 (fpimm0)), (FFINT_D_W (MOVGR2FR_W R0))>;
def : Pat<(fp_to_sint FPR64:$rs1), (MOVFR2GR_S (FTINTRZ_W_D FPR64:$rs1))>;
def : Pat<(sint_to_fp GPR:$rs1), (FFINT_D_W (MOVGR2FR_W $rs1))>;
} // Predicates = [HasStdExtD, IsLA32]

let Predicates = [HasStdExtD, IsLA64] in {
def : Pat<(f64 (fpimm0)), (MOVGR2FR_D R0)>;

def : Pat<(bitconvert GPR:$rs1), (MOVGR2FR_D GPR:$rs1)>;
def : Pat<(bitconvert FPR64:$rs1), (MOVFR2GR_D FPR64:$rs1)>;

def : Pat<(fp_to_sint FPR64:$rs1), (MOVFR2GR_D (FTINTRZ_L_D FPR64:$rs1))>;
def : Pat<(sint_to_fp GPR:$rs1), (FFINT_D_L (MOVGR2FR_D GPR:$rs1))>;
def : Pat<(sint_to_fp (sexti32 GPR:$rs1)), (FFINT_D_W (MOVGR2FR_W $rs1))>;
} // Predicates = [HasStdExtD, IsLA64]
