//===- LoongArchInstrInfoF.td - Target Description for LoongArch Target -*- tablegen -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the LoongArch implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//
// FP immediate patterns.
def fpimm0 : PatLeaf<(fpimm), [{
  return N->isExactlyValue(+0.0);
}]>;

def fpimm0neg : PatLeaf<(fpimm), [{
  return N->isExactlyValue(-0.0);
}]>;

def fpimm1 : PatLeaf<(fpimm), [{
  return N->isExactlyValue(+1.0);
}]>;

def IsNotSoftFloat   : Predicate<"!Subtarget->useSoftFloat()">,
                       AssemblerPredicate<(all_of FeatureSoftFloat)>;

class HARDFLOAT { list<Predicate> HardFloatPredicate = [IsNotSoftFloat]; }

def SDT_LoongArchTruncIntFP : SDTypeProfile<1, 1, [SDTCisFP<0>, SDTCisFP<1>]>;

def LoongArchTruncIntFP : SDNode<"LoongArchISD::TruncIntFP", SDT_LoongArchTruncIntFP>;

def SDT_LoongArchFPBrcond : SDTypeProfile<0, 3, [SDTCisInt<0>,
                                            SDTCisVT<1, i32>,
                                            SDTCisVT<2, OtherVT>]>;

def LoongArchFPBrcond : SDNode<"LoongArchISD::FPBrcond", SDT_LoongArchFPBrcond,
                          [SDNPHasChain, SDNPOptInGlue]>;

def SDT_LoongArchCMovFP : SDTypeProfile<1, 3, [SDTCisSameAs<0, 1>, SDTCisVT<2, i32>,
                                          SDTCisSameAs<1, 3>]>;

def LoongArchCMovFP_T : SDNode<"LoongArchISD::CMovFP_T", SDT_LoongArchCMovFP, [SDNPInGlue]>;

def LoongArchCMovFP_F : SDNode<"LoongArchISD::CMovFP_F", SDT_LoongArchCMovFP, [SDNPInGlue]>;

def SDT_LoongArchFPCmp : SDTypeProfile<0, 3, [SDTCisSameAs<0, 1>, SDTCisFP<1>,
                                         SDTCisVT<2, i32>]>;

def LoongArchFPCmp : SDNode<"LoongArchISD::FPCmp", SDT_LoongArchFPCmp, [SDNPOutGlue]>;

//===---------------------------------------------------------------------===/
//Instruction Class Templates
//===---------------------------------------------------------------------===/

class Float_MOVF<string opstr, RegisterOperand RO, RegisterOperand RC,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins RC:$fj),
          !strconcat(opstr, "\t$rd, $fj"),
          [(set RO:$rd, (OpNode RC:$fj))],
          FrmFR, opstr>, HARDFLOAT {
     let isMoveReg = 1;
}

class Float_MOVT<string opstr, RegisterOperand RO, RegisterOperand RC,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$fd), (ins RC:$rj),
          !strconcat(opstr, "\t$fd, $rj"),
          [(set RO:$fd, (OpNode RC:$rj))],
          FrmFR, opstr>, HARDFLOAT {
     let isMoveReg = 1;
}

class Float_CVT<string opstr, RegisterOperand RO, RegisterOperand RS,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$fd), (ins RS:$fj),
          !strconcat(opstr, "\t$fd, $fj"),
          [(set RO:$fd, (OpNode RS:$fj))],
          FrmFR, opstr>,
      HARDFLOAT {
    let hasSideEffects = 0;
}

/// float mov
class Gpr_2_Fcsr<string opstr, RegisterOperand RO,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs FCSROpnd:$fcsr), (ins RO:$rj),
               !strconcat(opstr, "\t$fcsr, $rj"),
               [(set FCSROpnd:$fcsr, (OpNode RO:$rj))],
               FrmR, opstr>;
class Fcsr_2_Gpr<string opstr, RegisterOperand RO,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins FCSROpnd:$fcsr),
               !strconcat(opstr, "\t$rd, $fcsr"),
               [(set RO:$rd, (OpNode FCSROpnd:$fcsr))],
               FrmR, opstr>;
class Fgr_2_Fcfr<string opstr, RegisterOperand RO,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs FCFROpnd:$cd), (ins RO:$fj),
                !strconcat(opstr, "\t$cd, $fj"),
                [(set FCFROpnd:$cd, (OpNode RO:$fj))],
                FrmR, opstr>;
class Fcfr_2_Fgr<string opstr, RegisterOperand RO,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$fd), (ins FCFROpnd:$cj),
               !strconcat(opstr, "\t$fd, $cj"),
               [(set RO:$fd, (OpNode FCFROpnd:$cj))],
               FrmR, opstr>;
class Gpr_2_Fcfr<string opstr, RegisterOperand RO,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs FCFROpnd:$cd), (ins RO:$rj),
                !strconcat(opstr, "\t$cd, $rj"),
                [(set FCFROpnd:$cd, (OpNode RO:$rj))],
                FrmR, opstr>;
class Fcfr_2_Gpr<string opstr, RegisterOperand RO,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins FCFROpnd:$cj),
               !strconcat(opstr, "\t$rd, $cj"),
               [(set RO:$rd, (OpNode FCFROpnd:$cj))],
               FrmR, opstr>;

class FLDX<string opstr, RegisterOperand DRC,
           SDPatternOperator OpNode = null_frag> :
  InstForm<(outs DRC:$fd), (ins PtrRC:$rj, PtrRC:$rk),
           !strconcat(opstr, "\t$fd, $rj, $rk"),
           [(set DRC:$fd, (OpNode (add iPTR:$rj, iPTR:$rk)))],
           FrmR, opstr> {
  let AddedComplexity = 20;
}

class FSTX<string opstr, RegisterOperand DRC,
           SDPatternOperator OpNode = null_frag> :
  InstForm<(outs), (ins DRC:$fd, PtrRC:$rj, PtrRC:$rk),
           !strconcat(opstr, "\t$fd, $rj, $rk"),
           [(OpNode DRC:$fd, (add iPTR:$rj, iPTR:$rk))],
           FrmR, opstr> {
  let AddedComplexity = 20;
}

/// f{maxa/mina}.{s/d}
class Float_Reg3_Fmaxa<string opstr, RegisterOperand RO,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$fd), (ins RO:$fj, RO:$fk),
          !strconcat(opstr, "\t$fd, $fj, $fk"),
          [(set RO:$fd, (OpNode (fabs RO:$fj), (fabs RO:$fk)))],
          FrmR, opstr>;
/// frecip
class Float_Reg2_Frecip<string opstr, RegisterOperand RO,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$fd), (ins RO:$fj),
          !strconcat(opstr, "\t$fd, $fj"),
          [(set RO:$fd, (OpNode fpimm1, RO:$fj))],
          FrmR, opstr>;
/// frsqrt
class Float_Reg2_Frsqrt<string opstr, RegisterOperand RO,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$fd), (ins RO:$fj),
          !strconcat(opstr, "\t$fd, $fj"),
          [(set RO:$fd, (OpNode fpimm1, (fsqrt RO:$fj)))],
          FrmR, opstr>;

class BceqzBr<string opstr, DAGOperand opnd,
              SDPatternOperator Op = null_frag> :
  InstForm<(outs), (ins FCFROpnd:$cj, opnd:$offset),
         !strconcat(opstr, "\t$cj, $offset"),
         [(LoongArchFPBrcond Op, FCFROpnd:$cj, bb:$offset)],
         FrmFI, opstr>, HARDFLOAT {
  let isBranch = 1;
  let isTerminator = 1;
  let hasFCCRegOperand = 1;
}

class FCMP_COND<string CondStr, string TypeStr, RegisterOperand RO,
                SDPatternOperator OpNode = null_frag>
    : InstForm<(outs FCFROpnd:$cd), (ins RO:$fj, RO:$fk),
               !strconcat("fcmp.", CondStr, ".", TypeStr, "\t$cd, $fj, $fk"),
               [(set FCFROpnd:$cd, (OpNode RO:$fj, RO:$fk))],
               FrmOther,
               !strconcat("fcmp.", CondStr, ".", TypeStr)> {
    bit isCTI = 1; // for what? from Mips32r6InstrInfo.td line 219
}

class FIELD_CMP_COND<bits<5> Val> {
  bits<5> Value = Val;
}
def FIELD_CMP_COND_CAF  : FIELD_CMP_COND<0x0>;
def FIELD_CMP_COND_CUN  : FIELD_CMP_COND<0x8>;
def FIELD_CMP_COND_CEQ  : FIELD_CMP_COND<0x4>;
def FIELD_CMP_COND_CUEQ : FIELD_CMP_COND<0xC>;
def FIELD_CMP_COND_CLT  : FIELD_CMP_COND<0x2>;
def FIELD_CMP_COND_CULT : FIELD_CMP_COND<0xA>;
def FIELD_CMP_COND_CLE  : FIELD_CMP_COND<0x6>;
def FIELD_CMP_COND_CULE : FIELD_CMP_COND<0xE>;
def FIELD_CMP_COND_CNE  : FIELD_CMP_COND<0x10>;
def FIELD_CMP_COND_COR  : FIELD_CMP_COND<0x14>;
def FIELD_CMP_COND_CUNE : FIELD_CMP_COND<0x18>;
def FIELD_CMP_COND_SAF  : FIELD_CMP_COND<0x1>;
def FIELD_CMP_COND_SUN  : FIELD_CMP_COND<0x9>;
def FIELD_CMP_COND_SEQ  : FIELD_CMP_COND<0x5>;
def FIELD_CMP_COND_SUEQ : FIELD_CMP_COND<0xD>;
def FIELD_CMP_COND_SLT  : FIELD_CMP_COND<0x3>;
def FIELD_CMP_COND_SULT : FIELD_CMP_COND<0xB>;
def FIELD_CMP_COND_SLE  : FIELD_CMP_COND<0x7>;
def FIELD_CMP_COND_SULE : FIELD_CMP_COND<0xF>;
def FIELD_CMP_COND_SNE  : FIELD_CMP_COND<0x11>;
def FIELD_CMP_COND_SOR  : FIELD_CMP_COND<0x15>;
def FIELD_CMP_COND_SUNE : FIELD_CMP_COND<0x19>;

multiclass FCMP_COND_M <bits<2> op, string TypeStr,
                        RegisterOperand RO> {
  def FCMP_CAF_#NAME  : FCMP_COND<"caf",  TypeStr, RO>,
                        R2_COND<op, FIELD_CMP_COND_CAF.Value>;
  def FCMP_CUN_#NAME  : FCMP_COND<"cun",  TypeStr, RO, setuo>,
                        R2_COND<op, FIELD_CMP_COND_CUN.Value>;
  def FCMP_CEQ_#NAME  : FCMP_COND<"ceq",  TypeStr, RO, setoeq>,
                        R2_COND<op, FIELD_CMP_COND_CEQ.Value>;
  def FCMP_CUEQ_#NAME : FCMP_COND<"cueq", TypeStr, RO, setueq>,
                        R2_COND<op, FIELD_CMP_COND_CUEQ.Value>;
  def FCMP_CLT_#NAME  : FCMP_COND<"clt",  TypeStr, RO, setolt>,
                        R2_COND<op, FIELD_CMP_COND_CLT.Value>;
  def FCMP_CULT_#NAME : FCMP_COND<"cult", TypeStr, RO, setult>,
                        R2_COND<op, FIELD_CMP_COND_CULT.Value>;
  def FCMP_CLE_#NAME  : FCMP_COND<"cle",  TypeStr, RO, setole>,
                        R2_COND<op, FIELD_CMP_COND_CLE.Value>;
  def FCMP_CULE_#NAME : FCMP_COND<"cule", TypeStr, RO, setule>,
                        R2_COND<op, FIELD_CMP_COND_CULE.Value>;
  def FCMP_CNE_#NAME  : FCMP_COND<"cne",  TypeStr, RO, setone>,
                        R2_COND<op, FIELD_CMP_COND_CNE.Value>;
  def FCMP_COR_#NAME  : FCMP_COND<"cor",  TypeStr, RO, seto>,
                        R2_COND<op, FIELD_CMP_COND_COR.Value>;
  def FCMP_CUNE_#NAME : FCMP_COND<"cune", TypeStr, RO, setune>,
                        R2_COND<op, FIELD_CMP_COND_CUNE.Value>;

  def FCMP_SAF_#NAME  : FCMP_COND<"saf",  TypeStr, RO>,
                        R2_COND<op, FIELD_CMP_COND_SAF.Value>;
  def FCMP_SUN_#NAME  : FCMP_COND<"sun",  TypeStr, RO>,
                        R2_COND<op, FIELD_CMP_COND_SUN.Value>;
  def FCMP_SEQ_#NAME  : FCMP_COND<"seq",  TypeStr, RO>,
                        R2_COND<op, FIELD_CMP_COND_SEQ.Value>;
  def FCMP_SUEQ_#NAME : FCMP_COND<"sueq", TypeStr, RO>,
                        R2_COND<op, FIELD_CMP_COND_SUEQ.Value>;
  def FCMP_SLT_#NAME  : FCMP_COND<"slt",  TypeStr, RO>,
                        R2_COND<op, FIELD_CMP_COND_SLT.Value>;
  def FCMP_SULT_#NAME : FCMP_COND<"sult", TypeStr, RO>,
                        R2_COND<op, FIELD_CMP_COND_SULT.Value>;
  def FCMP_SLE_#NAME  : FCMP_COND<"sle",  TypeStr, RO>,
                        R2_COND<op, FIELD_CMP_COND_SLE.Value>;
  def FCMP_SULE_#NAME : FCMP_COND<"sule", TypeStr, RO>,
                        R2_COND<op, FIELD_CMP_COND_SULE.Value>;
  def FCMP_SNE_#NAME  : FCMP_COND<"sne",  TypeStr, RO>,
                        R2_COND<op, FIELD_CMP_COND_SNE.Value>;
  def FCMP_SOR_#NAME  : FCMP_COND<"sor",  TypeStr, RO>,
                        R2_COND<op, FIELD_CMP_COND_SOR.Value>;
  def FCMP_SUNE_#NAME : FCMP_COND<"sune", TypeStr, RO>,
                        R2_COND<op, FIELD_CMP_COND_SUNE.Value>;
}

//// comparisons supported via another comparison
//multiclass FCmp_Pats<ValueType VT, Instruction NOROp, Register ZEROReg> {
//  def : LoongArchPat<(seteq VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("FCMP_CEQ_"#NAME) VT:$lhs, VT:$rhs)>;
//  def : LoongArchPat<(setgt VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("FCMP_CLE_"#NAME) VT:$rhs, VT:$lhs)>;
//  def : LoongArchPat<(setge VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("FCMP_CLT_"#NAME) VT:$rhs, VT:$lhs)>;
//  def : LoongArchPat<(setlt VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("FCMP_CLT_"#NAME) VT:$lhs, VT:$rhs)>;
//  def : LoongArchPat<(setle VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("FCMP_CLE_"#NAME) VT:$lhs, VT:$rhs)>;
//  def : LoongArchPat<(setne VT:$lhs, VT:$rhs),
//                     (NOROp
//                      (!cast<Instruction>("FCMP_CEQ_"#NAME) VT:$lhs, VT:$rhs),
//                      ZEROReg)>;
//}


///
/// R2
///
def FABS_S   : Float_Reg2<"fabs.s", FGR32Opnd, fabs>, R2F<0b0100000001>;
def FABS_D   : Float_Reg2<"fabs.d", FGR64Opnd, fabs>, R2F<0b0100000010>;
def FNEG_S   : Float_Reg2<"fneg.s", FGR32Opnd, fneg>, R2F<0b0100000101>;
def FNEG_D   : Float_Reg2<"fneg.d", FGR64Opnd, fneg>, R2F<0b0100000110>;
def FLOGB_S  : Float_Reg2<"flogb.s", FGR32Opnd>, R2F<0b0100001001>;
def FLOGB_D  : Float_Reg2<"flogb.d", FGR64Opnd>, R2F<0b0100001010>;
def FCLASS_S : Float_Reg2<"fclass.s", FGR32Opnd>, R2F<0b0100001101>;
def FCLASS_D : Float_Reg2<"fclass.d", FGR64Opnd>, R2F<0b0100001110>;
def FSQRT_S  : Float_Reg2<"fsqrt.s", FGR32Opnd, fsqrt>, R2F<0b0100010001>;
def FSQRT_D  : Float_Reg2<"fsqrt.d", FGR64Opnd, fsqrt>, R2F<0b0100010010>;
def FRECIP_S : Float_Reg2_Frecip<"frecip.s", FGR32Opnd, fdiv>, R2F<0b0100010101>;
def FRECIP_D : Float_Reg2_Frecip<"frecip.d", FGR64Opnd, fdiv>, R2F<0b0100010110>;
def FRSQRT_S : Float_Reg2_Frsqrt<"frsqrt.s", FGR32Opnd, fdiv>, R2F<0b0100011001>;
def FRSQRT_D : Float_Reg2_Frsqrt<"frsqrt.d", FGR64Opnd, fdiv>, R2F<0b0100011010>;
def FMOV_S   : Float_Reg2<"fmov.s", FGR32Opnd>, R2F<0b0100100101>;
def FMOV_D   : Float_Reg2<"fmov.d", FGR64Opnd>, R2F<0b0100100110>;

def MOVGR2FR_W  : Float_MOVT<"movgr2fr.w", FGR32Opnd, GPR32Opnd, bitconvert>, MOVFI<0b0100101001>;
def MOVGR2FR_D  : Float_MOVT<"movgr2fr.d", FGR64Opnd, GPR64Opnd, bitconvert>, MOVFI<0b0100101010>;
def MOVGR2FRH_W : Float_MOVT<"movgr2frh.w", FGR64Opnd, GPR32Opnd>, MOVFI<0b0100101011>; //not realize
def MOVFR2GR_S  : Float_MOVF<"movfr2gr.s", GPR32Opnd, FGR32Opnd, bitconvert>, MOVIF<0b0100101101>;
def MOVFR2GR_D  : Float_MOVF<"movfr2gr.d", GPR64Opnd, FGR64Opnd, bitconvert>, MOVIF<0b0100101110>;
def MOVFRH2GR_S : Float_MOVF<"movfrh2gr.s", GPR32Opnd, FGR32Opnd>, MOVIF<0b0100101111>; //not realize

let isCodeGenOnly = 1 in {
  def MOVGR2FR_DW : Float_MOVT<"movgr2fr.w", FGR64Opnd, GPR32Opnd>, MOVFI<0b0100101001>;
  def MOVFR2GR_WD : Float_MOVF<"movfr2gr.s", GPR32Opnd, FGR64Opnd>, MOVIF<0b0100101101>;
}

def FCVT_S_D : Float_CVT<"fcvt.s.d", FGR32Opnd, FGR64Opnd>, R2F<0b1001000110>;
def FCVT_D_S : Float_CVT<"fcvt.d.s", FGR64Opnd, FGR32Opnd>, R2F<0b1001001001>;

def FTINTRM_W_S  : Float_Reg2<"ftintrm.w.s", FGR32Opnd>, R2F<0b1010000001>;
def FTINTRM_W_D  : Float_Reg2<"ftintrm.w.d", FGR64Opnd>, R2F<0b1010000010>;
def FTINTRM_L_S  : Float_Reg2<"ftintrm.l.s", FGR32Opnd>, R2F<0b1010001001>;
def FTINTRM_L_D  : Float_Reg2<"ftintrm.l.d", FGR64Opnd>, R2F<0b1010001010>;
def FTINTRP_W_S  : Float_Reg2<"ftintrp.w.s", FGR32Opnd>, R2F<0b1010010001>;
def FTINTRP_W_D  : Float_Reg2<"ftintrp.w.d", FGR64Opnd>, R2F<0b1010010010>;
def FTINTRP_L_S  : Float_Reg2<"ftintrp.l.s", FGR32Opnd>, R2F<0b1010011001>;
def FTINTRP_L_D  : Float_Reg2<"ftintrp.l.d", FGR64Opnd>, R2F<0b1010011010>;
def FTINTRZ_W_S  : Float_Reg2<"ftintrz.w.s", FGR32Opnd>, R2F<0b1010100001>;
def FTINTRZ_L_D  : Float_Reg2<"ftintrz.l.d", FGR64Opnd>, R2F<0b1010101010>;
def FTINTRNE_W_S : Float_Reg2<"ftintrne.w.s", FGR32Opnd>, R2F<0b1010110001>;
def FTINTRNE_W_D : Float_Reg2<"ftintrne.w.d", FGR64Opnd>, R2F<0b1010110010>;
def FTINTRNE_L_S : Float_Reg2<"ftintrne.l.s", FGR32Opnd>, R2F<0b1010111001>;
def FTINTRNE_L_D : Float_Reg2<"ftintrne.l.d", FGR64Opnd>, R2F<0b1010111010>;

def FTINT_W_S    : Float_CVT<"ftint.w.s", FGR32Opnd, FGR32Opnd>, R2F<0b1011000001>;
def FTINT_W_D    : Float_CVT<"ftint.w.d", FGR32Opnd, FGR64Opnd>, R2F<0b1011000010>;
def FTINT_L_S    : Float_CVT<"ftint.l.s", FGR64Opnd, FGR32Opnd>, R2F<0b1011001001>;
def FTINT_L_D    : Float_CVT<"ftint.l.d", FGR64Opnd, FGR64Opnd>, R2F<0b1011001010>;
def FFINT_S_W    : Float_CVT<"ffint.s.w", FGR32Opnd, FGR32Opnd>, R2F<0b1101000100>;
def FFINT_S_L    : Float_CVT<"ffint.s.l", FGR32Opnd, FGR64Opnd>, R2F<0b1101000110>;
def FFINT_D_W    : Float_CVT<"ffint.d.w", FGR64Opnd, FGR32Opnd>, R2F<0b1101001000>;
def FFINT_D_L    : Float_CVT<"ffint.d.l", FGR64Opnd, FGR64Opnd>, R2F<0b1101001010>;

def FRINT_S      : Float_Reg2<"frint.s", FGR32Opnd, frint>, R2F<0b1110010001>;
def FRINT_D      : Float_Reg2<"frint.d", FGR64Opnd, frint>, R2F<0b1110010010>;

///
/// R3
///
def FADD_S      : Float_Reg3<"fadd.s", FGR32Opnd, fadd>, R3F<0b000001>;
def FADD_D      : Float_Reg3<"fadd.d", FGR64Opnd, fadd>, R3F<0b000010>;
def FSUB_S      : Float_Reg3<"fsub.s", FGR32Opnd, fsub>, R3F<0b000101>;
def FSUB_D      : Float_Reg3<"fsub.d", FGR64Opnd, fsub>, R3F<0b000110>;
def FMUL_S      : Float_Reg3<"fmul.s", FGR32Opnd, fmul>, R3F<0b001001>;
def FMUL_D      : Float_Reg3<"fmul.d", FGR64Opnd, fmul>, R3F<0b001010>;
def FDIV_S      : Float_Reg3<"fdiv.s", FGR32Opnd, fdiv>, R3F<0b001101>;
def FDIV_D      : Float_Reg3<"fdiv.d", FGR64Opnd, fdiv>, R3F<0b001110>;
def FMAX_S      : Float_Reg3<"fmax.s", FGR32Opnd, fmaximum>, R3F<0b010001>;
def FMAX_D      : Float_Reg3<"fmax.d", FGR64Opnd, fmaximum>, R3F<0b010010>;
def FMIN_S      : Float_Reg3<"fmin.s", FGR32Opnd, fminimum>, R3F<0b010101>;
def FMIN_D      : Float_Reg3<"fmin.d", FGR64Opnd, fminimum>, R3F<0b010110>;
def FMAXA_S     : Float_Reg3_Fmaxa<"fmaxa.s", FGR32Opnd, fmaximum>, R3F<0b011001>;
def FMAXA_D     : Float_Reg3_Fmaxa<"fmaxa.d", FGR64Opnd, fmaximum>, R3F<0b011010>;
def FMINA_S     : Float_Reg3_Fmaxa<"fmina.s", FGR32Opnd, fminimum>, R3F<0b011101>;
def FMINA_D     : Float_Reg3_Fmaxa<"fmina.d", FGR64Opnd, fminimum>, R3F<0b011110>;
def FSCALEB_S   : Float_Reg3<"fscaleb.s", FGR32Opnd>, R3F<0b100001>;
def FSCALEB_D   : Float_Reg3<"fscaleb.d", FGR64Opnd>, R3F<0b100010>;
def FCOPYSIGN_S : Float_Reg3<"fcopysign.s", FGR32Opnd, fcopysign>, R3F<0b100101>;
def FCOPYSIGN_D : Float_Reg3<"fcopysign.d", FGR64Opnd, fcopysign>, R3F<0b100110>;
///
/// R4_IMM21
///
def FMADD_S  : Mul_Reg4<"fmadd.s", FGR32Opnd>, R4MUL<0b0001>;
def FMADD_D  : Mul_Reg4<"fmadd.d", FGR64Opnd>, R4MUL<0b0010>;
def FMSUB_S  : Mul_Reg4<"fmsub.s", FGR32Opnd>, R4MUL<0b0101>;
def FMSUB_D  : Mul_Reg4<"fmsub.d", FGR64Opnd>, R4MUL<0b0110>;
def FNMADD_S : NMul_Reg4<"fnmadd.s", FGR32Opnd>, R4MUL<0b1001>;
def FNMADD_D : NMul_Reg4<"fnmadd.d", FGR64Opnd>, R4MUL<0b1010>;
def FNMSUB_S : NMul_Reg4<"fnmsub.s", FGR32Opnd>, R4MUL<0b1101>;
def FNMSUB_D : NMul_Reg4<"fnmsub.d", FGR64Opnd>, R4MUL<0b1110>;


// fmadd: fj * fk + fa
def : LoongArchPat<(fma FGR64Opnd:$fj, FGR64Opnd:$fk, FGR64Opnd:$fa),
                   (FMADD_D $fj, $fk, $fa)>;

def : LoongArchPat<(fma FGR32Opnd:$fj, FGR32Opnd:$fk, FGR32Opnd:$fa),
                   (FMADD_S $fj, $fk, $fa)>;


// fmsub: fj * fk - fa
def : LoongArchPat<(fma FGR64Opnd:$fj, FGR64Opnd:$fk, (fneg FGR64Opnd:$fa)),
                   (FMSUB_D FGR64Opnd:$fj, FGR64Opnd:$fk, FGR64Opnd:$fa)>;

def : LoongArchPat<(fma FGR32Opnd:$fj, FGR32Opnd:$fk, (fneg FGR32Opnd:$fa)),
                   (FMSUB_S FGR32Opnd:$fj, FGR32Opnd:$fk, FGR32Opnd:$fa)>;


// fnmadd: -(fj * fk + fa)
def : LoongArchPat<(fma (fneg FGR64Opnd:$fj), FGR64Opnd:$fk, (fneg FGR64Opnd:$fa)),
                   (FNMADD_D FGR64Opnd:$fj, FGR64Opnd:$fk, FGR64Opnd:$fa)>;

def : LoongArchPat<(fma (fneg FGR32Opnd:$fj), FGR32Opnd:$fk, (fneg FGR32Opnd:$fa)),
                   (FNMADD_S FGR32Opnd:$fj, FGR32Opnd:$fk, FGR32Opnd:$fa)>;

// fnmsub: -(fj * fk - fa)
def : LoongArchPat<(fma (fneg FGR64Opnd:$fj), FGR64Opnd:$fk, FGR64Opnd:$fa),
                   (FNMSUB_D FGR64Opnd:$fj, FGR64Opnd:$fk, FGR64Opnd:$fa)>;

def : LoongArchPat<(fma (fneg FGR32Opnd:$fj), FGR32Opnd:$fk, FGR32Opnd:$fa),
                   (FNMSUB_S FGR32Opnd:$fj, FGR32Opnd:$fk, FGR32Opnd:$fa)>;

let Pattern = []<dag> in {
defm S : FCMP_COND_M<0b01, "s", FGR32Opnd>;
defm D : FCMP_COND_M<0b10, "d", FGR64Opnd>;
}
//
//defm S : FCmp_Pats<f32, NOR32, ZERO>;
//defm D : FCmp_Pats<f64, NOR32, ZERO>;

///
/// Float point branching
///
def LoongArch_BRANCH_F  : PatLeaf<(i32 0)>;
def LoongArch_BRANCH_T  : PatLeaf<(i32 1)>;

def BCEQZ : BceqzBr<"bceqz", brtarget, LoongArch_BRANCH_F>, R1_BCEQZ<0>;
def BCNEZ : BceqzBr<"bcnez", brtarget, LoongArch_BRANCH_T>, R1_BCEQZ<1>;

///
/// FMOV
///
def MOVGR2FCSR : Gpr_2_Fcsr<"movgr2fcsr", GPR64Opnd>, MOVGPR2FCSR;
def MOVFCSR2GR : Fcsr_2_Gpr<"movfcsr2gr", GPR64Opnd>, MOVFCSR2GPR;
def MOVFR2CF   : Fgr_2_Fcfr<"movfr2cf", FGR64Opnd>,   MOVFGR2FCFR;
def MOVCF2FR   : Fcfr_2_Fgr<"movcf2fr", FGR64Opnd>,   MOVFCFR2FGR;
def MOVGR2CF   : Gpr_2_Fcfr<"movgr2cf", GPR64Opnd>,   MOVGPR2FCFR;
def MOVCF2GR   : Fcfr_2_Gpr<"movcf2gr", GPR64Opnd>,   MOVFCFR2GPR;

let isCodeGenOnly = 1 in {
  def MOVFR2CF32   : Fgr_2_Fcfr<"movfr2cf", FGR32Opnd>,   MOVFGR2FCFR;
  def MOVCF2FR32   : Fcfr_2_Fgr<"movcf2fr", FGR32Opnd>,   MOVFCFR2FGR;
  def MOVGR2CF32   : Gpr_2_Fcfr<"movgr2cf", GPR32Opnd>,   MOVGPR2FCFR;
  def MOVCF2GR32   : Fcfr_2_Gpr<"movcf2gr", GPR32Opnd>,   MOVFCFR2GPR;
}

class Sel_Reg4<string opstr, RegisterOperand RO,
               SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$fd), (ins FCFROpnd:$ca, RO:$fj, RO:$fk),
          !strconcat(opstr, "\t$fd, $fj, $fk, $ca"),
//          [(set RO:$fd, (select FCFROpnd:$ca, RO:$fk, RO:$fj))],
          [],
          FrmR, opstr>{
       let Defs = [FCC0, FCC1, FCC2, FCC3, FCC4, FCC5, FCC6];
       let hasFCCRegOperand = 1;
 }

def FSEL_T_S : Sel_Reg4<"fsel", FGR32Opnd>, R4SEL;
let isCodeGenOnly = 1 in {
  def FSEL_T_D : Sel_Reg4<"fsel", FGR64Opnd>, R4SEL;
}

///
/// Mem access
///
def FLD_S : FLd<"fld.s", FGR32Opnd, mem, load>, LOAD_STORE<0b1100>;
def FST_S : FSt<"fst.s", FGR32Opnd, mem, store>, LOAD_STORE<0b1101>;
def FLD_D : FLd<"fld.d", FGR64Opnd, mem, load>, LOAD_STORE<0b1110>;
def FST_D : FSt<"fst.d", FGR64Opnd, mem, store>, LOAD_STORE<0b1111>;

def FLDX_S  : FLDX<"fldx.s", FGR32Opnd, load>, R3MF<0b01100000>;
def FLDX_D  : FLDX<"fldx.d", FGR64Opnd, load>, R3MF<0b01101000>;
def FSTX_S  : FSTX<"fstx.s", FGR32Opnd, store>, R3MF<0b01110000>;
def FSTX_D  : FSTX<"fstx.d", FGR64Opnd, store>, R3MF<0b01111000>;

def FLDGT_S : Float_Int_Reg3<"fldgt.s", FGR32Opnd, GPR64Opnd>, R3MF<0b11101000>;
def FLDGT_D : Float_Int_Reg3<"fldgt.d", FGR64Opnd, GPR64Opnd>, R3MF<0b11101001>;
def FLDLE_S : Float_Int_Reg3<"fldle.s", FGR32Opnd, GPR64Opnd>, R3MF<0b11101010>;
def FLDLE_D : Float_Int_Reg3<"fldle.d", FGR64Opnd, GPR64Opnd>, R3MF<0b11101011>;
def FSTGT_S : Float_Int_Reg3<"fstgt.s", FGR32Opnd, GPR64Opnd>, R3MF<0b11101100>;
def FSTGT_D : Float_Int_Reg3<"fstgt.d", FGR64Opnd, GPR64Opnd>, R3MF<0b11101101>;
def FSTLE_S : Float_Int_Reg3<"fstle.s", FGR32Opnd, GPR64Opnd>, R3MF<0b11101110>;
def FSTLE_D : Float_Int_Reg3<"fstle.d", FGR64Opnd, GPR64Opnd>, R3MF<0b11101111>;

let isPseudo = 1, isCodeGenOnly = 1 in {
  def PseudoFFINT_S_W : Float_CVT<"", FGR32Opnd, GPR32Opnd>;
  def PseudoFFINT_D_W : Float_CVT<"", FGR64Opnd, GPR32Opnd>;
  def PseudoFFINT_S_L : Float_CVT<"", FGR64Opnd, GPR64Opnd>;
  def PseudoFFINT_D_L : Float_CVT<"", FGR64Opnd, GPR64Opnd>;
}

def : LoongArchPat<(f32 (fpround FGR64Opnd:$src)),
                   (FCVT_S_D FGR64Opnd:$src)>;
def : LoongArchPat<(f64 (fpextend FGR32Opnd:$src)),
                   (FCVT_D_S FGR32Opnd:$src)>;

def : LoongArchPat<(f32 (sint_to_fp GPR32Opnd:$src)),
                   (PseudoFFINT_S_W GPR32Opnd:$src)>;
def : LoongArchPat<(f64 (sint_to_fp GPR32Opnd:$src)),
                   (PseudoFFINT_D_W GPR32Opnd:$src)>;
def : LoongArchPat<(f32 (sint_to_fp GPR64Opnd:$src)),
                   (EXTRACT_SUBREG (PseudoFFINT_S_L GPR64Opnd:$src), sub_lo)>;
def : LoongArchPat<(f64 (sint_to_fp GPR64Opnd:$src)),
                   (PseudoFFINT_D_L GPR64Opnd:$src)>;

def : LoongArchPat<(f32 fpimm0), (MOVGR2FR_W ZERO)>;
def : LoongArchPat<(f32 fpimm0neg), (FNEG_S (MOVGR2FR_W ZERO))>;
def : LoongArchPat<(f32 fpimm1), (FFINT_S_W (MOVGR2FR_W (ADDI_W ZERO, 1)))>;
def : LoongArchPat<(f64 fpimm1), (FFINT_D_L (MOVGR2FR_D (ADDI_D ZERO_64, 1)))>;

// Patterns for loads/stores with a reg+imm operand.
let AddedComplexity = 40 in {
  def : LoadRegImmPat<FLD_S, f32, load>;
  def : StoreRegImmPat<FST_S, f32, store>;
  def : LoadRegImmPat<FLD_D, f64, load>;
  def : StoreRegImmPat<FST_D, f64, store>;
}

def : LoongArchPat<(LoongArchTruncIntFP FGR32Opnd:$src),
                   (FTINTRZ_W_S FGR32Opnd:$src)>;

def : LoongArchPat<(LoongArchTruncIntFP FGR64Opnd:$src),
                   (FTINTRZ_L_D FGR64Opnd:$src)>;

def : LoongArchPat<(LoongArchTruncIntFP FGR32Opnd:$src),
                   (FCVT_D_S (FTINTRZ_W_S FGR32Opnd:$src))>;

def : LoongArchPat<(f32 (fcopysign FGR32Opnd:$lhs, FGR64Opnd:$rhs)),
                   (FCOPYSIGN_S FGR32Opnd:$lhs, (FCVT_S_D FGR64Opnd:$rhs))>;
def : LoongArchPat<(f64 (fcopysign FGR64Opnd:$lhs, FGR32Opnd:$rhs)),
                   (FCOPYSIGN_D FGR64Opnd:$lhs, (FCVT_D_S FGR32Opnd:$rhs))>;

let PrintMethod = "printFCCOperand",EncoderMethod = "getFCMPEncoding" in
  def condcode : Operand<i32>;

class CEQS_FT<string typestr, RegisterClass RC,
              SDPatternOperator OpNode = null_frag>  :
  InstForm<(outs), (ins RC:$fj, RC:$fk, condcode:$cond),
         !strconcat("fcmp.$cond.", typestr, "\t$$fcc0, $fj, $fk"),
         [(OpNode RC:$fj, RC:$fk, imm:$cond)], FrmFR,
         !strconcat("fcmp.$cond.", typestr)>, HARDFLOAT {
  let Defs = [FCC0, FCC1, FCC2, FCC3, FCC4, FCC5, FCC6, FCC7];
  let isCodeGenOnly = 1;
  let hasFCCRegOperand = 1;
}

def FCMP_S32 : CEQS_FT<"s", FGR32, LoongArchFPCmp>, CEQS_FM<0b01> {
      bits<3> cd = 0;
}
def FCMP_D64 : CEQS_FT<"d", FGR64, LoongArchFPCmp>, CEQS_FM<0b10>{
      bits<3> cd = 0;
}


//multiclass FCmp_Pats2<ValueType VT, Instruction NOROp, Register ZEROReg> {
//  def : LoongArchPat<(seteq VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("SFCMP_CEQ_"#NAME) VT:$lhs, VT:$rhs)>;
//  def : LoongArchPat<(setgt VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("SFCMP_CLE_"#NAME) VT:$rhs, VT:$lhs)>;
//  def : LoongArchPat<(setge VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("SFCMP_CLT_"#NAME) VT:$rhs, VT:$lhs)>;
//  def : LoongArchPat<(setlt VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("SFCMP_CLT_"#NAME) VT:$lhs, VT:$rhs)>;
//  def : LoongArchPat<(setle VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("SFCMP_CLE_"#NAME) VT:$lhs, VT:$rhs)>;
//  def : LoongArchPat<(setne VT:$lhs, VT:$rhs),
//                     (NOROp
//                      (!cast<Instruction>("SFCMP_CEQ_"#NAME) VT:$lhs, VT:$rhs),
//                      ZEROReg)>;
//
//  def : LoongArchPat<(seteq VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("DFCMP_CEQ_"#NAME) VT:$lhs, VT:$rhs)>;
//  def : LoongArchPat<(setgt VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("DFCMP_CLE_"#NAME) VT:$rhs, VT:$lhs)>;
//  def : LoongArchPat<(setge VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("DFCMP_CLT_"#NAME) VT:$rhs, VT:$lhs)>;
//  def : LoongArchPat<(setlt VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("DFCMP_CLT_"#NAME) VT:$lhs, VT:$rhs)>;
//  def : LoongArchPat<(setle VT:$lhs, VT:$rhs),
//                     (!cast<Instruction>("DFCMP_CLE_"#NAME) VT:$lhs, VT:$rhs)>;
//  def : LoongArchPat<(setne VT:$lhs, VT:$rhs),
//                     (NOROp
//                      (!cast<Instruction>("DFCMP_CEQ_"#NAME) VT:$lhs, VT:$rhs),
//                      ZEROReg)>;
// }
//
//defm S : FCmp_Pats2<f32, NOR32, ZERO>;
//defm D : FCmp_Pats2<f64, NOR32, ZERO>;

let usesCustomInserter = 1 in {
  class Select_Pseudo<RegisterOperand RC> :
    LoongArchPseudo<(outs RC:$dst), (ins GPR32Opnd:$cond, RC:$T, RC:$F),
            [(set RC:$dst, (select GPR32Opnd:$cond, RC:$T, RC:$F))]>;

  class SelectFP_Pseudo_T<RegisterOperand RC> :
    LoongArchPseudo<(outs RC:$dst), (ins FCFROpnd:$cond, RC:$T, RC:$F),
             [(set RC:$dst, (LoongArchCMovFP_T RC:$T, FCFROpnd:$cond, RC:$F))]>;

  class SelectFP_Pseudo_F<RegisterOperand RC> :
    LoongArchPseudo<(outs RC:$dst), (ins FCFROpnd:$cond, RC:$T, RC:$F),
             [(set RC:$dst, (LoongArchCMovFP_F RC:$T, FCFROpnd:$cond, RC:$F))]>;
}

def PseudoSELECT_I : Select_Pseudo<GPR32Opnd>;
def PseudoSELECT_I64 : Select_Pseudo<GPR64Opnd>;
def PseudoSELECT_S : Select_Pseudo<FGR32Opnd>;
def PseudoSELECT_D64 : Select_Pseudo<FGR64Opnd>;

def PseudoSELECTFP_T_I : SelectFP_Pseudo_T<GPR32Opnd>;
def PseudoSELECTFP_T_I64 : SelectFP_Pseudo_T<GPR64Opnd>;
def PseudoSELECTFP_T_S : SelectFP_Pseudo_T<FGR32Opnd>;
def PseudoSELECTFP_T_D64 : SelectFP_Pseudo_T<FGR64Opnd>;

def PseudoSELECTFP_F_I : SelectFP_Pseudo_F<GPR32Opnd>;
def PseudoSELECTFP_F_I64 : SelectFP_Pseudo_F<GPR64Opnd>;
def PseudoSELECTFP_F_S : SelectFP_Pseudo_F<FGR32Opnd>;
def PseudoSELECTFP_F_D64 : SelectFP_Pseudo_F<FGR64Opnd>;

class ABSS_FT<string opstr, RegisterOperand DstRC, RegisterOperand SrcRC,
              SDPatternOperator OpNode= null_frag> :
  InstForm<(outs DstRC:$fd), (ins SrcRC:$fj), !strconcat(opstr, "\t$fd, $fj"),
         [(set DstRC:$fd, (OpNode SrcRC:$fj))], FrmFR, opstr>;

def TRUNC_W_D : ABSS_FT<"ftintrz.w.d", FGR32Opnd, FGR64Opnd>, R2F<0b1010100010>;

def FTINTRZ_L_S  : ABSS_FT<"ftintrz.l.s", FGR64Opnd, FGR32Opnd>, R2F<0b1010101001>;

def : LoongArchPat<(LoongArchTruncIntFP FGR64Opnd:$src),
              (TRUNC_W_D FGR64Opnd:$src)>;

def : LoongArchPat<(LoongArchTruncIntFP FGR32Opnd:$src),
              (FTINTRZ_L_S FGR32Opnd:$src)>;
