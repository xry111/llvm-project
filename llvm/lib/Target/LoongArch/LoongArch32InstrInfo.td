//===- LoongArch32InstrInfo.td - Target Description for LoongArch Target -*- tablegen -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes LoongArch32 instructions.
//
//===----------------------------------------------------------------------===//

//===---------------------------------------------------------------------===/
// Instruction Definitions.
//===---------------------------------------------------------------------===/

let DecoderNamespace = "LoongArch32" in {
  ///
  /// R2
  ///
  def CLO_W : Count1<"clo.w", GPR32Opnd, ctlz>, R2I<0b00100>;
  def CLZ_W : Int_Reg2<"clz.w", GPR32Opnd, ctlz>, R2I<0b00101>;
  def CTO_W : Count1<"cto.w", GPR32Opnd, cttz>, R2I<0b00110>;
  def CTZ_W : Int_Reg2<"ctz.w", GPR32Opnd, cttz>, R2I<0b00111>;

  def REVB_2H : Int_Reg2<"revb.2h", GPR32Opnd>, R2I<0b01100>;//see below bswap pattern

  def BITREV_4B : Int_Reg2<"bitrev.4b", GPR32Opnd>, R2I<0b10010>;
  def BITREV_W  : Int_Reg2<"bitrev.w", GPR32Opnd, bitreverse>, R2I<0b10100>;

  let isCodeGenOnly = 1 in {
    def EXT_W_H32 : SignExtInReg<"ext.w.h", GPR32Opnd, i16>, R2I<0b10110>;
    def EXT_W_B32 : SignExtInReg<"ext.w.b", GPR32Opnd, i8>, R2I<0b10111>;

  }

  def CPUCFG    : Int_Reg2<"cpucfg", GPR32Opnd, int_loongarch_cpucfg>, R2I<0b11011>;
  def RDTIMEL_W32 : Int_Reg2_Rdtime<"rdtimel.w", GPR32Opnd>, R2I<0b11000>;
  def RDTIMEH_W32 : Int_Reg2_Rdtime<"rdtimeh.w", GPR32Opnd>, R2I<0b11001>;

  ///
  /// R3
  ///
  def ADD_W : Int_Reg3<"add.w", GPR32Opnd, add>, R3I<0b0100000>;
  def SUB_W : Int_Reg3<"sub.w", GPR32Opnd, sub>, R3I<0b0100010>;

  let isCodeGenOnly = 1 in {
    def SLT32     : SetCC_R<"slt", GPR32Opnd, setlt>, R3I<0b0100100>;
    def SLTU32    : SetCC_R<"sltu", GPR32Opnd, setult>, R3I<0b0100101>;
    def MASKEQZ32 : Int_Reg3<"maskeqz", GPR32Opnd>, R3I<0b0100110>;//see below patterns
    def MASKNEZ32 : Int_Reg3<"masknez", GPR32Opnd>, R3I<0b0100111>;//see below patterns

    def NOR32   : Nor<"nor", GPR32Opnd>, R3I<0b0101000>;
    def AND32   : Int_Reg3<"and", GPR32Opnd, and>, R3I<0b0101001>;
    def OR32    : Int_Reg3<"or", GPR32Opnd, or>, R3I<0b0101010>;
    def XOR32   : Int_Reg3<"xor", GPR32Opnd, xor>, R3I<0b0101011>;
  }

  def SLL_W : Shift_Var<"sll.w", GPR32Opnd, shl>, R3I<0b0101110>;
  def SRL_W : Shift_Var<"srl.w", GPR32Opnd, srl>, R3I<0b0101111>;
  def SRA_W : Shift_Var<"sra.w", GPR32Opnd, sra>, R3I<0b0110000>;
  def ROTR_W: Shift_Var<"rotr.w", GPR32Opnd, rotr>, R3I<0b0110110>;

  def MUL_W     : Int_Reg3<"mul.w", GPR32Opnd, mul>, R3I<0b0111000>;
  def MULH_W    : Int_Reg3<"mulh.w", GPR32Opnd, mulhs>, R3I<0b0111001>;
  def MULH_WU   : Int_Reg3<"mulh.wu", GPR32Opnd, mulhu>, R3I<0b0111010>;

let usesCustomInserter = 1 in {
  def DIV_W  : Int_Reg3<"div.w", GPR32Opnd, sdiv>, R3I<0b1000000>;
  def MOD_W  : Int_Reg3<"mod.w", GPR32Opnd, srem>, R3I<0b1000001>;
  def DIV_WU : Int_Reg3<"div.wu", GPR32Opnd, udiv>, R3I<0b1000010>;
  def MOD_WU : Int_Reg3<"mod.wu", GPR32Opnd, urem>, R3I<0b1000011>;
}

  def CRC_W_B_W  : Int_Reg3<"crc.w.b.w", GPR32Opnd, int_loongarch_crc_w_b_w>, R3I<0b1001000>;
  def CRC_W_H_W  : Int_Reg3<"crc.w.h.w", GPR32Opnd, int_loongarch_crc_w_h_w>, R3I<0b1001001>;
  def CRC_W_W_W  : Int_Reg3<"crc.w.w.w", GPR32Opnd, int_loongarch_crc_w_w_w>, R3I<0b1001010>;
  def CRCC_W_B_W : Int_Reg3<"crcc.w.b.w", GPR32Opnd, int_loongarch_crcc_w_b_w>, R3I<0b1001100>;
  def CRCC_W_H_W : Int_Reg3<"crcc.w.h.w", GPR32Opnd, int_loongarch_crcc_w_h_w>, R3I<0b1001101>;
  def CRCC_W_W_W : Int_Reg3<"crcc.w.w.w", GPR32Opnd, int_loongarch_crcc_w_w_w>, R3I<0b1001110>;
  ///
  /// SLLI
  ///
  def SLLI_W  : Shift_Imm32<"slli.w", GPR32Opnd, shl>, R2_IMM5<0b00>;
  def SRLI_W  : Shift_Imm32<"srli.w", GPR32Opnd, srl>, R2_IMM5<0b01>;
  def SRAI_W  : Shift_Imm32<"srai.w", GPR32Opnd, sra>, R2_IMM5<0b10>;
  def ROTRI_W : Shift_Imm32<"rotri.w", GPR32Opnd, rotr>, R2_IMM5<0b11>;
  ///
  /// Misc
  ///
  def ALSL_W    : Reg3_Sa<"alsl.w", GPR32Opnd, uimm2_plus1>, R3_SA2<0b00010> {
    let Pattern = [(set GPR32Opnd:$rd,
                    (add GPR32Opnd:$rk, (shl GPR32Opnd:$rj, immZExt2Alsl:$sa)))];
  }
  def BYTEPICK_W : Reg3_Sa<"bytepick.w", GPR32Opnd, uimm2>, R3_SA2<0b00100>;//pattern:[]

  def BREAK   : Code15<"break", int_loongarch_break>, CODE15<0b1010100>;
  def SYSCALL : Code15<"syscall", int_loongarch_syscall>, CODE15<0b1010110>;
  def TRAP    : TrapBase<BREAK>;

  def BSTRINS_W  : InsBase_32<"bstrins.w", GPR32Opnd, uimm5, LoongArchBstrins>,
                   INSERT_BIT32<0>;
  def BSTRPICK_W : PickBase_32<"bstrpick.w", GPR32Opnd, uimm5, LoongArchBstrpick>,
                   INSERT_BIT32<1>;

  ///
  /// R2_IMM12
  ///
  let isCodeGenOnly = 1 in {
    def SLTI32    : SetCC_I<"slti", GPR32Opnd, simm12_32>, R2_IMM12<0b000>; //PatFrag
    def SLTUI32   : SetCC_I<"sltui", GPR32Opnd, simm12_32>, R2_IMM12<0b001>; //PatFrag
  }
  def ADDI_W  : Int_Reg2_Imm12<"addi.w", GPR32Opnd, simm12_32, add>, R2_IMM12<0b010>;

  let isCodeGenOnly = 1 in {
    def ANDI32 : Int_Reg2_Imm12<"andi", GPR32Opnd, uimm12_32, and>, R2_IMM12<0b101>;
    def ORI32  : Int_Reg2_Imm12<"ori", GPR32Opnd, uimm12_32, or>, R2_IMM12<0b110>;
    def XORI32 : Int_Reg2_Imm12<"xori", GPR32Opnd, uimm12_32, xor>, R2_IMM12<0b111>;
  }

  ///
  /// Privilege Instructions
  ///
  def CSRRD32 : CSR<"csrrd", GPR32Opnd, uimm14_32, int_loongarch_csrrd>, R1_CSR<0b0000000000100>;
  def CSRWR32 : CSRW<"csrwr", GPR32Opnd, uimm14_32, int_loongarch_csrwr>, R1_CSR<0b0000100000100>;
  def CSRXCHG32 : CSRX<"csrxchg", GPR32Opnd, uimm14_32, int_loongarch_csrxchg>, R2_CSR<0b00000100>;
  def IOCSRRD_B32 : Int_Reg2<"iocsrrd.b", GPR32Opnd, int_loongarch_iocsrrd_b>, R2P<0b000>;
  def IOCSRRD_H32 : Int_Reg2<"iocsrrd.h", GPR32Opnd, int_loongarch_iocsrrd_h>, R2P<0b001>;
  def IOCSRRD_W32 : Int_Reg2<"iocsrrd.w", GPR32Opnd, int_loongarch_iocsrrd_w>, R2P<0b010>;
  def IOCSRWR_B32 : Int_Reg2_Iocsrwr<"iocsrwr.b", GPR32Opnd, GPR32Opnd, int_loongarch_iocsrwr_b>, R2P<0b100>;
  def IOCSRWR_H32 : Int_Reg2_Iocsrwr<"iocsrwr.h", GPR32Opnd, GPR32Opnd, int_loongarch_iocsrwr_h>, R2P<0b101>;
  def IOCSRWR_W32 : Int_Reg2_Iocsrwr<"iocsrwr.w", GPR32Opnd, GPR32Opnd, int_loongarch_iocsrwr_w>, R2P<0b110>;
  def CACOP32 : CAC<"cacop", GPR32Opnd, simm12_32, int_loongarch_cacop>, R1_CACHE;
  def LDDIR32 : LEVEL<"lddir", GPR32Opnd>, R2_LEVEL<0b00000110010000>;
  def LDPTE32 : SEQ<"ldpte", GPR32Opnd>, R1_SEQ<0b00000110010001>;

  //def WAIT : Wait<"wait">;
  //
  //def IOCSRRD_D : R2P<0b011>, Int_Reg2<"iocsrrd.d", GPR32Opnd>;
  //def IOCSRWR_D : R2P<0b111>, Int_Reg2<"iocsrwr.d", GPR32Opnd>;
  //
  //def TLBINV   : IMM32<0b001000>, OP32<"tlbinv">;
  //def TLBFLUSH : IMM32<0b001001>, OP32<"tlbflush">;
  //def TLBP     : IMM32<0b001010>, OP32<"tlbp">;
  //def TLBR     : IMM32<0b001011>, OP32<"tlbr">;
  //def TLBWI    : IMM32<0b001100>, OP32<"tlbwi">;
  //def TLBWR    : IMM32<0b001101>, OP32<"tlbwr">;

  ///
  /// R1_IMM20
  ///
  let isCodeGenOnly = 1 in {
    def LU12I_W32   : SI20<"lu12i.w", GPR32Opnd, simm20_32>, R1_SI20<0b0001010>;
    def PCADDI32    : SI20<"pcaddi", GPR32Opnd, simm20_32>, R1_SI20<0b0001100>;
    def PCALAU12I32 : SI20<"pcalau12i", GPR32Opnd, simm20_32>, R1_SI20<0b0001101>;
    def PCADDU12I32 : SI20<"pcaddu12i", GPR32Opnd, simm20_32>, R1_SI20<0b0001110>;
  }

  let isCodeGenOnly = 1 in {
    def BEQZ32  : Beqz<"beqz", brtarget, seteq, GPR32Opnd>, R1_IMM21BEQZ<0b010000>;
    def BNEZ32  : Beqz<"bnez", brtarget, setne, GPR32Opnd>, R1_IMM21BEQZ<0b010001>;

    def JIRL32  : FJirl<"jirl", calltarget, GPR32Opnd>, R2_IMM16JIRL;

    def B32     : JumpFB<jmptarget, "b", br, bb>, IMM26B<0b010100>;

    def BEQ32   : Beq<"beq", brtarget, seteq, GPR32Opnd>, R2_IMM16BEQ<0b010110>;
    def BNE32   : Beq<"bne", brtarget, setne, GPR32Opnd>, R2_IMM16BEQ<0b010111>;
    def BLT32   : Beq<"blt", brtarget, setlt, GPR32Opnd>, R2_IMM16BEQ<0b011000>;
    def BGE32   : Beq<"bge", brtarget, setge, GPR32Opnd>, R2_IMM16BEQ<0b011001>;
    def BLTU32  : Beq<"bltu", brtarget, setult, GPR32Opnd>, R2_IMM16BEQ<0b011010>;
    def BGEU32  : Beq<"bgeu", brtarget, setuge, GPR32Opnd>, R2_IMM16BEQ<0b011011>;
  }

  ///
  /// Mem access
  ///
  def LL_W : LLBase<"ll.w", GPR32Opnd, mem_simm14_lsl2>, LL_SC<0b000>;
  def SC_W : SCBase<"sc.w", GPR32Opnd, mem_simm14_lsl2>, LL_SC<0b001>;

  def PRELD_Raw32  : Preld_Raw<"preld", GPR32Opnd>, PRELD_FM;

  let isCodeGenOnly = 1 in {
    def LD_B32  : Ld<"ld.b", GPR32Opnd, mem_simmptr, sextloadi8>, LOAD_STORE<0b0000>;
    def LD_H32  : Ld<"ld.h", GPR32Opnd, mem_simmptr, sextloadi16, addrDefault>, LOAD_STORE<0b0001>;
    def LD_W32  : Ld<"ld.w", GPR32Opnd, mem, load, addrDefault>, LOAD_STORE<0b0010>;
    def ST_B32  : St<"st.b", GPR32Opnd, mem, truncstorei8>, LOAD_STORE<0b0100>;
    def ST_H32  : St<"st.h", GPR32Opnd, mem, truncstorei16>, LOAD_STORE<0b0101>;
    def ST_W32  : St<"st.w", GPR32Opnd, mem, store>, LOAD_STORE<0b0110>;
    def LD_BU32 : Ld<"ld.bu", GPR32Opnd, mem_simmptr, zextloadi8, addrDefault>, LOAD_STORE<0b1000>;
    def LD_HU32 : Ld<"ld.hu", GPR32Opnd, mem_simmptr, zextloadi16>, LOAD_STORE<0b1001>;

    def PRELD32  : Preld<"preld", mem, GPR32Opnd>, PRELD_FM;

    def LDPTR_W32 : LdPtr<"ldptr.w", GPR32Opnd>, LL_SC<0b100>;
    def STPTR_W32 : StPtr<"stptr.w", GPR32Opnd>, LL_SC<0b101>;
  }

  def IBAR : Bar<"ibar", int_loongarch_ibar>, BAR_FM<1>;
  def DBAR : Bar<"dbar", int_loongarch_dbar>, BAR_FM<0>;

  def LONG_BRANCH_ADDIW : LoongArchPseudo<(outs GPR32Opnd:$dst),
      (ins GPR32Opnd:$src, brtarget:$tgt, brtarget:$baltgt), []>;

  def LONG_BRANCH_ADDIW2Op : LoongArchPseudo<(outs GPR32Opnd:$dst),
      (ins GPR32Opnd:$src, brtarget:$tgt), []>;

  def PseudoReturn : PseudoReturnBase<GPR32Opnd>;

  let isCodeGenOnly = 1 in {
    def LDX_W32 : LDX_FT_LA<"ldx.w", GPR32Opnd, load>,
                  R3MI<0b00010000>;
    def LDX_HU32 : LDX_FT_LA<"ldx.hu", GPR32Opnd, extloadi16>,
                   R3MI<0b01001000>;
    def LDX_BU32 : LDX_FT_LA<"ldx.bu", GPR32Opnd, extloadi8>,
                   R3MI<0b01000000>;
    def STX_W32 : SDX_FT_LA<"stx.w", GPR32Opnd, store>,
                  R3MI<0b00110000>;
    def LDX_H32 : LDX_FT_LA<"ldx.h", GPR32Opnd, sextloadi16>,
                  R3MI<0b00001000>;
    def LDX_B32 : LDX_FT_LA<"ldx.b", GPR32Opnd, sextloadi8>,
                  R3MI<0b00000000>;
    def STX_B32 : SDX_FT_LA<"stx.b", GPR32Opnd, truncstorei8>,
                  R3MI<0b00100000>;
    def STX_H32 : SDX_FT_LA<"stx.h", GPR32Opnd, truncstorei16>,
                  R3MI<0b00101000>;
  }
}

def LEA_ADDI_W: EffectiveAddress<"addi.w", GPR32Opnd>, LEA_ADDI_FM<0b010>;

def : LoongArchPat<(LoongArchAddress (i32 tglobaladdr:$in)),
                   (ADDI_W (PCADDU12I32 tglobaladdr:$in) ,0)>,GPR_32;
def : LoongArchPat<(LoongArchAddress (i32 tblockaddress:$in)),
                   (ADDI_W (PCADDU12I32 tblockaddress:$in),0)>, GPR_32;
def : LoongArchPat<(LoongArchAddress (i32 tjumptable:$in)),
                   (ADDI_W (PCADDU12I32 tjumptable:$in),0)>, GPR_32;
def : LoongArchPat<(LoongArchAddress (i32 texternalsym:$in)),
                   (ADDI_W (PCADDU12I32 texternalsym:$in),0)>, GPR_32;

//===----------------------------------------------------------------------===//
//  Arbitrary patterns that map to one or more instructions
//===----------------------------------------------------------------------===//

let isCodeGenOnly = 1 in {
  def REVB_2W_32 : Int_Reg2<"revb.2w", GPR32Opnd>, R2I<0b01110>;
  def REVH_2W_32 : Int_Reg2<"revh.2w", GPR32Opnd>, R2I<0b10000>;
}

// bswap pattern
def : LoongArchPat<(bswap GPR32:$rj), (ROTRI_W (REVB_2H GPR32:$rj), 16)>;
//def : LoongArchPat<(bswap GPR32:$rj), (REVB_2W_32 GPR32:$rj)>;
//def : LoongArchPat<(bswap GPR32:$rj), (REVH_2W_32 (REVB_2H GPR32:$rj))>;

// i32 selects
multiclass SelectInt_Pats<ValueType RC, Instruction OROp, Instruction XORiOp,
                          Instruction SLTiOp, Instruction SLTiuOp,
                          Instruction MASKNEZOp, Instruction MASKEQZOp,
                          SDPatternOperator imm_type, ValueType Opg> {

// reg, immz
def : LoongArchPat<(select (Opg (seteq RC:$cond, immz)), RC:$t, RC:$f),
                   (OROp (MASKNEZOp RC:$t, RC:$cond), (MASKEQZOp RC:$f, RC:$cond))>;
def : LoongArchPat<(select (Opg (setne RC:$cond, immz)), RC:$t, RC:$f),
                   (OROp (MASKEQZOp RC:$t, RC:$cond), (MASKNEZOp RC:$f, RC:$cond))>;

//def : LoongArchPat<(select (Opg (seteq RC:$cond, imm_type:$imm)), RC:$t, RC:$f),
//                   (OROp (MASKNEZOp RC:$t, (XORiOp RC:$cond, imm_type:$imm)),
//                         (MASKEQZOp RC:$f, (XORiOp RC:$cond, imm_type:$imm)))>;
//def : LoongArchPat<(select (Opg (setne RC:$cond, imm_type:$imm)), RC:$t, RC:$f),
//                   (OROp (MASKEQZOp RC:$t, (XORiOp RC:$cond, imm_type:$imm)),
//                         (MASKNEZOp RC:$f, (XORiOp RC:$cond, imm_type:$imm)))>;

// reg, immSExt12Plus1
//def : LoongArchPat<(select (Opg (setgt RC:$cond, immSExt12Plus1:$imm)), RC:$t, RC:$f),
//                   (OROp (MASKNEZOp RC:$t, (SLTiOp RC:$cond, (Plus1 imm:$imm))),
//                         (MASKEQZOp RC:$f, (SLTiOp RC:$cond, (Plus1 imm:$imm))))>;
//def : LoongArchPat<(select (Opg (setugt RC:$cond, immSExt16Plus1:$imm)), RC:$t, RC:$f),
//                   (OROp (MASKNEZOp RC:$t, (SLTiuOp RC:$cond, (Plus1 imm:$imm))),
//                         (MASKEQZOp RC:$f, (SLTiuOp RC:$cond, (Plus1 imm:$imm))))>;

def : LoongArchPat<(select (Opg (seteq RC:$cond, immz)), RC:$t, immz),
                   (MASKNEZOp RC:$t, RC:$cond)>;
def : LoongArchPat<(select (Opg (setne RC:$cond, immz)), RC:$t, immz),
                   (MASKEQZOp RC:$t, RC:$cond)>;
def : LoongArchPat<(select (Opg (seteq RC:$cond, immz)), immz, RC:$f),
                   (MASKEQZOp RC:$f, RC:$cond)>;
def : LoongArchPat<(select (Opg (setne RC:$cond, immz)), immz, RC:$f),
                   (MASKNEZOp RC:$f, RC:$cond)>;
}

defm : SelectInt_Pats<i32, OR32, XORI32, SLTI32, SLTUI32, MASKNEZ32, MASKEQZ32,
                      immZExt12, i32>;

def : LoongArchPat<(select i32:$cond, i32:$t, i32:$f),
                   (OR32 (MASKEQZ32 i32:$t, i32:$cond),
                         (MASKNEZ32 i32:$f, i32:$cond))>;
def : LoongArchPat<(select i32:$cond, i32:$t, immz),
                   (MASKEQZ32 i32:$t, i32:$cond)>;
def : LoongArchPat<(select i32:$cond, immz, i32:$f),
                   (MASKNEZ32 i32:$f, i32:$cond)>;

// truncate
def : LoongArchPat<(i32 (trunc (i64 (assertsext GPR64:$src)))),
                   (EXTRACT_SUBREG GPR64:$src, sub_32)>, GPR_64;
def : LoongArchPat<(i32 (trunc (assertzext_lt_i32 GPR64:$src))),
                   (EXTRACT_SUBREG GPR64:$src, sub_32)>, GPR_64;
def : LoongArchPat<(i32 (trunc GPR64:$src)),
                   (SLLI_W (EXTRACT_SUBREG GPR64:$src, sub_32), 0)>, GPR_64;

// Patterns used for matching away redundant sign extensions.
// LA32 arithmetic instructions sign extend their result implicitly.
def : LoongArchPat<(i64 (sext (i32 (add GPR32:$src, GPR32:$src2)))),
                   (INSERT_SUBREG (i64 (IMPLICIT_DEF)),
                   (ADD_W GPR32:$src, GPR32:$src2), sub_32)>;
def : LoongArchPat<(i64 (sext (i32 (sub GPR32:$src, GPR32:$src2)))),
                   (INSERT_SUBREG (i64 (IMPLICIT_DEF)),
                   (SUB_W GPR32:$src, GPR32:$src2), sub_32)>;
def : LoongArchPat<(i64 (sext (i32 (mul GPR32:$src, GPR32:$src2)))),
                   (INSERT_SUBREG (i64 (IMPLICIT_DEF)),
                   (MUL_W GPR32:$src, GPR32:$src2), sub_32)>;

def : LoongArchPat<(store (i32 0), addr:$dst), (ST_W32 ZERO, addr:$dst)>;

// Materialize constants.
def : LoongArchPat<(i32 LU12IORIPred:$imm), (ORI32 (LU12I_W32 (HI20 imm:$imm)), (LO12 imm:$imm))>;
def : LoongArchPat<(i32 LU12IPred:$imm), (LU12I_W32 (HI20 imm:$imm))>;
def : LoongArchPat<(i32 ORi12Pred:$imm), (ORI32 ZERO, imm:$imm)>;
def : LoongArchPat<(i32 immSExt12:$imm), (ADDI_W ZERO, imm:$imm)>;

def : InstAlias<"break", (BREAK 0), 1>;
def : InstAlias<"break $imm", (BREAK uimm15:$imm), 1>;
def : LoongArchInstAlias<"move $dst, $src",
                         (OR32 GPR32Opnd:$dst, GPR32Opnd:$src, ZERO), 1>, GPR_32;

def immSExt12Plus1 : PatLeaf<(imm), [{
  return isInt<13>(N->getSExtValue()) && isInt<12>(N->getSExtValue() + 1);
}]>;

def Plus1 : SDNodeXForm<imm, [{ return getImm(N, N->getSExtValue() + 1); }]>;

multiclass BrcondPats<RegisterClass RC, Instruction BEQOp, Instruction BEQOp1,
                      Instruction BNEOp, Instruction SLTOp, Instruction SLTUOp,
                      Instruction SLTIOp, Instruction SLTUIOp,
                      Register ZEROReg> {

def : LoongArchPat<(brcond (i32 (setne RC:$lhs, 0)), bb:$dst),
      (BNEOp RC:$lhs, ZEROReg, bb:$dst)>;
def : LoongArchPat<(brcond (i32 (seteq RC:$lhs, 0)), bb:$dst),
      (BEQOp RC:$lhs, ZEROReg, bb:$dst)>;
def : LoongArchPat<(brcond (i32 (setge RC:$lhs, RC:$rhs)), bb:$dst),
      (BEQOp1 (SLTOp RC:$lhs, RC:$rhs), ZEROReg, bb:$dst)>;
def : LoongArchPat<(brcond (i32 (setuge RC:$lhs, RC:$rhs)), bb:$dst),
      (BEQOp1 (SLTUOp RC:$lhs, RC:$rhs), ZEROReg, bb:$dst)>;
def : LoongArchPat<(brcond (i32 (setge RC:$lhs, immSExt12:$rhs)), bb:$dst),
      (BEQOp1 (SLTIOp RC:$lhs, immSExt12:$rhs), ZEROReg, bb:$dst)>;
def : LoongArchPat<(brcond (i32 (setuge RC:$lhs, immSExt12:$rhs)), bb:$dst),
      (BEQOp1 (SLTUIOp RC:$lhs, immSExt12:$rhs), ZEROReg, bb:$dst)>;
def : LoongArchPat<(brcond (i32 (setgt RC:$lhs, immSExt12Plus1:$rhs)), bb:$dst),
      (BEQOp1 (SLTIOp RC:$lhs, (Plus1 imm:$rhs)), ZEROReg, bb:$dst)>;
def : LoongArchPat<(brcond (i32 (setugt RC:$lhs, immSExt12Plus1:$rhs)), bb:$dst),
      (BEQOp1 (SLTUIOp RC:$lhs, (Plus1 imm:$rhs)), ZEROReg, bb:$dst)>;
def : LoongArchPat<(brcond (i32 (setle RC:$lhs, RC:$rhs)), bb:$dst),
      (BEQOp1 (SLTOp RC:$rhs, RC:$lhs), ZEROReg, bb:$dst)>;
def : LoongArchPat<(brcond (i32 (setule RC:$lhs, RC:$rhs)), bb:$dst),
      (BEQOp1 (SLTUOp RC:$rhs, RC:$lhs), ZEROReg, bb:$dst)>;
def : LoongArchPat<(brcond RC:$cond, bb:$dst),
      (BNEOp RC:$cond, ZEROReg, bb:$dst)>;
}

defm : BrcondPats<GPR32, BEQ32, BEQ32, BNE32, SLT32, SLTU32, SLTI32, SLTUI32, ZERO>, GPR_64;

let usesCustomInserter = 1 in {
  def ATOMIC_LOAD_ADD_I8   : Atomic2Ops<atomic_load_add_8, GPR32>;
  def ATOMIC_LOAD_ADD_I16  : Atomic2Ops<atomic_load_add_16, GPR32>;
  def ATOMIC_LOAD_ADD_I32  : Atomic2Ops<atomic_load_add_32, GPR32>;
  def ATOMIC_LOAD_SUB_I8   : Atomic2Ops<atomic_load_sub_8, GPR32>;
  def ATOMIC_LOAD_SUB_I16  : Atomic2Ops<atomic_load_sub_16, GPR32>;
  def ATOMIC_LOAD_SUB_I32  : Atomic2Ops<atomic_load_sub_32, GPR32>;
  def ATOMIC_LOAD_AND_I8   : Atomic2Ops<atomic_load_and_8, GPR32>;
  def ATOMIC_LOAD_AND_I16  : Atomic2Ops<atomic_load_and_16, GPR32>;
  def ATOMIC_LOAD_AND_I32  : Atomic2Ops<atomic_load_and_32, GPR32>;
  def ATOMIC_LOAD_OR_I8    : Atomic2Ops<atomic_load_or_8, GPR32>;
  def ATOMIC_LOAD_OR_I16   : Atomic2Ops<atomic_load_or_16, GPR32>;
  def ATOMIC_LOAD_OR_I32   : Atomic2Ops<atomic_load_or_32, GPR32>;
  def ATOMIC_LOAD_XOR_I8   : Atomic2Ops<atomic_load_xor_8, GPR32>;
  def ATOMIC_LOAD_XOR_I16  : Atomic2Ops<atomic_load_xor_16, GPR32>;
  def ATOMIC_LOAD_XOR_I32  : Atomic2Ops<atomic_load_xor_32, GPR32>;
  def ATOMIC_LOAD_NAND_I8  : Atomic2Ops<atomic_load_nand_8, GPR32>;
  def ATOMIC_LOAD_NAND_I16 : Atomic2Ops<atomic_load_nand_16, GPR32>;
  def ATOMIC_LOAD_NAND_I32 : Atomic2Ops<atomic_load_nand_32, GPR32>;

  def ATOMIC_SWAP_I8       : Atomic2Ops<atomic_swap_8, GPR32>;
  def ATOMIC_SWAP_I16      : Atomic2Ops<atomic_swap_16, GPR32>;
  def ATOMIC_SWAP_I32      : Atomic2Ops<atomic_swap_32, GPR32>;

  def ATOMIC_CMP_SWAP_I8   : AtomicCmpSwap<atomic_cmp_swap_8, GPR32>;
  def ATOMIC_CMP_SWAP_I16  : AtomicCmpSwap<atomic_cmp_swap_16, GPR32>;
  def ATOMIC_CMP_SWAP_I32  : AtomicCmpSwap<atomic_cmp_swap_32, GPR32>;

  def ATOMIC_LOAD_MAX_I8   : Atomic2Ops<atomic_load_max_8, GPR32>;
  def ATOMIC_LOAD_MAX_I16  : Atomic2Ops<atomic_load_max_16, GPR32>;
  def ATOMIC_LOAD_MAX_I32  : Atomic2Ops<atomic_load_max_32, GPR32>;

  def ATOMIC_LOAD_MIN_I8   : Atomic2Ops<atomic_load_min_8, GPR32>;
  def ATOMIC_LOAD_MIN_I16  : Atomic2Ops<atomic_load_min_16, GPR32>;
  def ATOMIC_LOAD_MIN_I32  : Atomic2Ops<atomic_load_min_32, GPR32>;

  def ATOMIC_LOAD_UMAX_I8   : Atomic2Ops<atomic_load_umax_8, GPR32>;
  def ATOMIC_LOAD_UMAX_I16  : Atomic2Ops<atomic_load_umax_16, GPR32>;
  def ATOMIC_LOAD_UMAX_I32  : Atomic2Ops<atomic_load_umax_32, GPR32>;

  def ATOMIC_LOAD_UMIN_I8   : Atomic2Ops<atomic_load_umin_8, GPR32>;
  def ATOMIC_LOAD_UMIN_I16  : Atomic2Ops<atomic_load_umin_16, GPR32>;
  def ATOMIC_LOAD_UMIN_I32  : Atomic2Ops<atomic_load_umin_32, GPR32>;
}

def ATOMIC_LOAD_ADD_I8_POSTRA   : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_ADD_I16_POSTRA  : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_ADD_I32_POSTRA  : Atomic2OpsPostRA<GPR32>;
def ATOMIC_LOAD_SUB_I8_POSTRA   : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_SUB_I16_POSTRA  : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_SUB_I32_POSTRA  : Atomic2OpsPostRA<GPR32>;
def ATOMIC_LOAD_AND_I8_POSTRA   : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_AND_I16_POSTRA  : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_AND_I32_POSTRA  : Atomic2OpsPostRA<GPR32>;
def ATOMIC_LOAD_OR_I8_POSTRA    : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_OR_I16_POSTRA   : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_OR_I32_POSTRA   : Atomic2OpsPostRA<GPR32>;
def ATOMIC_LOAD_XOR_I8_POSTRA   : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_XOR_I16_POSTRA  : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_XOR_I32_POSTRA  : Atomic2OpsPostRA<GPR32>;
def ATOMIC_LOAD_NAND_I8_POSTRA  : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_NAND_I16_POSTRA : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_NAND_I32_POSTRA : Atomic2OpsPostRA<GPR32>;

def ATOMIC_SWAP_I8_POSTRA  : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_SWAP_I16_POSTRA : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_SWAP_I32_POSTRA : Atomic2OpsPostRA<GPR32>;

def ATOMIC_CMP_SWAP_I8_POSTRA : AtomicCmpSwapSubwordPostRA<GPR32>;
def ATOMIC_CMP_SWAP_I16_POSTRA : AtomicCmpSwapSubwordPostRA<GPR32>;
def ATOMIC_CMP_SWAP_I32_POSTRA : AtomicCmpSwapPostRA<GPR32>;

def ATOMIC_LOAD_MAX_I8_POSTRA   : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_MAX_I16_POSTRA  : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_MAX_I32_POSTRA  : Atomic2OpsPostRA<GPR32>;

def ATOMIC_LOAD_MIN_I8_POSTRA   : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_MIN_I16_POSTRA  : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_MIN_I32_POSTRA  : Atomic2OpsPostRA<GPR32>;

def ATOMIC_LOAD_UMAX_I8_POSTRA   : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_UMAX_I16_POSTRA  : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_UMAX_I32_POSTRA  : Atomic2OpsPostRA<GPR32>;

def ATOMIC_LOAD_UMIN_I8_POSTRA   : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_UMIN_I16_POSTRA  : Atomic2OpsSubwordPostRA<GPR32>;
def ATOMIC_LOAD_UMIN_I32_POSTRA  : Atomic2OpsPostRA<GPR32>;

def : LoongArchPat<(atomic_load_8 addr:$a), (LD_B32 addr:$a)>;
def : LoongArchPat<(atomic_load_16 addr:$a), (LD_H32 addr:$a)>;
def : LoongArchPat<(atomic_load_32 addrimm14lsl2:$a), (LDPTR_W32 addrimm14lsl2:$a)>;
def : LoongArchPat<(atomic_load_32 addr:$a), (LD_W32 addr:$a)>;

def : LoongArchPat<(atomic_store_8 addr:$a, GPR32:$v),
      (ST_B32 GPR32:$v, addr:$a)>;
def : LoongArchPat<(atomic_store_16 addr:$a, GPR32:$v),
      (ST_H32 GPR32:$v, addr:$a)>;
def : LoongArchPat<(atomic_store_32 addrimm14lsl2:$a, GPR32:$v),
      (STPTR_W32 GPR32:$v, addrimm14lsl2:$a)>;
def : LoongArchPat<(atomic_store_32 addr:$a, GPR32:$v),
      (ST_W32 GPR32:$v, addr:$a)>;

def : LoongArchPat<(LoongArchDBAR (i32 immz)),
              (DBAR 0)>;

def : LoongArchPat<(i32 (extloadi1  addr:$src)), (LD_BU32 addr:$src)>;
def : LoongArchPat<(i32 (extloadi8  addr:$src)), (LD_BU32 addr:$src)>;
def : LoongArchPat<(i32 (extloadi16 addr:$src)), (LD_HU32 addr:$src)>;

def : LoongArchPat<(store (i32 0), addr:$dst), (ST_W32 ZERO, addr:$dst)>;

// Patterns for loads/stores with a reg+imm operand.
let AddedComplexity = 40 in {
  def : LoadRegImmPat<LD_B32, i32, sextloadi8>;
  def : LoadRegImmPat<LD_H32, i32, sextloadi16>;
  def : LoadRegImmPat<LD_W32, i32, load>;
  def : LoadRegImmPat<LD_BU32, i32, zextloadi8>;
  def : LoadRegImmPat<LD_HU32, i32, zextloadi16>;
  def : StoreRegImmPat<ST_B32, i32, truncstorei8>;
  def : StoreRegImmPat<ST_H32, i32, truncstorei16>;
  def : StoreRegImmPat<ST_W32, i32, store>;

  def : LoadRegImm14Lsl2Pat<LDPTR_W32, i32, load>;
  def : StoreRegImm14Lsl2Pat<STPTR_W32, i32, store>;
}

let isCall=1, isCTI=1, Defs = [RA] in {

  class JumpLinkRegPseudo<RegisterOperand RO, Instruction JIRLRInst,
                          Register RetReg, RegisterOperand ResRO = RO>:
    LoongArchPseudo<(outs), (ins RO:$rj), [(LoongArchJmpLink RO:$rj)]>,
    PseudoInstExpansion<(JIRLRInst RetReg, ResRO:$rj)> {
    let hasPostISelHook = 1;
  }

  class JumpLinkReg<string opstr, RegisterOperand RO>:
    InstForm<(outs RO:$rd), (ins RO:$rj), !strconcat(opstr, "\t$rd, $rj, 0"),
             [], FrmR, opstr> {
    let hasPostISelHook = 1;
  }

}

def JIRLR : JumpLinkReg<"jirl", GPR32Opnd>, R2_IMM16JIRL {
  let offs16 = 0;
}
def JIRLRPseudo : JumpLinkRegPseudo<GPR64Opnd, JIRLR, RA, GPR32Opnd>;

class BrindRegPseudo<RegisterOperand RO, Instruction JIRLRInst,
                     Register RetReg, RegisterOperand ResRO = RO>:
  LoongArchPseudo<(outs), (ins RO:$rj), [(brind RO:$rj)]>,
  PseudoInstExpansion<(JIRLRInst RetReg, ResRO:$rj)> {
  let isTerminator=1;
  let isBarrier=1;
  let isBranch = 1;
  let isIndirectBranch = 1;
  bit isCTI = 1;
}

def JIRLRBRIND : BrindRegPseudo<GPR64Opnd, JIRLR, ZERO, GPR32Opnd>;

def : LoongArchPat<(addc GPR32:$src, immSExt12:$imm),
                   (ADDI_W GPR32:$src, imm:$imm)>;

defm : SeteqPats<GPR32, SLTUI32, XOR32, SLTU32, ZERO>;
defm : SetlePats<GPR32, XORI32, SLT32, SLTU32>;
defm : SetgtPats<GPR32, SLT32, SLTU32>;
defm : SetgePats<GPR32, XORI32, SLT32, SLTU32>;
defm : SetgeImmPats<GPR32, XORI32, SLTI32, SLTUI32>;
