//===- LoongArchInstrInfo.td - Target Description for LoongArch Target -*- tablegen -*-=//
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
include "LoongArchInstrFormats.td"

def SDT_Bstrpick : SDTypeProfile<1, 3, [SDTCisInt<0>, SDTCisSameAs<0, 1>,
                                   SDTCisVT<2, i32>, SDTCisSameAs<2, 3>]>;
def SDT_Bstrins : SDTypeProfile<1, 4, [SDTCisInt<0>, SDTCisSameAs<0, 1>,
                                   SDTCisVT<2, i32>, SDTCisSameAs<2, 3>,
                                   SDTCisSameAs<0, 4>]>;

def LoongArchBstrpick :  SDNode<"LoongArchISD::BSTRPICK", SDT_Bstrpick>;

def LoongArchBstrins :  SDNode<"LoongArchISD::BSTRINS", SDT_Bstrins>;

def SDT_DBAR             : SDTypeProfile<0, 1, [SDTCisVT<0, i32>]>;
def LoongArchDBAR : SDNode<"LoongArchISD::DBAR", SDT_DBAR, [SDNPHasChain,SDNPSideEffect]>;

def SDT_LoongArchEHRET : SDTypeProfile<0, 2, [SDTCisInt<0>, SDTCisPtrTy<1>]>;

def LoongArchehret : SDNode<"LoongArchISD::EH_RETURN", SDT_LoongArchEHRET,
                      [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

//===---------------------------------------------------------------------===/
// Operand, Complex Patterns and Transformations Definitions.
//===---------------------------------------------------------------------===/

def assertzext_lt_i32 : PatFrag<(ops node:$src), (assertzext node:$src), [{
  return cast<VTSDNode>(N->getOperand(1))->getVT().bitsLT(MVT::i32);
}]>;

def immz : PatLeaf<(imm), [{ return N->getSExtValue() == 0; }]>;
def immZExt12 : PatLeaf<(imm), [{ return isUInt<12>(N->getZExtValue()); }]>;
def immSExt12 : PatLeaf<(imm), [{ return isInt<12>(N->getSExtValue()); }]>;
def immSExt13 : PatLeaf<(imm), [{ return isInt<13>(N->getSExtValue()); }]>;

def immZExt2Alsl : ImmLeaf<i32, [{return isUInt<2>(Imm - 1);}]>;
//class ImmAsmOperand<int Low, int High> : AsmOperandClass {
//  let RenderMethod = "addImmOperands";
//  let PredicateMethod = "isImmediate<" # Low # "," # High # ">";
//  let DiagnosticString = "operand must be an immediate in the range [" # Low # "," # High # "]";
//}
//
//def Imm8AsmOperand: ImmAsmOperand<8,8> { let Name = "Imm8"; }
//def imm8 : Operand<i64>, ImmLeaf<i64, [{ return Imm == 8; }]> {
//  let ParserMatchClass = Imm8AsmOperand;
//}

class SImmOperand<int width> : AsmOperandClass {
  let Name = "SImm" # width;
  let DiagnosticType = "InvalidSImm" # width;
  let RenderMethod = "addImmOperands";
  let PredicateMethod = "isSImm<" # width # ">";
}

def SImm2Operand : SImmOperand<2>;
def simm2 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= -2 && Imm < 2; }]> {
  let ParserMatchClass = SImm2Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<2>";
}
def SImm3Operand : SImmOperand<3>;
def simm3 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= -4 && Imm < 4; }]> {
  let ParserMatchClass = SImm3Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<3>";
}

def SImm5Operand : SImmOperand<5>;
def simm5 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= -16 && Imm < 16; }]> {
  let ParserMatchClass = SImm5Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<5>";
}

def simm5_32 : Operand<i32>, ImmLeaf<i32, [{ return Imm >= -16 && Imm < 16; }]> {
  let ParserMatchClass = SImm5Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<5>";
}

def SImm8Operand : SImmOperand<8>;
def simm8 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= -128 && Imm < 128; }]> {
  let ParserMatchClass = SImm8Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<8>";
}
def simm8_32 : Operand<i32>, ImmLeaf<i32, [{ return Imm >= -128 && Imm < 128; }]> {
  let ParserMatchClass = SImm8Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<8>";
}

def SImm12Operand : SImmOperand<12>;
def simm12 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= -2048 && Imm < 2048; }]> {
  let ParserMatchClass = SImm12Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<12>";
}
def simm12_32 : Operand<i32>, ImmLeaf<i32, [{ return Imm >= -2048 && Imm < 2048; }]> {
  let ParserMatchClass = SImm12Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<12>";
}

def SImm14Operand : SImmOperand<14>;
def simm14 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= -8192 && Imm < 8192; }]> {
  let ParserMatchClass = SImm14Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<14>";
}

def SImm15Operand : SImmOperand<15>;
def simm15 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= -16384 && Imm < 16384; }]> {
  let ParserMatchClass = SImm15Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<15>";
}

def SImm16Operand : SImmOperand<16>;
def simm16 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= -32768 && Imm < 32768; }]> {
  let ParserMatchClass = SImm16Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<16>";
}

def SImm20Operand : SImmOperand<20>;
def simm20 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= -524288 && Imm < 524288; }]> {
  let ParserMatchClass = SImm20Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<20>";
}
def simm20_32 : Operand<i32>, ImmLeaf<i32, [{ return Imm >= -524288 && Imm < 524288; }]> {
  let ParserMatchClass = SImm20Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<20>";
}

def SImm21Operand : SImmOperand<21>;
def simm21 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= -1048576 && Imm < 1048576; }]> {
  let ParserMatchClass = SImm21Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<21>";
}

def SImm26Operand : SImmOperand<26>;
def simm26 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= -33554432 && Imm < 33554432; }]> {
  let ParserMatchClass = SImm26Operand;
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<26>";
}

def UImm2Operand : AsmOperandClass {
  let Name = "UImm2";
  let RenderMethod = "addUImmOperands<2>";
  let PredicateMethod = "isUImm<2>";
  let DiagnosticType = "InvalidImm0_3";
}

def UImm3Operand : AsmOperandClass {
  let Name = "UImm3";
  let RenderMethod = "addUImmOperands<3>";
  let PredicateMethod = "isUImm<3>";
  let DiagnosticType = "InvalidImm0_7";
}

def UImm5Operand : AsmOperandClass {
  let Name = "UImm5";
  let RenderMethod = "addUImmOperands<5>";
  let PredicateMethod = "isUImm<5>";
  let DiagnosticType = "InvalidImm0_31";
}

def uimm2 : Operand<i32>, ImmLeaf<i32, [{ return Imm >= 0 && Imm < 4; }]> {
  let PrintMethod = "printUImm<2>";
  let ParserMatchClass = UImm2Operand;
}

def uimm3 : Operand<i32>, ImmLeaf<i32, [{ return Imm >= 0 && Imm < 8; }]> {
  let PrintMethod = "printUImm<3>";
  let ParserMatchClass = UImm3Operand;
}

def uimm5 : Operand<i32>, ImmLeaf<i32, [{ return Imm >= 0 && Imm < 32; }]> {
  let PrintMethod = "printUImm<5>";
  let ParserMatchClass = UImm5Operand;
}

def UImm6Operand : AsmOperandClass {
  let Name = "UImm6";
  let RenderMethod = "addUImmOperands<16>";
  let PredicateMethod = "isUImm<6>";
  let DiagnosticType = "InvalidImm0_63";
}
def uimm6 : Operand<i32>, ImmLeaf<i32, [{ return Imm >= 0 && Imm < 64; }]> {
  let PrintMethod = "printUImm<6>";
  let ParserMatchClass = UImm6Operand;
}

def UImm12Operand : AsmOperandClass {
  let Name = "UImm12";
  let RenderMethod = "addUImmOperands<12>";
  let PredicateMethod = "isUImm<12>";
  let DiagnosticType = "InvalidImm0_4095";
}
def uimm12 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= 0 && Imm < 4096; }]> {
  let PrintMethod = "printUImm<12>";
  let ParserMatchClass = UImm12Operand;
}
def uimm12_32 : Operand<i32>, ImmLeaf<i32, [{ return Imm >= 0 && Imm < 4096; }]> {
  let PrintMethod = "printUImm<12>";
  let ParserMatchClass = UImm12Operand;
}

def UImm15Operand : AsmOperandClass {
  let Name = "UImm15";
  let RenderMethod = "addUImmOperands<15>";
  let PredicateMethod = "isUImm<15>";
  let DiagnosticType = "InvalidImm0_32767";
}
def uimm15 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= 0 && Imm < 32768; }]> {
  let PrintMethod = "printUImm<15>";
  let ParserMatchClass = UImm15Operand;
}

def UImm14Operand : AsmOperandClass {
  let Name = "UImm14";
  let RenderMethod = "addUImmOperands<14>";
  let PredicateMethod = "isUImm<14>";
  let DiagnosticType = "InvalidImm0_16383";
}
def uimm14 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= 0 && Imm < 16384; }]> {
  let PrintMethod = "printUImm<14>";
  let ParserMatchClass = UImm14Operand;
}
def uimm14_32 : Operand<i32>, ImmLeaf<i32, [{ return Imm >= 0 && Imm < 16384; }]> {
  let PrintMethod = "printUImm<14>";
  let ParserMatchClass = UImm14Operand;
}

def UImm8Operand : AsmOperandClass {
  let Name = "UImm8";
  let RenderMethod = "addUImmOperands<8>";
  let PredicateMethod = "isUImm<8>";
  let DiagnosticType = "InvalidImm0_255";
}
def uimm8_64 : Operand<i64>, ImmLeaf<i64, [{ return Imm >= 0 && Imm < 256; }]> {
  let PrintMethod = "printUImm<8>";
  let ParserMatchClass = UImm8Operand;
}

// Transformation Function - get the higher 20 bits.
def HI20 : SDNodeXForm<imm, [{
  return getImm(N, (N->getZExtValue() >> 12) & 0xFFFFF);
}]>;

// Transformation Function - get the lower 12 bits.
def LO12 : SDNodeXForm<imm, [{
  return getImm(N, N->getZExtValue() & 0xFFF);
}]>;

def LU12IORIPred  : PatLeaf<(imm), [{
  int64_t SVal = N->getSExtValue();
  return isInt<32>(SVal) && (SVal & 0xfff);
}]>;

def ORi12Pred  : PatLeaf<(imm), [{
  return isUInt<12>(N->getZExtValue()) && !isInt<12>(N->getSExtValue());
}], LO12>;

def LU12IPred : PatLeaf<(imm), [{
  int64_t Val = N->getSExtValue();
  return !isInt<12>(Val) && isInt<32>(Val) && !(Val & 0xfff);
}]>;

def addr :
ComplexPattern<iPTR, 2, "selectIntAddr", [frameindex]>;

def addrDefault :
ComplexPattern<iPTR, 2, "selectAddrDefault", [frameindex]>;

def addrRegImm :
ComplexPattern<iPTR, 2, "selectAddrRegImm", [frameindex]>;

def addrimm14lsl2 : ComplexPattern<iPTR, 2, "selectIntAddrSImm14Lsl2",
                                   [frameindex]>;

class ConstantUImmAsmOperandClass<int Bits, list<AsmOperandClass> Supers = [],
                                  int Offset = 0> : AsmOperandClass {
  let Name = "ConstantUImm" # Bits # "_" # Offset;
  let RenderMethod = "addConstantUImmOperands<" # Bits # ", " # Offset # ">";
  let PredicateMethod = "isConstantUImm<" # Bits # ", " # Offset # ">";
  let SuperClasses = Supers;
  let DiagnosticType = "UImm" # Bits # "_" # Offset;
}
class SImmAsmOperandClass<int Bits, list<AsmOperandClass> Supers = []>
    : AsmOperandClass {
  let Name = "SImm" # Bits;
  let RenderMethod = "addSImmOperands<" # Bits # ">";
  let PredicateMethod = "isSImm<" # Bits # ">";
  let SuperClasses = Supers;
  let DiagnosticType = "SImm" # Bits;
}
class UImmAnyAsmOperandClass<int Bits, list<AsmOperandClass> Supers = []>
    : AsmOperandClass {
  let Name = "ImmAny";
  let RenderMethod = "addConstantUImmOperands<32>";
  let PredicateMethod = "isSImm<" # Bits # ">";
  let SuperClasses = Supers;
  let DiagnosticType = "ImmAny";
}

def UImm32CoercedAsmOperandClass : UImmAnyAsmOperandClass<33, []> {
  let Name = "UImm32_Coerced";
  let DiagnosticType = "UImm32_Coerced";
}
def SImm32RelaxedAsmOperandClass
    : SImmAsmOperandClass<32, [UImm32CoercedAsmOperandClass]> {
  let Name = "SImm32_Relaxed";
  let PredicateMethod = "isAnyImm<33>";
  let DiagnosticType = "SImm32_Relaxed";
}
def SImm32AsmOperandClass
    : SImmAsmOperandClass<32, [SImm32RelaxedAsmOperandClass]>;
def ConstantUImm26AsmOperandClass
    : ConstantUImmAsmOperandClass<26, [SImm32AsmOperandClass]>;

def ConstantUImm20AsmOperandClass
    : ConstantUImmAsmOperandClass<20, [ConstantUImm26AsmOperandClass]>;

def ConstantUImm2Plus1AsmOperandClass
    : ConstantUImmAsmOperandClass<2, [ConstantUImm20AsmOperandClass], 1>;

class UImmAsmOperandClass<int Bits, list<AsmOperandClass> Supers = []>
    : AsmOperandClass {
  let Name = "UImm" # Bits;
  let RenderMethod = "addUImmOperands<" # Bits # ">";
  let PredicateMethod = "isUImm<" # Bits # ">";
  let SuperClasses = Supers;
  let DiagnosticType = "UImm" # Bits;
}

def UImm16RelaxedAsmOperandClass
    : UImmAsmOperandClass<16, [ConstantUImm20AsmOperandClass]> {
  let Name = "UImm16_Relaxed";
  let PredicateMethod = "isAnyImm<16>";
  let DiagnosticType = "UImm16_Relaxed";
}

def ConstantSImm14Lsl2AsmOperandClass : AsmOperandClass {
  let Name = "SImm14Lsl2";
  let RenderMethod = "addImmOperands";
  let PredicateMethod = "isScaledSImm<14, 2>";
  let SuperClasses = [UImm16RelaxedAsmOperandClass];
  let DiagnosticType = "SImm14_Lsl2";
}

foreach I = {2} in
  def simm14_lsl # I : Operand<i64> {
    let DecoderMethod = "DecodeSImmWithOffsetAndScale<14, " # I # ">";
    let ParserMatchClass =
        !cast<AsmOperandClass>("ConstantSImm14Lsl" # I # "AsmOperandClass");
  }

def uimm16_64_relaxed : Operand<i64> {
  let PrintMethod = "printUImm<16>";
  let ParserMatchClass =
      !cast<AsmOperandClass>("UImm16RelaxedAsmOperandClass");
}

def uimm2_plus1 : Operand<i32> {
  let PrintMethod = "printUImm<2, 1>";
  let EncoderMethod = "getUImmWithOffsetEncoding<2, 1>";
  let DecoderMethod = "DecodeUImmWithOffset<2, 1>";
  let ParserMatchClass = ConstantUImm2Plus1AsmOperandClass;
}

// like simm32 but coerces simm32 to uimm32.
def uimm32_coerced : Operand<i32> {
  let ParserMatchClass = !cast<AsmOperandClass>("UImm32CoercedAsmOperandClass");
}

def imm64: Operand<i64>;

def LoongArchMemAsmOperand : AsmOperandClass {
  let Name = "Mem";
  let ParserMethod = "parseMemOperand";
}

def LoongArchMemSimm14AsmOperand : AsmOperandClass {
  let Name = "MemOffsetSimm14";
  let SuperClasses = [LoongArchMemAsmOperand];
  let RenderMethod = "addMemOperands";
  let ParserMethod = "parseMemOperand";
  let PredicateMethod = "isMemWithSimmOffset<14>";
  let DiagnosticType = "MemSImm14";
}

foreach I = {2} in
  def LoongArchMemSimm14Lsl # I # AsmOperand : AsmOperandClass {
    let Name = "MemOffsetSimm14_" # I;
    let SuperClasses = [LoongArchMemAsmOperand];
    let RenderMethod = "addMemOperands";
    let ParserMethod = "parseMemOperand";
    let PredicateMethod = "isMemWithSimmOffset<14, " # I # ">";
    let DiagnosticType = "MemSImm14Lsl" # I;
  }

def LoongArchMemSimmPtrAsmOperand : AsmOperandClass {
  let Name = "MemOffsetSimmPtr";
  let SuperClasses = [LoongArchMemAsmOperand];
  let RenderMethod = "addMemOperands";
  let ParserMethod = "parseMemOperand";
  let PredicateMethod = "isMemWithPtrSizeOffset";
  let DiagnosticType = "MemSImmPtr";
}

class mem_generic : Operand<iPTR> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops ptr_rc, simm12);
  let EncoderMethod = "getMemEncoding";
  let ParserMatchClass = LoongArchMemAsmOperand;
  let OperandType = "OPERAND_MEMORY";
}

// Address operand
def mem : mem_generic;
def mem_simmptr : mem_generic {
    let ParserMatchClass = LoongArchMemSimmPtrAsmOperand;
}

foreach I = {2} in
  def mem_simm14_lsl # I : mem_generic {
    let MIOperandInfo = (ops ptr_rc, !cast<Operand>("simm14_lsl" # I));
    let EncoderMethod = "getSimm14MemEncoding<" # I  # ">";
    let ParserMatchClass =
            !cast<AsmOperandClass>("LoongArchMemSimm14Lsl" # I # "AsmOperand");
  }

def mem_ea : Operand<iPTR> {
  let PrintMethod = "printMemOperandEA";
  let MIOperandInfo = (ops ptr_rc, simm12);
  let EncoderMethod = "getMemEncoding";
  let OperandType = "OPERAND_MEMORY";
}

def LoongArchJumpTargetAsmOperand : AsmOperandClass {
  let Name = "JumpTarget";
  let ParserMethod = "parseJumpTarget";
  let PredicateMethod = "isImm";
  let RenderMethod = "addImmOperands";
}

def jmptarget   : Operand<OtherVT> {
  let EncoderMethod = "getJumpTargetOpValue";
  let ParserMatchClass = LoongArchJumpTargetAsmOperand;
}

def brtarget    : Operand<OtherVT> {
  let EncoderMethod = "getBranchTargetOpValue";
  let OperandType = "OPERAND_PCREL";
  let DecoderMethod = "DecodeBranchTarget";
  let ParserMatchClass = LoongArchJumpTargetAsmOperand;
}

def calltarget  : Operand<iPTR> {
  let EncoderMethod = "getJumpTargetOpValue";
  let ParserMatchClass = LoongArchJumpTargetAsmOperand;
}

//
//SDNode
//
def IsGP64bit    :    Predicate<"Subtarget->is64Bit()">,
    AssemblerPredicate<(all_of Feature64Bit)>;
def IsGP32bit    :    Predicate<"!Subtarget->is64Bit()">,
    AssemblerPredicate<(all_of (not Feature64Bit))>;
def SDT_LoongArchCallSeqStart : SDCallSeqStart<[SDTCisVT<0, i32>, SDTCisVT<1, i32>]>;
def SDT_LoongArchCallSeqEnd   : SDCallSeqEnd<[SDTCisVT<0, i32>, SDTCisVT<1, i32>]>;

def LoongArchRet : SDNode<"LoongArchISD::Ret", SDTNone,
                     [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def LoongArchERet : SDNode<"LoongArchISD::ERet", SDTNone,
                      [SDNPHasChain, SDNPOptInGlue, SDNPSideEffect]>;

def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_LoongArchCallSeqStart,
                           [SDNPHasChain, SDNPSideEffect, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END", SDT_LoongArchCallSeqEnd,
                           [SDNPHasChain, SDNPSideEffect,
                            SDNPOptInGlue, SDNPOutGlue]>;
def LoongArchAddress : SDNode<"LoongArchISD::GlobalAddress", SDTIntUnaryOp>;

// Return RA.
let isReturn=1, isTerminator=1, isBarrier=1, hasCtrlDep=1, isCTI=1 in {
  def RetRA : LoongArchPseudo<(outs), (ins), [(LoongArchRet)]>;

  let hasSideEffects=1 in
  def ERet : LoongArchPseudo<(outs), (ins), [(LoongArchERet)]>;
}

let Defs = [SP], Uses = [SP], hasSideEffects = 1 in {
def ADJCALLSTACKDOWN : LoongArchPseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                                  [(callseq_start timm:$amt1, timm:$amt2)]>;
def ADJCALLSTACKUP   : LoongArchPseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                                  [(callseq_end timm:$amt1, timm:$amt2)]>;
}

class LoongArchPat<dag pattern, dag result> : Pat<pattern, result>, PredicateControl;

def SDT_LoongArchJmpLink      : SDTypeProfile<0, 1, [SDTCisVT<0, iPTR>]>;

def LoongArchJmpLink : SDNode<"LoongArchISD::JmpLink",SDT_LoongArchJmpLink,
                         [SDNPHasChain, SDNPOutGlue, SDNPOptInGlue,
                          SDNPVariadic]>;

def LoongArchTailCall : SDNode<"LoongArchISD::TailCall", SDT_LoongArchJmpLink,
                          [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

class GPR_32 { list<Predicate> GPRPredicates = [IsGP32bit]; }
class GPR_64 { list<Predicate> GPRPredicates = [IsGP64bit]; }

//===---------------------------------------------------------------------===/
// Instruction Class Templates
//===---------------------------------------------------------------------===/
///R2
class Int_Reg2<string opstr, RegisterOperand RO,
               SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj),
          !strconcat(opstr, "\t$rd, $rj"),
          [(set RO:$rd, (OpNode RO:$rj))],
          FrmR, opstr>;

class Int_Reg2_Iocsrrd<string opstr, RegisterOperand RD, RegisterOperand RS,
               SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RD:$rd), (ins RS:$rj),
          !strconcat(opstr, "\t$rd, $rj"),
          [(set RD:$rd, (OpNode RS:$rj))],
          FrmR, opstr>;

class Int_Reg2_Rdtime<string opstr, RegisterOperand RO,
               SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd, RO:$rj), (ins),
          !strconcat(opstr, "\t$rd, $rj"),
          [(set (OpNode RO:$rd, RO:$rj))],
          FrmR, opstr>;

class Int_Reg2_Iocsrwr<string opstr, RegisterOperand RD, RegisterOperand RS,
               SDPatternOperator OpNode= null_frag>
    : InstForm<(outs), (ins RD:$rd, RS:$rj),
          !strconcat(opstr, "\t$rd, $rj"),
          [(set (OpNode RD:$rd, RS:$rj))],
          FrmR, opstr>;

class Float_Reg2<string opstr, RegisterOperand RO,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$fd), (ins RO:$fj),
          !strconcat(opstr, "\t$fd, $fj"),
          [(set RO:$fd, (OpNode RO:$fj))],
          FrmFR, opstr>;

class Count1<string opstr, RegisterOperand RO,
               SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj),
          !strconcat(opstr, "\t$rd, $rj"),
          [(set RO:$rd, (OpNode (not RO:$rj)))],
          FrmR, opstr>;

class SignExtInReg<string opstr, RegisterOperand RO,
                   ValueType vt>
    : InstForm<(outs RO:$rd), (ins RO:$rj), !strconcat(opstr, "\t$rd, $rj"),
         [(set RO:$rd, (sext_inreg RO:$rj, vt))], FrmR, opstr>;

///R3
class Int_Reg3<string opstr, RegisterOperand RO,
               SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj, RO:$rk),
               !strconcat(opstr, "\t$rd, $rj, $rk"),
               [(set RO:$rd, (OpNode RO:$rj, RO:$rk))],
               FrmR, opstr>;

class Int_Reg3_Crc<string opstr, RegisterOperand RD, RegisterOperand RS,
               SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RS:$rd), (ins RD:$rj, RS:$rk),
               !strconcat(opstr, "\t$rd, $rj, $rk"),
               [(set RS:$rd, (OpNode RD:$rj, RS:$rk))],
               FrmR, opstr>;

class SetCC_R<string opstr, RegisterOperand RO,
              SDPatternOperator OpNode= null_frag>
    : InstForm<(outs GPR32Opnd:$rd), (ins RO:$rj, RO:$rk),
               !strconcat(opstr, "\t$rd, $rj, $rk"),
               [(set GPR32Opnd:$rd, (OpNode RO:$rj, RO:$rk))],
               FrmR, opstr>;

class SetCC_I<string opstr, RegisterOperand RO, Operand ImmOpnd,
              SDPatternOperator OpNode= null_frag>
    : InstForm<(outs GPR32Opnd:$rd), (ins RO:$rj, ImmOpnd:$imm12),
               !strconcat(opstr, "\t$rd, $rj, $imm12"),
               [(set GPR32Opnd:$rd, (OpNode RO:$rj, ImmOpnd:$imm12))],
               FrmR, opstr>;

class ATOMIC<string opstr, RegisterOperand RD, DAGOperand MO,
             SDPatternOperator OpNode= null_frag,
             ComplexPattern Addr = addr>
    : InstForm<(outs RD:$rd), (ins RD:$rk, MO:$addr),
               !strconcat(opstr, "\t$rd, $rk, $addr"),
               [(set RD:$rd, (OpNode RD:$rk, Addr:$addr))],
               FrmR, opstr> {
    let DecoderMethod = "DecodeMem";
    let canFoldAsLoad = 1;
    string BaseOpcode = opstr;
    let mayLoad = 1;
    let mayStore = 1;
    let Constraints = "@earlyclobber $rd";
}

class Nor<string opstr, RegisterOperand RO>
    : InstForm<(outs RO:$rd), (ins RO:$rj, RO:$rk),
          !strconcat(opstr, "\t$rd, $rj, $rk"),
          [(set RO:$rd, (not (or RO:$rj, RO:$rk)))],
          FrmR, opstr>;

class Shift_Var<string opstr, RegisterOperand RO,
                SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj, GPR32Opnd:$rk),
          !strconcat(opstr, "\t$rd, $rj, $rk"),
          [(set RO:$rd, (OpNode RO:$rj, GPR32Opnd:$rk))],
          FrmR, opstr>;

class Float_Reg3<string opstr, RegisterOperand RO,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$fd), (ins RO:$fj, RO:$fk),
          !strconcat(opstr, "\t$fd, $fj, $fk"),
          [(set RO:$fd, (OpNode RO:$fj, RO:$fk))],
          FrmR, opstr>;

class Float_Reg3_MA<string opstr, RegisterOperand RO,
                 SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$fd), (ins RO:$fj, RO:$fk),
          !strconcat(opstr, "\t$fd, $fj, $fk"),
          [(set RO:$fd, (OpNode (fabs RO:$fj), (fabs RO:$fk)))],
          FrmR, opstr>;

class Float_Int_Reg3<string opstr, RegisterOperand RD, RegisterOperand RS,
                     SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RD:$fd), (ins RS:$rj, RS:$rk),
          !strconcat(opstr, "\t$fd, $rj, $rk"),
          [(set RS:$fd, (OpNode RS:$rj, RS:$rk))],
          FrmR, opstr>;

///R4
class Mul_Reg4<string opstr, RegisterOperand RO,
               SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$fd), (ins RO:$fj, RO:$fk, RO:$fa),
          !strconcat(opstr, "\t$fd, $fj, $fk, $fa"),
          [],
          FrmFR, opstr>;

class NMul_Reg4<string opstr, RegisterOperand RO,
               SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$fd), (ins RO:$fj, RO:$fk, RO:$fa),
          !strconcat(opstr, "\t$fd, $fj, $fk, $fa"),
          [],
          FrmFR, opstr>;

///R2_IMM5
class Shift_Imm32<string opstr, RegisterOperand RO,
             SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj, uimm5:$imm5),
          !strconcat(opstr, "\t$rd, $rj, $imm5"),
          [(set RO:$rd, (OpNode RO:$rj, uimm5:$imm5))],
          FrmR, opstr>;

///R2_IMM6
class Shift_Imm64<string opstr, RegisterOperand RO,
             SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj, uimm6:$imm6),
          !strconcat(opstr, "\t$rd, $rj, $imm6"),
          [(set RO:$rd, (OpNode RO:$rj, uimm6:$imm6))],
          FrmR, opstr>;

///LOAD_STORE
class FLd<string opstr, RegisterOperand RD,DAGOperand MO,
          SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RD:$rd), (ins MO:$addr),
               !strconcat(opstr, "\t$rd, $addr"),
               [(set RD:$rd, (OpNode addrDefault:$addr))],
               FrmR, opstr> {
    let DecoderMethod = "DecodeFMem";
    let mayLoad = 1;
}

class Ld<string opstr, RegisterOperand RD, DAGOperand MO,
         SDPatternOperator OpNode= null_frag,
         ComplexPattern Addr = addr>
    : InstForm<(outs RD:$rd), (ins MO:$addr),
               !strconcat(opstr, "\t$rd, $addr"),
               [(set RD:$rd, (OpNode Addr:$addr))],
               FrmR, opstr> {
    let DecoderMethod = "DecodeMem";
    let canFoldAsLoad = 1;
    string BaseOpcode = opstr;
    let mayLoad = 1;
}

class FSt<string opstr, RegisterOperand RD, DAGOperand MO,
          SDPatternOperator OpNode= null_frag>
    : InstForm<(outs), (ins RD:$rd, MO:$addr),
               !strconcat(opstr, "\t$rd, $addr"),
               [(OpNode RD:$rd, addrDefault:$addr)],
               FrmR, opstr> {
    let DecoderMethod = "DecodeFMem";
    let mayStore = 1;
}

class St<string opstr, RegisterOperand RS, DAGOperand MO,
         SDPatternOperator OpNode= null_frag>
    : InstForm<(outs), (ins RS:$rd, MO:$addr),
               !strconcat(opstr, "\t$rd, $addr"),
               [(OpNode RS:$rd, addr:$addr)],
               FrmR, opstr> {
    let DecoderMethod = "DecodeMem";
    string BaseOpcode = opstr;
    let mayStore = 1;
}

/// R2_IMM12
class Int_Reg2_Imm12<string opstr, RegisterOperand RO, Operand ImmOpnd,
                     SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj, ImmOpnd:$imm12),
               !strconcat(opstr, "\t$rd, $rj, $imm12"),
               [(set RO:$rd, (OpNode RO:$rj, ImmOpnd:$imm12))],
               FrmR, opstr>;
class RELOC_rrii<string opstr, RegisterOperand RO, Operand ImmOpnd,
                     SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj, ImmOpnd:$imm12, ImmOpnd:$i12),
               !strconcat(opstr, "\t$rd, $rj, $imm12"),
               [(set RO:$rd, (OpNode RO:$rj, ImmOpnd:$imm12, ImmOpnd:$i12))],
               FrmR, opstr>;

///R2_IMM14
class LdPtr<string opstr, RegisterOperand RO>
    : InstForm<(outs RO:$rd), (ins mem_simm14_lsl2:$addr),
               !strconcat(opstr, "\t$rd, $addr"),
               [], FrmI, opstr>{
    let DecoderMethod = "DecodeMemSimm14";
    let canFoldAsLoad = 1;
    string BaseOpcode = opstr;
    let mayLoad = 1;
}

class StPtr<string opstr, RegisterOperand RO>
    : InstForm<(outs), (ins RO:$rd, mem_simm14_lsl2:$addr),
               !strconcat(opstr, "\t$rd, $addr"),
               [], FrmI, opstr> {
    let DecoderMethod = "DecodeMemSimm14";
    string BaseOpcode = opstr;
    let mayStore = 1;
}

///R2_IMM16
class FJirl<string opstr, DAGOperand opnd,
           RegisterOperand RO>
    : InstForm<(outs), (ins RO:$rd, RO:$rj, opnd:$offs16),
          !strconcat(opstr, "\t$rd, $rj, $offs16"),
          [], FrmJ, opstr>;

class Beq<string opstr, DAGOperand opnd, PatFrag cond_op,
          RegisterOperand RO>
    : InstForm<(outs), (ins RO:$rj, RO:$rd, opnd:$offs16),
               !strconcat(opstr, "\t$rj, $rd, $offs16"),
               [(brcond (i32 (cond_op RO:$rj, RO:$rd)), bb:$offs16)],
               FrmI, opstr> {
    let isBranch = 1;
    let isTerminator = 1;
    bit isCTI = 1;
}

///R1_IMM21
class Beqz<string opstr, DAGOperand opnd, PatFrag cond_op,
           RegisterOperand RO>
    : InstForm<(outs), (ins RO:$rj, opnd:$offs21),
          !strconcat(opstr, "\t$rj, $offs21"),
          [(brcond (i32 (cond_op RO:$rj, 0)), bb:$offs21)],
          FrmI, opstr> {
    let isBranch = 1;
    let isTerminator = 1;
    bit isCTI = 1;
}

///IMM26
class JumpFB<DAGOperand opnd, string opstr, SDPatternOperator operator,
             SDPatternOperator targetoperator> :
    InstForm<(outs), (ins opnd:$offset26), !strconcat(opstr, "\t$offset26"),
           [(operator targetoperator:$offset26)], FrmJ, opstr> {
    let isBranch = 1;
    let isTerminator=1;
    let isBarrier=1;
    let DecoderMethod = "DecodeJumpTarget";
    bit isCTI = 1;
}

/// R3_SA
class Reg3_Sa<string opstr, RegisterOperand RO, Operand ImmOpnd,
              SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj, RO:$rk, ImmOpnd:$sa),
          !strconcat(opstr, "\t$rd, $rj, $rk, $sa"),
          [(set RO:$rd, (OpNode RO:$rj, RO:$rk, ImmOpnd:$sa))],
          FrmR, opstr>;

class Reg3_SaU<string opstr, RegisterOperand RD, RegisterOperand RS, Operand ImmOpnd,
              SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RD:$rd), (ins RS:$rj, RS:$rk, ImmOpnd:$sa),
          !strconcat(opstr, "\t$rd, $rj, $rk, $sa"),
          [(set RD:$rd, (OpNode RS:$rj, RS:$rk, ImmOpnd:$sa))],
          FrmR, opstr>;

/// Assert
class Assert<string opstr, RegisterOperand RO,
             SDPatternOperator OpNode= null_frag>
    : InstForm<(outs), (ins RO:$rj, RO:$rk),
          !strconcat(opstr, "\t$rj, $rk"),
          [(set (OpNode RO:$rj, RO:$rk))],
          FrmR, opstr>;

class Code15<string opstr,
             SDPatternOperator OpNode= null_frag>
    : InstForm<(outs), (ins uimm15:$Code),
          !strconcat(opstr, "\t$Code"),
          [(set (OpNode uimm15:$Code))],
          FrmOther, opstr>;

class TrapBase<Instruction RealInst>
    : LoongArchPseudo<(outs), (ins), [(trap)]>,
      PseudoInstExpansion<(RealInst 0)> {
    let isBarrier = 1;
    let isTerminator = 1;
    let isCodeGenOnly = 1;
    let isCTI = 1;
}

class CSR<string opstr, RegisterOperand RO, Operand ImmOpnd,
          SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins ImmOpnd:$csr),
          !strconcat(opstr, "\t$rd, $csr"),
          [(set RO:$rd, (OpNode ImmOpnd:$csr))],
          FrmOther, opstr>;

class CSRW<string opstr, RegisterOperand RO, Operand ImmOpnd,
          SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$dst), (ins RO:$rd, ImmOpnd:$csr),
          !strconcat(opstr, "\t$rd, $csr"),
          [(set RO:$dst, (OpNode RO:$rd, ImmOpnd:$csr))],
          FrmOther, opstr>{
    let Constraints = "$rd = $dst";
}

class CSRX<string opstr, RegisterOperand RO, Operand ImmOpnd,
          SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$dst), (ins RO:$rd, RO:$rj, ImmOpnd:$csr),
          !strconcat(opstr, "\t$rd, $rj, $csr"),
          [(set RO:$dst, (OpNode RO:$rd, RO:$rj, ImmOpnd:$csr))],
          FrmOther, opstr>{
    let Constraints = "$rd = $dst";
}

class CAC<string opstr, RegisterOperand RO, Operand ImmOpnd,
          SDPatternOperator OpNode= null_frag>
    : InstForm<(outs), (ins uimm5:$op, RO:$rj, ImmOpnd:$si12),
          !strconcat(opstr, "\t$op, $rj, $si12"),
          [(set (OpNode uimm5:$op, RO:$rj, ImmOpnd:$si12))],
          FrmOther, opstr>;

class LEVEL<string opstr, RegisterOperand RO,
          SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj, uimm8_64:$level),
          !strconcat(opstr, "\t$rd, $rj, $level"),
          [(set RO:$rd, (OpNode RO:$rj, uimm8_64:$level))],
          FrmOther, opstr>;

class SEQ<string opstr, RegisterOperand RO,
          SDPatternOperator OpNode= null_frag>
    : InstForm<(outs), (ins RO:$rj, uimm8_64:$seq),
          !strconcat(opstr, "\t$rj, $seq"),
          [(set (OpNode RO:$rj, uimm8_64:$seq))],
          FrmOther, opstr>;

class Wait<string opstr,
          SDPatternOperator OpNode= null_frag>
    : InstForm<(outs), (ins uimm15:$hint),
          !strconcat(opstr, "\t$hint"),
          [(set (OpNode uimm15:$hint))],
          FrmOther, opstr>;

class Invtlb<string opstr, RegisterOperand RO,
          SDPatternOperator OpNode= null_frag>
    : InstForm<(outs), (ins uimm5:$op, RO:$rj, RO:$rk),
          !strconcat(opstr, "\t$op, $rj, $rk"),
          [(set (OpNode uimm5:$op, RO:$rj, RO:$rk))],
          FrmOther, opstr>;

class OP32<string opstr,
           SDPatternOperator OpNode= null_frag>
    : InstForm<(outs), (ins),
          !strconcat(opstr, ""),
          [(set (OpNode))],
          FrmOther, opstr>;

class Bar<string opstr,
          SDPatternOperator OpNode= null_frag>
    : InstForm<(outs), (ins uimm15:$hint),
          !strconcat(opstr, "\t$hint"),
          [(set (OpNode uimm15:$hint))],
          FrmOther, opstr>;

//class CA<bits<2> op, string opstr>
//    : R3_CA<op, (outs FGR64:$fd), (ins FGR64:$fj,FGR64:$fk,simm3:$ca),
//          !strconcat(opstr, "\t$fd, $fj, $fk, $ca"), NoItinerary>;

class SI16_R2<string opstr, RegisterOperand RO,
              SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj, simm16:$si16),
          !strconcat(opstr, "\t$rd, $rj, $si16"),
          [(set RO:$rd, (OpNode RO:$rj, simm16:$si16))],
          FrmR, opstr>;

class SI20<string opstr, RegisterOperand RO, Operand ImmOpnd,
           SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins ImmOpnd:$si20),
          !strconcat(opstr, "\t$rd, $si20"),
          [(set RO:$rd, (OpNode ImmOpnd:$si20))],
          FrmR, opstr>;
class RELOC_rii<string opstr, RegisterOperand RO, Operand ImmOpnd,
           SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins ImmOpnd:$si20, ImmOpnd:$i20),
          !strconcat(opstr, "\t$rd, $si20"),
          [(set RO:$rd, (OpNode ImmOpnd:$si20, ImmOpnd:$i20))],
          FrmR, opstr>;

// preld
class Preld<string opstr,Operand MemOpnd ,RegisterOperand RO,
            SDPatternOperator OpNode= null_frag>
    : InstForm<(outs), (ins RO:$rj, MemOpnd:$addr, uimm5:$hint),
          !strconcat(opstr, "\t$hint, $rj, $addr"),
          [(set (OpNode RO:$rj, MemOpnd:$addr, uimm5:$hint))],
          FrmR, opstr>;
class Preld_Raw<string opstr, RegisterOperand RO>
    : InstForm<(outs), (ins RO:$rj, simm12:$imm12, uimm5:$hint),
               !strconcat(opstr, "\t$hint, $rj, $imm12"),
               [],
               FrmR, opstr>;
class IsCall {
  bit isCall = 1;
  bit isCTI = 1;
}

class EffectiveAddress<string opstr, RegisterOperand RO>
    : InstForm<(outs RO:$rd), (ins mem_ea:$addr),
               !strconcat(opstr, "\t$rd, $addr"),
               [(set RO:$rd, addr:$addr)], FrmI,
               !strconcat(opstr, "_lea")> {
  let isCodeGenOnly = 1;
  let hasNoSchedulingInfo = 1;
  let DecoderMethod = "DecodeMem";
}

def PtrRC : Operand<iPTR> {
  let MIOperandInfo = (ops ptr_rc);
  let DecoderMethod = "DecodePtrRegisterClass";
  let ParserMatchClass = GPR32AsmOperand;
}

class Atomic2Ops<PatFrag Op, RegisterClass DRC> :
  LoongArchPseudo<(outs DRC:$dst), (ins PtrRC:$ptr, DRC:$incr),
           [(set DRC:$dst, (Op iPTR:$ptr, DRC:$incr))]>;

class Atomic2OpsPostRA<RegisterClass RC> :
  LoongArchPseudo<(outs RC:$dst), (ins PtrRC:$ptr, RC:$incr), []> {
  let mayLoad = 1;
  let mayStore = 1;
}

class Atomic2OpsSubwordPostRA<RegisterClass RC> :
  LoongArchPseudo<(outs RC:$dst), (ins PtrRC:$ptr, RC:$incr, RC:$mask, RC:$mask2,
                                RC:$shiftamnt), []>;
class AtomicCmpSwap<PatFrag Op, RegisterClass DRC> :
  LoongArchPseudo<(outs DRC:$dst), (ins PtrRC:$ptr, DRC:$cmp, DRC:$swap),
           [(set DRC:$dst, (Op iPTR:$ptr, DRC:$cmp, DRC:$swap))]>;

class AtomicCmpSwapPostRA<RegisterClass RC> :
  LoongArchPseudo<(outs RC:$dst), (ins PtrRC:$ptr, RC:$cmp, RC:$swap), []> {
  let mayLoad = 1;
  let mayStore = 1;
}

class AtomicCmpSwapSubwordPostRA<RegisterClass RC> :
  LoongArchPseudo<(outs RC:$dst), (ins PtrRC:$ptr, RC:$mask, RC:$ShiftCmpVal,
                                RC:$mask2, RC:$ShiftNewVal, RC:$ShiftAmt), []> {
  let mayLoad = 1;
  let mayStore = 1;
}

class LoongArchInstAlias<string Asm, dag Result, bit Emit = 0b1> :
  InstAlias<Asm, Result, Emit>, PredicateControl;

//===---------------------------------------------------------------------===/
// Instruction Definitions.
//===---------------------------------------------------------------------===/
///
/// R2
///

def CLO_D : Count1<"clo.d", GPR64Opnd, ctlz>, R2I<0b01000>;
def CLZ_D : Int_Reg2<"clz.d", GPR64Opnd, ctlz>, R2I<0b01001>;
def CTO_D : Count1<"cto.d", GPR64Opnd, cttz>, R2I<0b01010>;
def CTZ_D : Int_Reg2<"ctz.d", GPR64Opnd, cttz>, R2I<0b01011>;

def REVB_4H : Int_Reg2<"revb.4h", GPR64Opnd>, R2I<0b01101>; //[]
def REVB_2W : Int_Reg2<"revb.2w", GPR64Opnd>, R2I<0b01110>;
def REVB_D  : Int_Reg2<"revb.d", GPR64Opnd>, R2I<0b01111>;
def REVH_2W : Int_Reg2<"revh.2w", GPR64Opnd>, R2I<0b10000>;
def REVH_D  : Int_Reg2<"revh.d", GPR64Opnd>, R2I<0b10001>; //[]

def BITREV_8B : Int_Reg2<"bitrev.8b", GPR64Opnd>, R2I<0b10011>; //[]
def BITREV_D  : Int_Reg2<"bitrev.d", GPR64Opnd, bitreverse>, R2I<0b10101>;

def EXT_W_H : SignExtInReg<"ext.w.h", GPR64Opnd, i16>, R2I<0b10110>;
def EXT_W_B : SignExtInReg<"ext.w.b", GPR64Opnd, i8>, R2I<0b10111>;

def RDTIME_D  : Int_Reg2_Rdtime<"rdtime.d", GPR64Opnd>, R2I<0b11010>;
def RDTIMEL_W : Int_Reg2_Rdtime<"rdtimel.w", GPR64Opnd>, R2I<0b11000>;
def RDTIMEH_W : Int_Reg2_Rdtime<"rdtimeh.w", GPR64Opnd>, R2I<0b11001>;
///
/// R3
///
def ADD_D : Int_Reg3<"add.d", GPR64Opnd, add>, R3I<0b0100001>;
def SUB_D : Int_Reg3<"sub.d", GPR64Opnd, sub>, R3I<0b0100011>;

def SLT     : SetCC_R<"slt", GPR64Opnd, setlt>, R3I<0b0100100>;
def SLTU    : SetCC_R<"sltu", GPR64Opnd, setult>, R3I<0b0100101>;
def MASKEQZ : Int_Reg3<"maskeqz", GPR64Opnd>, R3I<0b0100110>; //[]
def MASKNEZ : Int_Reg3<"masknez", GPR64Opnd>, R3I<0b0100111>; //[]

def NOR   : Nor<"nor", GPR64Opnd>, R3I<0b0101000>;
def AND   : Int_Reg3<"and", GPR64Opnd, and>, R3I<0b0101001>;
def OR    : Int_Reg3<"or", GPR64Opnd, or>, R3I<0b0101010>;
def XOR   : Int_Reg3<"xor", GPR64Opnd, xor>, R3I<0b0101011>;
def ORN   : Int_Reg3<"orn", GPR64Opnd>, R3I<0b0101100>;
def ANDN  : Int_Reg3<"andn", GPR64Opnd>, R3I<0b0101101>;

def SLL_D : Shift_Var<"sll.d", GPR64Opnd, shl>, R3I<0b0110001>;
def SRL_D : Shift_Var<"srl.d", GPR64Opnd, srl>, R3I<0b0110010>;
def SRA_D : Shift_Var<"sra.d", GPR64Opnd, sra>, R3I<0b0110011>;
def ROTR_D: Shift_Var<"rotr.d", GPR64Opnd, rotr>, R3I<0b0110111>;

def MUL_D     : Int_Reg3<"mul.d", GPR64Opnd, mul>, R3I<0b0111011>;
def MULH_D    : Int_Reg3<"mulh.d", GPR64Opnd, mulhs>, R3I<0b0111100>;
def MULH_DU   : Int_Reg3<"mulh.du", GPR64Opnd, mulhu>, R3I<0b0111101>;
def MULW_D_W  : Int_Reg3<"mulw.d.w", GPR64Opnd>, R3I<0b0111110>;
def MULW_D_WU : Int_Reg3<"mulw.d.wu", GPR64Opnd>, R3I<0b0111111>;

let usesCustomInserter = 1 in {
def DIV_D  : Int_Reg3<"div.d", GPR64Opnd, sdiv>, R3I<0b1000100>;
def MOD_D  : Int_Reg3<"mod.d", GPR64Opnd, srem>, R3I<0b1000101>;
def DIV_DU : Int_Reg3<"div.du", GPR64Opnd, udiv>, R3I<0b1000110>;
def MOD_DU : Int_Reg3<"mod.du", GPR64Opnd, urem>, R3I<0b1000111>;
}

def CRC_W_D_W  : Int_Reg3_Crc<"crc.w.d.w", GPR64Opnd, GPR32Opnd, int_loongarch_crc_w_d_w>, R3I<0b1001011>;
def CRCC_W_D_W : Int_Reg3_Crc<"crcc.w.d.w", GPR64Opnd, GPR32Opnd, int_loongarch_crcc_w_d_w>, R3I<0b1001111>;
///
/// SLLI
///
def SLLI_D  : Shift_Imm64<"slli.d", GPR64Opnd, shl>, R2_IMM6<0b00>;
def SRLI_D  : Shift_Imm64<"srli.d", GPR64Opnd, srl>, R2_IMM6<0b01>;
def SRAI_D  : Shift_Imm64<"srai.d", GPR64Opnd, sra>, R2_IMM6<0b10>;
def ROTRI_D : Shift_Imm64<"rotri.d", GPR64Opnd, rotr>, R2_IMM6<0b11>;
///
/// Misc
///
def ALSL_WU    : Reg3_SaU<"alsl.wu", GPR64Opnd, GPR32Opnd, uimm2_plus1>, R3_SA2<0b00011> {
  let Pattern = [(set GPR64Opnd:$rd,
               (i64 (zext (add GPR32Opnd:$rk, (shl GPR32Opnd:$rj, immZExt2Alsl:$sa)))))];
}

def ALSL_D     : Reg3_Sa<"alsl.d", GPR64Opnd, uimm2_plus1>, R3_SA2<0b10110> {
  let Pattern = [(set GPR64Opnd:$rd,
                  (add GPR64Opnd:$rk, (shl GPR64Opnd:$rj, immZExt2Alsl:$sa)))];
}
def BYTEPICK_D : Reg3_Sa<"bytepick.d", GPR64Opnd, uimm3>, R3_SA3; //[]

def ASRTLE_D : Assert<"asrtle.d", GPR64Opnd, int_loongarch_asrtle_d>, ASSERT<0b10>;
def ASRTGT_D : Assert<"asrtgt.d", GPR64Opnd, int_loongarch_asrtgt_d>, ASSERT<0b11>;

def DBCL : Code15<"dbcl">, CODE15<0b1010101>;
def HYPCALL : Code15<"hypcall">, CODE15<0b1010111>;

///
/// R2_IMM12
///
def SLTI    : SetCC_I<"slti", GPR64Opnd, simm12, setlt>, R2_IMM12<0b000>;
def SLTUI   : SetCC_I<"sltui", GPR64Opnd, simm12, setult>, R2_IMM12<0b001>;
def ADDI_D  : Int_Reg2_Imm12<"addi.d", GPR64Opnd, simm12, add>, R2_IMM12<0b011>;
def LU52I_D : Int_Reg2_Imm12<"lu52i.d", GPR64Opnd, simm12>, R2_IMM12<0b100>;
def ANDI : Int_Reg2_Imm12<"andi", GPR64Opnd, uimm12, and>, R2_IMM12<0b101>;
def ORI  : Int_Reg2_Imm12<"ori", GPR64Opnd, uimm12, or>, R2_IMM12<0b110>;
def XORI : Int_Reg2_Imm12<"xori", GPR64Opnd, uimm12, xor>, R2_IMM12<0b111>;

///
/// Privilege Instructions
///
def CSRRD : CSR<"csrrd", GPR64Opnd, uimm14, int_loongarch_dcsrrd>, R1_CSR<0b0000000000100>;
def CSRWR : CSRW<"csrwr", GPR64Opnd, uimm14, int_loongarch_dcsrwr>, R1_CSR<0b0000100000100>;
def CSRXCHG : CSRX<"csrxchg", GPR64Opnd, uimm14, int_loongarch_dcsrxchg>, R2_CSR<0b00000100>;
def IOCSRRD_D : Int_Reg2_Iocsrrd<"iocsrrd.d", GPR64Opnd, GPR32Opnd, int_loongarch_iocsrrd_d>, R2P<0b011>;
def IOCSRWR_D : Int_Reg2_Iocsrwr<"iocsrwr.d", GPR64Opnd, GPR32Opnd, int_loongarch_iocsrwr_d>, R2P<0b111>;
def CACOP : CAC<"cacop", GPR64Opnd, simm12, int_loongarch_dcacop>, R1_CACHE;
def LDDIR : LEVEL<"lddir", GPR64Opnd>, R2_LEVEL<0b00000110010000>;
def LDPTE : SEQ<"ldpte", GPR64Opnd>, R1_SEQ<0b00000110010001>;

def IDLE : Wait<"idle">, WAIT_FM;
def INVTLB : Invtlb<"invtlb", GPR64Opnd>, R2_INVTLB;
//
def IOCSRRD_B : Int_Reg2<"iocsrrd.b", GPR64Opnd>, R2P<0b000>;
def IOCSRRD_H : Int_Reg2<"iocsrrd.h", GPR64Opnd>, R2P<0b001>;
def IOCSRRD_W : Int_Reg2<"iocsrrd.w", GPR64Opnd>, R2P<0b010>;
//
def TLBCLR   : OP32<"tlbclr", int_loongarch_tlbclr>, IMM32<0b001000>;
def TLBFLUSH : OP32<"tlbflush", int_loongarch_tlbflush>, IMM32<0b001001>;
def TLBSRCH     : OP32<"tlbsrch", int_loongarch_tlbsrch>, IMM32<0b001010>;
def TLBRD     : OP32<"tlbrd", int_loongarch_tlbrd>, IMM32<0b001011>;
def TLBWR    : OP32<"tlbwr", int_loongarch_tlbwr>, IMM32<0b001100>;
def TLBFILL    : OP32<"tlbfill", int_loongarch_tlbfill>, IMM32<0b001101>;
def ERTN     : OP32<"ertn">, IMM32<0b001110>;

///
/// R1_IMM20
///
def ADDU16I_D : SI16_R2<"addu16i.d", GPR64Opnd>, R2_SI16<0b000100>;
def LU12I_W   : SI20<"lu12i.w", GPR64Opnd, simm20>, R1_SI20<0b0001010>;
def LU32I_D   : SI20<"lu32i.d", GPR64Opnd, simm20>, R1_SI20<0b0001011>;
def PCADDI    : SI20<"pcaddi", GPR64Opnd, simm20>, R1_SI20<0b0001100>;
def PCALAU12I : SI20<"pcalau12i", GPR64Opnd, simm20>, R1_SI20<0b0001101>;
def PCADDU12I : SI20<"pcaddu12i", GPR64Opnd, simm20>, R1_SI20<0b0001110>;
def PCADDU18I : SI20<"pcaddu18i", GPR64Opnd, simm20>, R1_SI20<0b0001111>;


def BEQZ  : Beqz<"beqz", brtarget, seteq, GPR64Opnd>, R1_IMM21BEQZ<0b010000>;
def BNEZ  : Beqz<"bnez", brtarget, setne, GPR64Opnd>, R1_IMM21BEQZ<0b010001>;

def JIRL  : FJirl<"jirl", simm16, GPR64Opnd>, R2_IMM16JIRL;
let isCall = 1, isCTI=1, isCodeGenOnly = 1 in {
def JIRL_CALL  : FJirl<"jirl", simm16, GPR64Opnd>, R2_IMM16JIRL;
}

def B     : JumpFB<jmptarget, "b", br, bb>, IMM26B<0b010100>;

def BEQ   : Beq<"beq", brtarget, seteq, GPR64Opnd>, R2_IMM16BEQ<0b010110>;
def BNE   : Beq<"bne", brtarget, setne, GPR64Opnd>, R2_IMM16BEQ<0b010111>;
def BLT   : Beq<"blt", brtarget, setlt, GPR64Opnd>, R2_IMM16BEQ<0b011000>;
def BGE   : Beq<"bge", brtarget, setge, GPR64Opnd>, R2_IMM16BEQ<0b011001>;
def BLTU  : Beq<"bltu", brtarget, setult, GPR64Opnd>, R2_IMM16BEQ<0b011010>;
def BGEU  : Beq<"bgeu", brtarget, setuge, GPR64Opnd>, R2_IMM16BEQ<0b011011>;

///
/// Mem access
///
class LLBase<string opstr, RegisterOperand RO, DAGOperand MO = mem> :
  InstForm<(outs RO:$rd), (ins MO:$addr), !strconcat(opstr, "\t$rd, $addr"),
           [], FrmI, opstr> {
  let DecoderMethod = "DecodeMemSimm14";
  let mayLoad = 1;
}

class SCBase<string opstr, RegisterOperand RO, DAGOperand MO = mem> :
  InstForm<(outs RO:$dst), (ins RO:$rd, MO:$addr),
           !strconcat(opstr, "\t$rd, $addr"), [], FrmI> {
  let DecoderMethod = "DecodeMemSimm14";
  let mayStore = 1;
  let Constraints = "$rd = $dst";
}

def LL_D : LLBase<"ll.d", GPR64Opnd, mem_simm14_lsl2>, LL_SC<0b010>;
def SC_D : SCBase<"sc.d", GPR64Opnd, mem_simm14_lsl2>, LL_SC<0b011>;

def LDPTR_W : LdPtr<"ldptr.w", GPR64Opnd>, LL_SC<0b100>;
def STPTR_W : StPtr<"stptr.w", GPR64Opnd>, LL_SC<0b101>;
def LDPTR_D : LdPtr<"ldptr.d", GPR64Opnd>, LL_SC<0b110>;
def STPTR_D : StPtr<"stptr.d", GPR64Opnd>, LL_SC<0b111>;

def LD_B  : Ld<"ld.b", GPR64Opnd, mem, sextloadi8>, LOAD_STORE<0b0000>;
def LD_H  : Ld<"ld.h", GPR64Opnd, mem, sextloadi16>, LOAD_STORE<0b0001>;
def LD_W  : Ld<"ld.w", GPR64Opnd, mem, sextloadi32>, LOAD_STORE<0b0010>;
def LD_D  : Ld<"ld.d", GPR64Opnd, mem_simmptr, load>, LOAD_STORE<0b0011>;
def ST_B  : St<"st.b", GPR64Opnd, mem, truncstorei8>, LOAD_STORE<0b0100>;
def ST_H  : St<"st.h", GPR64Opnd, mem, truncstorei16>, LOAD_STORE<0b0101>;
def ST_W  : St<"st.w", GPR64Opnd, mem, truncstorei32>, LOAD_STORE<0b0110>;
def ST_D  : St<"st.d", GPR64Opnd, mem_simmptr, store>, LOAD_STORE<0b0111>;
def LD_BU : Ld<"ld.bu", GPR64Opnd, mem, zextloadi8>, LOAD_STORE<0b1000>;
def LD_HU : Ld<"ld.hu", GPR64Opnd, mem, zextloadi16>, LOAD_STORE<0b1001>;
def LD_WU : Ld<"ld.wu", GPR64Opnd, mem, zextloadi32>, LOAD_STORE<0b1010>;

def AMSWAP_W  : ATOMIC<"amswap.w", GPR32Opnd, mem>, AM<0b000000>;
def AMSWAP_D  : ATOMIC<"amswap.d", GPR64Opnd, mem>, AM<0b000001>;
def AMADD_W   : ATOMIC<"amadd.w", GPR32Opnd, mem>, AM<0b000010>;
def AMADD_D   : ATOMIC<"amadd.d", GPR64Opnd, mem>, AM<0b000011>;
def AMAND_W   : ATOMIC<"amand.w", GPR32Opnd, mem>, AM<0b000100>;
def AMAND_D   : ATOMIC<"amand.d", GPR64Opnd, mem>, AM<0b000101>;
def AMOR_W    : ATOMIC<"amor.w", GPR32Opnd, mem>, AM<0b000110>;
def AMOR_D    : ATOMIC<"amor.d", GPR64Opnd, mem>, AM<0b000111>;
def AMXOR_W   : ATOMIC<"amxor.w", GPR32Opnd, mem>, AM<0b001000>;
def AMXOR_D   : ATOMIC<"amxor.d", GPR64Opnd, mem>, AM<0b001001>;
def AMMAX_W   : ATOMIC<"ammax.w", GPR32Opnd, mem>, AM<0b001010>;
def AMMAX_D   : ATOMIC<"ammax.d", GPR64Opnd, mem>, AM<0b001011>;
def AMMIN_W   : ATOMIC<"ammin.w", GPR32Opnd, mem>, AM<0b001100>;
def AMMIN_D   : ATOMIC<"ammin.d", GPR64Opnd, mem>, AM<0b001101>;
def AMMAX_WU  : ATOMIC<"ammax.wu", GPR32Opnd, mem>, AM<0b001110>;
def AMMAX_DU  : ATOMIC<"ammax.du", GPR64Opnd, mem>, AM<0b001111>;
def AMMIN_WU  : ATOMIC<"ammin.wu", GPR32Opnd, mem>, AM<0b010000>;
def AMMIN_DU  : ATOMIC<"ammin.du", GPR64Opnd, mem>, AM<0b010001>;


def AMSWAP_DB_W  : ATOMIC<"amswap_db.w", GPR32Opnd, mem>, AM<0b010010>;
def AMSWAP_DB_D  : ATOMIC<"amswap_db.d", GPR64Opnd, mem>, AM<0b010011>;
def AMADD_DB_W   : ATOMIC<"amadd_db.w", GPR32Opnd, mem>, AM<0b010100>;
def AMADD_DB_D   : ATOMIC<"amadd_db.d", GPR64Opnd, mem>, AM<0b010101>;
def AMAND_DB_W   : ATOMIC<"amand_db.w", GPR32Opnd, mem>, AM<0b010110>;
def AMAND_DB_D   : ATOMIC<"amand_db.d", GPR64Opnd, mem>, AM<0b010111>;
def AMOR_DB_W    : ATOMIC<"amor_db.w", GPR32Opnd, mem>, AM<0b011000>;
def AMOR_DB_D    : ATOMIC<"amor_db.d", GPR64Opnd, mem>, AM<0b011001>;
def AMXOR_DB_W   : ATOMIC<"amxor_db.w", GPR32Opnd, mem>, AM<0b011010>;
def AMXOR_DB_D   : ATOMIC<"amxor_db.d", GPR64Opnd, mem>, AM<0b011011>;
def AMMAX_DB_W   : ATOMIC<"ammax_db.w", GPR32Opnd, mem>, AM<0b011100>;
def AMMAX_DB_D   : ATOMIC<"ammax_db.d", GPR64Opnd, mem>, AM<0b011101>;
def AMMIN_DB_W   : ATOMIC<"ammin_db.w", GPR32Opnd, mem>, AM<0b011110>;
def AMMIN_DB_D   : ATOMIC<"ammin_db.d", GPR64Opnd, mem>, AM<0b011111>;
def AMMAX_DB_WU  : ATOMIC<"ammax_db.wu", GPR32Opnd, mem>, AM<0b100000>;
def AMMAX_DB_DU  : ATOMIC<"ammax_db.du", GPR64Opnd, mem>, AM<0b100001>;
def AMMIN_DB_WU  : ATOMIC<"ammin_db.wu", GPR32Opnd, mem>, AM<0b100010>;
def AMMIN_DB_DU  : ATOMIC<"ammin_db.du", GPR64Opnd, mem>, AM<0b100011>;

def LDGT_B : Int_Reg3<"ldgt.b", GPR64Opnd>, R3MI<0b11110000>;
def LDGT_H : Int_Reg3<"ldgt.h", GPR64Opnd>, R3MI<0b11110001>;
def LDGT_W : Int_Reg3<"ldgt.w", GPR64Opnd>, R3MI<0b11110010>;
def LDGT_D : Int_Reg3<"ldgt.d", GPR64Opnd>, R3MI<0b11110011>;
def LDLE_B : Int_Reg3<"ldle.b", GPR64Opnd>, R3MI<0b11110100>;
def LDLE_H : Int_Reg3<"ldle.h", GPR64Opnd>, R3MI<0b11110101>;
def LDLE_W : Int_Reg3<"ldle.w", GPR64Opnd>, R3MI<0b11110110>;
def LDLE_D : Int_Reg3<"ldle.d", GPR64Opnd>, R3MI<0b11110111>;
def STGT_B : Int_Reg3<"stgt.b", GPR64Opnd>, R3MI<0b11111000>;
def STGT_H : Int_Reg3<"stgt.h", GPR64Opnd>, R3MI<0b11111001>;
def STGT_W : Int_Reg3<"stgt.w", GPR64Opnd>, R3MI<0b11111010>;
def STGT_D : Int_Reg3<"stgt.d", GPR64Opnd>, R3MI<0b11111011>;
def STLE_B : Int_Reg3<"stle.b", GPR64Opnd>, R3MI<0b11111100>;
def STLE_H : Int_Reg3<"stle.h", GPR64Opnd>, R3MI<0b11111101>;
def STLE_W : Int_Reg3<"stle.w", GPR64Opnd>, R3MI<0b11111110>;
def STLE_D : Int_Reg3<"stle.d", GPR64Opnd>, R3MI<0b11111111>;

let isCodeGenOnly = 1 in {
def PRELD  : Preld<"preld", mem, GPR64Opnd>, PRELD_FM;
}

def PRELD_Raw  : Preld_Raw<"preld", GPR64Opnd>, PRELD_FM;

let isCall=1, isCTI=1, Defs = [RA] in {
  class JumpLink<string opstr, DAGOperand opnd> :
    InstForm<(outs), (ins opnd:$target), !strconcat(opstr, "\t$target"),
             [(LoongArchJmpLink tglobaladdr:$target)], FrmJ, opstr> {
               let DecoderMethod = "DecodeJumpTarget";
             }
}
def LONG_BRANCH_PCADDU12I : LoongArchPseudo<(outs GPR64Opnd:$dst),
    (ins brtarget:$tgt), []>, GPR_64;

def LONG_BRANCH_ADDID2Op : LoongArchPseudo<(outs GPR64Opnd:$dst),
    (ins GPR64Opnd:$src, brtarget:$tgt), []>, GPR_64;

def LONG_BRANCH_ADDID : LoongArchPseudo<(outs GPR64Opnd:$dst),
    (ins GPR64Opnd:$src, brtarget:$tgt, brtarget:$baltgt), []>, GPR_64;

def LEA_ADDI_D: EffectiveAddress<"addi.d", GPR64Opnd>, LEA_ADDI_FM<0b011>, GPR_64;

class PseudoReturnBase<RegisterOperand RO> : LoongArchPseudo<(outs), (ins RO:$rs),
                                                        []> {
  let isTerminator = 1;
  let isBarrier = 1;
  let isReturn = 1;
  let isCodeGenOnly = 1;
  let hasCtrlDep = 1;
  let hasExtraSrcRegAllocReq = 1;
  bit isCTI = 1;
}

def PseudoReturn64 : PseudoReturnBase<GPR64Opnd>;
//def PseudoReturn : PseudoReturnBase<GPR32Opnd>;


let isCall=1, isCTI=1, Defs=[RA], isCodeGenOnly=1 in {
def PseudoCall : LoongArchPseudo<(outs), (ins calltarget:$target),
                                        []>;
}

def : LoongArchPat<(LoongArchJmpLink tglobaladdr:$dst),
              (PseudoCall tglobaladdr:$dst)>;

def : LoongArchPat<(LoongArchJmpLink (i32 texternalsym:$dst)),
              (PseudoCall texternalsym:$dst)>;
def : LoongArchPat<(LoongArchJmpLink (i64 texternalsym:$dst)),
              (PseudoCall texternalsym:$dst)>;

def : LoongArchPat<(LoongArchJmpLink (i64 texternalsym:$dst)),
              (PseudoCall texternalsym:$dst)>;

def BL  : JumpLink<"bl", calltarget>, FJ<0b010101>;

class IsAsCheapAsAMove {
  bit isAsCheapAsAMove = 1;
}
class LoadUpper<string opstr, RegisterOperand RO, Operand Imm>:
  InstForm<(outs RO:$rt), (ins Imm:$imm16), !strconcat(opstr, "\t$rt, $imm16"),
         [], FrmI, opstr>, IsAsCheapAsAMove {
  let hasSideEffects = 0;
  let isReMaterializable = 1;
  let mayLoad = 1;
}

let isCodeGenOnly = 1 in {
def LAPCREL   : LoadUpper<"la.pcrel", GPR64Opnd, uimm16_64_relaxed>, LUI_FM, GPR_64;
}

def NOP : LoongArchPseudo<(outs), (ins), []>,
                     PseudoInstExpansion<(ANDI ZERO_64, ZERO_64, 0)>;

def : LoongArchInstAlias<"nop", (ANDI ZERO_64, ZERO_64, 0), 1>;
def : LoongArchInstAlias<"jr $rd", (JIRL ZERO_64, GPR64Opnd:$rd, 0), 1>;
def : LoongArchInstAlias<"move $dst, $src",
                         (OR GPR64Opnd:$dst,  GPR64Opnd:$src, ZERO_64), 1>, GPR_64;

// Materialize i64 constants.
def : LoongArchPat<(i64 LU12IORIPred:$imm),
                   (ORI (LU12I_W (HI20 imm:$imm)), (LO12 imm:$imm))>, GPR_64;
def : LoongArchPat<(i64 LU12IPred:$imm), (LU12I_W (HI20 imm:$imm))>, GPR_64;
def : LoongArchPat<(i64 ORi12Pred:$imm), (ORI ZERO_64, imm:$imm)>, GPR_64;
def : LoongArchPat<(i64 immSExt12:$imm), (ADDI_D ZERO_64, imm:$imm)>, GPR_64;

def UImm12RelaxedAsmOperandClass
: UImmAsmOperandClass<12, [ConstantUImm20AsmOperandClass]> {
  let Name = "UImm12_Relaxed";
  let PredicateMethod = "isAnyImm<12>";
  let DiagnosticType = "UImm12_Relaxed";
}

def SImm12RelaxedAsmOperandClass
: SImmAsmOperandClass<12, [UImm12RelaxedAsmOperandClass]> {
  let Name = "SImm12_Relaxed";
  let PredicateMethod = "isAnyImm<12>";
  let DiagnosticType = "SImm12_Relaxed";
}

def simm12_relaxed : Operand<i32> {
  let DecoderMethod = "DecodeSImmWithOffsetAndScale<12>";
  let ParserMatchClass = !cast<AsmOperandClass>("SImm12RelaxedAsmOperandClass");
}

def : LoongArchPat<(i64 (anyext GPR32:$src)),
              (INSERT_SUBREG (i64 (IMPLICIT_DEF)), GPR32:$src, sub_32)>,GPR_64;

let usesCustomInserter = 1 in {
  def ATOMIC_LOAD_ADD_I64  : Atomic2Ops<atomic_load_add_64, GPR64>;
  def ATOMIC_LOAD_SUB_I64  : Atomic2Ops<atomic_load_sub_64, GPR64>;
  def ATOMIC_LOAD_AND_I64  : Atomic2Ops<atomic_load_and_64, GPR64>;
  def ATOMIC_LOAD_OR_I64   : Atomic2Ops<atomic_load_or_64, GPR64>;
  def ATOMIC_LOAD_XOR_I64  : Atomic2Ops<atomic_load_xor_64, GPR64>;
  def ATOMIC_LOAD_NAND_I64 : Atomic2Ops<atomic_load_nand_64, GPR64>;
  def ATOMIC_SWAP_I64      : Atomic2Ops<atomic_swap_64, GPR64>;
  def ATOMIC_CMP_SWAP_I64  : AtomicCmpSwap<atomic_cmp_swap_64, GPR64>;

  def ATOMIC_LOAD_MAX_I64  : Atomic2Ops<atomic_load_max_64, GPR64>;
  def ATOMIC_LOAD_MIN_I64  : Atomic2Ops<atomic_load_min_64, GPR64>;
  def ATOMIC_LOAD_UMAX_I64  : Atomic2Ops<atomic_load_umax_64, GPR64>;
  def ATOMIC_LOAD_UMIN_I64  : Atomic2Ops<atomic_load_umin_64, GPR64>;
}

def ATOMIC_LOAD_ADD_I64_POSTRA  : Atomic2OpsPostRA<GPR64>;
def ATOMIC_LOAD_SUB_I64_POSTRA  : Atomic2OpsPostRA<GPR64>;
def ATOMIC_LOAD_AND_I64_POSTRA  : Atomic2OpsPostRA<GPR64>;
def ATOMIC_LOAD_OR_I64_POSTRA   : Atomic2OpsPostRA<GPR64>;
def ATOMIC_LOAD_XOR_I64_POSTRA  : Atomic2OpsPostRA<GPR64>;
def ATOMIC_LOAD_NAND_I64_POSTRA : Atomic2OpsPostRA<GPR64>;

def ATOMIC_SWAP_I64_POSTRA      : Atomic2OpsPostRA<GPR64>;

def ATOMIC_CMP_SWAP_I64_POSTRA  : AtomicCmpSwapPostRA<GPR64>;

def ATOMIC_LOAD_MAX_I64_POSTRA  : Atomic2OpsPostRA<GPR64>;

def ATOMIC_LOAD_MIN_I64_POSTRA  : Atomic2OpsPostRA<GPR64>;

def ATOMIC_LOAD_UMAX_I64_POSTRA  : Atomic2OpsPostRA<GPR64>;

def ATOMIC_LOAD_UMIN_I64_POSTRA  : Atomic2OpsPostRA<GPR64>;

def : LoongArchPat<(atomic_load_8 addr:$a), (LD_B addr:$a)>, GPR_64;
def : LoongArchPat<(atomic_load_16 addr:$a), (LD_H addr:$a)>, GPR_64;
def : LoongArchPat<(atomic_load_32 addrimm14lsl2:$a), (LDPTR_W addrimm14lsl2:$a)>, GPR_64;
def : LoongArchPat<(atomic_load_32 addr:$a), (LD_W addr:$a)>, GPR_64;
def : LoongArchPat<(atomic_load_64 addrimm14lsl2:$a), (LDPTR_D addrimm14lsl2:$a)>, GPR_64;
def : LoongArchPat<(atomic_load_64 addr:$a), (LD_D addr:$a)>, GPR_64;

def : LoongArchPat<(atomic_store_8 addr:$a, GPR64:$v),
      (ST_B GPR64:$v, addr:$a)>, GPR_64;
def : LoongArchPat<(atomic_store_16 addr:$a, GPR64:$v),
      (ST_H GPR64:$v, addr:$a)>, GPR_64;
def : LoongArchPat<(atomic_store_32 addrimm14lsl2:$a, GPR64:$v),
      (STPTR_W GPR64:$v, addrimm14lsl2:$a)>, GPR_64;
def : LoongArchPat<(atomic_store_32 addr:$a, GPR64:$v),
      (ST_W GPR64:$v, addr:$a)>, GPR_64;
def : LoongArchPat<(atomic_store_64 addrimm14lsl2:$a, GPR64:$v),
      (STPTR_D GPR64:$v, addrimm14lsl2:$a)>, GPR_64;
def : LoongArchPat<(atomic_store_64 addr:$a, GPR64:$v),
      (ST_D GPR64:$v, addr:$a)>, GPR_64;

def : LoongArchPat<(bswap GPR64:$rt), (REVH_D (REVB_4H GPR64:$rt))>;

def immZExt5 : ImmLeaf<i32, [{return Imm == (Imm & 0x1f);}]>;

def immZExtRange2To64 : PatLeaf<(imm), [{
  return isUInt<7>(N->getZExtValue()) && (N->getZExtValue() >= 2) &&
         (N->getZExtValue() <= 64);
}]>;

// bstrins and bstrpick
class InsBase<string opstr, RegisterOperand RO, Operand ImmOpnd,
                   SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj, ImmOpnd:$msbd, ImmOpnd:$lsbd, RO:$src),
          !strconcat(opstr, "\t$rd, $rj, $msbd, $lsbd"),
          [(set RO:$rd, (OpNode RO:$rj, ImmOpnd:$msbd, ImmOpnd:$lsbd, RO:$src))],
          FrmR, opstr> {
  let Constraints = "$src = $rd";
 }

class InsBase_32<string opstr, RegisterOperand RO, Operand ImmOpnd,
                   SDPatternOperator OpNode= null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj, ImmOpnd:$msbw, ImmOpnd:$lsbw, RO:$src),
          !strconcat(opstr, "\t$rd, $rj, $msbw, $lsbw"),
          [(set RO:$rd, (OpNode RO:$rj, ImmOpnd:$msbw, ImmOpnd:$lsbw, RO:$src))],
          FrmR, opstr> {
  let Constraints = "$src = $rd";
}

class PickBase<string opstr, RegisterOperand RO, Operand ImmOpnd,
               SDPatternOperator Op = null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj, ImmOpnd:$msbd, ImmOpnd:$lsbd),
               !strconcat(opstr, "\t$rd, $rj, $msbd, $lsbd"),
               [(set RO:$rd, (Op RO:$rj, ImmOpnd:$msbd, ImmOpnd:$lsbd))],
               FrmR, opstr>;

class PickBase_32<string opstr, RegisterOperand RO,  Operand ImmOpnd,
                  SDPatternOperator Op = null_frag>
    : InstForm<(outs RO:$rd), (ins RO:$rj, ImmOpnd:$msbw, ImmOpnd:$lsbw),
               !strconcat(opstr, "\t$rd, $rj, $msbw, $lsbw"),
               [(set RO:$rd, (Op RO:$rj, ImmOpnd:$msbw, ImmOpnd:$lsbw))],
               FrmR, opstr>;

 def BSTRINS_D  : InsBase<"bstrins.d", GPR64Opnd, uimm6, LoongArchBstrins>,
                  INSERT_BIT64<0>;
 def BSTRPICK_D  : PickBase<"bstrpick.d", GPR64Opnd, uimm6, LoongArchBstrpick>,
                   INSERT_BIT64<1>;

let isCodeGenOnly = 1 in {
  def ZEXT64_32 : InstForm<(outs GPR64Opnd:$rd),
                         (ins GPR32Opnd:$rj, uimm6:$msbd,
                              uimm6:$lsbd),
                         "bstrpick.d $rd, $rj, $msbd, $lsbd", [], FrmR, "bstrpick.d">,
                          INSERT_BIT64<1>;
}

//32-to-64-bit extension
def : LoongArchPat<(i64 (zext GPR32:$src)), (ZEXT64_32 GPR32:$src, 31, 0)>;
def : LoongArchPat<(i64 (extloadi1  addr:$src)), (LD_B addr:$src)>,
      GPR_64;
def : LoongArchPat<(i64 (extloadi8  addr:$src)), (LD_B addr:$src)>,
      GPR_64;
def : LoongArchPat<(i64 (extloadi16 addr:$src)), (LD_H addr:$src)>,
      GPR_64;
def : LoongArchPat<(i64 (extloadi32 addr:$src)), (LD_W addr:$src)>,
      GPR_64;

class LDX_FT_LA<string opstr, RegisterOperand DRC,
                SDPatternOperator OpNode = null_frag> :
  InstForm<(outs DRC:$rd), (ins PtrRC:$rj, PtrRC:$rk),
           !strconcat(opstr, "\t$rd, $rj, $rk"),
           [(set DRC:$rd, (OpNode (add iPTR:$rj, iPTR:$rk)))],
           FrmR, opstr> {
  let AddedComplexity = 20;
  let canFoldAsLoad = 1;
  string BaseOpcode = opstr;
  let mayLoad = 1;
}

class SDX_FT_LA<string opstr, RegisterOperand DRC,
                SDPatternOperator OpNode = null_frag> :
  InstForm<(outs), (ins DRC:$rd, PtrRC:$rj, PtrRC:$rk),
           !strconcat(opstr, "\t$rd, $rj, $rk"),
           [(OpNode DRC:$rd, (add iPTR:$rj, iPTR:$rk))],
           FrmI, opstr> {
  string BaseOpcode = opstr;
  let mayStore = 1;
  let AddedComplexity = 20;
}


def LDX_B : LDX_FT_LA<"ldx.b", GPR64Opnd, sextloadi8>,
            R3MI<0b00000000>;
def LDX_H : LDX_FT_LA<"ldx.h", GPR64Opnd, sextloadi16>,
            R3MI<0b00001000>;
def LDX_W : LDX_FT_LA<"ldx.w", GPR64Opnd, sextloadi32>,
            R3MI<0b00010000>;
def LDX_D : LDX_FT_LA<"ldx.d", GPR64Opnd, load>,
            R3MI<0b00011000>;
def STX_B : SDX_FT_LA<"stx.b", GPR64Opnd, truncstorei8>,
            R3MI<0b00100000>;
def STX_H : SDX_FT_LA<"stx.h", GPR64Opnd, truncstorei16>,
            R3MI<0b00101000>;
def STX_W : SDX_FT_LA<"stx.w", GPR64Opnd, truncstorei32>,
            R3MI<0b00110000>;
def STX_D : SDX_FT_LA<"stx.d", GPR64Opnd, store>,
            R3MI<0b00111000>;
def LDX_BU : LDX_FT_LA<"ldx.bu", GPR64Opnd, extloadi8>,
             R3MI<0b01000000>;
def LDX_HU : LDX_FT_LA<"ldx.hu", GPR64Opnd, extloadi16>,
             R3MI<0b01001000>;
def LDX_WU : LDX_FT_LA<"ldx.wu", GPR64Opnd, zextloadi32>,
             R3MI<0b01010000>;

//def : LoongArchPat<(bswap GPR64:$rj), (REVH_D (REVB_4H GPR64:$rj))>;
//def : LoongArchPat<(bswap GPR64:$rj), (ROTRI_D (REVB_2W GPR64:$rj), 32)>;
def : LoongArchPat<(bswap GPR64:$rj), (REVB_D GPR64:$rj)>;

let isCodeGenOnly = 1 in {
  def SLLI_D_64_32 : Shift_Imm64<"", GPR64Opnd>, R2_IMM6<0b00>, GPR_64 {
    let imm6 = 0;
    let AsmString = "slli.d\t$rd, $rj, 32";
    let InOperandList = (ins GPR32:$rj);
    let OutOperandList = (outs GPR64:$rd);
  }

  let isMoveReg = 1, imm5 = 0,
      AsmString = "slli.w\t$rd, $rj, 0",
      OutOperandList = (outs GPR64:$rd) in {
      let InOperandList = (ins GPR32:$rj) in
        def SLLI_W_64_32 : Shift_Imm32<"", GPR32Opnd>, R2_IMM5<0b00>, GPR_64;
      let InOperandList = (ins GPR64:$rj) in
        def SLLI_W_64_64 : Shift_Imm32<"", GPR32Opnd>, R2_IMM5<0b00>, GPR_64;
  }
}

// 32-to-64-bit extension
//def : LoongArchPat<(i64 (zext GPR32:$src)), (SRLI_D (SLLI_D_64_32 GPR32:$src), 32)>, GPR_64;
def : LoongArchPat<(i64 (sext GPR32:$src)), (SLLI_W_64_32 GPR32:$src)>, GPR_64;
def : LoongArchPat<(i64 (sext_inreg GPR64:$src, i32)), (SLLI_W_64_64 GPR64:$src)>, GPR_64;

let Uses = [A0, A1], isTerminator = 1, isReturn = 1, isBarrier = 1, isCTI = 1 in {
  def LoongArcheh_return32 : LoongArchPseudo<(outs), (ins GPR32:$spoff, GPR32:$dst),
                                [(LoongArchehret GPR32:$spoff, GPR32:$dst)]>;
  def LoongArcheh_return64 : LoongArchPseudo<(outs), (ins GPR64:$spoff,GPR64:$dst),
                                [(LoongArchehret GPR64:$spoff, GPR64:$dst)]>;
}

def : LoongArchPat<(select i32:$cond, i64:$t, i64:$f),
                   (OR (MASKEQZ i64:$t, (SLLI_W_64_32 i32:$cond)),
                       (MASKNEZ i64:$f, (SLLI_W_64_32 i32:$cond)))>;
// setcc patterns
multiclass SeteqPats<RegisterClass RC, Instruction SLTiuOp, Instruction XOROp,
                     Instruction SLTuOp, Register ZEROReg> {
  def : LoongArchPat<(seteq RC:$lhs, 0),
                     (SLTiuOp RC:$lhs, 1)>;
  def : LoongArchPat<(setne RC:$lhs, 0),
                     (SLTuOp ZEROReg, RC:$lhs)>;
  def : LoongArchPat<(seteq RC:$lhs, RC:$rhs),
                     (SLTiuOp (XOROp RC:$lhs, RC:$rhs), 1)>;
  def : LoongArchPat<(setne RC:$lhs, RC:$rhs),
                     (SLTuOp ZEROReg, (XOROp RC:$lhs, RC:$rhs))>;
}

multiclass SetlePats<RegisterClass RC, Instruction XORiOp, Instruction SLTOp,
                     Instruction SLTuOp> {
  def : LoongArchPat<(setle RC:$lhs, RC:$rhs),
                     (XORiOp (SLTOp RC:$rhs, RC:$lhs), 1)>;
  def : LoongArchPat<(setule RC:$lhs, RC:$rhs),
                     (XORiOp (SLTuOp RC:$rhs, RC:$lhs), 1)>;
}

multiclass SetgtPats<RegisterClass RC, Instruction SLTOp, Instruction SLTuOp> {
  def : LoongArchPat<(setgt RC:$lhs, RC:$rhs),
                     (SLTOp RC:$rhs, RC:$lhs)>;
  def : LoongArchPat<(setugt RC:$lhs, RC:$rhs),
                     (SLTuOp RC:$rhs, RC:$lhs)>;
}

multiclass SetgePats<RegisterClass RC, Instruction XORiOp, Instruction SLTOp,
                     Instruction SLTuOp> {
  def : LoongArchPat<(setge RC:$lhs, RC:$rhs),
                     (XORiOp (SLTOp RC:$lhs, RC:$rhs), 1)>;
  def : LoongArchPat<(setuge RC:$lhs, RC:$rhs),
                     (XORiOp (SLTuOp RC:$lhs, RC:$rhs), 1)>;
}

multiclass SetgeImmPats<RegisterClass RC, Instruction XORiOp,
                        Instruction SLTiOp, Instruction SLTiuOp> {
  def : LoongArchPat<(setge RC:$lhs, immSExt12:$rhs),
                     (XORiOp (SLTiOp RC:$lhs, immSExt12:$rhs), 1)>;
  def : LoongArchPat<(setuge RC:$lhs, immSExt12:$rhs),
                     (XORiOp (SLTiuOp RC:$lhs, immSExt12:$rhs), 1)>;
}

class LoadRegImmPat<Instruction LoadInst, ValueType ValTy, PatFrag Node> :
  LoongArchPat<(ValTy (Node addrRegImm:$a)), (LoadInst addrRegImm:$a)>;

class StoreRegImmPat<Instruction StoreInst, ValueType ValTy, PatFrag Node> :
  LoongArchPat<(Node ValTy:$v, addrRegImm:$a), (StoreInst ValTy:$v, addrRegImm:$a)>;

class LoadRegImm14Lsl2Pat<Instruction LoadInst, ValueType ValTy, PatFrag Node> :
  LoongArchPat<(ValTy (Node addrimm14lsl2:$a)), (LoadInst addrimm14lsl2:$a)>;

class StoreRegImm14Lsl2Pat<Instruction StoreInst, ValueType ValTy, PatFrag Node> :
  LoongArchPat<(Node ValTy:$v, addrimm14lsl2:$a), (StoreInst ValTy:$v, addrimm14lsl2:$a)>;

// Patterns for loads/stores with a reg+imm operand.
// let AddedComplexity = 40 so that these instructions are selected instead of
// LDX/STX which needs one more register and an ANDI instruction.
let AddedComplexity = 40 in {
  def : LoadRegImmPat<LD_B, i64, sextloadi8>;
  def : LoadRegImmPat<LD_H, i64, sextloadi16>;
  def : LoadRegImmPat<LD_W, i64, sextloadi32>;
  def : LoadRegImmPat<LD_D, i64, load>;
  def : LoadRegImmPat<LD_BU, i64, zextloadi8>;
  def : LoadRegImmPat<LD_HU, i64, zextloadi16>;
  def : LoadRegImmPat<LD_WU, i64, zextloadi32>;
  def : StoreRegImmPat<ST_B, i64, truncstorei8>;
  def : StoreRegImmPat<ST_H, i64, truncstorei16>;
  def : StoreRegImmPat<ST_W, i64, truncstorei32>;
  def : StoreRegImmPat<ST_D, i64, store>;

  def : LoadRegImm14Lsl2Pat<LDPTR_W, i64, sextloadi32>;
  def : LoadRegImm14Lsl2Pat<LDPTR_D, i64, load>;
  def : StoreRegImm14Lsl2Pat<STPTR_W, i64, truncstorei32>;
  def : StoreRegImm14Lsl2Pat<STPTR_D, i64, store>;
}

//===----------------------------------------------------------------------===//
// Base Extension Support
//===----------------------------------------------------------------------===//

include "LoongArch32InstrInfo.td"
include "LoongArchInstrInfoF.td"

defm : SeteqPats<GPR64, SLTUI, XOR, SLTU, ZERO_64>, GPR_64;
defm : SetlePats<GPR64, XORI32, SLT, SLTU>, GPR_64;
defm : SetgtPats<GPR64, SLT, SLTU>, GPR_64;
defm : SetgePats<GPR64, XORI32, SLT, SLTU>, GPR_64;
defm : SetgeImmPats<GPR64, XORI32, SLTI, SLTUI>, GPR_64;

///
/// for relocation
///
let isCodeGenOnly = 1 in {
def PCADDU12I_ri : SI20<"pcaddu12i", GPR64Opnd, simm20>, R1_SI20<0b0001110>;
def PCADDU12I_rii : RELOC_rii<"pcaddu12i", GPR64Opnd, simm20>, R1_SI20<0b0001110>;
def ORI_rri : Int_Reg2_Imm12<"ori", GPR64Opnd, uimm12, or>, R2_IMM12<0b110>;
def ORI_rrii : RELOC_rrii<"ori", GPR64Opnd, uimm12>, R2_IMM12<0b110>;
def LU12I_W_ri   : SI20<"lu12i.w", GPR64Opnd, simm20>, R1_SI20<0b0001010>;
def LU32I_D_ri : SI20<"lu32i.d", GPR64Opnd, simm20>, R1_SI20<0b0001011>;
def LU32I_D_rii : RELOC_rii<"lu32i.d", GPR64Opnd, simm20>, R1_SI20<0b0001011>;
def LU52I_D_rri : Int_Reg2_Imm12<"lu52i.d", GPR64Opnd, simm12>, R2_IMM12<0b100>;
def LU52I_D_rrii : RELOC_rrii<"lu52i.d", GPR64Opnd, simm12>, R2_IMM12<0b100>;
def ADDI_D_rri : Int_Reg2_Imm12<"addi.d", GPR64Opnd, simm12, add>, R2_IMM12<0b011>;
def ADDI_D_rrii : RELOC_rrii<"addi.d", GPR64Opnd, simm12>, R2_IMM12<0b011>;
def LD_D_rri : Ld<"ld.d", GPR64Opnd, mem_simmptr, load>, LOAD_STORE<0b0011>;
def LD_D_rrii : RELOC_rrii<"ld.d", GPR64Opnd, simm12>, LOAD_STORE_RRI<0b0011>;
def ADD_D_rrr : Int_Reg3<"add.d", GPR64Opnd, add>, R3I<0b0100001>;
def LDX_D_rrr : LDX_FT_LA<"ldx.d", GPR64Opnd, load>,
                R3MI<0b00011000>;
}

//===----------------------------------------------------------------------===//
// Assembler Pseudo Instructions
//===----------------------------------------------------------------------===//
def LoadImm32 : LoongArchAsmPseudoInst<(outs GPR32Opnd:$rd),
                                       (ins uimm32_coerced:$imm32),
                                       "li.w\t$rd, $imm32">;
def LoadImm64 : LoongArchAsmPseudoInst<(outs GPR64Opnd:$rd),
                                       (ins imm64:$imm64),
                                       "li.d\t$rd, $imm64">;
// load address
def LoadAddrLocal : LoongArchAsmPseudoInst<(outs GPR64Opnd:$rd),
                                           (ins imm64:$imm64),
                                           "la.local\t$rd, $imm64">;
def LoadAddrGlobal : LoongArchAsmPseudoInst<(outs GPR64Opnd:$rd),
                                            (ins imm64:$imm64),
                                            "la.global\t$rd, $imm64">;
def LoadAddrGlobal_Alias : LoongArchAsmPseudoInst<(outs GPR64Opnd:$rd),
                                                  (ins imm64:$imm64),
                                                  "la\t$rd, $imm64">;
def LoadAddrTLS_LE : LoongArchAsmPseudoInst<(outs GPR64Opnd:$rd),
                                            (ins imm64:$imm64),
                                            "la.tls.le\t$rd, $imm64">;
def LoadAddrTLS_IE : LoongArchAsmPseudoInst<(outs GPR64Opnd:$rd),
                                            (ins imm64:$imm64),
                                            "la.tls.ie\t$rd, $imm64">;
def LoadAddrTLS_GD : LoongArchAsmPseudoInst<(outs GPR64Opnd:$rd),
                                            (ins imm64:$imm64),
                                            "la.tls.gd\t$rd, $imm64">;
def LoadAddrTLS_LD : LoongArchAsmPseudoInst<(outs GPR64Opnd:$rd),
                                            (ins imm64:$imm64),
                                            "la.tls.ld\t$rd, $imm64">;

// load address with a temp reg
def LoadAddrLocalRR : LoongArchAsmPseudoInst<(outs GPR64Opnd:$rd),
                                      (ins GPR64Opnd:$rt, imm64:$imm64),
                                      "la.local\t$rd, $rt, $imm64">;
def LoadAddrGlobalRR : LoongArchAsmPseudoInst<(outs GPR64Opnd:$rd),
                                      (ins GPR64Opnd:$rt, imm64:$imm64),
                                      "la.global\t$rd, $rt, $imm64">;
def LoadAddrTLS_IE_RR : LoongArchAsmPseudoInst<(outs GPR64Opnd:$rd),
                                      (ins GPR64Opnd:$rt, imm64:$imm64),
                                      "la.tls.ie\t$rd, $rt, $imm64">;
def LoadAddrTLS_GD_RR : LoongArchAsmPseudoInst<(outs GPR64Opnd:$rd),
                                      (ins GPR64Opnd:$rt, imm64:$imm64),
                                      "la.tls.gd\t$rd, $rt, $imm64">;
def LoadAddrTLS_LD_RR : LoongArchAsmPseudoInst<(outs GPR64Opnd:$rd),
                                      (ins GPR64Opnd:$rt, imm64:$imm64),
                                      "la.tls.ld\t$rd, $rt, $imm64">;

// trap when div zero
def PseudoTEQ : LoongArchPseudo<(outs), (ins GPR64Opnd:$rt), []>;


def : LoongArchPat<(i64 (sext (i32 (add GPR32:$src, immSExt12:$imm12)))),
              (INSERT_SUBREG (i64 (IMPLICIT_DEF)),
              (ADDI_W GPR32:$src, immSExt12:$imm12), sub_32)>;

def : LoongArchPat<(i64 (sext (i32 (add GPR32:$src, GPR32:$src2)))),
              (INSERT_SUBREG (i64 (IMPLICIT_DEF)),
              (ADD_W GPR32:$src, GPR32:$src2), sub_32)>;

def : LoongArchPat<(i64 (sext (i32 (sub GPR32:$src, GPR32:$src2)))),
              (INSERT_SUBREG (i64 (IMPLICIT_DEF)),
              (SUB_W GPR32:$src, GPR32:$src2), sub_32)>;

def : LoongArchPat<(i64 (sext (i32 (mul GPR32:$src, GPR32:$src2)))),
              (INSERT_SUBREG (i64 (IMPLICIT_DEF)),
              (MUL_W GPR32:$src, GPR32:$src2), sub_32)>;

def : LoongArchPat<(i64 (sext (i32 (shl GPR32:$src, immZExt5:$imm5)))),
              (INSERT_SUBREG (i64 (IMPLICIT_DEF)),
              (SLLI_W GPR32:$src, immZExt5:$imm5), sub_32)>;

def : LoongArchPat<(i64 (sext (i32 (shl GPR32:$src, GPR32:$src2)))),
              (INSERT_SUBREG (i64 (IMPLICIT_DEF)),
              (SLL_W GPR32:$src, GPR32:$src2), sub_32)>;

def : LoongArchPat<(i64 (sext (i32 (srl GPR32:$src, immZExt5:$imm5)))),
              (INSERT_SUBREG (i64 (IMPLICIT_DEF)),
              (SRLI_W GPR32:$src, immZExt5:$imm5), sub_32)>;

def : LoongArchPat<(i64 (sext (i32 (srl GPR32:$src, GPR32:$src2)))),
              (INSERT_SUBREG (i64 (IMPLICIT_DEF)),
              (SRL_W GPR32:$src, GPR32:$src2), sub_32)>;

def : LoongArchPat<(i64 (sext (i32 (sra GPR32:$src, immZExt5:$imm5)))),
              (INSERT_SUBREG (i64 (IMPLICIT_DEF)),
              (SRAI_W GPR32:$src, immZExt5:$imm5), sub_32)>;

def : LoongArchPat<(i64 (sext (i32 (sra GPR32:$src, GPR32:$src2)))),
              (INSERT_SUBREG (i64 (IMPLICIT_DEF)),
              (SRA_W GPR32:$src, GPR32:$src2), sub_32)>;
