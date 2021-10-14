//===-- LoongArchInstrInfo.td - Target Description for LoongArch ---*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file describes the RISC-V instructions in TableGen format.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// RISC-V specific DAG Nodes.
//===----------------------------------------------------------------------===//

// Target-independent type requirements, but with target-specific formats.
def SDT_CallSeqStart : SDCallSeqStart<[SDTCisVT<0, i32>,
                                       SDTCisVT<1, i32>]>;
def SDT_CallSeqEnd   : SDCallSeqEnd<[SDTCisVT<0, i32>,
                                     SDTCisVT<1, i32>]>;

// Target-dependent type requirements.
def SDT_LoongArchCall     : SDTypeProfile<0, -1, [SDTCisVT<0, XLenVT>]>;
def SDT_LoongArchReadCycleWide : SDTypeProfile<2, 0, [SDTCisVT<0, i32>,
                                                  SDTCisVT<1, i32>]>;

// Target-independent nodes, but with target-specific formats.
def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_CallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END", SDT_CallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

// Target-dependent nodes.
def larch_call      : SDNode<"LoongArchISD::CALL", SDT_LoongArchCall,
                             [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                              SDNPVariadic]>;
def larch_ret_flag  : SDNode<"LoongArchISD::RET_FLAG", SDTNone,
                             [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def larch_tail      : SDNode<"LoongArchISD::TAIL", SDT_LoongArchCall,
                             [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                              SDNPVariadic]>;
def larch_sllw      : SDNode<"LoongArchISD::SLLW", SDTIntShiftOp>;
def larch_sraw      : SDNode<"LoongArchISD::SRAW", SDTIntShiftOp>;
def larch_srlw      : SDNode<"LoongArchISD::SRLW", SDTIntShiftOp>;
def larch_rolw      : SDNode<"LoongArchISD::ROLW", SDTIntShiftOp>;
def larch_rorw      : SDNode<"LoongArchISD::RORW", SDTIntShiftOp>;

def larch_divw      : SDNode<"LoongArchISD::DIVW",  SDTIntBinOp>;
def larch_divuw     : SDNode<"LoongArchISD::DIVUW", SDTIntBinOp>;
def larch_remuw     : SDNode<"LoongArchISD::REMUW", SDTIntBinOp>;

def larch_read_cycle_wide : SDNode<"LoongArchISD::READ_CYCLE_WIDE",
                                   SDT_LoongArchReadCycleWide,
                                   [SDNPHasChain, SDNPSideEffect]>;

//===----------------------------------------------------------------------===//
// Operand and SDNode transformation definitions.
//===----------------------------------------------------------------------===//

class Imm32AsmOperand<string prefix, string suffix = ""> : AsmOperandClass {
  let Name = prefix # "Imm32" # suffix;
  let RenderMethod = "addImmOperands";
  let DiagnosticType = !strconcat("Invalid", Name);
}

class Imm64AsmOperand<string prefix, string suffix = ""> : AsmOperandClass {
  let Name = prefix # "Imm64" # suffix;
  let RenderMethod = "addImmOperands";
  let DiagnosticType = !strconcat("Invalid", Name);
}

class ImmAsmOperand<string prefix, int width, string suffix> : AsmOperandClass {
  let Name = prefix # "Imm" # width # suffix;
  let RenderMethod = "addImmOperands";
  let DiagnosticType = !strconcat("Invalid", Name);
}

def ImmZeroAsmOperand : AsmOperandClass {
  let Name = "ImmZero";
  let RenderMethod = "addImmOperands";
  let DiagnosticType = !strconcat("Invalid", Name);
}

class SImmAsmOperand<int width, string suffix = "">
    : ImmAsmOperand<"S", width, suffix> {
}

class UImmAsmOperand<int width, string suffix = "">
    : ImmAsmOperand<"U", width, suffix> {
}

def UImmAlslAsmOperand : ImmAsmOperand<"U", 2, "Alsl">;

def UImmLog2XLenAsmOperand : AsmOperandClass {
  let Name = "UImmLog2XLen";
  let RenderMethod = "addImmOperands";
  let DiagnosticType = "InvalidUImmLog2XLen";
}

def uimmlog2xlen : Operand<XLenVT>, ImmLeaf<XLenVT, [{
  if (Subtarget->is64Bit())
    return isUInt<6>(Imm);
  return isUInt<5>(Imm);
}]> {
  let ParserMatchClass = UImmLog2XLenAsmOperand;
  // TODO: should ensure invalid shamt is rejected when decoding.
  let DecoderMethod = "decodeUImmOperand<6>";
  let MCOperandPredicate = [{
    int64_t Imm;
    if (!MCOp.evaluateAsConstantImm(Imm))
      return false;
    if (STI.getTargetTriple().isArch64Bit())
      return  isUInt<6>(Imm);
    return isUInt<5>(Imm);
  }];
  let OperandType = "OPERAND_UIMMLOG2XLEN";
  let OperandNamespace = "LoongArchOp";
}

def uimm2 : Operand<XLenVT>, ImmLeaf<XLenVT, [{return isUInt<2>(Imm);}]> {
  let ParserMatchClass = UImmAsmOperand<2>;
  let DecoderMethod = "decodeUImmOperand<2>";
  let OperandType = "OPERAND_UIMM2";
  let OperandNamespace = "LoongArchOp";
}

def uimm2_alsl : Operand<XLenVT>,
                 ImmLeaf<XLenVT, [{return 1 <= Imm && Imm <= 4;}]> {
  let ParserMatchClass = UImmAlslAsmOperand;
  let EncoderMethod = "getImmOpValueMinus1";
  let DecoderMethod = "decodeUImmAlslOperand";
  let OperandType = "OPERAND_UIMM2_ALSL";
  let OperandNamespace = "LoongArchOp";
}

def uimm5 : Operand<XLenVT>, ImmLeaf<XLenVT, [{return isUInt<5>(Imm);}]> {
  let ParserMatchClass = UImmAsmOperand<5>;
  let DecoderMethod = "decodeUImmOperand<5>";
  let OperandType = "OPERAND_UIMM5";
  let OperandNamespace = "LoongArchOp";
}

def uimm6 : Operand<XLenVT>, ImmLeaf<XLenVT, [{return isUInt<6>(Imm);}]> {
  let ParserMatchClass = UImmAsmOperand<6>;
  let DecoderMethod = "decodeUImmOperand<6>";
  let OperandType = "OPERAND_UIMM6";
  let OperandNamespace = "LoongArchOp";
}

def uimm12 : Operand<XLenVT>, ImmLeaf<XLenVT, [{return isUInt<12>(Imm);}]> {
  let ParserMatchClass = UImmAsmOperand<12>;
  let DecoderMethod = "decodeUImmOperand<12>";
  let OperandType = "OPERAND_UIMM12";
  let OperandNamespace = "LoongArchOp";
}

def simm12 : Operand<XLenVT>, ImmLeaf<XLenVT, [{return isInt<12>(Imm);}]> {
  let ParserMatchClass = SImmAsmOperand<12>;
  let DecoderMethod = "decodeSImmOperand<12>";
  let OperandType = "OPERAND_SIMM12";
  let OperandNamespace = "LoongArchOp";
}

// A 12-bit signed immediate plus one where the imm range will be -2047~2048.
def simm12_plus1 : Operand<XLenVT>, ImmLeaf<XLenVT,
  [{return (isInt<12>(Imm) && Imm != -2048) || Imm == 2048;}]> {
  let ParserMatchClass = SImmAsmOperand<12>;
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeSImmOperand<12>";
  let MCOperandPredicate = [{
    int64_t Imm;
    if (MCOp.evaluateAsConstantImm(Imm))
      return (isInt<12>(Imm) && Imm != -2048) || Imm == 2048;
    return MCOp.isBareSymbolRef();
  }];
}

def simm14 : Operand<XLenVT>, ImmLeaf<XLenVT, [{return isInt<14>(Imm);}]> {
  let ParserMatchClass = SImmAsmOperand<14>;
  let DecoderMethod = "decodeSImmOperand<14>";
  let OperandType = "OPERAND_SIMM14";
  let OperandNamespace = "LoongArchOp";
}

def uimm15 : Operand<XLenVT>, ImmLeaf<XLenVT, [{return isUInt<15>(Imm);}]> {
  let ParserMatchClass = UImmAsmOperand<15>;
  let DecoderMethod = "decodeUImmOperand<15>";
  let OperandType = "OPERAND_UIMM15";
  let OperandNamespace = "LoongArchOp";
}

def simm16 : Operand<XLenVT>, ImmLeaf<XLenVT, [{return isInt<16>(Imm);}]> {
  let ParserMatchClass = SImmAsmOperand<16>;
  let DecoderMethod = "decodeSImmOperand<16>";
  let OperandType = "OPERAND_SIMM16";
  let OperandNamespace = "LoongArchOp";
}

def simm20 : Operand<XLenVT>, ImmLeaf<XLenVT, [{return isInt<20>(Imm);}]> {
  let ParserMatchClass = SImmAsmOperand<20>;
  let DecoderMethod = "decodeSImmOperand<20>";
  let OperandType = "OPERAND_SIMM20";
  let OperandNamespace = "LoongArchOp";
}

def simm16_lsb00 : Operand<XLenVT>,
                   ImmLeaf<XLenVT, [{return isShiftedInt<16, 2>(Imm);}]> {
  let ParserMatchClass = SImmAsmOperand<16, "Lsb00">;
  let EncoderMethod = "getImmOpValueAsr2";
  let DecoderMethod = "decodeSImmOperandAndLsl2<18>";
}

// A 18-bit signed immediate where the least significant bit is zero.
def simm18_lsb00 : Operand<XLenVT>,
                   ImmLeaf<XLenVT, [{return isShiftedInt<16, 2>(Imm);}]>;

// A 18-bit signed immediate as branch offset where the least significant
// bit is zero.
def off18_lsb00 : Operand<OtherVT> {
  let ParserMatchClass = SImmAsmOperand<18, "Lsb00">;
  let PrintMethod = "printBranchOperand";
  let EncoderMethod = "getImmOpValueAsr2";
  let DecoderMethod = "decodeSImmOperandAndLsl2<18>";
  let MCOperandPredicate = [{
    int64_t Imm;
    if (MCOp.evaluateAsConstantImm(Imm))
      return isShiftedInt<16, 2>(Imm);
    return MCOp.isBareSymbolRef();
  }];
  let OperandType = "OPERAND_PCREL";
}

class UImm20Operand : Operand<XLenVT> {
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeUImmOperand<20>";
  let MCOperandPredicate = [{
    int64_t Imm;
    if (MCOp.evaluateAsConstantImm(Imm))
      return isUInt<20>(Imm);
    return MCOp.isBareSymbolRef();
  }];
  let OperandType = "OPERAND_UIMM20";
  let OperandNamespace = "LoongArchOp";
}

// A 23-bit signed immediate where the two least significant bits are zero.
def off23_lsb00 : Operand<OtherVT> {
  let ParserMatchClass = SImmAsmOperand<23, "Lsb00">;
  let PrintMethod = "printBranchOperand";
  let EncoderMethod = "getImmOpValueAsr2";
  let DecoderMethod = "decodeSImmOperandAndLsl2<23>";
  let MCOperandPredicate = [{
    int64_t Imm;
    if (MCOp.evaluateAsConstantImm(Imm))
      return isShiftedInt<21, 2>(Imm);
    return MCOp.isBareSymbolRef();
  }];
  let OperandType = "OPERAND_PCREL";
}

// A 28-bit signed immediate where the two least significant bits are zero.
def off28_lsb00 : Operand<OtherVT> {
  let ParserMatchClass = SImmAsmOperand<28, "Lsb00">;
  let PrintMethod = "printBranchOperand";
  let EncoderMethod = "getImmOpValueAsr2";
  let DecoderMethod = "decodeSImmOperandAndLsl2<28>";
  let MCOperandPredicate = [{
    int64_t Imm;
    if (MCOp.evaluateAsConstantImm(Imm))
      return isShiftedInt<26, 2>(Imm);
    return MCOp.isBareSymbolRef();
  }];
  let OperandType = "OPERAND_PCREL";
}

// A non-zero mask can be simplified to use BSTRPICK.
def bstr_mask32 : Operand<XLenVT>, ImmLeaf<XLenVT,
  [{return Imm != 0 && isMask_32(Imm);}]>;
def bstr_mask64 : Operand<XLenVT>, ImmLeaf<XLenVT,
  [{return Imm != 0 && isMask_64(Imm);}]>;

// A non-zero mask can be simplified to use BSTRINS.
def bstrins_mask32 : Operand<XLenVT>, ImmLeaf<XLenVT,
  [{return isUInt<32>(Imm) && Imm != 0xFFFFFFFFU &&
           isShiftedMask_32(~Imm);}]>;
def bstrins_mask64 : Operand<XLenVT>, ImmLeaf<XLenVT,
  [{return (~Imm) != 0 && isShiftedMask_64(~Imm);}]>;

def BareSymbol : AsmOperandClass {
  let Name = "BareSymbol";
  let RenderMethod = "addImmOperands";
  let DiagnosticType = "InvalidBareSymbol";
  let ParserMethod = "parseBareSymbol";
}

// A bare symbol.
def bare_symbol : Operand<XLenVT> {
  let ParserMatchClass = BareSymbol;
}

def CallSymbol : AsmOperandClass {
  let Name = "CallSymbol";
  let RenderMethod = "addImmOperands";
  let DiagnosticType = "InvalidCallSymbol";
  let ParserMethod = "parseCallSymbol";
}

// A bare symbol used in call/tail only.
def call_symbol : Operand<XLenVT> {
  let ParserMatchClass = CallSymbol;
}

// A parameterized register class alternative to i32imm/i64imm from Target.td.
def ixlenimm : Operand<XLenVT>;

def imm32_li : Operand<XLenVT> {
  let ParserMatchClass = Imm32AsmOperand<"", "LI">;
}

def imm64_li : Operand<XLenVT> {
  let ParserMatchClass = Imm64AsmOperand<"", "LI">;
}

// Standalone (codegen-only) immleaf patterns.
def simm32     : ImmLeaf<XLenVT, [{return isInt<32>(Imm);}]>;
def simm32hi20 : ImmLeaf<XLenVT, [{return isShiftedInt<20, 12>(Imm);}]>;
// A mask value that won't affect significant shift bits.
def immbottomxlenset : ImmLeaf<XLenVT, [{
  if (Subtarget->is64Bit())
    return countTrailingOnes<uint64_t>(Imm) >= 6;
  return countTrailingOnes<uint64_t>(Imm) >= 5;
}]>;

// A 6-bit constant greater than 32.
def uimm6gt32 : ImmLeaf<XLenVT, [{
  return isUInt<6>(Imm) && Imm > 32;
}]>;

// Addressing modes.
// Necessary because a frameindex can't be matched directly in a pattern.
def AddrFI : ComplexPattern<iPTR, 1, "SelectAddrFI", [frameindex], []>;

// Extract least significant 12 bits from an immediate value.
def LO12 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(N->getZExtValue() & 0xfff,
                                   SDLoc(N), N->getValueType(0));
}]>;

// Extract the most significant 20 bits from an immediate value.
def HI20 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant((N->getZExtValue() >> 12) & 0xfffff,
                                   SDLoc(N), N->getValueType(0));
}]>;

// Return the negation of an immediate value.
def NegImm : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(-N->getSExtValue(), SDLoc(N),
                                   N->getValueType(0));
}]>;

// Return an immediate value minus 32.
def ImmSub32 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(N->getSExtValue() - 32, SDLoc(N),
                                   N->getValueType(0));
}]>;

// Return an immediate subtracted from XLen.
def ImmSubFromXLen : SDNodeXForm<imm, [{
  uint64_t XLen = Subtarget->getXLen();
  return CurDAG->getTargetConstant(XLen - N->getZExtValue(), SDLoc(N),
                                   N->getValueType(0));
}]>;

// Return an immediate subtracted from 32.
def ImmSubFrom32 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(32 - N->getZExtValue(), SDLoc(N),
                                   N->getValueType(0));
}]>;

def BstrMaskMsb : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(
    63 - countLeadingZeros<uint64_t>(N->getZExtValue()), SDLoc(N),
                                     N->getValueType(0));
}]>;

def BstrinsMaskLsb : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(
    countTrailingOnes<uint64_t>(N->getZExtValue()), SDLoc(N),
                                N->getValueType(0));
}]>;

def BstrinsMaskMsb32 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(
    31 - countLeadingOnes<uint32_t>(N->getZExtValue()), SDLoc(N),
                                    N->getValueType(0));
}]>;

def BstrinsMaskMsb64 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(
    63 - countLeadingOnes<uint64_t>(N->getZExtValue()), SDLoc(N),
                                    N->getValueType(0));
}]>;

def ImmShr2 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant(N->getSExtValue() >> 2, SDLoc(N),
                                   N->getValueType(0));
}]>;

//===----------------------------------------------------------------------===//
// Instruction Formats
//===----------------------------------------------------------------------===//

include "LoongArchInstrFormats.td"

//===----------------------------------------------------------------------===//
// Instruction Class Templates
//===----------------------------------------------------------------------===//

let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in
class Load_ri<bits<3> funct3, string opcodestr>
    : RVInstI<funct3, OPC_LOAD, (outs GPR:$rd), (ins GPR:$rs1, simm12:$imm12),
              opcodestr, "$rd, ${imm12}(${rs1})">;

// Operands for stores are in the order srcreg, base, offset rather than
// reflecting the order these fields are specified in the instruction
// encoding.
let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in
class Store_rri<bits<3> funct3, string opcodestr>
    : RVInstS<funct3, OPC_STORE, (outs),
              (ins GPR:$rs2, GPR:$rs1, simm12:$imm12),
              opcodestr, "$rs2, ${imm12}(${rs1})">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_ri<bits<3> funct3, string opcodestr>
    : RVInstI<funct3, OPC_OP_IMM, (outs GPR:$rd), (ins GPR:$rs1, simm12:$imm12),
              opcodestr, "$rd, $rs1, $imm12">,
      Sched<[WriteIALU, ReadIALU]>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class Shift_ri<bit arithshift, bits<3> funct3, string opcodestr>
    : RVInstIShift<arithshift, funct3, OPC_OP_IMM, (outs GPR:$rd),
                   (ins GPR:$rs1, uimmlog2xlen:$shamt), opcodestr,
                   "$rd, $rs1, $shamt">,
      Sched<[WriteShift, ReadShift]>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALU_rr<bits<7> funct7, bits<3> funct3, string opcodestr>
    : RVInstR<funct7, funct3, OPC_OP, (outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2),
              opcodestr, "$rd, $rs1, $rs2">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ShiftW_ri<bit arithshift, bits<3> funct3, string opcodestr>
    : RVInstIShiftW<arithshift, funct3, OPC_OP_IMM_32, (outs GPR:$rd),
                    (ins GPR:$rs1, uimm5:$shamt), opcodestr,
                    "$rd, $rs1, $shamt">,
      Sched<[WriteShift32, ReadShift32]>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ALUW_rr<bits<7> funct7, bits<3> funct3, string opcodestr>
    : RVInstR<funct7, funct3, OPC_OP_32, (outs GPR:$rd),
              (ins GPR:$rs1, GPR:$rs2), opcodestr, "$rd, $rs1, $rs2">;

let hasSideEffects = 1, mayLoad = 0, mayStore = 0 in
class Priv<string opcodestr, bits<7> funct7>
    : RVInstR<funct7, 0b000, OPC_SYSTEM, (outs), (ins GPR:$rs1, GPR:$rs2),
              opcodestr, "">;

// Note that the size is 8/16 because up to 2/4 32-bit instructions are
// needed to generate an arbitrary 32/64-bit immediate. However, the size
// does not really matter since PseudoLI_* is currently only used in the
// AsmParser where it gets expanded to real instructions immediately.
let hasSideEffects = 0, mayLoad = 0, mayStore = 0,
    isCodeGenOnly = 0, isAsmParserOnly = 1 in {
let Size = 8 in
def PseudoLI_W : Pseudo<(outs GPR:$rd), (ins imm32_li:$imm), [],
                        "li.w", "$rd, $imm">;
let Size = 16, Predicates = [IsLA64] in
def PseudoLI_D : Pseudo<(outs GPR:$rd), (ins imm64_li:$imm), [],
                        "li.d", "$rd, $imm">;
}

//===----------------------------------------------------------------------===//
// Pseudo-instructions and codegen patterns
//
// Naming convention: For 'generic' pattern classes, we use the naming
// convention PatTy1Ty2. For pattern classes which offer a more complex
// expansion, prefix the class name, e.g. BccPat.
//===----------------------------------------------------------------------===//

/// Generic pattern classes

class PatGprGpr<SDPatternOperator OpNode, RVInst Inst>
    : Pat<(OpNode GPR:$rs1, GPR:$rs2), (Inst GPR:$rs1, GPR:$rs2)>;
class PatGprSimm12<SDPatternOperator OpNode, RVInstI Inst>
    : Pat<(OpNode GPR:$rs1, simm12:$imm12), (Inst GPR:$rs1, simm12:$imm12)>;
class PatGprUimmLog2XLen<SDPatternOperator OpNode, RVInstIShift Inst>
    : Pat<(OpNode GPR:$rs1, uimmlog2xlen:$shamt),
          (Inst GPR:$rs1, uimmlog2xlen:$shamt)>;

/// Predicates

def IsOrAdd: PatFrag<(ops node:$A, node:$B), (or node:$A, node:$B), [{
  return isOrEquivalentToAdd(N);
}]>;
def assertsexti32 : PatFrag<(ops node:$src), (assertsext node:$src), [{
  return cast<VTSDNode>(N->getOperand(1))->getVT().bitsLE(MVT::i32);
}]>;
def sexti32 : PatFrags<(ops node:$src),
                       [(sext_inreg node:$src, i32),
                        (assertsexti32 node:$src)]>;
def assertzexti32 : PatFrag<(ops node:$src), (assertzext node:$src), [{
  return cast<VTSDNode>(N->getOperand(1))->getVT().bitsLE(MVT::i32);
}]>;
def zexti32 : PatFrags<(ops node:$src),
                       [(and node:$src, 0xffffffff),
                        (assertzexti32 node:$src)]>;

def SRLIWPat : PatFrag<(ops node:$A, node:$B),
                       (srl (and node:$A, imm), node:$B), [{
  return MatchSRLIW(N);
}]>;

// Check that it is a SLLIUW (Shift Logical Left Immediate Unsigned i32
// on LA64). Also used to optimize the same sequence without SLLIUW.
def SLLIUWPat : PatFrag<(ops node:$A, node:$B),
                        (and (shl node:$A, node:$B), imm), [{
  return MatchSLLIUW(N);
}]>;

/// Simple arithmetic operations

// Match both a plain shift and one where the shift amount is masked (this is
// typically introduced when the legalizer promotes the shift amount and
// zero-extends it). For RISC-V, the mask is unnecessary as shifts in the base
// ISA only read the least significant 5 bits (LA32I) or 6 bits (LA64I).
class shiftop<SDPatternOperator operator>
    : PatFrags<(ops node:$val, node:$count),
               [(operator node:$val, node:$count),
                (operator node:$val, (and node:$count, immbottomxlenset))]>;
class shiftopw<SDPatternOperator operator>
    : PatFrags<(ops node:$val, node:$count),
               [(operator node:$val, node:$count),
                (operator node:$val, (and node:$count, (XLenVT 31)))]>;

/// Branches and jumps

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isCodeGenOnly = 0,
    isAsmParserOnly = 0 in {
def PseudoLLA : Pseudo<(outs GPR:$dst), (ins bare_symbol:$src), [],
                       "la.local", "$dst, $src">;
def PseudoLA_TLS_LE : Pseudo<(outs GPR:$dst), (ins bare_symbol:$src), [],
                             "la.tls.le", "$dst, $src">;
}

let hasSideEffects = 0, mayLoad = 1, mayStore = 0, isCodeGenOnly = 0,
    isAsmParserOnly = 0, DisableEncoding = "$got" in {
def PseudoLA : Pseudo<(outs GPR:$dst),
                      (ins bare_symbol:$src, bare_symbol:$got), [],
                      "la.global", "$dst, $src">;
def PseudoLA_TLS_IE : Pseudo<(outs GPR:$dst),
                             (ins bare_symbol:$src, bare_symbol:$got), [],
                             "la.tls.ie", "$dst, $src">;
def PseudoLA_TLS_GD : Pseudo<(outs GPR:$dst),
                             (ins bare_symbol:$src, bare_symbol:$got), [],
                             "la.tls.gd", "$dst, $src">;
def PseudoLA_TLS_LD : Pseudo<(outs GPR:$dst),
                             (ins bare_symbol:$src, bare_symbol:$got), [],
                             "la.tls.ld", "$dst, $src">;
}

multiclass LdPat<PatFrag LoadOp, LAInst Inst> {
  def : Pat<(LoadOp GPR:$rs1), (Inst GPR:$rs1, 0)>;
  def : Pat<(LoadOp AddrFI:$rs1), (Inst AddrFI:$rs1, 0)>;
  def : Pat<(LoadOp (add GPR:$rs1, simm12:$imm12)),
            (Inst GPR:$rs1, simm12:$imm12)>;
  def : Pat<(LoadOp (add AddrFI:$rs1, simm12:$imm12)),
            (Inst AddrFI:$rs1, simm12:$imm12)>;
  def : Pat<(LoadOp (IsOrAdd AddrFI:$rs1, simm12:$imm12)),
            (Inst AddrFI:$rs1, simm12:$imm12)>;
}

/// Stores

multiclass StPat<PatFrag StoreOp, RVInst Inst, RegisterClass StTy> {
  def : Pat<(StoreOp StTy:$rs2, GPR:$rs1), (Inst StTy:$rs2, GPR:$rs1, 0)>;
  def : Pat<(StoreOp StTy:$rs2, AddrFI:$rs1), (Inst StTy:$rs2, AddrFI:$rs1, 0)>;
  def : Pat<(StoreOp StTy:$rs2, (add GPR:$rs1, simm12:$imm12)),
            (Inst StTy:$rs2, GPR:$rs1, simm12:$imm12)>;
  def : Pat<(StoreOp StTy:$rs2, (add AddrFI:$rs1, simm12:$imm12)),
            (Inst StTy:$rs2, AddrFI:$rs1, simm12:$imm12)>;
  def : Pat<(StoreOp StTy:$rs2, (IsOrAdd AddrFI:$rs1, simm12:$imm12)),
            (Inst StTy:$rs2, AddrFI:$rs1, simm12:$imm12)>;
}

/// Other pseudo-instructions

// Pessimistically assume the stack pointer will be clobbered
let Defs = [R3], Uses = [R3] in {
def ADJCALLSTACKDOWN : Pseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                              [(callseq_start timm:$amt1, timm:$amt2)]>;
def ADJCALLSTACKUP   : Pseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                              [(callseq_end timm:$amt1, timm:$amt2)]>;
} // Defs = [R3], Uses = [R3]

let hasNoSchedulingInfo = 1, hasSideEffects = 1, mayLoad = 0, mayStore = 0 in
class LA_rdtime<bits<5> funct5, string opcodestr>
    : LAInst2R<funct5, (outs GPR:$rd), (ins GPR:$rj),
               opcodestr, "$rd, $rj">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
class LA_ALU_r<bits<5> funct5, string opcodestr>
    : LAInst2R<funct5, (outs GPR:$rd), (ins GPR:$rj),
               opcodestr, "$rd, $rj">;
class LA_ALU_s20<bits<3> funct3, string opcodestr>
    : LAInst1RI20<funct3, (outs GPR:$rd), (ins simm20:$imm20),
                  opcodestr, "$rd, $imm20">;
class LA_ALU_rr<bits<7> funct7, string opcodestr>
    : LAInst3R<0, funct7, (outs GPR:$rd), (ins GPR:$rj, GPR:$rk),
               opcodestr, "$rd, $rj, $rk">;
class LA_ALU_ru5<bits<2> funct2, string opcodestr>
    : LAInst2RI5<funct2, (outs GPR:$rd), (ins GPR:$rj, uimm5:$imm5),
	             opcodestr, "$rd, $rj, $imm5">;
class LA_ALU_ru6<bits<2> funct2, string opcodestr>
    : LAInst2RI6<funct2, (outs GPR:$rd), (ins GPR:$rj, uimm6:$imm6),
	             opcodestr, "$rd, $rj, $imm6">;
class LA_ALU_rs12<bits<4> funct4, string opcodestr>
    : LAInst2RI12<0, funct4, (outs GPR:$rd), (ins GPR:$rj, simm12:$imm12),
	              opcodestr, "$rd, $rj, $imm12">;
class LA_ALU_ru12<bits<4> funct4, string opcodestr>
    : LAInst2RI12<0, funct4, (outs GPR:$rd), (ins GPR:$rj, uimm12:$imm12),
	              opcodestr, "$rd, $rj, $imm12">;
class LA_BSTRD<bits<1> funct1, string opcodestr>
    : LAInstBSTRD<funct1, (outs GPR:$rd),
                  (ins GPR:$rj, uimm6:$msbd, uimm6:$lsbd),
                  opcodestr, "$rd, $rj, $msbd, $lsbd">;
class LA_BranchCC_rri16<bits<4> funct4, string opcodestr>
    : LAInst2RI16<funct4, (outs),
                  (ins GPR:$rj, GPR:$rd, off18_lsb00:$imm16),
                  opcodestr, "$rj, $rd, $imm16">, // this is not a typo!
      Sched<[WriteJmp, ReadJmp, ReadJmp]> {
  let isBranch = 1;
  let isTerminator = 1;
}
class LA_BranchCC_ri23<bits<1> funct1, string opcodestr>
    : LAInstBR<funct1, (outs),
               (ins GPR:$rj, off23_lsb00:$imm21),
               opcodestr, "$rj, $imm21">,
      Sched<[]> {
  let isBranch = 1;
  let isTerminator = 1;
}
class LA_Branch_i26<bits<1> funct1, string opcodestr>
    : LAInstI26<funct1, (outs), (ins off28_lsb00:$imm26),
                opcodestr, "$imm26">, // this is not a typo!
      Sched<[WriteJmp, ReadJmp, ReadJmp]>;
}

let hasSideEffects = 1, mayLoad = 0, mayStore = 0 in {
class LA_Trap<bits<2> funct2, string opcodestr>
    : LAInstI15<0b000000000010101, funct2, (outs), (ins uimm15:$imm15),
                opcodestr, "$imm15">,
      Sched<[]>;
class LA_Fence<bits<2> funct2, string opcodestr>
    : LAInstI15<0b001110000111001, funct2, (outs), (ins uimm15:$imm15),
                opcodestr, "$imm15">,
      Sched<[]>;
}

let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in {
class LA_Load_rs12<bits<4> funct4, string opcodestr>
    : LAInst2RI12<0b001010, funct4,
                  (outs GPR:$rd), (ins GPR:$rj, simm12:$imm12),
                  opcodestr, "$rd, $rj, $imm12">;
class LA_Load_rs14<bits<3> funct3, string opcodestr>
    : LAInst2RI14<funct3, (outs GPR:$rd),
                  (ins GPR:$rj, simm16_lsb00:$imm14),
                  opcodestr, "$rd, $rj, $imm14">;
class LA_Load_rr<bits<4> funct4, string opcodestr>
    : LAInst3R<0b0011100000, 0,
               (outs GPR:$rd), (ins GPR:$rj, GPR:$rk),
               opcodestr, "$rd, $rj, $rk"> {
  let Inst{21-18} = funct4;
}
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in {
class LA_Store_rs12<bits<4> funct4, string opcodestr>
    : LAInst2RI12<0b001010, funct4, (outs),
                  (ins GPR:$rd, GPR:$rj, simm12:$imm12),
                  opcodestr, "$rd, $rj, $imm12">;
class LA_Store_rs14<bits<3> funct3, string opcodestr>
    : LAInst2RI14<funct3, (outs),
                  (ins GPR:$rd, GPR:$rj, simm16_lsb00:$imm14),
                  opcodestr, "$rd, $rj, $imm14">;
class LA_Store_rr<bits<4> funct4, string opcodestr>
    : LAInst3R<0b0011100000, 0,
               (outs), (ins GPR:$rd, GPR:$rj, GPR:$rk),
               opcodestr, "$rd, $rj, $rk"> {
  let Inst{21-18} = funct4;
}
}

def CLO_W       : LA_ALU_r<0b00100, "clo.w">, Sched<[]>;
def CLZ_W       : LA_ALU_r<0b00101, "clz.w">, Sched<[]>;
def CTO_W       : LA_ALU_r<0b00110, "cto.w">, Sched<[]>;
def CTZ_W       : LA_ALU_r<0b00111, "ctz.w">, Sched<[]>;
def CLO_D       : LA_ALU_r<0b01000, "clo.d">, Sched<[]>;
def CLZ_D       : LA_ALU_r<0b01001, "clz.d">, Sched<[]>;
def CTO_D       : LA_ALU_r<0b01010, "cto.d">, Sched<[]>;
def CTZ_D       : LA_ALU_r<0b01011, "ctz.d">, Sched<[]>;
def REVB_2H     : LA_ALU_r<0b01100, "revb.2h">, Sched<[]>;
def REVB_4H     : LA_ALU_r<0b01101, "revb.4h">, Sched<[]>;
def REVB_2W     : LA_ALU_r<0b01110, "revb.2w">, Sched<[]>;
def REVB_D      : LA_ALU_r<0b01111, "revb.d">, Sched<[]>;
def REVH_2W     : LA_ALU_r<0b10000, "revh.2w">, Sched<[]>;
def REVH_D      : LA_ALU_r<0b10001, "revh.d">, Sched<[]>;
def BITREV_4B   : LA_ALU_r<0b10010, "bitrev.4b">, Sched<[]>;
def BITREV_8B   : LA_ALU_r<0b10011, "bitrev.8b">, Sched<[]>;
def BITREV_W    : LA_ALU_r<0b10100, "bitrev.w">, Sched<[]>;
def BITREV_D    : LA_ALU_r<0b10101, "bitrev.d">, Sched<[]>;
def EXT_W_H     : LA_ALU_r<0b10110, "ext.w.h">, Sched<[]>;
def EXT_W_B     : LA_ALU_r<0b10111, "ext.w.b">, Sched<[]>;
def RDTIMEL_W   : LA_rdtime<0b11000, "rdtimel.w">, Sched<[]>;
def RDTIMEH_W   : LA_rdtime<0b11001, "rdtimeh.w">, Sched<[]>;
def RDTIME_D    : LA_rdtime<0b11010, "rdtime.d">, Sched<[]>;

let Predicates = [IsLA64] in
def : Pat<(readcyclecounter), (RDTIME_D R0)>;
// On LA32, ReadCycleWide will be expanded to a loop reading both
// halves of the 64-bit "cycle" CSR.
let Predicates = [IsLA32], usesCustomInserter = 1, hasNoSchedulingInfo = 1 in
def ReadCycleWide : Pseudo<(outs GPR:$lo, GPR:$hi), (ins),
                           [(set GPR:$lo, GPR:$hi, (larch_read_cycle_wide))],
                           "", "">;


let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
def ALSL_W      : LAInst3RI2<0b00010, (outs GPR:$rd),
                             (ins GPR:$rj, GPR:$rk, uimm2_alsl:$imm2),
                             "alsl.w", "$rd, $rj, $rk, $imm2">,
                  Sched<[]>;
def ALSL_WU     : LAInst3RI2<0b00011, (outs GPR:$rd),
                             (ins GPR:$rj, GPR:$rk, uimm2_alsl:$imm2),
                             "alsl.wu", "$rd, $rj, $rk, $imm2">,
                  Sched<[]>;
def BYTEPICK_W  : LAInst3RI2<0b00100, (outs GPR:$rd),
                             (ins GPR:$rj, GPR:$rk, uimm2:$imm2),
                             "bytepick.w", "$rd, $rj, $rk, $imm2">,
                  Sched<[]>;
def ALSL_D      : LAInst3RI2<0b10110, (outs GPR:$rd),
                             (ins GPR:$rj, GPR:$rk, uimm2_alsl:$imm2),
                             "alsl.d", "$rd, $rj, $rk, $imm2">,
                  Sched<[]>;
}

def ADD_W       : LA_ALU_rr<0b0100000, "add.w">, Sched<[]>;
def ADD_D       : LA_ALU_rr<0b0100001, "add.d">, Sched<[]>;
def SUB_W       : LA_ALU_rr<0b0100010, "sub.w">, Sched<[]>;
def SUB_D       : LA_ALU_rr<0b0100011, "sub.d">, Sched<[]>;
def SLT         : LA_ALU_rr<0b0100100, "slt">, Sched<[]>;
def SLTU        : LA_ALU_rr<0b0100101, "sltu">, Sched<[]>;
def MASKEQZ     : LA_ALU_rr<0b0100110, "maskeqz">, Sched<[]>;
def MASKNEZ     : LA_ALU_rr<0b0100111, "masknez">, Sched<[]>;
def NOR         : LA_ALU_rr<0b0101000, "nor">, Sched<[]>;
def AND         : LA_ALU_rr<0b0101001, "and">, Sched<[]>;
def OR          : LA_ALU_rr<0b0101010, "or">, Sched<[]>;
def XOR         : LA_ALU_rr<0b0101011, "xor">, Sched<[]>;
def ORN         : LA_ALU_rr<0b0101100, "orn">, Sched<[]>;
def ANDN        : LA_ALU_rr<0b0101101, "andn">, Sched<[]>;
def SLL_W       : LA_ALU_rr<0b0101110, "sll.w">, Sched<[]>;
def SRL_W       : LA_ALU_rr<0b0101111, "srl.w">, Sched<[]>;
def SRA_W       : LA_ALU_rr<0b0110000, "sra.w">, Sched<[]>;
def SLL_D       : LA_ALU_rr<0b0110001, "sll.d">, Sched<[]>;
def SRL_D       : LA_ALU_rr<0b0110010, "srl.d">, Sched<[]>;
def SRA_D       : LA_ALU_rr<0b0110011, "sra.d">, Sched<[]>;
def ROTR_W      : LA_ALU_rr<0b0110110, "rotr.w">, Sched<[]>;
def ROTR_D      : LA_ALU_rr<0b0110111, "rotr.d">, Sched<[]>;
def MUL_W       : LA_ALU_rr<0b0111000, "mul.w">, Sched<[]>;
def MULH_W      : LA_ALU_rr<0b0111001, "mulh.w">, Sched<[]>;
def MULH_WU     : LA_ALU_rr<0b0111010, "mulh.wu">, Sched<[]>;
def MUL_D       : LA_ALU_rr<0b0111011, "mul.d">, Sched<[]>;
def MULH_D      : LA_ALU_rr<0b0111100, "mulh.d">, Sched<[]>;
def MULH_DU     : LA_ALU_rr<0b0111101, "mulh.du">, Sched<[]>;
def MULW_D_W    : LA_ALU_rr<0b0111110, "mulw.d.w">, Sched<[]>;
def MULW_D_WU   : LA_ALU_rr<0b0111111, "mulw.d.wu">, Sched<[]>;
def DIV_W       : LA_ALU_rr<0b1000000, "div.w">, Sched<[]>;
def MOD_W       : LA_ALU_rr<0b1000001, "mod.w">, Sched<[]>;
def DIV_WU      : LA_ALU_rr<0b1000010, "div.wu">, Sched<[]>;
def MOD_WU      : LA_ALU_rr<0b1000011, "mod.wu">, Sched<[]>;
def DIV_D       : LA_ALU_rr<0b1000100, "div.d">, Sched<[]>;
def MOD_D       : LA_ALU_rr<0b1000101, "mod.d">, Sched<[]>;
def DIV_DU      : LA_ALU_rr<0b1000110, "div.du">, Sched<[]>;
def MOD_DU      : LA_ALU_rr<0b1000111, "mod.du">, Sched<[]>;

def SLLI_W      : LA_ALU_ru5<0b00, "slli.w">, Sched<[]>;
def SLLI_D      : LA_ALU_ru6<0b00, "slli.d">, Sched<[]>;
def SRLI_W      : LA_ALU_ru5<0b01, "srli.w">, Sched<[]>;
def SRLI_D      : LA_ALU_ru6<0b01, "srli.d">, Sched<[]>;
def SRAI_W      : LA_ALU_ru5<0b10, "srai.w">, Sched<[]>;
def SRAI_D      : LA_ALU_ru6<0b10, "srai.d">, Sched<[]>;
def ROTRI_W     : LA_ALU_ru5<0b11, "rotri.w">, Sched<[]>;
def ROTRI_D     : LA_ALU_ru6<0b11, "rotri.d">, Sched<[]>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
let Constraints = "$rd_dst = $rd" in
def BSTRINS_W   : LAInstBSTRW<0, (outs GPR:$rd_dst),
                  (ins GPR:$rd, GPR:$rj, uimm5:$msbw, uimm5:$lsbw),
                  "bstrins.w", "$rd, $rj, $msbw, $lsbw">, Sched<[]>;
def BSTRPICK_W  : LAInstBSTRW<1, (outs GPR:$rd),
                  (ins GPR:$rj, uimm5:$msbw, uimm5:$lsbw),
                  "bstrpick.w", "$rd, $rj, $msbw, $lsbw">, Sched<[]>;
let Constraints = "$rd_dst = $rd" in
def BSTRINS_D   : LAInstBSTRD<0, (outs GPR:$rd_dst),
                  (ins GPR:$rd, GPR:$rj, uimm6:$msbd, uimm6:$lsbd),
                  "bstrins.d", "$rd, $rj, $msbd, $lsbd">, Sched<[]>;
def BSTRPICK_D  : LAInstBSTRD<1, (outs GPR:$rd),
                  (ins GPR:$rj, uimm6:$msbd, uimm6:$lsbd),
                  "bstrpick.d", "$rd, $rj, $msbd, $lsbd">, Sched<[]>;
}

def SLTI        : LA_ALU_rs12<0b1000, "slti">, Sched<[]>;
def SLTUI       : LA_ALU_rs12<0b1001, "sltui">, Sched<[]>;
def ADDI_W      : LA_ALU_rs12<0b1010, "addi.w">, Sched<[]>;
def ADDI_D      : LA_ALU_rs12<0b1011, "addi.d">, Sched<[]>;

let isReMaterializable = 1, isAsCheapAsAMove = 1 in
def LU52I_D     : LA_ALU_rs12<0b1100, "lu52i.d">, Sched<[]>;

def ANDI        : LA_ALU_ru12<0b1101, "andi">, Sched<[]>;
def ORI         : LA_ALU_ru12<0b1110, "ori">, Sched<[]>;
def XORI        : LA_ALU_ru12<0b1111, "xori">, Sched<[]>;

def : InstAlias<"nop", (ANDI R0, R0, 0)>;

let isReMaterializable = 1, isAsCheapAsAMove = 1,
    hasSideEffects = 0, mayStore = 0, mayLoad = 0 in {
def LU12I_W     : LA_ALU_s20<0b010, "lu12i.w">, Sched<[]>;
let Constraints = "$rd_dst = $rd" in
def LU32I_D     : LAInst1RI20<0b011, (outs GPR:$rd_dst),
                              (ins GPR:$rd, simm20:$imm20),
                              "lu32i.d", "$rd, $imm20">,
                  Sched<[]>;
def PCADDI      : LAInst1RI20<0b100, (outs GPR:$rd), (ins simm20:$imm20),
                              "pcaddi", "$rd, $imm20">,
                  Sched<[]>;
def PCALAU12I   : LAInst1RI20<0b101, (outs GPR:$rd), (ins simm20:$imm20),
                              "pcalau12i", "$rd, $imm20">,
                  Sched<[]>;
def PCADDU12I   : LAInst1RI20<0b110, (outs GPR:$rd), (ins simm20:$imm20),
                              "pcaddu12i", "$rd, $imm20">,
                  Sched<[]>;
def PCADDU18I   : LAInst1RI20<0b111, (outs GPR:$rd), (ins simm20:$imm20),
                              "pcaddu18i", "$rd, $imm20">,
                  Sched<[]>;
}

def LDPTR_W     : LA_Load_rs14< 0b100, "ldptr.w">, Sched<[]>;
def STPTR_W     : LA_Store_rs14<0b101, "stptr.w">, Sched<[]>;
def LDPTR_D     : LA_Load_rs14< 0b110, "ldptr.d">, Sched<[]>;
def STPTR_D     : LA_Store_rs14<0b111, "stptr.d">, Sched<[]>;
def LD_B        : LA_Load_rs12< 0b0000, "ld.b">, Sched<[]>;
def LD_H        : LA_Load_rs12< 0b0001, "ld.h">, Sched<[]>;
def LD_W        : LA_Load_rs12< 0b0010, "ld.w">, Sched<[]>;
def LD_D        : LA_Load_rs12< 0b0011, "ld.d">, Sched<[]>;
def ST_B        : LA_Store_rs12<0b0100, "st.b">, Sched<[]>;
def ST_H        : LA_Store_rs12<0b0101, "st.h">, Sched<[]>;
def ST_W        : LA_Store_rs12<0b0110, "st.w">, Sched<[]>;
def ST_D        : LA_Store_rs12<0b0111, "st.d">, Sched<[]>;
def LD_BU       : LA_Load_rs12< 0b1000, "ld.bu">, Sched<[]>;
def LD_HU       : LA_Load_rs12< 0b1001, "ld.hu">, Sched<[]>;
def LD_WU       : LA_Load_rs12< 0b1010, "ld.wu">, Sched<[]>;

def LDX_B       : LA_Load_rr< 0b0000, "ldx.b">, Sched<[]>;
def LDX_H       : LA_Load_rr< 0b0001, "ldx.h">, Sched<[]>;
def LDX_W       : LA_Load_rr< 0b0010, "ldx.w">, Sched<[]>;
def LDX_D       : LA_Load_rr< 0b0011, "ldx.d">, Sched<[]>;
def STX_B       : LA_Store_rr<0b0100, "stx.b">, Sched<[]>;
def STX_H       : LA_Store_rr<0b0101, "stx.h">, Sched<[]>;
def STX_W       : LA_Store_rr<0b0110, "stx.w">, Sched<[]>;
def STX_D       : LA_Store_rr<0b0111, "stx.d">, Sched<[]>;
def LDX_BU      : LA_Load_rr< 0b1000, "ldx.bu">, Sched<[]>;
def LDX_HU      : LA_Load_rr< 0b1001, "ldx.hu">, Sched<[]>;
def LDX_WU      : LA_Load_rr< 0b1010, "ldx.wu">, Sched<[]>;

/// JIRL and related pseudo ops

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
let isCall = 1 in
def JIRL : LAInst2RI16<0b0011, (outs GPR:$rd),
                       (ins GPR:$rj, simm16:$imm16),
                       "jirl", "$rd, $rj, $imm16">,
           Sched<[]>;
}

def : InstAlias<"jr $rs", (JIRL R0, GPR:$rs, 0), 3>;

let isBarrier = 1, isReturn = 1, isTerminator = 1 in
def PseudoRET : Pseudo<(outs), (ins), [(larch_ret_flag)]>,
                PseudoInstExpansion<(JIRL R0, R1, 0)>;

let isCall = 1, Defs = [R1] in
def PseudoCALLIndirect : Pseudo<(outs), (ins GPR:$rs1),
                                [(larch_call GPR:$rs1)]>,
                         PseudoInstExpansion<(JIRL R1, GPR:$rs1, 0)>;

let isCall = 1, isTerminator = 1, isReturn = 1, isBarrier = 1, Uses = [R3] in
def PseudoTAILIndirect : Pseudo<(outs), (ins GPRTC:$rs1),
                                [(larch_tail GPRTC:$rs1)]>,
                         PseudoInstExpansion<(JIRL R0, GPR:$rs1, 0)>;

let isBarrier = 1, isBranch = 1, isIndirectBranch = 1, isTerminator = 1 in
def PseudoBRIND : Pseudo<(outs), (ins GPR:$rs1, simm16:$imm), []>,
                  PseudoInstExpansion<(JIRL R0, GPR:$rs1, simm16:$imm)>;

// B and BL

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
let isBarrier = 1, isBranch = 1, isTerminator = 1 in
def B           : LA_Branch_i26<0, "b">;
let isCall = 1, Defs = [R1] in
def BL          : LA_Branch_i26<1, "bl">;

let isCall = 1, isTerminator = 1, isReturn = 1, isBarrier = 1,
    Uses = [R3] in
def LAPseudoTAIL : Pseudo<(outs), (ins call_symbol:$func), []>,
                   PseudoInstExpansion<(B off28_lsb00:$func)>;

let isCall = 1, Defs = [R1] in
def LAPseudoCALL : Pseudo<(outs), (ins call_symbol:$func), []>,
                   PseudoInstExpansion<(BL off28_lsb00:$func)>;
}

def : Pat<(larch_call tglobaladdr:$func), (LAPseudoCALL tglobaladdr:$func)>;
def : Pat<(larch_call texternalsym:$func), (LAPseudoCALL texternalsym:$func)>;

def : Pat<(larch_tail (iPTR tglobaladdr:$dst)),
          (LAPseudoTAIL texternalsym:$dst)>;
def : Pat<(larch_tail (iPTR texternalsym:$dst)),
          (LAPseudoTAIL texternalsym:$dst)>;

// Conditional Branch

def BEQ         : LA_BranchCC_rri16<0b0110, "beq">;
def BNE         : LA_BranchCC_rri16<0b0111, "bne">;
def BLT         : LA_BranchCC_rri16<0b1000, "blt">;
def BGE         : LA_BranchCC_rri16<0b1001, "bge">;
def BLTU        : LA_BranchCC_rri16<0b1010, "bltu">;
def BGEU        : LA_BranchCC_rri16<0b1011, "bgeu">;

def BEQZ        : LA_BranchCC_ri23<0, "beqz">;
def BNEZ        : LA_BranchCC_ri23<1, "bnez">;

// Trap

def BREAK       : LA_Trap<0b00, "break">;
def DBCL        : LA_Trap<0b01, "dbcl">;
def SYSCALL     : LA_Trap<0b10, "syscall">;

def : Pat<(debugtrap), (BREAK 0)>;

// Fence

def DBAR        : LA_Fence<0b00, "dbar">;
def IBAR        : LA_Fence<0b01, "ibar">;

def : Pat<(atomic_fence (timm), (timm)), (DBAR 0)>;

/// FrameIndex calculations

let Predicates = [IsLA64] in {
def : Pat<(add (XLenVT AddrFI:$Rs), simm12:$imm12),
          (ADDI_D (XLenVT AddrFI:$Rs), simm12:$imm12)>;
def : Pat<(IsOrAdd (XLenVT AddrFI:$Rs), simm12:$imm12),
          (ADDI_D (XLenVT AddrFI:$Rs), simm12:$imm12)>;
}

let Predicates = [IsLA32] in {
def : Pat<(add (XLenVT AddrFI:$Rs), simm12:$imm12),
          (ADDI_W (XLenVT AddrFI:$Rs), simm12:$imm12)>;
def : Pat<(IsOrAdd (XLenVT AddrFI:$Rs), simm12:$imm12),
          (ADDI_W (XLenVT AddrFI:$Rs), simm12:$imm12)>;
}

/// Generic Patterns

class LAPatGprGpr<SDPatternOperator OpNode, LAInst Inst>
    : Pat<(OpNode GPR:$rs1, GPR:$rs2), (Inst GPR:$rs1, GPR:$rs2)>;
class LAPatGprUimm5<SDPatternOperator OpNode, LAInst Inst>
    : Pat<(OpNode GPR:$rs, uimm6:$imm),  (Inst GPR:$rs, uimm5:$imm)>;
class LAPatGprUimm6<SDPatternOperator OpNode, LAInst Inst>
    : Pat<(OpNode GPR:$rs, uimm6:$imm),  (Inst GPR:$rs, uimm6:$imm)>;
class LAPatGprSimm12<SDPatternOperator OpNode, LAInst Inst>
    : Pat<(OpNode GPR:$rs, simm12:$imm), (Inst GPR:$rs, simm12:$imm)>;
class LAPatGprUimm12<SDPatternOperator OpNode, LAInst Inst>
    : Pat<(OpNode GPR:$rs, uimm12:$imm), (Inst GPR:$rs, uimm12:$imm)>;

/// Patterns of Simple Ops

def : LAPatGprGpr<setlt, SLT>;
def : LAPatGprGpr<setult, SLTU>;
def : LAPatGprGpr<or,  OR>;
def : LAPatGprGpr<and, AND>;
def : LAPatGprGpr<xor, XOR>;

def : Pat<(and GPR:$rs1, (not GPR:$rs2)), (ANDN GPR:$rs1, GPR:$rs2)>;
def : Pat<(or  GPR:$rs1, (not GPR:$rs2)), (ORN  GPR:$rs1, GPR:$rs2)>;
def : Pat<(not (or GPR:$rs1,  GPR:$rs2)), (NOR  GPR:$rs1, GPR:$rs2)>;

def : LAPatGprSimm12<setlt, SLTI>;
def : LAPatGprSimm12<setult, SLTUI>;
def : LAPatGprUimm12<and, ANDI>;
def : LAPatGprUimm12<or,  ORI>;
def : LAPatGprUimm12<xor, XORI>;

def : InstAlias<"move $rd, $rs", (OR GPR:$rd, GPR:$rs, R0)>;

def : Pat<(sext_inreg GPR:$rs1, i8), (EXT_W_B GPR:$rs1)>;
def : Pat<(sext_inreg GPR:$rs1, i16), (EXT_W_H GPR:$rs1)>;

// Arch specific

let Predicates = [IsLA64] in {
def : Pat<(ctlz GPR:$rs1), (CLZ_D GPR:$rs1)>;
def : Pat<(cttz GPR:$rs1), (CTZ_D GPR:$rs1)>;

def : LAPatGprGpr<add, ADD_D>;
def : LAPatGprGpr<sub, SUB_D>;
def : LAPatGprSimm12<add, ADDI_D>;

def : LAPatGprGpr<mul, MUL_D>;
def : LAPatGprGpr<mulhs, MULH_D>;
def : LAPatGprGpr<mulhu, MULH_DU>;

def : LAPatGprGpr<sdiv, DIV_D>;
def : LAPatGprGpr<udiv, DIV_DU>;
def : LAPatGprGpr<srem, MOD_D>;
def : LAPatGprGpr<urem, MOD_DU>;

def : LAPatGprGpr<shiftop<shl>, SLL_D>;
def : LAPatGprGpr<shiftop<srl>, SRL_D>;
def : LAPatGprGpr<shiftop<sra>, SRA_D>;
def : LAPatGprGpr<shiftop<rotr>, ROTR_D>;
def : Pat<(shiftop<rotl> GPR:$rs1, GPR:$rs2),
          (ROTR_D GPR:$rs1, (SUB_D R0, GPR:$rs2))>;

def : LAPatGprUimm6<shl, SLLI_D>;
def : LAPatGprUimm6<srl, SRLI_D>;
def : LAPatGprUimm6<sra, SRAI_D>;
def : LAPatGprUimm6<rotr, ROTRI_D>;
def : Pat<(rotl GPR:$rs1, uimmlog2xlen:$shamt),
          (ROTRI_D GPR:$rs1, (ImmSubFromXLen uimmlog2xlen:$shamt))>;

def : Pat<(bswap GPR:$rs1), (REVB_D GPR:$rs1)>;
def : Pat<(bitreverse GPR:$rs1), (BITREV_D GPR:$rs1)>;

def : Pat<(add (shl GPR:$rs1, uimm2_alsl:$shmat), GPR:$rs2),
          (ALSL_D GPR:$rs1, GPR:$rs2, uimm2_alsl:$shmat)>;

def : Pat<(add (ctlz (and GPR:$rs1, (i64 0xFFFFFFFF))), (i64 -32)),
          (CLZ_W GPR:$rs1)>;

// computeKnownBits can't figure out that the and mask on the add result is
// unnecessary so we need to pattern match it away.
def : Pat<(and (add (ctlz (and GPR:$rs1, (i64 0xFFFFFFFF))), (i64 -32)),
               (i64 0xFFFFFFFF)),
          (CLZ_W GPR:$rs1)>;
def : Pat<(cttz (or GPR:$rs1, (i64 0x100000000))),
          (CTZ_W GPR:$rs1)>;

def : Pat<(sext_inreg GPR:$rs1, i32), (ADDI_W GPR:$rs1, 0)>;
def : Pat<(sext_inreg (add GPR:$rs1, GPR:$rs2), i32),
          (ADD_W GPR:$rs1, GPR:$rs2)>;
def : Pat<(sext_inreg (sub GPR:$rs1, GPR:$rs2), i32),
          (SUB_W GPR:$rs1, GPR:$rs2)>;
def : Pat<(sext_inreg (mul GPR:$rs1, GPR:$rs2), i32),
          (MUL_W GPR:$rs1, GPR:$rs2)>;
def : Pat<(mul (sext_inreg GPR:$rs1, i32), (sext_inreg GPR:$rs2, i32)),
          (MULW_D_W GPR:$rs1, GPR:$rs2)>;
def : Pat<(mul (and GPR:$rs1, 0xffffffff), (and GPR:$rs2, 0xffffffff)),
          (MULW_D_WU GPR:$rs1, GPR:$rs2)>;

def : LAPatGprGpr<larch_divw,  DIV_W>;
def : LAPatGprGpr<larch_divuw, DIV_WU>;
def : LAPatGprGpr<larch_remuw, MOD_WU>;

// Handle the specific cases where using DIVU/REMU would be correct and result
// in fewer instructions than emitting DIVUW/REMUW then zero-extending the
// result.
def : Pat<(and (larch_divuw (assertzexti32 GPR:$rs1),
                                (assertzexti32 GPR:$rs2)), 0xffffffff),
          (DIV_DU GPR:$rs1, GPR:$rs2)>;
def : Pat<(and (larch_remuw (assertzexti32 GPR:$rs1),
                                (assertzexti32 GPR:$rs2)), 0xffffffff),
          (MOD_DU GPR:$rs1, GPR:$rs2)>;

// Although the sexti32 operands may not have originated from an i32 srem,
// this pattern is safe as it is impossible for two sign extended inputs to
// produce a result where res[63:32]=0 and res[31]=1.
def : Pat<(srem (sexti32 GPR:$rs1), (sexti32 GPR:$rs2)),
          (MOD_WU GPR:$rs1, GPR:$rs2)>;
def : Pat<(sext_inreg (srem (sexti32 GPR:$rs1),
                            (sexti32 GPR:$rs2)), i32),
          (MOD_W GPR:$rs1, GPR:$rs2)>;

def : Pat<(sext_inreg (add GPR:$rs1, simm12:$imm12), i32),
          (ADDI_W GPR:$rs1, simm12:$imm12)>;
def : Pat<(and GPR:$rs1, bstr_mask64:$imm),
          (BSTRPICK_D $rs1, (BstrMaskMsb $imm), 0)>;
def : Pat<(and GPR:$rs1, bstrins_mask64:$imm),
          (BSTRINS_D $rs1, R0,
                     (BstrinsMaskMsb64 $imm), (BstrinsMaskLsb $imm))>;

def : LAPatGprGpr<shiftopw<larch_sllw>, SLL_W>;
def : LAPatGprGpr<shiftopw<larch_srlw>, SRL_W>;
def : LAPatGprGpr<shiftopw<larch_sraw>, SRA_W>;
def : LAPatGprGpr<shiftopw<larch_rorw>, ROTR_W>;
def : Pat<(shiftopw<larch_rolw> GPR:$rs1, GPR:$rs2),
          (ROTR_W GPR:$rs1, (SUB_W R0, GPR:$rs2))>;

def : Pat<(sext_inreg (shl GPR:$rs1, uimm5:$shamt), i32),
          (SLLI_W GPR:$rs1, uimm5:$shamt)>;
def : Pat<(SRLIWPat GPR:$rs1, uimm5:$shamt),
          (SRLI_W GPR:$rs1, uimm5:$shamt)>;
def : Pat<(srl (shl GPR:$rs1, (i64 32)), uimm6gt32:$shamt),
          (SRLI_W GPR:$rs1, (ImmSub32 uimm6gt32:$shamt))>;
def : Pat<(sra (sext_inreg GPR:$rs1, i32), uimm5:$shamt),
          (SRAI_W GPR:$rs1, uimm5:$shamt)>;
def : Pat<(sra (shl GPR:$rs1, (i64 32)), uimm6gt32:$shamt),
          (SRAI_W GPR:$rs1, (ImmSub32 uimm6gt32:$shamt))>;

def : Pat<(larch_rorw GPR:$rs1, uimm5:$imm5),
          (ROTRI_W GPR:$rs1, uimm5:$imm5)>;
def : Pat<(larch_rolw GPR:$rs1, uimmlog2xlen:$shamt),
          (ROTRI_W GPR:$rs1, (ImmSubFromXLen uimmlog2xlen:$shamt))>;

// If we're shifting a 32-bit zero extended value left by 0-31 bits, use 2
// shifts instead of 3. This can occur when unsigned is used to index an array.
def : Pat<(shl (and GPR:$rs1, 0xffffffff), uimm5:$shamt),
          (SRLI_D (SLLI_D GPR:$rs1, 32), (ImmSubFrom32 uimm5:$shamt))>;
// shl/and can appear in the other order too.
def : Pat<(SLLIUWPat GPR:$rs1, uimm5:$shamt),
          (SRLI_D (SLLI_D GPR:$rs1, 32), (ImmSubFrom32 uimm5:$shamt))>;
}

let Predicates = [IsLA32] in {
def : Pat<(ctlz GPR:$rs1), (CLZ_W GPR:$rs1)>;
def : Pat<(cttz GPR:$rs1), (CTZ_W GPR:$rs1)>;

def : LAPatGprGpr<add, ADD_W>;
def : LAPatGprGpr<sub, SUB_W>;
def : LAPatGprSimm12<add, ADDI_W>;

def : LAPatGprGpr<mul, MUL_W>;
def : LAPatGprGpr<mulhs, MULH_W>;
def : LAPatGprGpr<mulhu, MULH_WU>;

def : LAPatGprGpr<sdiv, DIV_W>;
def : LAPatGprGpr<udiv, DIV_WU>;
def : LAPatGprGpr<srem, MOD_W>;
def : LAPatGprGpr<urem, MOD_WU>;

def : LAPatGprGpr<shiftop<shl>, SLL_W>;
def : LAPatGprGpr<shiftop<srl>, SRL_W>;
def : LAPatGprGpr<shiftop<sra>, SRA_W>;
def : LAPatGprGpr<shiftop<rotr>, ROTR_W>;
def : Pat<(shiftop<rotl> GPR:$rs1, GPR:$rs2),
          (ROTR_W GPR:$rs1, (SUB_W R0, GPR:$rs2))>;

def : LAPatGprUimm5<shl, SLLI_W>;
def : LAPatGprUimm5<srl, SRLI_W>;
def : LAPatGprUimm5<sra, SRAI_W>;
def : LAPatGprUimm5<rotr, ROTRI_W>;
def : Pat<(rotl GPR:$rs1, uimm5:$shamt),
          (ROTRI_W GPR:$rs1, (ImmSubFrom32 uimm5:$shamt))>;

def : Pat<(bswap GPR:$rs1), (ROTRI_W (REVB_2H GPR:$rs1), 16)>;
def : Pat<(bitreverse GPR:$rs1), (BITREV_W GPR:$rs1)>;

def : Pat<(and GPR:$rs1, bstr_mask32:$imm),
          (BSTRPICK_W $rs1, (BstrMaskMsb $imm), 0)>;
def : Pat<(and GPR:$rs1, bstrins_mask32:$imm),
          (BSTRINS_W $rs1, R0,
                     (BstrinsMaskMsb32 $imm), (BstrinsMaskLsb $imm))>;

def : Pat<(add (shl GPR:$rs1, uimm2_alsl:$shmat), GPR:$rs2),
          (ALSL_W GPR:$rs1, GPR:$rs2, uimm2_alsl:$shmat)>;
}

/// Immediates

def : Pat<(simm12:$imm), (ADDI_W R0, simm12:$imm)>;
def : Pat<(simm32hi20:$imm), (LU12I_W (HI20 imm:$imm))>;
def : Pat<(simm32:$imm), (ORI (LU12I_W (HI20 imm:$imm)), (LO12 imm:$imm))>;

/// Loads (Arch specific)

multiclass LALdPat12<PatFrag LoadOp, LAInst Inst> {
  def : Pat<(LoadOp GPR:$rs1), (Inst GPR:$rs1, 0)>;
  def : Pat<(LoadOp AddrFI:$rs1), (Inst AddrFI:$rs1, 0)>;
  def : Pat<(LoadOp (add GPR:$rs1, simm12:$imm12)),
            (Inst GPR:$rs1, simm12:$imm12)>;
  def : Pat<(LoadOp (add AddrFI:$rs1, simm12:$imm12)),
            (Inst AddrFI:$rs1, simm12:$imm12)>;
  def : Pat<(LoadOp (IsOrAdd AddrFI:$rs1, simm12:$imm12)),
            (Inst AddrFI:$rs1, simm12:$imm12)>;
}

class LALdxPat<PatFrag LoadOp, LAInst Inst>
    : Pat<(LoadOp (add GPR:$rs1, GPR:$rs2)), (Inst GPR:$rs1, GPR:$rs2)>;

multiclass LALdPat14<PatFrag LoadOp, LAInst Inst> {
  def : Pat<(LoadOp GPR:$rs1), (Inst GPR:$rs1, 0)>;
  def : Pat<(LoadOp AddrFI:$rs1), (Inst AddrFI:$rs1, 0)>;
  def : Pat<(LoadOp (add GPR:$rs1, simm14:$imm14)),
            (Inst GPR:$rs1, simm14:$imm14)>;
  def : Pat<(LoadOp (add AddrFI:$rs1, simm14:$imm14)),
            (Inst AddrFI:$rs1, simm14:$imm14)>;
  def : Pat<(LoadOp (IsOrAdd AddrFI:$rs1, simm14:$imm14)),
            (Inst AddrFI:$rs1, simm14:$imm14)>;
}

defm : LALdPat12<sextloadi8,  LD_B>;
defm : LALdPat12<extloadi8,   LD_B>;
defm : LALdPat12<sextloadi16, LD_H>;
defm : LALdPat12<extloadi16,  LD_H>;
defm : LALdPat12<load,        LD_W>, Requires<[IsLA32]>;
defm : LALdPat12<zextloadi8,  LD_BU>;
defm : LALdPat12<zextloadi16, LD_HU>;

def  : LALdxPat<sextloadi8,   LDX_B>;
def  : LALdxPat<extloadi8,    LDX_B>;
def  : LALdxPat<sextloadi16,  LDX_H>;
def  : LALdxPat<extloadi16,   LDX_H>;
def  : LALdxPat<load,         LDX_W>, Requires<[IsLA32]>;
def  : LALdxPat<zextloadi8,   LDX_BU>;
def  : LALdxPat<zextloadi16,  LDX_HU>;

let Predicates = [IsLA64] in {
defm : LALdPat12<sextloadi32, LD_W>;
defm : LALdPat12<extloadi32,  LD_W>;
defm : LALdPat12<zextloadi32, LD_WU>;
defm : LALdPat12<load,        LD_D>;

def  : LALdxPat<sextloadi32,  LDX_W>;
def  : LALdxPat<extloadi32,   LDX_W>;
def  : LALdxPat<zextloadi32,  LDX_WU>;
def  : LALdxPat<load,         LDX_D>;
}

/// Store (Arch specific)

multiclass LAStPat12<PatFrag StoreOp, LAInst Inst, RegisterClass StTy> {
  def : Pat<(StoreOp StTy:$rs2, GPR:$rs1), (Inst StTy:$rs2, GPR:$rs1, 0)>;
  def : Pat<(StoreOp StTy:$rs2, AddrFI:$rs1), (Inst StTy:$rs2, AddrFI:$rs1, 0)>;
  def : Pat<(StoreOp StTy:$rs2, (add GPR:$rs1, simm12:$imm12)),
            (Inst StTy:$rs2, GPR:$rs1, simm12:$imm12)>;
  def : Pat<(StoreOp StTy:$rs2, (add AddrFI:$rs1, simm12:$imm12)),
            (Inst StTy:$rs2, AddrFI:$rs1, simm12:$imm12)>;
  def : Pat<(StoreOp StTy:$rs2, (IsOrAdd AddrFI:$rs1, simm12:$imm12)),
            (Inst StTy:$rs2, AddrFI:$rs1, simm12:$imm12)>;
}

class LAStxPat<PatFrag StoreOp, LAInst Inst, RegisterClass StTy>
    : Pat<(StoreOp StTy:$rd, (add GPR:$rs1, GPR:$rs2)),
          (Inst StTy:$rd, GPR:$rs1, GPR:$rs2)>;

defm : LAStPat12<truncstorei8, ST_B, GPR>;
defm : LAStPat12<truncstorei16, ST_H, GPR>;
defm : LAStPat12<store, ST_W, GPR>, Requires<[IsLA32]>;
def  : LAStxPat<truncstorei8, STX_B, GPR>;
def  : LAStxPat<truncstorei16, STX_H, GPR>;
def  : LAStxPat<store, STX_W, GPR>, Requires<[IsLA32]>;

let Predicates = [IsLA64] in {
defm : LAStPat12<truncstorei32, ST_W, GPR>;
defm : LAStPat12<store, ST_D, GPR>;
def  : LAStxPat<truncstorei32, STX_W, GPR>;
def  : LAStxPat<store, STX_D, GPR>;
}

///
// Define pattern expansions for setcc operations that aren't directly
// handled by a LoongArch instruction.

def : Pat<(seteq GPR:$rs1, 0), (SLTUI GPR:$rs1, 1)>;
def : Pat<(seteq GPR:$rs1, GPR:$rs2), (SLTUI (XOR GPR:$rs1, GPR:$rs2), 1)>;
def : Pat<(setne GPR:$rs1, 0), (SLTU R0, GPR:$rs1)>;
def : Pat<(setne GPR:$rs1, GPR:$rs2), (SLTU R0, (XOR GPR:$rs1, GPR:$rs2))>;
def : Pat<(setugt GPR:$rs1, GPR:$rs2), (SLTU GPR:$rs2, GPR:$rs1)>;
def : Pat<(setuge GPR:$rs1, GPR:$rs2), (XORI (SLTU GPR:$rs1, GPR:$rs2), 1)>;
def : Pat<(setule GPR:$rs1, GPR:$rs2), (XORI (SLTU GPR:$rs2, GPR:$rs1), 1)>;
def : Pat<(setgt GPR:$rs1, GPR:$rs2), (SLT GPR:$rs2, GPR:$rs1)>;
def : Pat<(setge GPR:$rs1, GPR:$rs2), (XORI (SLT GPR:$rs1, GPR:$rs2), 1)>;
def : Pat<(setle GPR:$rs1, GPR:$rs2), (XORI (SLT GPR:$rs2, GPR:$rs1), 1)>;

// Arch Specific

let Predicates = [IsLA64] in {
def : Pat<(seteq GPR:$rs1, simm12_plus1:$imm12),
          (SLTUI (ADDI_D GPR:$rs1, (NegImm simm12_plus1:$imm12)), 1)>;
def : Pat<(setne GPR:$rs1, simm12_plus1:$imm12),
          (SLTU R0, (ADDI_D GPR:$rs1, (NegImm simm12_plus1:$imm12)))>;
}

let Predicates = [IsLA32] in {
def : Pat<(seteq GPR:$rs1, simm12_plus1:$imm12),
          (SLTUI (ADDI_W GPR:$rs1, (NegImm simm12_plus1:$imm12)), 1)>;
def : Pat<(setne GPR:$rs1, simm12_plus1:$imm12),
          (SLTU R0, (ADDI_W GPR:$rs1, (NegImm simm12_plus1:$imm12)))>;
}

def : Pat<(brind GPR:$rs1), (PseudoBRIND GPR:$rs1, 0)>;
def : Pat<(brind (add GPR:$rs1, simm18_lsb00:$imm16)),
          (PseudoBRIND GPR:$rs1, (ImmShr2 simm18_lsb00:$imm16))>;

def : Pat<(br bb:$imm26), (B off28_lsb00:$imm26)>;

class LABccPat<PatFrag CondOp, LA_BranchCC_rri16 Inst>
    : Pat<(brcond (XLenVT (CondOp GPR:$rj, GPR:$rd)), bb:$imm16),
          (Inst GPR:$rj, GPR:$rd, bb:$imm16)>;

def : LABccPat<seteq, BEQ>;
def : LABccPat<setne, BNE>;
def : LABccPat<setlt, BLT>;
def : LABccPat<setge, BGE>;
def : LABccPat<setult, BLTU>;
def : LABccPat<setuge, BGEU>;

class LABccSwapPat<PatFrag CondOp, LA_BranchCC_rri16 InstBcc>
    : Pat<(brcond (XLenVT (CondOp GPR:$rj, GPR:$rd)), bb:$imm16),
          (InstBcc GPR:$rd, GPR:$rj, bb:$imm16)>;

def : LABccSwapPat<setgt, BLT>;
def : LABccSwapPat<setle, BGE>;
def : LABccSwapPat<setugt, BLTU>;
def : LABccSwapPat<setule, BGEU>;

def : Pat<(brcond GPR:$cond, bb:$imm16), (BNE GPR:$cond, R0, bb:$imm16)>;
def : Pat<(brcond (XLenVT (xor GPR:$cond, 1)), bb:$imm16),
          (BEQ GPR:$cond, R0, bb:$imm16)>;

def : Pat<(select GPR:$cond, GPR:$tv, GPR:$fv),
          (OR (MASKEQZ $tv, $cond), (MASKNEZ $fv, $cond))>;
def : Pat<(select GPR:$cond, GPR:$tv, 0), (MASKEQZ $tv, $cond)>;
def : Pat<(select GPR:$cond, 0, GPR:$fv), (MASKNEZ $fv, $cond)>;
def : Pat<(select (XLenVT (setne GPR:$cond, 0)), GPR:$tv, GPR:$fv),
          (OR (MASKEQZ $tv, $cond), (MASKNEZ $fv, $cond))>;
def : Pat<(select (XLenVT (seteq GPR:$cond, 0)), GPR:$tv, GPR:$fv),
          (OR (MASKNEZ $tv, $cond), (MASKEQZ $fv, $cond))>;
def : Pat<(select (XLenVT (setne GPR:$cond, 0)), GPR:$tv, 0),
          (MASKEQZ $tv, $cond)>;
def : Pat<(select (XLenVT (seteq GPR:$cond, 0)), GPR:$tv, 0),
          (MASKNEZ $tv, $cond)>;

include "LoongArchInstrInfoA.td"

// This thing will generate an Instruction Non-defined Exception.
def PseudoUNIMP : Pseudo<(outs), (ins), [(trap)]>,
                  PseudoInstExpansion<(AMSWAP_W R0, R0, R1)>;

include "LoongArchInstrInfoF.td"
include "LoongArchInstrInfoD.td"
