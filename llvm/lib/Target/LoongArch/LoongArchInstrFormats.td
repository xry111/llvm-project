//===-- LoongArchInstrFormats.td - LoongArch Instruction Formats ---*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//
//  These instruction format definitions are structured to match the
//  description in the RISC-V User-Level ISA specification as closely as
//  possible. For instance, the specification describes instructions with the
//  MSB (31st bit) on the left and the LSB (0th bit) on the right. This is
//  reflected in the order of parameters to each instruction class.
//
//  One area of divergence is in the description of immediates. The
//  specification describes immediate encoding in terms of bit-slicing
//  operations on the logical value represented. The immediate argument to
//  these instruction formats instead represents the bit sequence that will be
//  inserted into the instruction. e.g. although JAL's immediate is logically
//  a 21-bit value (where the LSB is always zero), we describe it as an imm20
//  to match how it is encoded.
//
//===----------------------------------------------------------------------===//

// Format specifies the encoding used by the instruction. This is used by
// LoongArchMCCodeEmitter to determine which form of fixup to use. These
// definitions must be kept in-sync with LoongArchBaseInfo.h.
class InstFormat<bits<5> val> {
  bits<5> Value = val;
}
def InstFormatPseudo : InstFormat<0>;
def InstFormatR      : InstFormat<1>;
def InstFormatR4     : InstFormat<2>;
def InstFormatI      : InstFormat<3>;
def InstFormatS      : InstFormat<4>;
def InstFormatB      : InstFormat<5>;
def InstFormatU      : InstFormat<6>;
def InstFormatJ      : InstFormat<7>;
def InstFormatCR     : InstFormat<8>;
def InstFormatCI     : InstFormat<9>;
def InstFormatCSS    : InstFormat<10>;
def InstFormatCIW    : InstFormat<11>;
def InstFormatCL     : InstFormat<12>;
def InstFormatCS     : InstFormat<13>;
def InstFormatCA     : InstFormat<14>;
def InstFormatCB     : InstFormat<15>;
def InstFormatCJ     : InstFormat<16>;
def InstFormatOther  : InstFormat<17>;

class LoongArchVConstraint<bits<3> val> {
  bits<3> Value = val;
}
def NoConstraint  : LoongArchVConstraint<0b000>;
def VS2Constraint : LoongArchVConstraint<0b001>;
def VS1Constraint : LoongArchVConstraint<0b010>;
def VMConstraint  : LoongArchVConstraint<0b100>;

// Illegal instructions:
//
// * The destination vector register group for a masked vector instruction
// cannot overlap the source mask register (v0), unless the destination vector
// register is being written with a mask value (e.g., comparisons) or the
// scalar result of a reduction.
//
// * Widening: The destination EEW is greater than the source EEW, the source
// EMUL is at least 1. The destination vector register group cannot overlap
// with the source vector register groups besides the highest-numbered part of
// the destination register group.
//
// * Narrowing: The destination EEW is smaller than the source EEW. The
// destination vector register group cannot overlap with the source vector
// register groups besides the lowest-numbered part of the source register
// group.
//
// * vmsbf.m/vmsif.m/vmsof.m: The destination register cannot overlap the
// source register and, if masked, cannot overlap the mask register ('v0').
//
// * viota: The destination register cannot overlap the source register and,
// if masked, cannot overlap the mask register ('v0').
//
// * v[f]slide[1]up: The destination vector register group for vslideup cannot
// overlap the source vector register group.
//
// * vrgather: The destination vector register group cannot overlap with the
// source vector register groups.
//
// * vcompress: The destination vector register group cannot overlap the
// source vector register group or the source mask register
def WidenV       : LoongArchVConstraint<!or(VS2Constraint.Value,
                                        VS1Constraint.Value,
                                        VMConstraint.Value)>;
def WidenW       : LoongArchVConstraint<!or(VS1Constraint.Value,
                                        VMConstraint.Value)>;
def WidenCvt     : LoongArchVConstraint<!or(VS2Constraint.Value,
                                        VMConstraint.Value)>;
def Iota         : LoongArchVConstraint<!or(VS2Constraint.Value,
                                        VMConstraint.Value)>;
def SlideUp      : LoongArchVConstraint<!or(VS2Constraint.Value,
                                        VMConstraint.Value)>;
def Vrgather     : LoongArchVConstraint<!or(VS2Constraint.Value,
                                        VS1Constraint.Value,
                                        VMConstraint.Value)>;
def Vcompress    : LoongArchVConstraint<!or(VS2Constraint.Value,
                                        VS1Constraint.Value)>;

// The following opcode names match those given in Table 19.1 in the
// RISC-V User-level ISA specification ("RISC-V base opcode map").
class LoongArchOpcode<bits<7> val> {
  bits<7> Value = val;
}
def OPC_LOAD      : LoongArchOpcode<0b0000011>;
def OPC_LOAD_FP   : LoongArchOpcode<0b0000111>;
def OPC_MISC_MEM  : LoongArchOpcode<0b0001111>;
def OPC_OP_IMM    : LoongArchOpcode<0b0010011>;
def OPC_AUIPC     : LoongArchOpcode<0b0010111>;
def OPC_OP_IMM_32 : LoongArchOpcode<0b0011011>;
def OPC_STORE     : LoongArchOpcode<0b0100011>;
def OPC_STORE_FP  : LoongArchOpcode<0b0100111>;
def OPC_AMO       : LoongArchOpcode<0b0101111>;
def OPC_OP        : LoongArchOpcode<0b0110011>;
def OPC_OP_32     : LoongArchOpcode<0b0111011>;
def OPC_MADD      : LoongArchOpcode<0b1000011>;
def OPC_MSUB      : LoongArchOpcode<0b1000111>;
def OPC_NMSUB     : LoongArchOpcode<0b1001011>;
def OPC_NMADD     : LoongArchOpcode<0b1001111>;
def OPC_OP_FP     : LoongArchOpcode<0b1010011>;
def OPC_OP_V      : LoongArchOpcode<0b1010111>;
def OPC_BRANCH    : LoongArchOpcode<0b1100011>;
def OPC_JALR      : LoongArchOpcode<0b1100111>;
def OPC_JAL       : LoongArchOpcode<0b1101111>;
def OPC_SYSTEM    : LoongArchOpcode<0b1110011>;

class LAInst<dag outs, dag ins, string opcodestr, string argstr,
             list<dag> pattern>
    : Instruction {
  field bits<32> Inst;
  field bits<32> SoftFail = 0;
  let Size = 4;

  let Namespace = "LoongArch";
  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = opcodestr # "\t" # argstr;
  let Pattern = pattern;
}

class RVInst<dag outs, dag ins, string opcodestr, string argstr,
             list<dag> pattern, InstFormat format>
    : Instruction {
  field bits<32> Inst;
  // SoftFail is a field the disassembler can use to provide a way for
  // instructions to not match without killing the whole decode process. It is
  // mainly used for ARM, but Tablegen expects this field to exist or it fails
  // to build the decode table.
  field bits<32> SoftFail = 0;
  let Size = 4;

  bits<7> Opcode = 0;

  let Inst{6-0} = Opcode;

  let Namespace = "LoongArch";

  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = opcodestr # "\t" # argstr;
  let Pattern = pattern;

  let TSFlags{4-0} = format.Value;

  // Defaults
  LoongArchVConstraint RVVConstraint = NoConstraint;
  let TSFlags{7-5} = RVVConstraint.Value;

  bits<3> VLMul = 0;
  let TSFlags{10-8} = VLMul;

  bit HasDummyMask = 0;
  let TSFlags{11} = HasDummyMask;

  bit WritesElement0 = 0;
  let TSFlags{12} = WritesElement0;

  bit HasMergeOp = 0;
  let TSFlags{13} = HasMergeOp;

  bit HasSEWOp = 0;
  let TSFlags{14} = HasSEWOp;

  bit HasVLOp = 0;
  let TSFlags{15} = HasVLOp;
}

// Pseudo instructions
class Pseudo<dag outs, dag ins, list<dag> pattern, string opcodestr = "", string argstr = "">
    : RVInst<outs, ins, opcodestr, argstr, pattern, InstFormatPseudo>,
      Sched<[]> {
  let isPseudo = 1;
  let isCodeGenOnly = 1;
}

// Vol. 1 Annex B, from LU12I.W to PCADDU18I
class LAInst1RI20<bits<3> funct3, dag outs, dag ins, string opcodestr,
                  string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<5> rd;
  bits<20> imm20;

  let Inst{31-28} = 0b0001;
  let Inst{27-25} = funct3;
  let Inst{24-5} = imm20;
  let Inst{4-0} = rd;
}

// Normal 2-register instruction encoding for LoongArch
// Vol. 1 Annex B, from CLO.W to CPUCFG
class LAInst2R<bits<5> funct5, dag outs, dag ins, string opcodestr,
               string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<5> rj;
  bits<5> rd;

  let Inst{31-15} = 0;
  let Inst{14-10} = funct5;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

// From FABS.S to FRINT.D, except MOV*CF*
class LAInst2R_F<bits<10> funct10, dag outs, dag ins, string opcodestr,
                 string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<5> fj;
  bits<5> fd;

  let Inst{31-20} = 0b000000010001;
  let Inst{19-10} = funct10;
  let Inst{9-5} = fj;
  let Inst{4-0} = fd;
}

// Normal 3-register instruction encoding for LoongArch
// Vol. 1 Annex B, from ADD.W to CRCC.W.D.W
class LAInst3R<bits<10> funct10, bits<7> funct7, dag outs, dag ins,
               string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<5> rk;
  bits<5> rj;
  bits<5> rd;

  let Inst{31-22} = funct10;
  let Inst{21-15} = funct7;
  let Inst{14-10} = rk;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

// FMA style instructions
// Vol. 1 Annex B, from FMADD_S to FNMSUB_D
class LAInst4R<bits<4> funct4, dag outs, dag ins,
               string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<5> fa;
  bits<5> fk;
  bits<5> fj;
  bits<5> fd;

  let Inst{31-24} = 0b00001000;
  let Inst{23-20} = funct4;
  let Inst{19-15} = fa;
  let Inst{14-10} = fk;
  let Inst{9-5} = fj;
  let Inst{4-0} = fd;
}

// ALSL.W, ALSL.WU, BYTEPICK.W, and ALSL.D
class LAInst3RI2<bits<5> funct5, dag outs, dag ins, string opcodestr,
                 string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<2> imm2;
  bits<5> rk;
  bits<5> rj;
  bits<5> rd;

  let Inst{31-22} = 0;
  let Inst{21-17} = funct5;
  let Inst{16-15} = imm2;
  let Inst{14-10} = rk;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

// 32-bit Immediate Shift
class LAInst2RI5<bits<2> funct2, dag outs, dag ins,
                 string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<5> imm5;
  bits<5> rj;
  bits<5> rd;

  let Inst{31-20} = 0b000000000100;
  let Inst{19-18} = funct2;
  let Inst{17-15} = 0b001;
  let Inst{14-10} = imm5;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

// 64-bit Immediate Shift
class LAInst2RI6<bits<2> funct2, dag outs, dag ins,
                 string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<6> imm6;
  bits<5> rj;
  bits<5> rd;

  let Inst{31-20} = 0b000000000100;
  let Inst{19-18} = funct2;
  let Inst{17-16} = 0b01;
  let Inst{15-10} = imm6;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

// BSTR{INS,PICK}.W
class LAInstBSTRW<bits<1> funct1, dag outs, dag ins,
                  string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<5> msbw;
  bits<5> lsbw;
  bits<5> rj;
  bits<5> rd;

  let Inst{31-21} = 0b00000000011;
  let Inst{20-16} = msbw;
  let Inst{15} = funct1;
  let Inst{14-10} = lsbw;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

// BSTR{INS,PICK}.D
class LAInstBSTRD<bits<1> funct1, dag outs, dag ins,
                  string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<6> msbd;
  bits<6> lsbd;
  bits<5> rj;
  bits<5> rd;

  let Inst{31-23} = 0b000000001;
  let Inst{22} = funct1;
  let Inst{21-16} = msbd;
  let Inst{15-10} = lsbd;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

// Normal 2-register and 12-bit immediate instruction encoding for LoongArch
// Vol. 1 Annex B, from SLTI to XORI, and from LD.B to FST.D
class LAInst2RI12<bits<6> funct6, bits<4> funct4, dag outs, dag ins,
                  string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<12> imm12;
  bits<5> rj;
  bits<5> rd;

  let Inst{31-26} = funct6;
  let Inst{25-22} = funct4;
  let Inst{21-10} = imm12;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

// Normal 2-register and 14-bit immediate instruction encoding for LoongArch
// Vol. 1 Annex B, from LL.W to STPTR.D
class LAInst2RI14<bits<3> funct3, dag outs, dag ins,
                  string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<14> imm14;
  bits<5> rj;
  bits<5> rd;

  let Inst{31-27} = 0b00100;
  let Inst{26-24} = funct3;
  let Inst{23-10} = imm14;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

// Normal 2-register and 16-bit immediate instruction encoding for LoongArch
// Vol. 1 Annex B, JIRL, and from BEQ to BGEU
class LAInst2RI16<bits<4> funct4, dag outs, dag ins,
                  string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<16> imm16;
  bits<5> rj;
  bits<5> rd;

  let Inst{31-30} = 0b01;
  let Inst{29-26} = funct4;
  let Inst{25-10} = imm16;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

// B and BL
class LAInstI26<bits<1> funct1, dag outs, dag ins,
                string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<26> imm26;

  let Inst{31-27} = 0b01010;
  let Inst{26} = funct1;
  let Inst{25-10} = imm26{15-0};
  let Inst{9-0} = imm26{25-16};
}

// FCMP
class LAInstFcmp<bits<2> funct2, bits<5> cond, dag outs, dag ins,
                 string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<5> fk;
  bits<5> fj;
  bits<3> cd;

  let Inst{31-22} = 0b0000110000;
  let Inst{21-20} = funct2;
  let Inst{19-15} = cond;
  let Inst{14-10} = fk;
  let Inst{9-5} = fj;
  let Inst{4-3} = 0b00;
  let Inst{2-0} = cd;
}

class LAInstFsel<dag outs, dag ins, string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<3> ca;
  bits<5> fd;
  bits<5> fj;
  bits<5> fk;

  let Inst{31-18} = 0b00001101000000;
  let Inst{17-15} = ca;
  let Inst{14-10} = fk;
  let Inst{9-5} = fj;
  let Inst{4-0} = fd;
}

class LAInstCR<bits<2> funct2, dag outs, dag ins,
               string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<5> rd;
  bits<3> cj;

  let Inst{31-12} = 0b00000001000101001101;
  let Inst{11-10} = funct2;
  let Inst{9-8} = 0b00;
  let Inst{7-5} = cj;
  let Inst{4-0} = rd;
}

class LAInstRC<bits<2> funct2, dag outs, dag ins,
               string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<5> rj;
  bits<3> cd;

  let Inst{31-12} = 0b00000001000101001101;
  let Inst{11-10} = funct2;
  let Inst{9-5} = rj;
  let Inst{4-3} = 0b00;
  let Inst{2-0} = cd;
}

class LAInstBC<bits<1> funct1, dag outs, dag ins,
               string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<3> cj;
  bits<21> imm21;

  let Inst{31-26} = 0b010010;
  let Inst{25-10} = imm21{15-0};
  let Inst{9} = 0b0;
  let Inst{8} = funct1;
  let Inst{7-5} = cj;
  let Inst{4-0} = imm21{20-16};
}

class LAInstBR<bits<1> funct1, dag outs, dag ins,
               string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<5> rj;
  bits<21> imm21;

  let Inst{31-27} = 0b01000;
  let Inst{26} = funct1;
  let Inst{25-10} = imm21{15-0};
  let Inst{9-5} = rj;
  let Inst{4-0} = imm21{20-16};
}

class LAInstI15<bits<15> funct15, bits<2> funct2, dag outs, dag ins,
                string opcodestr, string argstr>
    : LAInst<outs, ins, opcodestr, argstr, []> {
  bits<15> imm15;

  let Inst{31-17} = funct15;
  let Inst{16-15} = funct2;
  let Inst{14-0} = imm15;
}

// Instruction formats are listed in the order they appear in the RISC-V
// instruction set manual (R, I, S, B, U, J) with sub-formats (e.g. RVInstR4,
// RVInstRAtomic) sorted alphabetically.

class RVInstR<bits<7> funct7, bits<3> funct3, LoongArchOpcode opcode, dag outs,
              dag ins, string opcodestr, string argstr>
    : RVInst<outs, ins, opcodestr, argstr, [], InstFormatR> {
  bits<5> rs2;
  bits<5> rs1;
  bits<5> rd;

  let Inst{31-25} = funct7;
  let Inst{24-20} = rs2;
  let Inst{19-15} = rs1;
  let Inst{14-12} = funct3;
  let Inst{11-7} = rd;
  let Opcode = opcode.Value;
}

class RVInstR4<bits<2> funct2, LoongArchOpcode opcode, dag outs, dag ins,
               string opcodestr, string argstr>
    : RVInst<outs, ins, opcodestr, argstr, [], InstFormatR4> {
  bits<5> rs3;
  bits<5> rs2;
  bits<5> rs1;
  bits<3> funct3;
  bits<5> rd;

  let Inst{31-27} = rs3;
  let Inst{26-25} = funct2;
  let Inst{24-20} = rs2;
  let Inst{19-15} = rs1;
  let Inst{14-12} = funct3;
  let Inst{11-7} = rd;
  let Opcode = opcode.Value;
}

class RVInstRAtomic<bits<5> funct5, bit aq, bit rl, bits<3> funct3,
                    LoongArchOpcode opcode, dag outs, dag ins, string opcodestr,
                    string argstr>
    : RVInst<outs, ins, opcodestr, argstr, [], InstFormatR> {
  bits<5> rs2;
  bits<5> rs1;
  bits<5> rd;

  let Inst{31-27} = funct5;
  let Inst{26} = aq;
  let Inst{25} = rl;
  let Inst{24-20} = rs2;
  let Inst{19-15} = rs1;
  let Inst{14-12} = funct3;
  let Inst{11-7} = rd;
  let Opcode = opcode.Value;
}

class RVInstRFrm<bits<7> funct7, LoongArchOpcode opcode, dag outs, dag ins,
                 string opcodestr, string argstr>
    : RVInst<outs, ins, opcodestr, argstr, [], InstFormatR> {
  bits<5> rs2;
  bits<5> rs1;
  bits<3> funct3;
  bits<5> rd;

  let Inst{31-25} = funct7;
  let Inst{24-20} = rs2;
  let Inst{19-15} = rs1;
  let Inst{14-12} = funct3;
  let Inst{11-7} = rd;
  let Opcode = opcode.Value;
}

class RVInstI<bits<3> funct3, LoongArchOpcode opcode, dag outs, dag ins,
              string opcodestr, string argstr>
    : RVInst<outs, ins, opcodestr, argstr, [], InstFormatI> {
  bits<12> imm12;
  bits<5> rs1;
  bits<5> rd;

  let Inst{31-20} = imm12;
  let Inst{19-15} = rs1;
  let Inst{14-12} = funct3;
  let Inst{11-7} = rd;
  let Opcode = opcode.Value;
}

class RVInstIShift<bit arithshift, bits<3> funct3, LoongArchOpcode opcode,
                   dag outs, dag ins, string opcodestr, string argstr>
    : RVInst<outs, ins, opcodestr, argstr, [], InstFormatI> {
  bits<6> shamt;
  bits<5> rs1;
  bits<5> rd;

  let Inst{31} = 0;
  let Inst{30} = arithshift;
  let Inst{29-26} = 0;
  let Inst{25-20} = shamt;
  let Inst{19-15} = rs1;
  let Inst{14-12} = funct3;
  let Inst{11-7} = rd;
  let Opcode = opcode.Value;
}

class RVInstIShiftW<bit arithshift, bits<3> funct3, LoongArchOpcode opcode,
                    dag outs, dag ins, string opcodestr, string argstr>
    : RVInst<outs, ins, opcodestr, argstr, [], InstFormatI> {
  bits<5> shamt;
  bits<5> rs1;
  bits<5> rd;

  let Inst{31} = 0;
  let Inst{30} = arithshift;
  let Inst{29-25} = 0;
  let Inst{24-20} = shamt;
  let Inst{19-15} = rs1;
  let Inst{14-12} = funct3;
  let Inst{11-7} = rd;
  let Opcode = opcode.Value;
}

class RVInstS<bits<3> funct3, LoongArchOpcode opcode, dag outs, dag ins,
              string opcodestr, string argstr>
    : RVInst<outs, ins, opcodestr, argstr, [], InstFormatS> {
  bits<12> imm12;
  bits<5> rs2;
  bits<5> rs1;

  let Inst{31-25} = imm12{11-5};
  let Inst{24-20} = rs2;
  let Inst{19-15} = rs1;
  let Inst{14-12} = funct3;
  let Inst{11-7} = imm12{4-0};
  let Opcode = opcode.Value;
}

class RVInstB<bits<3> funct3, LoongArchOpcode opcode, dag outs, dag ins,
              string opcodestr, string argstr>
    : RVInst<outs, ins, opcodestr, argstr, [], InstFormatB> {
  bits<12> imm12;
  bits<5> rs2;
  bits<5> rs1;

  let Inst{31} = imm12{11};
  let Inst{30-25} = imm12{9-4};
  let Inst{24-20} = rs2;
  let Inst{19-15} = rs1;
  let Inst{14-12} = funct3;
  let Inst{11-8} = imm12{3-0};
  let Inst{7} = imm12{10};
  let Opcode = opcode.Value;
}

class RVInstU<LoongArchOpcode opcode, dag outs, dag ins, string opcodestr,
              string argstr>
    : RVInst<outs, ins, opcodestr, argstr, [], InstFormatU> {
  bits<20> imm20;
  bits<5> rd;

  let Inst{31-12} = imm20;
  let Inst{11-7} = rd;
  let Opcode = opcode.Value;
}

class RVInstJ<LoongArchOpcode opcode, dag outs, dag ins, string opcodestr,
              string argstr>
    : RVInst<outs, ins, opcodestr, argstr, [], InstFormatJ> {
  bits<20> imm20;
  bits<5> rd;

  let Inst{31} = imm20{19};
  let Inst{30-21} = imm20{9-0};
  let Inst{20} = imm20{10};
  let Inst{19-12} = imm20{18-11};
  let Inst{11-7} = rd;
  let Opcode = opcode.Value;
}
