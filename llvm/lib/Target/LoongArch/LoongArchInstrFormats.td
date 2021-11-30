//===-- LoongArchInstrFormats.td - LoongArch Instruction Formats -----*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Describe LoongArch instructions format
//
//  CPU INSTRUCTION FORMATS
//
//  opcode  - operation code.
//  rs      - src reg.
//  rt      - dst reg (on a 2 regs instr) or src reg (on a 3 reg instr).
//  rd      - dst reg, only used on 3 regs instr.
//  shamt   - only used on shift instructions, contains the shift amount.
//  funct   - combined with opcode field give us an operation code.
//
//===----------------------------------------------------------------------===//

class StdArch {

  bits<32> Inst;
}

// Format specifies the encoding used by the instruction.  This is part of the
// ad-hoc solution used to emit machine instruction encodings by our machine
// code emitter.
class Format<bits<4> val> {
  bits<4> Value = val;
}

def Pseudo    : Format<0>;
def FrmR      : Format<1>;
def FrmI      : Format<2>;
def FrmJ      : Format<3>;
def FrmFR     : Format<4>;
def FrmFI     : Format<5>;
def FrmOther  : Format<6>;

// Generic LoongArch Format
class InstLA<dag outs, dag ins, string asmstr, list<dag> pattern, Format f>
    : Instruction
{
  field bits<32> Inst;
  Format Form = f;

  let Namespace = "LoongArch";

  let Size = 4;

  let OutOperandList = outs;
  let InOperandList  = ins;
  let AsmString   = asmstr;
  let Pattern     = pattern;

  //
  // Attributes specific to LoongArch instructions...
  //
  bits<4> FormBits     = Form.Value;
  bit isCTI            = 0; // Any form of Control Transfer Instruction.
                            // Required for LoongArch
  bit hasForbiddenSlot = 0; // Instruction has a forbidden slot.
  bit IsPCRelativeLoad = 0; // Load instruction with implicit source register
                            // ($pc) and with explicit offset and destination
                            // register
  bit hasFCCRegOperand = 0; // Instruction uses $fcc<X> register

  // TSFlags layout should be kept in sync with MCTargetDesc/LoongArchBaseInfo.h.
  let TSFlags{3-0}   = FormBits;
  let TSFlags{4}     = isCTI;
  let TSFlags{5}     = hasForbiddenSlot;
  let TSFlags{6}     = IsPCRelativeLoad;
  let TSFlags{7}     = hasFCCRegOperand;

  let DecoderNamespace = "LoongArch";

  field bits<32> SoftFail = 0;
}

class InstForm<dag outs, dag ins, string asmstr, list<dag> pattern,
               Format f, string opstr = ""> :
  InstLA<outs, ins, asmstr, pattern, f> {
  string BaseOpcode = opstr;
  string Arch;
}

class LoongArch_str<string opstr> {
  string Arch;
  string BaseOpcode = opstr;
}

//===-----------------------------------------------------------===//
// Format instruction classes in the LoongArch
//===-----------------------------------------------------------===//

// R2 classes: 2 registers
//
class R2 : StdArch {
  bits<5> rj;
  bits<5> rd;

  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class R2I<bits<5> op>
    : R2 {
  let Inst{31-15} = 0x0;
  let Inst{14-10} = op;
}

class R2F<bits<10> op>
    : R2 {
  bits<5> fj;
  bits<5> fd;

  let Inst{31-20} = 0x11;
  let Inst{19-10} = op;
  let Inst{9-5} = fj;
  let Inst{4-0} = fd;
}

class MOVFI<bits<10> op>
    : R2 {
  bits<5> rj;
  bits<5> fd;

  let Inst{31-20} = 0x11;
  let Inst{19-10} = op;
  let Inst{9-5} = rj;
  let Inst{4-0} = fd;
}

class MOVIF<bits<10> op>
    : R2 {
  bits<5> fj;
  bits<5> rd;

  let Inst{31-20} = 0x11;
  let Inst{19-10} = op;
  let Inst{9-5} = fj;
  let Inst{4-0} = rd;
}

class R2P<bits<3> op>
    : R2 {
  let Inst{31-13} = 0x3240;
  let Inst{12-10} = op;
}

class R2_CSR<bits<8> op>
    : StdArch {
  bits<5> rj;
  bits<5> rd;
  bits<14> csr;

  let Inst{31-24} = op;
  let Inst{23-10} = csr;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class R2_SI16<bits<6> op>
    : StdArch {
  bits<5> rd;
  bits<5> rj;
  bits<16> si16;

  let Inst{31-26} = op;
  let Inst{25-10} = si16;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class R2_COND<bits<2> op, bits<5> cond>
    : StdArch {
  bits<5> fj;
  bits<5> fk;
  bits<3> cd;

  let Inst{31-22} = 0x30;
  let Inst{21-20} = op;
  let Inst{19-15} = cond;
  let Inst{14-10} = fk;
  let Inst{9-5} = fj;
  let Inst{4-3} = 0b00;
  let Inst{2-0} = cd;
}

class R2_LEVEL<bits<14> op>
    : StdArch {
  bits<5> rj;
  bits<5> rd;
  bits<8> level;

  let Inst{31-18} = op;
  let Inst{17-10} = level;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class IMM32<bits<6> op>
    : StdArch {
  let Inst{31-16} = 0x0648;
  let Inst{15-10} = op;
  let Inst{9-0} = 0;
}

class WAIT_FM : StdArch {
  bits<15> hint;

  let Inst{31-15} = 0xc91;
  let Inst{14-0} = hint;
}

class R2_INVTLB : StdArch {
  bits<5> rj;
  bits<5> op;
  bits<5> rk;

  let Inst{31-15} = 0xc93;
  let Inst{14-10} = rk;
  let Inst{9-5} = rj;
  let Inst{4-0} = op;
}

class BAR_FM<bits<1> op>
    : StdArch {
  bits<15> hint;

  let Inst{31-16} = 0x3872;
  let Inst{15} = op;
  let Inst{14-0} = hint;
}

class PRELD_FM : StdArch {
  bits<5> rj;
  bits<5> hint;
  bits<12> imm12;

  let Inst{31-22} = 0xab;
  let Inst{21-10} = imm12;
  let Inst{9-5} = rj;
  let Inst{4-0} = hint;
}

// R3 classes: 3 registers
//
class R3 : StdArch {
  bits<5> rk;
  bits<5> rj;
  bits<5> rd;

  let Inst{14-10} = rk;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class R3I<bits<7> op>
    : R3 {
  let Inst{31-22} = 0x0;
  let Inst{21-15} = op;
}

class R3F<bits<6> op>
    : R3 {
  bits<5> fk;
  bits<5> fj;
  bits<5> fd;

  let Inst{31-21} = 0x8;
  let Inst{20-15} = op;
  let Inst{14-10} = fk;
  let Inst{9-5} = fj;
  let Inst{4-0} = fd;
}

class R3MI<bits<8> op>
    : R3 {
  let Inst{31-23} = 0x70;
  let Inst{22-15} = op;
}

class AM<bits<6> op> : StdArch {
  bits<5> rk;
  bits<17> addr; // rj + 12 bits offset 0
  bits<5> rd;

  let Inst{31-21} = 0x1c3;
  let Inst{20-15} = op;
  let Inst{14-10} = rk;
  let Inst{9-5} = addr{16-12};
  let Inst{4-0} = rd;
}

class R3MF<bits<8> op>
    : R3 {
  bits<5> fd;

  let Inst{31-23} = 0x70;
  let Inst{22-15} = op;
  let Inst{4-0} = fd;
}

class R3_SA2<bits<5> op>
    : StdArch {
  bits<5> rk;
  bits<5> rj;
  bits<5> rd;
  bits<2> sa;

  let Inst{31-22} = 0x0;
  let Inst{21-17} = op;
  let Inst{16-15} = sa;
  let Inst{14-10} = rk;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class R3_SA3 : StdArch {
  bits<5> rk;
  bits<5> rj;
  bits<5> rd;
  bits<3> sa;

  let Inst{31-18} = 3;
  let Inst{17-15} = sa;
  let Inst{14-10} = rk;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

// R4 classes: 4 registers
//
class R4MUL<bits<4> op>
    : StdArch {
  bits<5> fa;
  bits<5> fk;
  bits<5> fj;
  bits<5> fd;

  let Inst{31-24} = 0x8;
  let Inst{23-20} = op;
  let Inst{19-15} = fa;
  let Inst{14-10} = fk;
  let Inst{9-5} = fj;
  let Inst{4-0} = fd;
}

class R4CMP<bits<2> op>
    : StdArch {
  bits<5> cond;
  bits<5> fk;
  bits<5> fj;
  bits<3> cd;

  let Inst{31-22} = 0x30;
  let Inst{21-20} = op;
  let Inst{19-15} = cond;
  let Inst{14-10} = fk;
  let Inst{9-5} = fj;
  let Inst{4-3} = 0;
  let Inst{2-0} = cd;
}

class R4SEL : StdArch {
  bits<3> ca;
  bits<5> fk;
  bits<5> fj;
  bits<5> fd;

  let Inst{31-18} = 0x340;
  let Inst{17-15} = ca;
  let Inst{14-10} = fk;
  let Inst{9-5} = fj;
  let Inst{4-0} = fd;
}

// R2_IMM5 classes: 2registers and 1 5bit-immediate
//
class R2_IMM5<bits<2> op>
    : StdArch {
  bits<5> rj;
  bits<5> rd;
  bits<5> imm5;

  let Inst{31-20} = 0x4;
  let Inst{19-18} = op;
  let Inst{17-15} = 0x1;
  let Inst{14-10} = imm5;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

// R2_IMM6 classes: 2registers and 1 6bit-immediate
//
class R2_IMM6<bits<2> op>
    : StdArch {
  bits<5> rj;
  bits<5> rd;
  bits<6> imm6;

  let Inst{31-20} = 0x4;
  let Inst{19-18} = op;
  let Inst{17-16} = 0x1;
  let Inst{15-10} = imm6;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

// R2_IMM12 classes: 2 registers and 1 12bit-immediate
//
class LOAD_STORE<bits<4> op>
    : StdArch {
  bits<5> rd;
  bits<17> addr;

  let Inst{31-26} = 0xa;
  let Inst{25-22} = op;
  let Inst{21-10} = addr{11-0};
  let Inst{9-5} = addr{16-12};
  let Inst{4-0} = rd;
}
// for reloc
class LOAD_STORE_RRI<bits<4> op>
    : StdArch {
  bits<5> rj;
  bits<5> rd;
  bits<12> imm12;

  let Inst{31-26} = 0xa;
  let Inst{25-22} = op;
  let Inst{21-10} = imm12;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}


class R2_IMM12<bits<3> op>
    : StdArch {
  bits<5> rj;
  bits<5> rd;
  bits<12> imm12;

  let Inst{31-25} = 0x1;
  let Inst{24-22} = op;
  let Inst{21-10} = imm12;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class LEA_ADDI_FM<bits<3> op>
    : StdArch {
  bits<5> rd;
  bits<17> addr;

  let Inst{31-25} = 0x1;
  let Inst{24-22} = op;
  let Inst{21-10} = addr{11-0};
  let Inst{9-5} = addr{16-12};
  let Inst{4-0} = rd;
}

// R2_IMM14 classes: 2 registers and 1 14bit-immediate
//
class LL_SC<bits<3> op>
    : StdArch {
  bits<5> rd;
  bits<19> addr;

  let Inst{31-27} = 4;
  let Inst{26-24} = op;
  let Inst{23-10} = addr{13-0};
  let Inst{9-5} = addr{18-14};
  let Inst{4-0} = rd;
}

// R2_IMM16 classes: 2 registers and 1 16bit-immediate
//
class R2_IMM16BEQ<bits<6> op>
    : StdArch {
  bits<5> rj;
  bits<5> rd;
  bits<16> offs16;

  let Inst{31-26} = op;
  let Inst{25-10} = offs16;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class R2_IMM16JIRL : StdArch {
  bits<5> rj;
  bits<5> rd;
  bits<16> offs16;

  let Inst{31-26} = 0x13;
  let Inst{25-10} = offs16;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

// R1_IMM21 classes: 1 registers and 1 21bit-immediate
//
class R1_IMM21BEQZ<bits<6> op>
    : StdArch {
  bits<5> rj;
  bits<21> offs21;

  let Inst{31-26} = op;
  let Inst{25-10} = offs21{15-0};
  let Inst{9-5} = rj;
  let Inst{4-0} = offs21{20-16};
}

class R1_CSR<bits<13> op>
    : StdArch {
  bits<5> rd;
  bits<14> csr;

  let Inst{31-24} = op{7-0};
  let Inst{23-10} = csr;
  let Inst{9-5} = op{12-8};
  let Inst{4-0} = rd;
}

class R1_SI20<bits<7> op>
    : StdArch {
  bits<5> rd;
  bits<20> si20;

  let Inst{31-25} = op;
  let Inst{24-5} = si20;
  let Inst{4-0} = rd;
}

class R1_CACHE : StdArch {
  bits<5> rj;
  bits<5> op;
  bits<12> si12;

  let Inst{31-22} = 0x18;
  let Inst{21-10} = si12;
  let Inst{9-5} = rj;
  let Inst{4-0} = op;
}

class R1_SEQ<bits<14> op>
    : StdArch {
  bits<5> rj;
  bits<5> offset;
  bits<8> seq;

  let Inst{31-18} = op;
  let Inst{17-10} = seq;
  let Inst{9-5} = rj;
  let Inst{4-0} = 0b00000;
}

class R1_BCEQZ<bits<2> op>
    : StdArch {
  bits<21> offset;
  bits<3> cj;

  let Inst{31-26} = 0x12;
  let Inst{25-10} = offset{15-0};
  let Inst{9-8} = op;
  let Inst{7-5} = cj;
  let Inst{4-0} = offset{20-16};
}

// IMM26 classes: 1 26bit-immediate
//
class IMM26B<bits<6> op>
    : StdArch {
  bits<26> offs26;

  let Inst{31-26} = op;
  let Inst{25-10} = offs26{15-0};
  let Inst{9-0} = offs26{25-16};
}

// LoongArch Pseudo Instructions Format
class LoongArchPseudo<dag outs, dag ins, list<dag> pattern> :
  InstLA<outs, ins, "", pattern, Pseudo> {
  let isCodeGenOnly = 1;
  let isPseudo = 1;
}

// Pseudo-instructions for alternate assembly syntax (never used by codegen).
// These are aliases that require C++ handling to convert to the target
// instruction, while InstAliases can be handled directly by tblgen.
class LoongArchAsmPseudoInst<dag outs, dag ins, string asmstr>:
  InstLA<outs, ins, asmstr, [], Pseudo> {
  let isPseudo = 1;
  let Pattern = [];
}

//
// Misc instruction classes
class ASSERT<bits<2> op>
    : StdArch {
  bits<5> rk;
  bits<5> rj;

  let Inst{31-17} = 0x0;
  let Inst{16-15} = op;
  let Inst{14-10} = rk;
  let Inst{9-5} = rj;
  let Inst{4-0} = 0x0;
}

class CODE15<bits<7> op>
    : StdArch {
  bits<15> Code;

  let Inst{31-22} = 0x0;
  let Inst{21-15} = op;
  let Inst{14-0} = Code;
}

class INSERT_BIT32<bits<1> op>
    : StdArch {
  bits<5> msbw;
  bits<5> lsbw;
  bits<5> rj;
  bits<5> rd;

  let Inst{31-21} = 0x3;
  let Inst{20-16} = msbw;
  let Inst{15} = op;
  let Inst{14-10} = lsbw;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class INSERT_BIT64<bits<1> op>
    : StdArch {
  bits<6> msbd;
  bits<6> lsbd;
  bits<5> rj;
  bits<5> rd;

  let Inst{31-23} = 0x1;
  let Inst{22} = op;
  let Inst{21-16} = msbd;
  let Inst{15-10} = lsbd;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class MOVGPR2FCSR: StdArch {
  bits<5> fcsr;
  bits<5> rj;

  let Inst{31-10} = 0x4530;
  let Inst{9-5} = rj;
  let Inst{4-0} = fcsr;
}

class MOVFCSR2GPR: StdArch {
  bits<5> fcsr;
  bits<5> rd;

  let Inst{31-10} = 0x4532;
  let Inst{9-5} = fcsr;
  let Inst{4-0} = rd;
}

class MOVFGR2FCFR: StdArch {
  bits<3> cd;
  bits<5> fj;

  let Inst{31-10} = 0x4534;
  let Inst{9-5} = fj;
  let Inst{4-3} = 0;
  let Inst{2-0} = cd;
}

class MOVFCFR2FGR: StdArch {
  bits<3> cj;
  bits<5> fd;

  let Inst{31-10} = 0x4535;
  let Inst{9-8} = 0;
  let Inst{7-5} = cj;
  let Inst{4-0} = fd;
}

class MOVGPR2FCFR: StdArch {
  bits<3> cd;
  bits<5> rj;

  let Inst{31-10} = 0x4536;
  let Inst{9-5} = rj;
  let Inst{4-3} = 0;
  let Inst{2-0} = cd;
}

class MOVFCFR2GPR: StdArch {
  bits<3> cj;
  bits<5> rd;

  let Inst{31-10} = 0x4537;
  let Inst{9-8} = 0;
  let Inst{7-5} = cj;
  let Inst{4-0} = rd;
}

class LoongArchInst : InstLA<(outs), (ins), "", [], FrmOther> {
}
class JMP_OFFS_2R<bits<6> op> : LoongArchInst {
  bits<5>  rs;
  bits<5>  rd;
  bits<16> offset;

  bits<32> Inst;

  let Inst{31-26} = op;
  let Inst{25-10} = offset;
  let Inst{9-5} = rs;
  let Inst{4-0}  = rd;
}

class FJ<bits<6> op> : StdArch
{
  bits<26> target;

  let Inst{31-26} = op;
  let Inst{25-10}  = target{15-0};
  let Inst{9-0}  = target{25-16};
}

class LUI_FM : StdArch {
  bits<5> rt;
  bits<16> imm16;

  let Inst{31-26} = 0xf;
  let Inst{25-21} = 0;
  let Inst{20-16} = rt;
  let Inst{15-0}  = imm16;
}

class  R2_IMM12M_STD<bits<4> op> : StdArch {
  bits<5> rj;
  bits<5> rd;
  bits<12> imm12;

  let Inst{31-26} = 0xa;
  let Inst{25-22} = op;
  let Inst{21-10} = imm12;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class LLD_2R<bits<3> Code> : LoongArchInst {
  bits<5> rd;
  bits<19> addr;
  bits<5> rj = addr{18-14};
  bits<14> offset = addr{13-0};

  bits<32> Inst;

  let Inst{31-27} = 0x4;
  let Inst{26-24} = Code;
  let Inst{23-10} = offset;
  let Inst{9-5} = rj;
  let Inst{4-0} = rd;
}

class CEQS_FM<bits<2> op> {
  bits<5> fj;
  bits<5> fk;
  bits<3> cd;
  bits<5> cond;

  bits<32> Inst;

  let Inst{31-22} = 0x30;
  let Inst{21-20} = op;
  let Inst{19-15} = cond;
  let Inst{14-10} = fk;
  let Inst{9-5} = fj;
  let Inst{4-3} = 0b00;
  let Inst{2-0} = cd;
}

