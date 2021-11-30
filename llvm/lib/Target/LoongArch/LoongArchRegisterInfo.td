//===-- LoongArchRegisterInfo.td - LoongArch Register defs -----------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Declarations that describe the LoongArch register file
//===----------------------------------------------------------------------===//
let Namespace = "LoongArch" in {
def sub_32     : SubRegIndex<32>;
def sub_64     : SubRegIndex<64>;
def sub_128    : SubRegIndex<128>;
def sub_fcsr1  : SubRegIndex<5>;
def sub_fcsr2  : SubRegIndex<13, 16>;
def sub_fcsr3  : SubRegIndex<2, 8>;
def sub_lo     : SubRegIndex<32>;
def sub_hi     : SubRegIndex<32, 32>;
def PC : Register<"pc">;
}

class Unallocatable {
  bit isAllocatable = 0;
}

/// We have banks of registers each.
class LoongArchReg<bits<16> Enc, string n> : Register<n> {
  let HWEncoding = Enc;
  let Namespace = "LoongArch";
}

class LoongArchRegWithSubRegs<bits<16> Enc, string n, list<Register> subregs>
  : RegisterWithSubRegs<n, subregs> {
  let HWEncoding = Enc;
  let Namespace = "LoongArch";
}

/// LoongArch 32-bit CPU Registers.
class LoongArch32GPR<bits<16> Enc, string n> : LoongArchReg<Enc, n>;

/// LoongArch 64-bit CPU Registers.
class LoongArch64GPR<bits<16> Enc, string n, list<Register> subregs>
  : LoongArchRegWithSubRegs<Enc, n, subregs> {
  let SubRegIndices = [sub_32];
}

/// LoongArch 64-bit Floating-point Registers
class FGR32<bits<16> Enc, string n> : LoongArchReg<Enc, n>;
class FGR64<bits<16> Enc, string n, list<Register> subregs>
  : LoongArchRegWithSubRegs<Enc, n, subregs> {
  let SubRegIndices = [sub_lo];
}

//===----------------------------------------------------------------------===//
//  Registers
//===----------------------------------------------------------------------===//

/// General Purpose 32-bit Registers
def ZERO : LoongArch32GPR<0,  "zero">, DwarfRegNum<[0]>;
def RA   : LoongArch32GPR<1,  "ra">,   DwarfRegNum<[1]>;
def TP   : LoongArch32GPR<2,  "tp">,   DwarfRegNum<[2]>;
def SP   : LoongArch32GPR<3,  "sp">,   DwarfRegNum<[3]>;
def A0   : LoongArch32GPR<4,  "r4">,   DwarfRegNum<[4]>;
def A1   : LoongArch32GPR<5,  "r5">,   DwarfRegNum<[5]>;
def A2   : LoongArch32GPR<6,  "r6">,   DwarfRegNum<[6]>;
def A3   : LoongArch32GPR<7,  "r7">,   DwarfRegNum<[7]>;
def A4   : LoongArch32GPR<8,  "r8">,   DwarfRegNum<[8]>;
def A5   : LoongArch32GPR<9,  "r9">,   DwarfRegNum<[9]>;
def A6   : LoongArch32GPR<10, "r10">,  DwarfRegNum<[10]>;
def A7   : LoongArch32GPR<11, "r11">,  DwarfRegNum<[11]>;
def T0   : LoongArch32GPR<12, "r12">,  DwarfRegNum<[12]>;
def T1   : LoongArch32GPR<13, "r13">,  DwarfRegNum<[13]>;
def T2   : LoongArch32GPR<14, "r14">,  DwarfRegNum<[14]>;
def T3   : LoongArch32GPR<15, "r15">,  DwarfRegNum<[15]>;
def T4   : LoongArch32GPR<16, "r16">,  DwarfRegNum<[16]>;
def T5   : LoongArch32GPR<17, "r17">,  DwarfRegNum<[17]>;
def T6   : LoongArch32GPR<18, "r18">,  DwarfRegNum<[18]>;
def T7   : LoongArch32GPR<19, "r19">,  DwarfRegNum<[19]>;
def T8   : LoongArch32GPR<20, "r20">,  DwarfRegNum<[20]>;
def T9   : LoongArch32GPR<21, "r21">,  DwarfRegNum<[21]>;
def FP   : LoongArch32GPR<22, "r22">,  DwarfRegNum<[22]>;
def S0   : LoongArch32GPR<23, "r23">,  DwarfRegNum<[23]>;
def S1   : LoongArch32GPR<24, "r24">,  DwarfRegNum<[24]>;
def S2   : LoongArch32GPR<25, "r25">,  DwarfRegNum<[25]>;
def S3   : LoongArch32GPR<26, "r26">,  DwarfRegNum<[26]>;
def S4   : LoongArch32GPR<27, "r27">,  DwarfRegNum<[27]>;
def S5   : LoongArch32GPR<28, "r28">,  DwarfRegNum<[28]>;
def S6   : LoongArch32GPR<29, "r29">,  DwarfRegNum<[29]>;
def S7   : LoongArch32GPR<30, "r30">,  DwarfRegNum<[30]>;
def S8   : LoongArch32GPR<31, "r31">,  DwarfRegNum<[31]>;

let SubRegIndices = [sub_32] in {
def V0   : LoongArchRegWithSubRegs<4, "r4",  [A0]>, DwarfRegNum<[4]>;
def V1   : LoongArchRegWithSubRegs<5, "r5",  [A1]>, DwarfRegNum<[5]>;
}

/// General Purpose 64-bit Registers
def ZERO_64 : LoongArch64GPR<0, "zero", [ZERO]>, DwarfRegNum<[0]>;
def RA_64   : LoongArch64GPR<1, "ra",    [RA]>, DwarfRegNum<[1]>;
def TP_64   : LoongArch64GPR<2, "tp",    [TP]>, DwarfRegNum<[2]>;
def SP_64   : LoongArch64GPR<3, "sp",    [SP]>, DwarfRegNum<[3]>;
def A0_64   : LoongArch64GPR<4, "r4",    [A0]>, DwarfRegNum<[4]>;
def A1_64   : LoongArch64GPR<5, "r5",    [A1]>, DwarfRegNum<[5]>;
def A2_64   : LoongArch64GPR<6, "r6",    [A2]>, DwarfRegNum<[6]>;
def A3_64   : LoongArch64GPR<7, "r7",    [A3]>, DwarfRegNum<[7]>;
def A4_64   : LoongArch64GPR<8, "r8",    [A4]>, DwarfRegNum<[8]>;
def A5_64   : LoongArch64GPR<9, "r9",    [A5]>, DwarfRegNum<[9]>;
def A6_64   : LoongArch64GPR<10, "r10",  [A6]>, DwarfRegNum<[10]>;
def A7_64   : LoongArch64GPR<11, "r11",  [A7]>, DwarfRegNum<[11]>;
def T0_64   : LoongArch64GPR<12, "r12",  [T0]>, DwarfRegNum<[12]>;
def T1_64   : LoongArch64GPR<13, "r13",  [T1]>, DwarfRegNum<[13]>;
def T2_64   : LoongArch64GPR<14, "r14",  [T2]>, DwarfRegNum<[14]>;
def T3_64   : LoongArch64GPR<15, "r15",  [T3]>, DwarfRegNum<[15]>;
def T4_64   : LoongArch64GPR<16, "r16",  [T4]>, DwarfRegNum<[16]>;
def T5_64   : LoongArch64GPR<17, "r17",  [T5]>, DwarfRegNum<[17]>;
def T6_64   : LoongArch64GPR<18, "r18",  [T6]>, DwarfRegNum<[18]>;
def T7_64   : LoongArch64GPR<19, "r19",  [T7]>, DwarfRegNum<[19]>;
def T8_64   : LoongArch64GPR<20, "r20",  [T8]>, DwarfRegNum<[20]>;
def T9_64   : LoongArch64GPR<21, "r21",  [T9]>, DwarfRegNum<[21]>;
def FP_64   : LoongArch64GPR<22, "r22",  [FP]>, DwarfRegNum<[22]>;
def S0_64   : LoongArch64GPR<23, "r23",  [S0]>, DwarfRegNum<[23]>;
def S1_64   : LoongArch64GPR<24, "r24",  [S1]>, DwarfRegNum<[24]>;
def S2_64   : LoongArch64GPR<25, "r25",  [S2]>, DwarfRegNum<[25]>;
def S3_64   : LoongArch64GPR<26, "r26",  [S3]>, DwarfRegNum<[26]>;
def S4_64   : LoongArch64GPR<27, "r27",  [S4]>, DwarfRegNum<[27]>;
def S5_64   : LoongArch64GPR<28, "r28",  [S5]>, DwarfRegNum<[28]>;
def S6_64   : LoongArch64GPR<29, "r29",  [S6]>, DwarfRegNum<[29]>;
def S7_64   : LoongArch64GPR<30, "r30",  [S7]>, DwarfRegNum<[30]>;
def S8_64   : LoongArch64GPR<31, "r31",  [S8]>, DwarfRegNum<[31]>;

let SubRegIndices = [sub_64] in {
def V0_64   : LoongArch64GPR<4, "r4",  [A0_64]>, DwarfRegNum<[4]>;
def V1_64   : LoongArch64GPR<5, "r5",  [A1_64]>, DwarfRegNum<[5]>;
}

/// FP registers
foreach I = 0-31 in
def F#I : FGR32<I, "f"#I>, DwarfRegNum<[!add(I, 32)]>;

foreach I = 0-31 in
def F#I#_64 : FGR64<I, "f"#I, [!cast<FGR32>("F"#I)]>, DwarfRegNum<[!add(I, 32)]>;

/// FP Condition Flag 0~7
foreach I = 0-7 in
def FCC#I : LoongArchReg<I, "fcc"#I>;

/// FP Control and Status Registers, FCSR 1~3
foreach I = 1-3 in
def FCSR#I : LoongArchReg<I, "fcsr"#I>;

class FCSRReg<bits<16> Enc, string n, list<Register> subregs> :
    RegisterWithSubRegs<n, subregs> {
//  field bits<2> chan_encoding = 0;
  let Namespace = "LoongArch";
  let SubRegIndices = [sub_fcsr1, sub_fcsr2, sub_fcsr3];
//  let HWEncoding{8-0} = encoding{8-0};
//  let HWEncoding{10-9} = chan_encoding;
}

def FCSR0 : FCSRReg<0, "fcsr0", [FCSR1, FCSR2, FCSR3]>;

/// PC register
//let NameSpace = "LoongArch" in
//def PC : Register<"pc">;

//===----------------------------------------------------------------------===//
// Register Classes
//===----------------------------------------------------------------------===//

def GPR32 : RegisterClass<"LoongArch", [i32], 32, (add
  // Reserved
  ZERO,
  // Return Values and Arguments
  A0, A1, A2, A3, A4, A5, A6, A7,
  // Not preserved across procedure calls
  T0, T1, T2, T3, T4, T5, T6, T7, T8,
  // Callee save
  S0, S1, S2, S3, S4, S5, S6, S7, S8,
  // Reserved
  RA, TP, SP,
  // Reserved
  T9, FP)>;

def GPR64 : RegisterClass<"LoongArch", [i64], 64, (add
  // Reserved
  ZERO_64,
  // Return Values and Arguments
  A0_64, A1_64, A2_64, A3_64, A4_64, A5_64, A6_64, A7_64,
  // Not preserved across procedure calls
  T0_64, T1_64, T2_64, T3_64, T4_64, T5_64, T6_64, T7_64, T8_64,
  // Callee save
  S0_64, S1_64, S2_64, S3_64, S4_64, S5_64, S6_64, S7_64, S8_64,
  // Reserved
  RA_64, TP_64, SP_64,
  // Reserved
  T9_64, FP_64)>;

/// FP Registers.
def FGR64 : RegisterClass<"LoongArch", [f64], 64, (sequence "F%u_64", 0, 31)>;
def FGR32 : RegisterClass<"LoongArch", [f32], 64, (sequence "F%u", 0, 31)>;

/// FP condition Flag registers.
def FCFR : RegisterClass<"LoongArch", [i32], 32, (sequence "FCC%u", 0, 7)>,
           Unallocatable;

def SP32 : RegisterClass<"LoongArch", [i32], 32, (add SP)>, Unallocatable;
def SP64 : RegisterClass<"LoongArch", [i64], 64, (add SP_64)>, Unallocatable;
def TP32 : RegisterClass<"LoongArch", [i32], 32, (add TP)>, Unallocatable;
def TP64 : RegisterClass<"LoongArch", [i64], 64, (add TP_64)>, Unallocatable;

/// FP control and Status registers.
def FCSR : RegisterClass<"LoongArch", [i32], 4, (sequence "FCSR%u", 0, 3)>,
           Unallocatable;

//===----------------------------------------------------------------------===//
// Register Operands.
//===----------------------------------------------------------------------===//

class LoongArchAsmRegOperand : AsmOperandClass {
  let ParserMethod = "parseAnyRegister";
}

def GPR32AsmOperand : LoongArchAsmRegOperand {
  let Name = "GPR32AsmReg";
  let PredicateMethod = "isGPRAsmReg";
}

def GPR64AsmOperand : LoongArchAsmRegOperand {
  let Name = "GPR64AsmReg";
  let PredicateMethod = "isGPRAsmReg";
}

def FGR32AsmOperand : LoongArchAsmRegOperand {
  let Name = "FGR32AsmReg";
  let PredicateMethod = "isFGRAsmReg";
}

def FGR64AsmOperand : LoongArchAsmRegOperand {
  let Name = "FGR64AsmReg";
  let PredicateMethod = "isFGRAsmReg";
}

def FCSRAsmOperand : LoongArchAsmRegOperand {
  let Name = "FCSRAsmReg";
}

def FCFRAsmOperand : LoongArchAsmRegOperand {
  let Name = "FCFRAsmReg";
}

def GPR32Opnd : RegisterOperand<GPR32> {
  let ParserMatchClass = GPR32AsmOperand;
}

def GPR64Opnd : RegisterOperand<GPR64> {
  let ParserMatchClass = GPR64AsmOperand;
}

def FGR32Opnd : RegisterOperand<FGR32> {
  let ParserMatchClass = FGR32AsmOperand;
}

def FGR64Opnd : RegisterOperand<FGR64> {
  let ParserMatchClass = FGR64AsmOperand;
}

def FCSROpnd : RegisterOperand<FCSR> {
  let ParserMatchClass = FCSRAsmOperand;
}

def FCFROpnd : RegisterOperand<FCFR> {
  let ParserMatchClass = FCFRAsmOperand;
}
