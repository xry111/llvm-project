//===-- LoongArchRegisterInfo.td - RISC-V Register defs --------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Declarations that describe the RISC-V register files
//===----------------------------------------------------------------------===//

let Namespace = "LoongArch" in {
class LoongArchReg<bits<5> Enc, string n, list<string> alt = []> : Register<n> {
  let HWEncoding{4-0} = Enc;
  let AltNames = alt;
}

class LoongArchReg32<bits<5> Enc, string n, list<string> alt = []> : Register<n> {
  let HWEncoding{4-0} = Enc;
  let AltNames = alt;
}

class LoongArchCFR<bits<3> Enc, string n, list<string> alt = []> : Register<n> {
  let HWEncoding{2-0} = Enc;
  let AltNames = alt;
}

def sub_32 : SubRegIndex<32>;
class LoongArchReg64<LoongArchReg32 subreg> : Register<""> {
  let HWEncoding{4-0} = subreg.HWEncoding{4-0};
  let SubRegs = [subreg];
  let SubRegIndices = [sub_32];
  let AsmName = subreg.AsmName;
  let AltNames = subreg.AltNames;
}

class LoongArchRegWithSubRegs<bits<5> Enc, string n, list<Register> subregs,
                          list<string> alt = []>
      : RegisterWithSubRegs<n, subregs> {
  let HWEncoding{4-0} = Enc;
  let AltNames = alt;
}

def ABIRegAltName : RegAltNameIndex;

} // Namespace = "LoongArch"

// Integer registers
// CostPerUse is set higher for registers that may not be compressible as they
// are not part of GPRC, the most restrictive register class used by the
// compressed instruction set. This will influence the greedy register
// allocator to reduce the use of registers that can't be encoded in 16 bit
// instructions. This affects register allocation even when compressed
// instruction isn't targeted, we see no major negative codegen impact.

let RegAltNameIndices = [ABIRegAltName] in {
  def R0  : LoongArchReg<0, "r0", ["zero"]>, DwarfRegNum<[0]>;
  def R1  : LoongArchReg<1, "r1", ["ra"]>, DwarfRegNum<[1]>;
  def R2  : LoongArchReg<2, "r2", ["tp"]>, DwarfRegNum<[2]>;
  def R3  : LoongArchReg<3, "r3", ["sp"]>, DwarfRegNum<[3]>;
  def R4  : LoongArchReg<4, "r4", ["a0", "v0"]>, DwarfRegNum<[4]>;
  def R5  : LoongArchReg<5, "r5", ["a1", "v1"]>, DwarfRegNum<[5]>;
  def R6  : LoongArchReg<6, "r6", ["a2"]>, DwarfRegNum<[6]>;
  def R7  : LoongArchReg<7, "r7", ["a3"]>, DwarfRegNum<[7]>;
  def R8  : LoongArchReg<8, "r8", ["a4"]>, DwarfRegNum<[8]>;
  def R9  : LoongArchReg<9, "r9", ["a5"]>, DwarfRegNum<[9]>;
  def R10 : LoongArchReg<10,"r10", ["a6"]>, DwarfRegNum<[10]>;
  def R11 : LoongArchReg<11,"r11", ["a7"]>, DwarfRegNum<[11]>;
  def R12 : LoongArchReg<12,"r12", ["t0"]>, DwarfRegNum<[12]>;
  def R13 : LoongArchReg<13,"r13", ["t1"]>, DwarfRegNum<[13]>;
  def R14 : LoongArchReg<14,"r14", ["t2"]>, DwarfRegNum<[14]>;
  def R15 : LoongArchReg<15,"r15", ["t3"]>, DwarfRegNum<[15]>;
  def R16 : LoongArchReg<16,"r16", ["t4"]>, DwarfRegNum<[16]>;
  def R17 : LoongArchReg<17,"r17", ["t5"]>, DwarfRegNum<[17]>;
  def R18 : LoongArchReg<18,"r18", ["t6"]>, DwarfRegNum<[18]>;
  def R19 : LoongArchReg<19,"r19", ["t7"]>, DwarfRegNum<[19]>;
  def R20 : LoongArchReg<20,"r20", ["t8"]>, DwarfRegNum<[20]>;
  def R21 : LoongArchReg<21,"r21", ["x"]>, DwarfRegNum<[21]>;
  def R22 : LoongArchReg<22,"r22", ["fp", "s9"]>, DwarfRegNum<[22]>;
  def R23 : LoongArchReg<23,"r23", ["s0"]>, DwarfRegNum<[23]>;
  def R24 : LoongArchReg<24,"r24", ["s1"]>, DwarfRegNum<[24]>;
  def R25 : LoongArchReg<25,"r25", ["s2"]>, DwarfRegNum<[25]>;
  def R26 : LoongArchReg<26,"r26", ["s3"]>, DwarfRegNum<[26]>;
  def R27 : LoongArchReg<27,"r27", ["s4"]>, DwarfRegNum<[27]>;
  def R28 : LoongArchReg<28,"r28", ["s5"]>, DwarfRegNum<[28]>;
  def R29 : LoongArchReg<29,"r29", ["s6"]>, DwarfRegNum<[29]>;
  def R30 : LoongArchReg<30,"r30", ["s7"]>, DwarfRegNum<[30]>;
  def R31 : LoongArchReg<31,"r31", ["s8"]>, DwarfRegNum<[31]>;
}

def XLenVT : ValueTypeByHwMode<[LA32, LA64],
                               [i32,  i64]>;

// The order of registers represents the preferred allocation sequence.
// Registers are listed in the order caller-save, callee-save, specials.
def GPR : RegisterClass<"LoongArch", [XLenVT], 32, (add
    (sequence "R%u", 4, 11),
    (sequence "R%u", 12, 20),
    (sequence "R%u", 22, 31),
    (sequence "R%u", 0, 3),
    R21
  )> {
  let RegInfos = RegInfoByHwMode<
      [LA32,              LA64],
      [RegInfo<32,32,32>, RegInfo<64,64,64>]>;
}

def GPRR0 : RegisterClass<"LoongArch", [XLenVT], 32, (add R0)> {
  let RegInfos = RegInfoByHwMode<
      [LA32,              LA64],
      [RegInfo<32,32,32>, RegInfo<64,64,64>]>;
}

// For indirect tail calls, we can't use callee-saved registers, as they are
// restored to the saved value before the tail call, which would clobber a call
// address.
def GPRTC : RegisterClass<"LoongArch", [XLenVT], 32, (add
    (sequence "R%u", 4, 11),
    (sequence "R%u", 12, 20)
  )> {
  let RegInfos = RegInfoByHwMode<
      [LA32,              LA64],
      [RegInfo<32,32,32>, RegInfo<64,64,64>]>;
}

def SP : RegisterClass<"LoongArch", [XLenVT], 32, (add R3)> {
  let RegInfos = RegInfoByHwMode<
      [LA32,              LA64],
      [RegInfo<32,32,32>, RegInfo<64,64,64>]>;
}

// Floating point registers
let RegAltNameIndices = [ABIRegAltName] in {
  def F0_F  : LoongArchReg32<0, "f0", ["fa0", "fv0"]>, DwarfRegNum<[32]>;
  def F1_F  : LoongArchReg32<1, "f1", ["fa1", "fv1"]>, DwarfRegNum<[33]>;
  def F2_F  : LoongArchReg32<2, "f2", ["fa2"]>, DwarfRegNum<[34]>;
  def F3_F  : LoongArchReg32<3, "f3", ["fa3"]>, DwarfRegNum<[35]>;
  def F4_F  : LoongArchReg32<4, "f4", ["fa4"]>, DwarfRegNum<[36]>;
  def F5_F  : LoongArchReg32<5, "f5", ["fa5"]>, DwarfRegNum<[37]>;
  def F6_F  : LoongArchReg32<6, "f6", ["fa6"]>, DwarfRegNum<[38]>;
  def F7_F  : LoongArchReg32<7, "f7", ["fa7"]>, DwarfRegNum<[39]>;
  def F8_F  : LoongArchReg32<8, "f8", ["ft0"]>, DwarfRegNum<[40]>;
  def F9_F  : LoongArchReg32<9, "f9", ["ft1"]>, DwarfRegNum<[41]>;
  def F10_F : LoongArchReg32<10,"f10", ["ft2"]>, DwarfRegNum<[42]>;
  def F11_F : LoongArchReg32<11,"f11", ["ft3"]>, DwarfRegNum<[43]>;
  def F12_F : LoongArchReg32<12,"f12", ["ft4"]>, DwarfRegNum<[44]>;
  def F13_F : LoongArchReg32<13,"f13", ["ft5"]>, DwarfRegNum<[45]>;
  def F14_F : LoongArchReg32<14,"f14", ["ft6"]>, DwarfRegNum<[46]>;
  def F15_F : LoongArchReg32<15,"f15", ["ft7"]>, DwarfRegNum<[47]>;
  def F16_F : LoongArchReg32<16,"f16", ["ft8"]>, DwarfRegNum<[48]>;
  def F17_F : LoongArchReg32<17,"f17", ["ft9"]>, DwarfRegNum<[49]>;
  def F18_F : LoongArchReg32<18,"f18", ["ft10"]>, DwarfRegNum<[50]>;
  def F19_F : LoongArchReg32<19,"f19", ["ft11"]>, DwarfRegNum<[51]>;
  def F20_F : LoongArchReg32<20,"f20", ["ft12"]>, DwarfRegNum<[52]>;
  def F21_F : LoongArchReg32<21,"f21", ["ft13"]>, DwarfRegNum<[53]>;
  def F22_F : LoongArchReg32<22,"f22", ["ft14"]>, DwarfRegNum<[54]>;
  def F23_F : LoongArchReg32<23,"f23", ["ft15"]>, DwarfRegNum<[55]>;
  def F24_F : LoongArchReg32<24,"f24", ["ft0"]>, DwarfRegNum<[56]>;
  def F25_F : LoongArchReg32<25,"f25", ["fs1"]>, DwarfRegNum<[57]>;
  def F26_F : LoongArchReg32<26,"f26", ["fs2"]>, DwarfRegNum<[58]>;
  def F27_F : LoongArchReg32<27,"f27", ["fs3"]>, DwarfRegNum<[59]>;
  def F28_F : LoongArchReg32<28,"f28", ["ft4"]>, DwarfRegNum<[60]>;
  def F29_F : LoongArchReg32<29,"f29", ["ft5"]>, DwarfRegNum<[61]>;
  def F30_F : LoongArchReg32<30,"f30", ["ft6"]>, DwarfRegNum<[62]>;
  def F31_F : LoongArchReg32<31,"f31", ["ft7"]>, DwarfRegNum<[63]>;

  foreach Index = 0-31 in {
    def F#Index#_D : LoongArchReg64<!cast<LoongArchReg32>("F"#Index#"_F")>,
      DwarfRegNum<[!add(Index, 32)]>;
  }
}

// The order of registers represents the preferred allocation sequence,
// meaning caller-save regs are listed before callee-save.

def FPR32 : RegisterClass<"LoongArch", [f32], 32, (add
    (sequence "F%u_F", 0, 31)
)>;

// The order of registers represents the preferred allocation sequence,
// meaning caller-save regs are listed before callee-save.
def FPR64 : RegisterClass<"LoongArch", [f64], 64, (add
    (sequence "F%u_D", 0, 31)
)>;

let RegAltNameIndices = [ABIRegAltName] in {
  def FCC0 : LoongArchCFR<0, "fcc0", ["fcc0"]>;
  def FCC1 : LoongArchCFR<1, "fcc1", ["fcc1"]>;
  def FCC2 : LoongArchCFR<2, "fcc2", ["fcc2"]>;
  def FCC3 : LoongArchCFR<3, "fcc3", ["fcc3"]>;
  def FCC4 : LoongArchCFR<4, "fcc4", ["fcc4"]>;
  def FCC5 : LoongArchCFR<5, "fcc5", ["fcc5"]>;
  def FCC6 : LoongArchCFR<6, "fcc6", ["fcc6"]>;
  def FCC7 : LoongArchCFR<7, "fcc7", ["fcc7"]>;
}

def CFR : RegisterClass<"LoongArch", [i1], 1, (add
    (sequence "FCC%u", 0, 7)
)>;
