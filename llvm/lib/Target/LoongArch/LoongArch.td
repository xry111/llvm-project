//===-- LoongArch.td - Describe the LoongArch Target Machine -------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// RISC-V subtarget features and instruction predicates.
//===----------------------------------------------------------------------===//

def FeatureStdExtF
    : SubtargetFeature<"f", "HasStdExtF", "true",
                       "'F' (Single-Precision Floating-Point)">;
def HasStdExtF : Predicate<"Subtarget->hasStdExtF()">,
                           AssemblerPredicate<(all_of FeatureStdExtF),
                           "'F' (Single-Precision Floating-Point)">;

def FeatureStdExtD
    : SubtargetFeature<"d", "HasStdExtD", "true",
                       "'D' (Double-Precision Floating-Point)",
                       [FeatureStdExtF]>;
def HasStdExtD : Predicate<"Subtarget->hasStdExtD()">,
                           AssemblerPredicate<(all_of FeatureStdExtD),
                           "'D' (Double-Precision Floating-Point)">;

def Feature64Bit
    : SubtargetFeature<"64bit", "HasLA64", "true", "Implements LA64", []>;
def IsLA64 : Predicate<"Subtarget->is64Bit()">,
                       AssemblerPredicate<(all_of Feature64Bit),
                       "Loongson LA64 Base Instruction Set">;
def IsLA32 : Predicate<"!Subtarget->is64Bit()">,
                       AssemblerPredicate<(all_of (not Feature64Bit)),
                       "Loongson LA32 Base Instruction Set">;

def FeatureFixedLLSC
    : SubtargetFeature<"l", "HasFixedLLSC", "true",
                       "LL/SC errata is fixed in hardware">;
def HasFixedLLSC : Predicate<"Subtarget->hasStdExtD()">,
                             AssemblerPredicate<(all_of FeatureFixedLLSC),
                             "LL/SC errata is fixed in hardware">;

defvar LA32 = DefaultMode;
def LA64           : HwMode<"+64bit">;

foreach i = {1-31} in
    def FeatureReserveR#i :
        SubtargetFeature<"reserve-r"#i, "UserReservedRegister[LoongArch::R"#i#"]",
                         "true", "Reserve R"#i>;

//===----------------------------------------------------------------------===//
// Named operands for CSR instructions.
//===----------------------------------------------------------------------===//

include "LoongArchSystemOperands.td"

//===----------------------------------------------------------------------===//
// Registers, calling conventions, instruction descriptions.
//===----------------------------------------------------------------------===//

include "LoongArchSchedule.td"
include "LoongArchRegisterInfo.td"
include "LoongArchCallingConv.td"
include "LoongArchInstrInfo.td"
include "LoongArchRegisterBanks.td"

//===----------------------------------------------------------------------===//
// RISC-V processors supported.
//===----------------------------------------------------------------------===//

def : ProcessorModel<"loongarch32", NoSchedModel, [FeatureStdExtD]>;
def : ProcessorModel<"loongarch64", NoSchedModel,
                     [Feature64Bit, FeatureStdExtD]>;

// TODO: add models with soft or single float here

//===----------------------------------------------------------------------===//
// Define the RISC-V target.
//===----------------------------------------------------------------------===//

def LoongArchInstrInfo : InstrInfo {
  let guessInstructionProperties = 0;
}

def LoongArchAsmParser : AsmParser {
  let ShouldEmitMatchRegisterAltName = 1;
  let AllowDuplicateRegisterNames = 1;
}

def LoongArchAsmWriter : AsmWriter {
  int PassSubtarget = 1;
}

def LoongArch : Target {
  let InstructionSet = LoongArchInstrInfo;
  let AssemblyParsers = [LoongArchAsmParser];
  let AssemblyWriters = [LoongArchAsmWriter];
  let AllowRegisterRenaming = 1;
}
