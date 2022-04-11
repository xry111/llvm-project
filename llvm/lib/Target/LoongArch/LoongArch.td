//===-- LoongArch.td - Describe the LoongArch Target Machine ---------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This is the top level entry point for the LoongArch target.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

// The overall idea of the PredicateControl class is to chop the Predicates list
// into subsets that are usually overridden independently. This allows
// subclasses to partially override the predicates of their superclasses without
// having to re-add all the existing predicates.
class PredicateControl {
  // Predicates for the encoding scheme in use such as HasStdEnc
  list<Predicate> EncodingPredicates = [];
  // Predicates for the GPR size such as is64Bit
  list<Predicate> GPRPredicates = [];
  // Predicates for the FGR size and layout such as HasBasicD
  list<Predicate> FGRPredicates = [];
  // Predicates for the instruction group membership such as ISA's
  list<Predicate> InsnPredicates = [];
  // Predicate for the ISA extension that an instruction belongs to
  list<Predicate> ExtPredicate = [];
  // Predicate for marking the instruction as usable in hard-float mode only
  list<Predicate> HardFloatPredicate = [];
  // Predicates for anything else
  list<Predicate> AdditionalPredicates = [];
  list<Predicate> Predicates = !listconcat(EncodingPredicates,
                                           GPRPredicates,
                                           FGRPredicates,
                                           InsnPredicates,
                                           HardFloatPredicate,
                                           ExtPredicate,
                                           AdditionalPredicates);
}

// Like Requires<> but for the AdditionalPredicates list
class AdditionalRequires<list<Predicate> preds> {
  list<Predicate> AdditionalPredicates = preds;
}

//===----------------------------------------------------------------------===//
// LoongArch Subtarget features                                               //
//===----------------------------------------------------------------------===//

def Feature64Bit
    : SubtargetFeature<"64bit", "HasLA64", "true",
                       "LA64 Basic Integer and Privilege Instruction Set">;
def FeatureBasicF : SubtargetFeature<"f", "HasBasicF", "true",
                                     "'F' (Single-Precision Floating-Point)">;
def FeatureBasicD : SubtargetFeature<"d", "HasBasicD", "true",
                                     "'D' (Double-Precision Floating-Point)",
                                     [FeatureBasicF]>;

//===----------------------------------------------------------------------===//
// Register File, Calling Conv, Instruction Descriptions
//===----------------------------------------------------------------------===//

include "LoongArchRegisterInfo.td"
include "LoongArchInstrInfo.td"
include "LoongArchCallingConv.td"

def LoongArchInstrInfo : InstrInfo;

//===----------------------------------------------------------------------===//
// LoongArch processors supported.
//===----------------------------------------------------------------------===//

def : ProcessorModel<"la464", NoSchedModel, [Feature64Bit]>;

def LoongArchAsmParser : AsmParser {
  let ShouldEmitMatchRegisterName = 0;
}

def LoongArchAsmParserVariant : AsmParserVariant {
  int Variant = 0;

  // Recognize hard coded registers.
  string RegisterPrefix = "$";
}

def LoongArch : Target {
  let InstructionSet = LoongArchInstrInfo;
  let AssemblyParsers = [LoongArchAsmParser];
  let AssemblyParserVariants = [LoongArchAsmParserVariant];
  let AllowRegisterRenaming = 1;
}
