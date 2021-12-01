//===-- LoongArchTargetMachine.cpp - Define TargetMachine for LoongArch -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implements the info about LoongArch target spec.
//
//===----------------------------------------------------------------------===//

#include "LoongArchTargetMachine.h"
#include "MCTargetDesc/LoongArchABIInfo.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "LoongArch.h"
#include "LoongArchISelDAGToDAG.h"
#include "LoongArchSubtarget.h"
#include "LoongArchTargetObjectFile.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/CodeGen/BasicTTIImpl.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetOptions.h"
#include <string>
#include <cassert>

using namespace llvm;

#define DEBUG_TYPE "loongarch"

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeLoongArchTarget() {
  // Register the target.
  RegisterTargetMachine<LoongArchTargetMachine> X(getTheLoongArch32Target());
  RegisterTargetMachine<LoongArchTargetMachine> A(getTheLoongArch64Target());
}

static std::string computeDataLayout(const Triple &TT, StringRef CPU,
                                     const TargetOptions &Options) {
  std::string Ret;
  LoongArchABIInfo ABI = LoongArchABIInfo::computeTargetABI(TT, CPU, Options.MCOptions);

  Ret += "e";

  if (ABI.IsLP32())
    Ret += "-m:m";
  else
    Ret += "-m:e";

  // Pointers are 32 bit on some ABIs.
  if (!ABI.IsLP64())
    Ret += "-p:32:32";

  // 8 and 16 bit integers only need to have natural alignment, but try to
  // align them to 32 bits. 64 bit integers have natural alignment.
  Ret += "-i8:8:32-i16:16:32-i64:64";

  // 32 bit registers are always available and the stack is at least 64 bit
  // aligned. On LP64 64 bit registers are also available and the stack is
  // 128 bit aligned.
  if (ABI.IsLP64() || ABI.IsLPX32())
    Ret += "-n32:64-S128";
  else
    Ret += "-n32-S64";

  return Ret;
}

static Reloc::Model getEffectiveRelocModel(bool JIT,
                                           Optional<Reloc::Model> RM) {
  if (!RM.hasValue() || JIT)
    return Reloc::Static;
  return *RM;
}

// On function prologue, the stack is created by decrementing
// its pointer. Once decremented, all references are done with positive
// offset from the stack/frame pointer, using StackGrowsUp enables
// an easier handling.
// Using CodeModel::Large enables different CALL behavior.
LoongArchTargetMachine::LoongArchTargetMachine(const Target &T, const Triple &TT,
                                     StringRef CPU, StringRef FS,
                                     const TargetOptions &Options,
                                     Optional<Reloc::Model> RM,
                                     Optional<CodeModel::Model> CM,
                                     CodeGenOpt::Level OL, bool JIT)
    : LLVMTargetMachine(T, computeDataLayout(TT, CPU, Options), TT,
                        CPU, FS, Options, getEffectiveRelocModel(JIT, RM),
                        getEffectiveCodeModel(CM, CodeModel::Small), OL),
      TLOF(std::make_unique<LoongArchTargetObjectFile>()),
      ABI(LoongArchABIInfo::computeTargetABI(TT, CPU, Options.MCOptions)) {
  initAsmInfo();
}

LoongArchTargetMachine::~LoongArchTargetMachine() = default;

const LoongArchSubtarget *
LoongArchTargetMachine::getSubtargetImpl(const Function &F) const {
  Attribute CPUAttr = F.getFnAttribute("target-cpu");
  Attribute FSAttr = F.getFnAttribute("target-features");
  Attribute TuneAttr = F.getFnAttribute("tune-cpu");

  std::string CPU =
      CPUAttr.isValid() ? CPUAttr.getValueAsString().str()
                        : TargetCPU;
  std::string TuneCPU =
      TuneAttr.isValid() ? CPUAttr.getValueAsString().str()
                         : CPU;
  std::string FS =
      FSAttr.isValid() ? FSAttr.getValueAsString().str()
                       : TargetFS;

  // FIXME: This is related to the code below to reset the target options,
  // we need to know whether or not the soft float flag is set on the
  // function, so we can enable it as a subtarget feature.
  bool softFloat =
      F.hasFnAttribute("use-soft-float") &&
      F.getFnAttribute("use-soft-float").getValueAsString() == "true";

  if (softFloat)
    FS += FS.empty() ? "+soft-float" : ",+soft-float";

  auto &I = SubtargetMap[CPU + FS];
  if (!I) {
    // This needs to be done before we create a new subtarget since any
    // creation will depend on the TM and the code generation flags on the
    // function that reside in TargetOptions.
    resetTargetOptions(F);

    I = std::make_unique<LoongArchSubtarget>(
        TargetTriple, CPU, TuneCPU, FS, *this,
        MaybeAlign(F.getParent()->getOverrideStackAlignment()));
  }
  return I.get();
}

namespace {

/// LoongArch Code Generator Pass Configuration Options.
class LoongArchPassConfig : public TargetPassConfig {
public:
  LoongArchPassConfig(LoongArchTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {
  }

  LoongArchTargetMachine &getLoongArchTargetMachine() const {
    return getTM<LoongArchTargetMachine>();
  }

  void addIRPasses() override;
  bool addInstSelector() override;
  void addPreEmitPass() override;
};

} // end anonymous namespace

TargetPassConfig *LoongArchTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new LoongArchPassConfig(*this, PM);
}

void LoongArchPassConfig::addIRPasses() {
  TargetPassConfig::addIRPasses();
  addPass(createAtomicExpandPass());
}
// Install an instruction selector pass using
// the ISelDag to gen LoongArch code.
bool LoongArchPassConfig::addInstSelector() {
  addPass(createLoongArchModuleISelDagPass());
  addPass(createLoongArchISelDag(getLoongArchTargetMachine(), getOptLevel()));
  return false;
}

TargetTransformInfo
LoongArchTargetMachine::getTargetTransformInfo(const Function &F) {
  LLVM_DEBUG(errs() << "Target Transform Info Pass Added\n");
  return TargetTransformInfo(BasicTTIImpl(this, F));
}

// Implemented by targets that want to run passes immediately before
// machine code is emitted. return true if -print-machineinstrs should
// print out the code after the passes.
void LoongArchPassConfig::addPreEmitPass() {
  // Expand pseudo instructions that are sensitive to register allocation.
  addPass(createLoongArchExpandPseudoPass());

  // Relax conditional branch instructions if they're otherwise out of
  // range of their destination.
  // This pass must be run after any pseudo instruction expansion
  addPass(&BranchRelaxationPassID);
}
