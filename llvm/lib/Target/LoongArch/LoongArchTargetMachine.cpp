//===-- LoongArchTargetMachine.cpp - Define TargetMachine for LoongArch ---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Implements the info about LoongArch target spec.
//
//===----------------------------------------------------------------------===//

#include "LoongArchTargetMachine.h"
#include "LoongArch.h"
#include "LoongArchTargetObjectFile.h"
#include "LoongArchTargetTransformInfo.h"
#include "MCTargetDesc/LoongArchBaseInfo.h"
#include "TargetInfo/LoongArchTargetInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/CodeGen/GlobalISel/IRTranslator.h"
#include "llvm/CodeGen/GlobalISel/InstructionSelect.h"
#include "llvm/CodeGen/GlobalISel/Legalizer.h"
#include "llvm/CodeGen/GlobalISel/RegBankSelect.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetOptions.h"
using namespace llvm;

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeLoongArchTarget() {
  RegisterTargetMachine<LoongArchTargetMachine> X(getTheLoongArch32Target());
  RegisterTargetMachine<LoongArchTargetMachine> Y(getTheLoongArch64Target());
  auto *PR = PassRegistry::getPassRegistry();
  initializeGlobalISel(*PR);
  initializeLoongArchExpandPseudoPass(*PR);
}

static StringRef computeDataLayout(const Triple &TT) {
  if (TT.isArch64Bit())
    return "e-m:e-p:64:64-i64:64-i128:128-n64-S128";
  assert(TT.isArch32Bit() && "only LA32 and LA64 are currently supported");
  return "e-m:e-p:32:32-i64:64-n32-S128";
}

static Reloc::Model getEffectiveRelocModel(const Triple &TT,
                                           Optional<Reloc::Model> RM) {
  if (!RM.hasValue())
    return Reloc::Static;
  return *RM;
}

LoongArchTargetMachine::LoongArchTargetMachine(
    const Target &T, const Triple &TT, StringRef CPU, StringRef FS,
    const TargetOptions &Options, Optional<Reloc::Model> RM,
    Optional<CodeModel::Model> CM, CodeGenOpt::Level OL, bool JIT)
    : LLVMTargetMachine(T, computeDataLayout(TT), TT, CPU, FS, Options,
                        getEffectiveRelocModel(TT, RM),
                        getEffectiveCodeModel(CM, CodeModel::Small), OL),
      TLOF(std::make_unique<LoongArchELFTargetObjectFile>()) {
  initAsmInfo();

  // RISC-V supports the MachineOutliner.
  setMachineOutliner(true);
}

const LoongArchSubtarget *
LoongArchTargetMachine::getSubtargetImpl(const Function &F) const {
  Attribute CPUAttr = F.getFnAttribute("target-cpu");
  Attribute TuneAttr = F.getFnAttribute("tune-cpu");
  Attribute FSAttr = F.getFnAttribute("target-features");

  std::string CPU =
      CPUAttr.isValid() ? CPUAttr.getValueAsString().str() : TargetCPU;
  std::string TuneCPU =
      TuneAttr.isValid() ? TuneAttr.getValueAsString().str() : CPU;
  std::string FS =
      FSAttr.isValid() ? FSAttr.getValueAsString().str() : TargetFS;
  std::string Key = CPU + TuneCPU + FS;
  auto &I = SubtargetMap[Key];
  if (!I) {
    // This needs to be done before we create a new subtarget since any
    // creation will depend on the TM and the code generation flags on the
    // function that reside in TargetOptions.
    resetTargetOptions(F);
    auto ABIName = Options.MCOptions.getABIName();
    if (const MDString *ModuleTargetABI = dyn_cast_or_null<MDString>(
            F.getParent()->getModuleFlag("target-abi"))) {
      auto TargetABI = LoongArchABI::getTargetABI(ABIName);
      if (TargetABI != LoongArchABI::ABI_Unknown &&
          ModuleTargetABI->getString() != ABIName) {
        report_fatal_error("-target-abi option != target-abi module flag");
      }
      ABIName = ModuleTargetABI->getString();
    }
    I = std::make_unique<LoongArchSubtarget>(TargetTriple, CPU, TuneCPU, FS,
                                             ABIName, *this);
  }
  return I.get();
}

TargetTransformInfo
LoongArchTargetMachine::getTargetTransformInfo(const Function &F) {
  return TargetTransformInfo(LoongArchTTIImpl(this, F));
}

// A RISC-V hart has a single byte-addressable address space of 2^XLEN bytes
// for all memory accesses, so it is reasonable to assume that an
// implementation has no-op address space casts. If an implementation makes a
// change to this, they can override it here.
bool LoongArchTargetMachine::isNoopAddrSpaceCast(unsigned SrcAS,
                                                 unsigned DstAS) const {
  return true;
}

namespace {
class LoongArchPassConfig : public TargetPassConfig {
public:
  LoongArchPassConfig(LoongArchTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  LoongArchTargetMachine &getLoongArchTargetMachine() const {
    return getTM<LoongArchTargetMachine>();
  }

  void addIRPasses() override;
  bool addInstSelector() override;
  bool addIRTranslator() override;
  bool addLegalizeMachineIR() override;
  bool addRegBankSelect() override;
  bool addGlobalInstructionSelect() override;
  void addPreEmitPass() override;
  void addPreEmitPass2() override;
  void addPreSched2() override;
  void addPreRegAlloc() override;
};
} // namespace

TargetPassConfig *
LoongArchTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new LoongArchPassConfig(*this, PM);
}

void LoongArchPassConfig::addIRPasses() {
  addPass(createAtomicExpandPass());
  TargetPassConfig::addIRPasses();
}

bool LoongArchPassConfig::addInstSelector() {
  addPass(createLoongArchISelDag(getLoongArchTargetMachine()));

  return false;
}

bool LoongArchPassConfig::addIRTranslator() {
  addPass(new IRTranslator(getOptLevel()));
  return false;
}

bool LoongArchPassConfig::addLegalizeMachineIR() {
  addPass(new Legalizer());
  return false;
}

bool LoongArchPassConfig::addRegBankSelect() {
  addPass(new RegBankSelect());
  return false;
}

bool LoongArchPassConfig::addGlobalInstructionSelect() {
  addPass(new InstructionSelect());
  return false;
}

void LoongArchPassConfig::addPreSched2() {}

void LoongArchPassConfig::addPreEmitPass() { addPass(&BranchRelaxationPassID); }

void LoongArchPassConfig::addPreEmitPass2() {
  addPass(createLoongArchExpandPseudoPass());
  // Schedule the expansion of AMOs at the last possible moment, avoiding the
  // possibility for other passes to break the requirements for forward
  // progress in the LR/SC block.
  addPass(createLoongArchExpandAtomicPseudoPass());
}

void LoongArchPassConfig::addPreRegAlloc() {}
