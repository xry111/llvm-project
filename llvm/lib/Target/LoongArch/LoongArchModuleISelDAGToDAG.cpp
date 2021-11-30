//===----------------------------------------------------------------------===//
// Instruction Selector Subtarget Control
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// This file defines a pass used to change the subtarget for the
// LoongArch Instruction selector.
//
//===----------------------------------------------------------------------===//

#include "LoongArch.h"
#include "LoongArchTargetMachine.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/CodeGen/StackProtector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "loongarch-isel"

namespace {
  class LoongArchModuleDAGToDAGISel : public MachineFunctionPass {
  public:
    static char ID;

    LoongArchModuleDAGToDAGISel() : MachineFunctionPass(ID) {}

    // Pass Name
    StringRef getPassName() const override {
      return "LoongArch DAG->DAG Pattern Instruction Selection";
    }

    void getAnalysisUsage(AnalysisUsage &AU) const override {
      AU.addRequired<TargetPassConfig>();
      AU.addPreserved<StackProtector>();
      MachineFunctionPass::getAnalysisUsage(AU);
    }

    bool runOnMachineFunction(MachineFunction &MF) override;
  };

  char LoongArchModuleDAGToDAGISel::ID = 0;
}

bool LoongArchModuleDAGToDAGISel::runOnMachineFunction(MachineFunction &MF) {
  LLVM_DEBUG(errs() << "In LoongArchModuleDAGToDAGISel::runMachineFunction\n");
  auto &TPC = getAnalysis<TargetPassConfig>();
  auto &TM = TPC.getTM<LoongArchTargetMachine>();
  return false;
}

llvm::FunctionPass *llvm::createLoongArchModuleISelDagPass() {
  return new LoongArchModuleDAGToDAGISel();
}
