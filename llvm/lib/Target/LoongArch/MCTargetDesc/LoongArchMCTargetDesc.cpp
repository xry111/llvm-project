//===-- LoongArchMCTargetDesc.cpp - LoongArch Target Descriptions -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides LoongArch specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "LoongArchMCTargetDesc.h"
#include "LoongArchTargetStreamer.h"
#include "MCTargetDesc/LoongArchAsmBackend.h"
#include "MCTargetDesc/LoongArchELFStreamer.h"
#include "MCTargetDesc/LoongArchInstPrinter.h"
#include "MCTargetDesc/LoongArchMCAsmInfo.h"
#include "TargetInfo/LoongArchTargetInfo.h"
#include "llvm/ADT/Triple.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCELFStreamer.h"
#include "llvm/MC/MCInstrAnalysis.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MachineLocation.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

#define GET_INSTRINFO_MC_DESC
#include "LoongArchGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "LoongArchGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "LoongArchGenRegisterInfo.inc"

/// Select the LoongArch CPU for the given triple and cpu name.
/// FIXME: Merge with the copy in LoongArchSubtarget.cpp
StringRef LoongArch_MC::selectLoongArchCPU(const Triple &TT, StringRef CPU) {
  if (CPU.empty() || CPU == "generic") {
    if (TT.isLoongArch32())
      CPU = "loongarch32"; //FIXME
    else
      CPU = "la464";
  }
  return CPU;
}

static MCInstrInfo *createLoongArchMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitLoongArchMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createLoongArchMCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitLoongArchMCRegisterInfo(X, LoongArch::RA);
  return X;
}

static MCSubtargetInfo *createLoongArchMCSubtargetInfo(const Triple &TT,
                                                  StringRef CPU, StringRef FS) {
  CPU = LoongArch_MC::selectLoongArchCPU(TT, CPU);
  return createLoongArchMCSubtargetInfoImpl(TT, CPU, CPU, FS);
}

static MCAsmInfo *createLoongArchMCAsmInfo(const MCRegisterInfo &MRI,
                                           const Triple &TT,
                                           const MCTargetOptions &Options) {
  MCAsmInfo *MAI = new LoongArchMCAsmInfo(TT, Options);

  unsigned SP = MRI.getDwarfRegNum(LoongArch::SP, true);
  MCCFIInstruction Inst = MCCFIInstruction::createDefCfaRegister(nullptr, SP);
  MAI->addInitialFrameState(Inst);

  return MAI;
}

static MCInstPrinter *createLoongArchMCInstPrinter(const Triple &T,
                                                   unsigned SyntaxVariant,
                                                   const MCAsmInfo &MAI,
                                                   const MCInstrInfo &MII,
                                                   const MCRegisterInfo &MRI) {
  return new LoongArchInstPrinter(MAI, MII, MRI);
}

static MCStreamer *createMCStreamer(const Triple &T, MCContext &Context,
                                    std::unique_ptr<MCAsmBackend> &&MAB,
                                    std::unique_ptr<MCObjectWriter> &&OW,
                                    std::unique_ptr<MCCodeEmitter> &&Emitter,
                                    bool RelaxAll) {
  MCStreamer *S;
  S = createLoongArchELFStreamer(Context, std::move(MAB), std::move(OW),
                              std::move(Emitter), RelaxAll);
  return S;
}

static MCTargetStreamer *createLoongArchAsmTargetStreamer(MCStreamer &S,
                                                     formatted_raw_ostream &OS,
                                                     MCInstPrinter *InstPrint,
                                                     bool isVerboseAsm) {
  return new LoongArchTargetAsmStreamer(S, OS);
}

static MCTargetStreamer *createLoongArchNullTargetStreamer(MCStreamer &S) {
  return new LoongArchTargetStreamer(S);
}

static MCTargetStreamer *
createLoongArchObjectTargetStreamer(MCStreamer &S, const MCSubtargetInfo &STI) {
  return new LoongArchTargetELFStreamer(S, STI);
}

namespace {

class LoongArchMCInstrAnalysis : public MCInstrAnalysis {
public:
  LoongArchMCInstrAnalysis(const MCInstrInfo *Info) : MCInstrAnalysis(Info) {}

  bool evaluateBranch(const MCInst &Inst, uint64_t Addr, uint64_t Size,
                      uint64_t &Target) const override {
    unsigned NumOps = Inst.getNumOperands();
    if (NumOps == 0)
      return false;
    switch (Info->get(Inst.getOpcode()).OpInfo[NumOps - 1].OperandType) {
    case MCOI::OPERAND_UNKNOWN:
    case MCOI::OPERAND_IMMEDIATE:
      Target = Inst.getOperand(NumOps - 1).getImm();
      return true;
    case MCOI::OPERAND_PCREL:
      // b, beq ...
      Target = Addr + Inst.getOperand(NumOps - 1).getImm();
      return true;
    default:
      return false;
    }
  }
};
}

static MCInstrAnalysis *createLoongArchMCInstrAnalysis(const MCInstrInfo *Info) {
  return new LoongArchMCInstrAnalysis(Info);
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeLoongArchTargetMC() {
  for (Target *T : {&getTheLoongArch32Target(), &getTheLoongArch64Target()}) {
    // Register the MC asm info.
    RegisterMCAsmInfoFn X(*T, createLoongArchMCAsmInfo);

    // Register the MC instruction info.
    TargetRegistry::RegisterMCInstrInfo(*T, createLoongArchMCInstrInfo);

    // Register the MC register info.
    TargetRegistry::RegisterMCRegInfo(*T, createLoongArchMCRegisterInfo);

    // Register the elf streamer.
    TargetRegistry::RegisterELFStreamer(*T, createMCStreamer);

    // Register the asm target streamer.
    TargetRegistry::RegisterAsmTargetStreamer(*T, createLoongArchAsmTargetStreamer);

    TargetRegistry::RegisterNullTargetStreamer(*T,
                                               createLoongArchNullTargetStreamer);

    // Register the MC subtarget info.
    TargetRegistry::RegisterMCSubtargetInfo(*T, createLoongArchMCSubtargetInfo);

    // Register the MC instruction analyzer.
    TargetRegistry::RegisterMCInstrAnalysis(*T, createLoongArchMCInstrAnalysis);

    // Register the MCInstPrinter.
    TargetRegistry::RegisterMCInstPrinter(*T, createLoongArchMCInstPrinter);

    TargetRegistry::RegisterObjectTargetStreamer(
        *T, createLoongArchObjectTargetStreamer);

    // Register the asm backend.
    TargetRegistry::RegisterMCAsmBackend(*T, createLoongArchAsmBackend);
  }

  // Register the MC Code Emitter
  for (Target *T : {&getTheLoongArch32Target(), &getTheLoongArch64Target()})
    TargetRegistry::RegisterMCCodeEmitter(*T, createLoongArchMCCodeEmitter);
}
