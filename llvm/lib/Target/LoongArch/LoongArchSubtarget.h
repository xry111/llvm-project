//===-- LoongArchSubtarget.h - Define Subtarget for the LoongArch ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the LoongArch specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_LOONGARCHSUBTARGET_H
#define LLVM_LIB_TARGET_LOONGARCH_LOONGARCHSUBTARGET_H

#include "MCTargetDesc/LoongArchABIInfo.h"
#include "LoongArchFrameLowering.h"
#include "LoongArchISelLowering.h"
#include "LoongArchInstrInfo.h"
#include "llvm/CodeGen/SelectionDAGTargetInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/MC/MCInstrItineraries.h"
#include "llvm/Support/ErrorHandling.h"
#include <string>

#define GET_SUBTARGETINFO_HEADER
#include "LoongArchGenSubtargetInfo.inc"

namespace llvm {
class StringRef;

class LoongArchTargetMachine;

class LoongArchSubtarget : public LoongArchGenSubtargetInfo {
  virtual void anchor();

  // HasLA64 - The target processor has LA64 ISA support.
  bool HasLA64;

  // HasBasicF - The target restricts the use of hardware floating-point
  // instructions to 32-bit operations.
  bool HasBasicF;

  // HasBasicD - The target allows hardware floating-point instructions to
  // cover both 32-bit and 64-bit operations.
  bool HasBasicD;

  /// The minimum alignment known to hold of the stack frame on
  /// entry to the function and which must be maintained by every function.
  Align stackAlignment;

  /// The overridden stack alignment.
  MaybeAlign StackAlignOverride;

  InstrItineraryData InstrItins;

  const LoongArchTargetMachine &TM;

  Triple TargetTriple;

  const SelectionDAGTargetInfo TSInfo;
  const LoongArchInstrInfo InstrInfo;
  const LoongArchFrameLowering FrameLowering;
  const LoongArchTargetLowering TLInfo;

public:
  bool isPositionIndependent() const;
  /// This overrides the PostRAScheduler bit in the SchedModel for each CPU.
  bool enablePostRAScheduler() const override;
  void getCriticalPathRCs(RegClassVector &CriticalPathRCs) const override;
  CodeGenOpt::Level getOptLevelToEnablePostRAScheduler() const override;

  bool isABI_LP64() const;
  bool isABI_LP64D() const;
  bool isABI_LP64S() const;
  bool isABI_LP64F() const;
  bool isABI_ILP32() const;
  bool isABI_ILP32D() const;
  bool isABI_ILP32F() const;
  bool isABI_ILP32S() const;
  const LoongArchABIInfo &getABI() const;

  /// This constructor initializes the data members to match that
  /// of the specified triple.
  LoongArchSubtarget(const Triple &TT, StringRef CPU, StringRef TuneCPU,
                     StringRef FS, const LoongArchTargetMachine &TM,
                     MaybeAlign StackAlignOverride);

  /// ParseSubtargetFeatures - Parses features string setting specified
  /// subtarget options.  Definition of function is auto generated by tblgen.
  void ParseSubtargetFeatures(StringRef CPU, StringRef TuneCPU, StringRef FS);

  bool is64Bit() const { return HasLA64; }
  bool hasBasicD() const { return HasBasicD; }
  unsigned getGPRSizeInBytes() const { return is64Bit() ? 8 : 4; }
  bool hasBasicF() const { return HasBasicF; }
  bool useSoftFloat() const { return (!HasBasicD && !HasBasicF); }

  // After compiler-rt is supported in LA, this returns true.
  bool isXRaySupported() const override { return false; }

  Align getStackAlignment() const { return stackAlignment; }

  // Grab relocation model
  Reloc::Model getRelocationModel() const;

  LoongArchSubtarget &initializeSubtargetDependencies(StringRef CPU,
                                                      StringRef TuneCPU,
                                                      StringRef FS,
                                                      const TargetMachine &TM);

  const SelectionDAGTargetInfo *getSelectionDAGInfo() const override {
    return &TSInfo;
  }
  const LoongArchInstrInfo *getInstrInfo() const override {
    return &InstrInfo;
  }
  const TargetFrameLowering *getFrameLowering() const override {
    return &FrameLowering;
  }
  const LoongArchRegisterInfo *getRegisterInfo() const override {
    return &InstrInfo.getRegisterInfo();
  }
  const LoongArchTargetLowering *getTargetLowering() const override {
    return &TLInfo;
  }
  const InstrItineraryData *getInstrItineraryData() const override {
    return &InstrItins;
  }
};
} // End llvm namespace

#endif
