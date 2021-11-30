//===- LoongArchRegisterInfo.cpp - LoongArch Register Information -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the LoongArch implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "LoongArchRegisterInfo.h"
#include "MCTargetDesc/LoongArchABIInfo.h"
#include "LoongArch.h"
#include "LoongArchMachineFunction.h"
#include "LoongArchSubtarget.h"
#include "LoongArchTargetMachine.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cstdint>

using namespace llvm;

#define DEBUG_TYPE "loongarch-reg-info"

#define GET_REGINFO_TARGET_DESC
#include "LoongArchGenRegisterInfo.inc"

LoongArchRegisterInfo::LoongArchRegisterInfo() : LoongArchGenRegisterInfo(LoongArch::RA) {}

unsigned LoongArchRegisterInfo::getPICCallReg() { return LoongArch::T8; }

const TargetRegisterClass *
LoongArchRegisterInfo::getPointerRegClass(const MachineFunction &MF,
                                     unsigned Kind) const {
  LoongArchABIInfo ABI = MF.getSubtarget<LoongArchSubtarget>().getABI();
  LoongArchPtrClass PtrClassKind = static_cast<LoongArchPtrClass>(Kind);

  switch (PtrClassKind) {
  case LoongArchPtrClass::Default:
    return ABI.ArePtrs64bit() ? &LoongArch::GPR64RegClass : &LoongArch::GPR32RegClass;
  case LoongArchPtrClass::StackPointer:
    return ABI.ArePtrs64bit() ? &LoongArch::SP64RegClass : &LoongArch::SP32RegClass;
  }

  llvm_unreachable("Unknown pointer kind");
}

unsigned
LoongArchRegisterInfo::getRegPressureLimit(const TargetRegisterClass *RC,
                                      MachineFunction &MF) const {
  switch (RC->getID()) {
  default:
    return 0;
  case LoongArch::GPR32RegClassID:
  case LoongArch::GPR64RegClassID:
  {
    const TargetFrameLowering *TFI = MF.getSubtarget().getFrameLowering();
    return 28 - TFI->hasFP(MF);
  }
  case LoongArch::FGR32RegClassID:
    return 32;
  case LoongArch::FGR64RegClassID:
    return 32;
  }
}

//===----------------------------------------------------------------------===//
// Callee Saved Registers methods
//===----------------------------------------------------------------------===//

/// LoongArch Callee Saved Registers
const MCPhysReg *
LoongArchRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  const LoongArchSubtarget &Subtarget = MF->getSubtarget<LoongArchSubtarget>();

  if (Subtarget.isSingleFloat())
    return CSR_SingleFloatOnly_SaveList;

  if (Subtarget.isABI_LP64())
    return CSR_LP64_SaveList;

  if (Subtarget.isABI_LPX32())
    return CSR_LPX32_SaveList;

  return CSR_LP32_SaveList;
}

const uint32_t *
LoongArchRegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                       CallingConv::ID) const {
  const LoongArchSubtarget &Subtarget = MF.getSubtarget<LoongArchSubtarget>();

  if (Subtarget.isSingleFloat())
    return CSR_SingleFloatOnly_RegMask;

  if (Subtarget.isABI_LP64())
    return CSR_LP64_RegMask;

  return CSR_LP32_RegMask;
}

BitVector LoongArchRegisterInfo::
getReservedRegs(const MachineFunction &MF) const {
  static const MCPhysReg ReservedGPR32[] = {
    LoongArch::ZERO, LoongArch::SP, LoongArch::TP, LoongArch::T9
  };

  static const MCPhysReg ReservedGPR64[] = {
    LoongArch::ZERO_64, LoongArch::SP_64, LoongArch::TP_64, LoongArch::T9_64
  };

  BitVector Reserved(getNumRegs());
  const LoongArchSubtarget &Subtarget = MF.getSubtarget<LoongArchSubtarget>();

  for (unsigned I = 0; I < array_lengthof(ReservedGPR32); ++I)
    Reserved.set(ReservedGPR32[I]);

  for (unsigned I = 0; I < array_lengthof(ReservedGPR64); ++I)
    Reserved.set(ReservedGPR64[I]);

  // Reserve FP if this function should have a dedicated frame pointer register.
  if (Subtarget.getFrameLowering()->hasFP(MF)) {
    Reserved.set(LoongArch::FP);
    Reserved.set(LoongArch::FP_64);

    // Reserve the base register if we need to both realign the stack and
    // allocate variable-sized objects at runtime. This should test the
    // same conditions as LoongArchFrameLowering::hasBP().
    if (needsStackRealignment(MF) &&
        MF.getFrameInfo().hasVarSizedObjects()) {
      Reserved.set(LoongArch::S7);
      Reserved.set(LoongArch::S7_64);
    }
  }

  return Reserved;
}

bool
LoongArchRegisterInfo::requiresRegisterScavenging(const MachineFunction &MF) const {
  return true;
}

bool LoongArchRegisterInfo::
requiresFrameIndexScavenging(const MachineFunction &MF) const {
  return true;
}

bool
LoongArchRegisterInfo::trackLivenessAfterRegAlloc(const MachineFunction &MF) const {
  return true;
}

/// Get the size of the offset supported by the given load/store/inline asm.
/// The result includes the effects of any scale factors applied to the
/// instruction immediate.
static inline unsigned getLoadStoreOffsetSizeInBits(const unsigned Opcode,
                                                    MachineOperand MO) {
  switch (Opcode) {
  case LoongArch::LDPTR_W:
  case LoongArch::LDPTR_W32:
  case LoongArch::LDPTR_D:
  case LoongArch::STPTR_W:
  case LoongArch::STPTR_W32:
  case LoongArch::STPTR_D:
  case LoongArch::LL_W:
  case LoongArch::LL_D:
  case LoongArch::SC_W:
  case LoongArch::SC_D:
    return 14 + 2 /* scale factor */;
  case LoongArch::INLINEASM: {
    unsigned ConstraintID = InlineAsm::getMemoryConstraintID(MO.getImm());
    switch (ConstraintID) {
    case InlineAsm::Constraint_ZC: {
      return 14 + 2 /* scale factor */;
    }
    default:
      return 12;
    }
  }
  default:
    return 12;
  }
}

/// Get the scale factor applied to the immediate in the given load/store.
static inline unsigned getLoadStoreOffsetAlign(const unsigned Opcode) {
  switch (Opcode) {
  case LoongArch::LDPTR_W:
  case LoongArch::LDPTR_W32:
  case LoongArch::LDPTR_D:
  case LoongArch::STPTR_W:
  case LoongArch::STPTR_W32:
  case LoongArch::STPTR_D:
  case LoongArch::LL_W:
  case LoongArch::LL_D:
  case LoongArch::SC_W:
  case LoongArch::SC_D:
    return 4;
  default:
    return 1;
  }
}

// FrameIndex represent objects inside a abstract stack.
// We must replace FrameIndex with an stack/frame pointer
// direct reference.
void LoongArchRegisterInfo::
eliminateFrameIndex(MachineBasicBlock::iterator II, int SPAdj,
                    unsigned FIOperandNum, RegScavenger *RS) const {
  MachineInstr &MI = *II;
  MachineFunction &MF = *MI.getParent()->getParent();

  LLVM_DEBUG(errs() << "\nFunction : " << MF.getName() << "\n";
             errs() << "<--------->\n"
                    << MI);

  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  uint64_t stackSize = MF.getFrameInfo().getStackSize();
  int64_t spOffset = MF.getFrameInfo().getObjectOffset(FrameIndex);

  LLVM_DEBUG(errs() << "FrameIndex : " << FrameIndex << "\n"
                    << "spOffset   : " << spOffset << "\n"
                    << "stackSize  : " << stackSize << "\n"
                    << "SPAdj      : " << SPAdj << "\n"
                    << "alignment  : "
                    << DebugStr(MF.getFrameInfo().getObjectAlign(FrameIndex))
                    << "\n");

  MachineFrameInfo &MFI = MF.getFrameInfo();
  LoongArchFunctionInfo *LoongArchFI = MF.getInfo<LoongArchFunctionInfo>();

  LoongArchABIInfo ABI =
    static_cast<const LoongArchTargetMachine &>(MF.getTarget()).getABI();
  const LoongArchRegisterInfo *RegInfo =
    static_cast<const LoongArchRegisterInfo *>(MF.getSubtarget().getRegisterInfo());

  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
  int MinCSFI = 0;
  int MaxCSFI = -1;

  if (CSI.size()) {
    MinCSFI = CSI[0].getFrameIdx();
    MaxCSFI = CSI[CSI.size() - 1].getFrameIdx();
  }

  bool EhDataRegFI = LoongArchFI->isEhDataRegFI(FrameIndex);
  // The following stack frame objects are always referenced relative to $sp:
  //  1. Outgoing arguments.
  //  2. Pointer to dynamically allocated stack space.
  //  3. Locations for callee-saved registers.
  //  4. Locations for eh data registers.
  // Everything else is referenced relative to whatever register
  // getFrameRegister() returns.
  unsigned FrameReg;

  if ((FrameIndex >= MinCSFI && FrameIndex <= MaxCSFI) || EhDataRegFI)
    FrameReg = ABI.GetStackPtr();
  else if (RegInfo->needsStackRealignment(MF)) {
    if (MFI.hasVarSizedObjects() && !MFI.isFixedObjectIndex(FrameIndex))
      FrameReg = ABI.GetBasePtr();
    else if (MFI.isFixedObjectIndex(FrameIndex))
      FrameReg = getFrameRegister(MF);
    else
      FrameReg = ABI.GetStackPtr();
  } else
    FrameReg = getFrameRegister(MF);

  // Calculate final offset.
  // - There is no need to change the offset if the frame object is one of the
  //   following: an outgoing argument, pointer to a dynamically allocated
  //   stack space or a $gp restore location,
  // - If the frame object is any of the following, its offset must be adjusted
  //   by adding the size of the stack:
  //   incoming argument, callee-saved register location or local variable.
  bool IsKill = false;
  int64_t Offset;

  Offset = spOffset + (int64_t)stackSize;
  Offset += MI.getOperand(FIOperandNum + 1).getImm();
  // If the frame-pointer register($fp) is omited, the offset must be adjusted
  // by adding the SP adjustment.
  if (FrameReg == ABI.GetStackPtr())
    Offset += SPAdj;

  LLVM_DEBUG(errs() << "Offset     : " << Offset << "\n"
                    << "<--------->\n");

  if (!MI.isDebugValue()) {
    // Make sure Offset fits within the field available.
    // For ldptr/stptr/ll/sc instructions, this is a 14-bit signed immediate (scaled by
    // 2), otherwise it is a 12-bit signed immediate.
    unsigned OffsetBitSize =
        getLoadStoreOffsetSizeInBits(MI.getOpcode(), MI.getOperand(FIOperandNum - 1));
    const Align OffsetAlign(getLoadStoreOffsetAlign(MI.getOpcode()));

    if (OffsetBitSize == 16 && isInt<12>(Offset) &&
        !isAligned(OffsetAlign, Offset)) {
      // If we have an offset that needs to fit into a signed n-bit immediate
      // (where n == 16) and doesn't aligned and does fit into 12-bits
      // then use an ADDI
      MachineBasicBlock &MBB = *MI.getParent();
      DebugLoc DL = II->getDebugLoc();
      const TargetRegisterClass *PtrRC =
          ABI.ArePtrs64bit() ? &LoongArch::GPR64RegClass : &LoongArch::GPR32RegClass;
      MachineRegisterInfo &RegInfo = MBB.getParent()->getRegInfo();
      unsigned Reg = RegInfo.createVirtualRegister(PtrRC);
      const LoongArchInstrInfo &TII =
          *static_cast<const LoongArchInstrInfo *>(
              MBB.getParent()->getSubtarget().getInstrInfo());
      BuildMI(MBB, II, DL, TII.get(ABI.GetPtrAddiOp()), Reg)
          .addReg(FrameReg)
          .addImm(Offset);

      FrameReg = Reg;
      Offset = 0;
      IsKill = true;
    } else if (!isInt<12>(Offset)) {
      // Otherwise split the offset into several pieces and add it in multiple
      // instructions.
      MachineBasicBlock &MBB = *MI.getParent();
      DebugLoc DL = II->getDebugLoc();
      bool IsNewImm = false;
      unsigned NewImm = 0;
      const LoongArchInstrInfo &TII =
          *static_cast<const LoongArchInstrInfo *>(
              MBB.getParent()->getSubtarget().getInstrInfo());
      if (OffsetBitSize == 12 || isAligned(OffsetAlign, Offset))
        IsNewImm = true;
      unsigned Reg = TII.loadImmediate(Offset, MBB, II, DL,
                                       IsNewImm ? &NewImm : nullptr);
      BuildMI(MBB, II, DL, TII.get(ABI.GetPtrAddOp()), Reg).addReg(FrameReg)
        .addReg(Reg, RegState::Kill);

      FrameReg = Reg;
      Offset = SignExtend64<12>(NewImm);
      IsKill = true;
    }
  }

  MI.getOperand(FIOperandNum).ChangeToRegister(FrameReg, false, false, IsKill);
  MI.getOperand(FIOperandNum + 1).ChangeToImmediate(Offset);
}

Register LoongArchRegisterInfo::
getFrameRegister(const MachineFunction &MF) const {
  const LoongArchSubtarget &Subtarget = MF.getSubtarget<LoongArchSubtarget>();
  const TargetFrameLowering *TFI = Subtarget.getFrameLowering();
  bool IsLP64 =
      static_cast<const LoongArchTargetMachine &>(MF.getTarget()).getABI().IsLP64();

  return TFI->hasFP(MF) ? (IsLP64 ? LoongArch::FP_64 : LoongArch::FP) :
                            (IsLP64 ? LoongArch::SP_64 : LoongArch::SP);
}

const TargetRegisterClass *
LoongArchRegisterInfo::intRegClass(unsigned Size) const {
  if (Size == 4)
    return &LoongArch::GPR32RegClass;

  assert(Size == 8);
  return &LoongArch::GPR64RegClass;
}

bool LoongArchRegisterInfo::canRealignStack(const MachineFunction &MF) const {
  // Avoid realigning functions that explicitly do not want to be realigned.
  // Normally, we should report an error when a function should be dynamically
  // realigned but also has the attribute no-realign-stack. Unfortunately,
  // with this attribute, MachineFrameInfo clamps each new object's alignment
  // to that of the stack's alignment as specified by the ABI. As a result,
  // the information of whether we have objects with larger alignment
  // requirement than the stack's alignment is already lost at this point.
  if (!TargetRegisterInfo::canRealignStack(MF))
    return false;

  const LoongArchSubtarget &Subtarget = MF.getSubtarget<LoongArchSubtarget>();
  unsigned FP = Subtarget.is64Bit() ? LoongArch::FP_64 : LoongArch::FP;
  unsigned BP = Subtarget.is64Bit() ? LoongArch::S7_64 : LoongArch::S7;

  // We can't perform dynamic stack realignment if we can't reserve the
  // frame pointer register.
  if (!MF.getRegInfo().canReserveReg(FP))
    return false;

  // We can realign the stack if we know the maximum call frame size and we
  // don't have variable sized objects.
  if (Subtarget.getFrameLowering()->hasReservedCallFrame(MF))
    return true;

  // We have to reserve the base pointer register in the presence of variable
  // sized objects.
  return MF.getRegInfo().canReserveReg(BP);
}
