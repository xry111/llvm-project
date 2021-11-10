//===-- LoongArchRegisterInfo.cpp - LoongArch Register Information --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the LoongArch implementation of the TargetRegisterInfo
// class.
//
//===----------------------------------------------------------------------===//

#include "LoongArchRegisterInfo.h"
#include "LoongArch.h"
#include "LoongArchMachineFunctionInfo.h"
#include "LoongArchSubtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/Support/ErrorHandling.h"

#define GET_REGINFO_TARGET_DESC
#include "LoongArchGenRegisterInfo.inc"

using namespace llvm;

static_assert(LoongArch::R1 == LoongArch::R0 + 1,
              "Register list not consecutive");
static_assert(LoongArch::R31 == LoongArch::R0 + 31,
              "Register list not consecutive");
static_assert(LoongArch::F1_F == LoongArch::F0_F + 1,
              "Register list not consecutive");
static_assert(LoongArch::F31_F == LoongArch::F0_F + 31,
              "Register list not consecutive");
static_assert(LoongArch::F1_D == LoongArch::F0_D + 1,
              "Register list not consecutive");
static_assert(LoongArch::F31_D == LoongArch::F0_D + 31,
              "Register list not consecutive");

LoongArchRegisterInfo::LoongArchRegisterInfo(unsigned HwMode)
    : LoongArchGenRegisterInfo(LoongArch::R1, /*DwarfFlavour*/ 0,
                               /*EHFlavor*/ 0,
                               /*PC*/ 0, HwMode) {}

const MCPhysReg *
LoongArchRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  auto &Subtarget = MF->getSubtarget<LoongArchSubtarget>();
  if (MF->getFunction().getCallingConv() == CallingConv::GHC)
    return CSR_NoRegs_SaveList;
  if (MF->getFunction().hasFnAttribute("interrupt")) {
    if (Subtarget.hasStdExtD())
      return CSR_XLEN_F64_Interrupt_SaveList;
    if (Subtarget.hasStdExtF())
      return CSR_XLEN_F32_Interrupt_SaveList;
    return CSR_Interrupt_SaveList;
  }

  switch (Subtarget.getTargetABI()) {
  default:
    llvm_unreachable("Unrecognized ABI");
  case LoongArchABI::ABI_ILP32S:
  case LoongArchABI::ABI_LP64S:
    return CSR_ILP32S_LP64S_SaveList;
  case LoongArchABI::ABI_ILP32F:
  case LoongArchABI::ABI_LP64F:
    return CSR_ILP32F_LP64F_SaveList;
  case LoongArchABI::ABI_ILP32D:
  case LoongArchABI::ABI_LP64D:
    return CSR_ILP32D_LP64D_SaveList;
  }
}

BitVector
LoongArchRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  const LoongArchFrameLowering *TFI = getFrameLowering(MF);
  BitVector Reserved(getNumRegs());

  // Mark any registers requested to be reserved as such
  for (size_t Reg = 0; Reg < getNumRegs(); Reg++) {
    if (MF.getSubtarget<LoongArchSubtarget>().isRegisterReservedByUser(Reg))
      markSuperRegs(Reserved, Reg);
  }

  // Use markSuperRegs to ensure any register aliases are also reserved
  markSuperRegs(Reserved, LoongArch::R0);  // zero
  markSuperRegs(Reserved, LoongArch::R2);  // tp
  markSuperRegs(Reserved, LoongArch::R3);  // fp
  markSuperRegs(Reserved, LoongArch::R21); // reserved for some reason
  if (TFI->hasFP(MF))
    markSuperRegs(Reserved, LoongArch::R22); // fp
  // Reserve the base register if we need to realign the stack and allocate
  // variable-sized objects at runtime.
  if (TFI->hasBP(MF))
    markSuperRegs(Reserved, LoongArchABI::getBPReg()); // bp

  assert(checkAllSuperRegsMarked(Reserved));
  return Reserved;
}

bool LoongArchRegisterInfo::isAsmClobberable(const MachineFunction &MF,
                                             MCRegister PhysReg) const {
  return !MF.getSubtarget<LoongArchSubtarget>().isRegisterReservedByUser(
      PhysReg);
}

bool LoongArchRegisterInfo::isConstantPhysReg(MCRegister PhysReg) const {
  return PhysReg == LoongArch::R0;
}

const uint32_t *LoongArchRegisterInfo::getNoPreservedMask() const {
  return CSR_NoRegs_RegMask;
}

// Frame indexes representing locations of CSRs which are given a fixed location
// by save/restore libcalls.
static const std::map<unsigned, int> FixedCSRFIMap = {
    {/*ra*/ LoongArch::R1, -1},   {/*fp*/ LoongArch::R22, -2},
    {/*s0*/ LoongArch::R23, -3},  {/*s1*/ LoongArch::R24, -4},
    {/*s2*/ LoongArch::R25, -5},  {/*s3*/ LoongArch::R26, -6},
    {/*s4*/ LoongArch::R27, -7},  {/*s5*/ LoongArch::R28, -8},
    {/*s6*/ LoongArch::R29, -9},  {/*s7*/ LoongArch::R30, -10},
    {/*s8*/ LoongArch::R31, -11},
};

bool LoongArchRegisterInfo::hasReservedSpillSlot(const MachineFunction &MF,
                                                 Register Reg,
                                                 int &FrameIdx) const {
  return false;
}

void LoongArchRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                                int SPAdj,
                                                unsigned FIOperandNum,
                                                RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected non-zero SPAdj value");

  MachineInstr &MI = *II;
  MachineFunction &MF = *MI.getParent()->getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  const LoongArchSubtarget &STI = MF.getSubtarget<LoongArchSubtarget>();
  const LoongArchInstrInfo *TII = STI.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();

  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  Register FrameReg;
  int Offset = getFrameLowering(MF)
                   ->getFrameIndexReference(MF, FrameIndex, FrameReg)
                   .getFixed() +
               MI.getOperand(FIOperandNum + 1).getImm();

  if (!isInt<32>(Offset)) {
    report_fatal_error(
        "Frame offsets outside of the signed 32-bit range not supported");
  }

  MachineBasicBlock &MBB = *MI.getParent();
  bool FrameRegIsKill = false;

  if (!isInt<12>(Offset)) {
    assert(isInt<32>(Offset) && "Int32 expected");
    // The offset won't fit in an immediate, so use a scratch register instead
    // Modify Offset and FrameReg appropriately
    Register ScratchReg = MRI.createVirtualRegister(&LoongArch::GPRRegClass);
    TII->movImm(MBB, II, DL, ScratchReg, Offset);
    BuildMI(MBB, II, DL, TII->get(STI.getADD()), ScratchReg)
        .addReg(FrameReg)
        .addReg(ScratchReg, RegState::Kill);
    Offset = 0;
    FrameReg = ScratchReg;
    FrameRegIsKill = true;
  }

  MI.getOperand(FIOperandNum)
      .ChangeToRegister(FrameReg, false, false, FrameRegIsKill);
  MI.getOperand(FIOperandNum + 1).ChangeToImmediate(Offset);
}

Register
LoongArchRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = getFrameLowering(MF);
  return TFI->hasFP(MF) ? LoongArch::R22 : LoongArch::R3;
}

const uint32_t *
LoongArchRegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                            CallingConv::ID CC) const {
  auto &Subtarget = MF.getSubtarget<LoongArchSubtarget>();

  if (CC == CallingConv::GHC)
    return CSR_NoRegs_RegMask;
  switch (Subtarget.getTargetABI()) {
  default:
    llvm_unreachable("Unrecognized ABI");
  case LoongArchABI::ABI_ILP32S:
  case LoongArchABI::ABI_LP64S:
    return CSR_ILP32S_LP64S_RegMask;
  case LoongArchABI::ABI_ILP32F:
  case LoongArchABI::ABI_LP64F:
    return CSR_ILP32F_LP64F_RegMask;
  case LoongArchABI::ABI_ILP32D:
  case LoongArchABI::ABI_LP64D:
    return CSR_ILP32D_LP64D_RegMask;
  }
}
