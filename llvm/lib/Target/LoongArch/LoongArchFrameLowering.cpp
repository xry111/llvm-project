//===-- LoongArchFrameLowering.cpp - LoongArch Frame Information --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the LoongArch implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#include "LoongArchFrameLowering.h"
#include "MCTargetDesc/LoongArchBaseInfo.h"
#include "MCTargetDesc/LoongArchABIInfo.h"
#include "LoongArchInstrInfo.h"
#include "LoongArchMachineFunction.h"
#include "LoongArchTargetMachine.h"
#include "LoongArchRegisterInfo.h"
#include "LoongArchSubtarget.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Function.h"
#include "llvm/MC/MCDwarf.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MachineLocation.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Target/TargetOptions.h"
#include <cassert>
#include <cstdint>
#include <utility>
#include <vector>

using namespace llvm;

//===----------------------------------------------------------------------===//
//
// Stack Frame Processing methods
// +----------------------------+
//
// The stack is allocated decrementing the stack pointer on
// the first instruction of a function prologue. Once decremented,
// all stack references are done thought a positive offset
// from the stack/frame pointer, so the stack is considering
// to grow up! Otherwise terrible hacks would have to be made
// to get this stack ABI compliant :)
//
//  The stack frame required by the ABI (after call):
//  Offset
//
//  0                 ----------
//  4                 Args to pass
//  .                 Alloca allocations
//  .                 Local Area
//  .                 CPU "Callee Saved" Registers
//  .                 saved FP
//  .                 saved RA
//  .                 FPU "Callee Saved" Registers
//  StackSize         -----------
//
// Offset - offset from sp after stack allocation on function prologue
//
// The sp is the stack pointer subtracted/added from the stack size
// at the Prologue/Epilogue
//
// References to the previous stack (to obtain arguments) are done
// with offsets that exceeds the stack size: (stacksize+(4*(num_arg-1))
//
// Examples:
// - reference to the actual stack frame
//   for any local area var there is smt like : FI >= 0, StackOffset: 4
//     st.w REGX, SP, 4
//
// - reference to previous stack frame
//   suppose there's a load to the 5th arguments : FI < 0, StackOffset: 16.
//   The emitted instruction will be something like:
//     ld.w REGX, SP, 16+StackSize
//
// Since the total stack size is unknown on LowerFormalArguments, all
// stack references (ObjectOffset) created to reference the function
// arguments, are negative numbers. This way, on eliminateFrameIndex it's
// possible to detect those references and the offsets are adjusted to
// their real location.
//
//===----------------------------------------------------------------------===//
//
LoongArchFrameLowering::LoongArchFrameLowering(const LoongArchSubtarget &STI)
      : TargetFrameLowering(StackGrowsDown, STI.getStackAlignment(), 0,
                                    STI.getStackAlignment()), STI(STI) {}

void LoongArchFrameLowering::emitPrologue(MachineFunction &MF,
                                          MachineBasicBlock &MBB) const {
  MachineFrameInfo &MFI    = MF.getFrameInfo();
  LoongArchFunctionInfo *LoongArchFI = MF.getInfo<LoongArchFunctionInfo>();

  const LoongArchInstrInfo &TII =
      *static_cast<const LoongArchInstrInfo *>(STI.getInstrInfo());
  const LoongArchRegisterInfo &RegInfo =
      *static_cast<const LoongArchRegisterInfo *>(STI.getRegisterInfo());
  MachineBasicBlock::iterator MBBI = MBB.begin();
  DebugLoc dl;
  LoongArchABIInfo ABI = STI.getABI();
  unsigned SP = ABI.GetStackPtr();
  unsigned FP = ABI.GetFramePtr();
  unsigned ZERO = ABI.GetNullPtr();
  unsigned MOVE = ABI.GetGPRMoveOp();
  unsigned ADDI = ABI.GetPtrAddiOp();
  unsigned AND = ABI.IsLP64() ? LoongArch::AND : LoongArch::AND32;
  unsigned SLLI = ABI.IsLP64() ? LoongArch::SLLI_D : LoongArch::SLLI_W;

  const TargetRegisterClass *RC = ABI.ArePtrs64bit() ?
        &LoongArch::GPR64RegClass : &LoongArch::GPR32RegClass;

  // First, compute final stack size.
  uint64_t StackSize = MFI.getStackSize();

  // No need to allocate space on the stack.
  if (StackSize == 0 && !MFI.adjustsStack()) return;

  MachineModuleInfo &MMI = MF.getMMI();
  const MCRegisterInfo *MRI = MMI.getContext().getRegisterInfo();

  // Adjust stack.
  TII.adjustStackPtr(SP, -StackSize, MBB, MBBI);

  // emit ".cfi_def_cfa_offset StackSize"
  unsigned CFIIndex = MF.addFrameInst(
      MCCFIInstruction::cfiDefCfaOffset(nullptr, StackSize));
  BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);

  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();

  if (!CSI.empty()) {
    // Find the instruction past the last instruction that saves a callee-saved
    // register to the stack.
    for (unsigned i = 0; i < CSI.size(); ++i)
      ++MBBI;

    // Iterate over list of callee-saved registers and emit .cfi_offset
    // directives.
    for (std::vector<CalleeSavedInfo>::const_iterator I = CSI.begin(),
           E = CSI.end(); I != E; ++I) {
      int64_t Offset = MFI.getObjectOffset(I->getFrameIdx());
      unsigned Reg = I->getReg();
      // If Reg is a double precision register, emit two cfa_offsets,
      // one for each of the paired single precision registers.
      if (LoongArch::FGR64RegClass.contains(Reg)) {
        unsigned Reg0 = MRI->getDwarfRegNum(Reg, true);
        unsigned Reg1 = MRI->getDwarfRegNum(Reg, true) + 1;

        unsigned CFIIndex = MF.addFrameInst(
          MCCFIInstruction::createOffset(nullptr, Reg0, Offset));
        BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
            .addCFIIndex(CFIIndex);

        CFIIndex = MF.addFrameInst(
          MCCFIInstruction::createOffset(nullptr, Reg1, Offset + 4));
        BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
            .addCFIIndex(CFIIndex);
      } else {
        // Reg is either in GPR32 or FGR32.
        unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createOffset(
            nullptr, MRI->getDwarfRegNum(Reg, true), Offset));
        BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
            .addCFIIndex(CFIIndex);
      }
    }
  }

  if (LoongArchFI->callsEhReturn()) {
    // Insert instructions that spill eh data registers.
    for (int I = 0; I < 4; ++I) {
      if (!MBB.isLiveIn(ABI.GetEhDataReg(I)))
        MBB.addLiveIn(ABI.GetEhDataReg(I));
      TII.storeRegToStackSlot(MBB, MBBI, ABI.GetEhDataReg(I), false,
                              LoongArchFI->getEhDataRegFI(I), RC, &RegInfo);
    }

    // Emit .cfi_offset directives for eh data registers.
    for (int I = 0; I < 4; ++I) {
      int64_t Offset = MFI.getObjectOffset(LoongArchFI->getEhDataRegFI(I));
      unsigned Reg = MRI->getDwarfRegNum(ABI.GetEhDataReg(I), true);
      unsigned CFIIndex = MF.addFrameInst(
          MCCFIInstruction::createOffset(nullptr, Reg, Offset));
      BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
          .addCFIIndex(CFIIndex);
    }
  }

  // if framepointer enabled, set it to point to the stack pointer.
  if (hasFP(MF)) {
    // Insert instruction "move $fp, $sp" at this location.
    BuildMI(MBB, MBBI, dl, TII.get(MOVE), FP).addReg(SP).addReg(ZERO)
      .setMIFlag(MachineInstr::FrameSetup);

    // emit ".cfi_def_cfa_register $fp"
    unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createDefCfaRegister(
        nullptr, MRI->getDwarfRegNum(FP, true)));
    BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
        .addCFIIndex(CFIIndex);

    if (RegInfo.needsStackRealignment(MF)) {
      // addiu $Reg, $zero, -MaxAlignment
      // andi $sp, $sp, $Reg
      unsigned VR = MF.getRegInfo().createVirtualRegister(RC);
      assert((Log2(MFI.getMaxAlign()) < 16) &&
             "Function's alignment size requirement is not supported.");
      int MaxAlign = -(int)MFI.getMaxAlign().value();
      int Alignment = (int)MFI.getMaxAlign().value();

      if( Alignment <= 2048 ){
         BuildMI(MBB, MBBI, dl, TII.get(ADDI), VR).addReg(ZERO) .addImm(MaxAlign);
         BuildMI(MBB, MBBI, dl, TII.get(AND), SP).addReg(SP).addReg(VR);
      }else{
         const unsigned NrBitsToZero = countTrailingZeros((unsigned)Alignment);
         BuildMI(MBB, MBBI, dl, TII.get(ADDI), VR).addReg(ZERO).addImm(-1);
         BuildMI(MBB, MBBI, dl, TII.get(SLLI), VR).addReg(VR).addImm(NrBitsToZero);
         BuildMI(MBB, MBBI, dl, TII.get(AND), SP).addReg(SP).addReg(VR);
      }

      if (hasBP(MF)) {
        // move $s7, $sp
        unsigned BP = STI.isABI_LP64() ? LoongArch::S7_64 : LoongArch::S7;
        BuildMI(MBB, MBBI, dl, TII.get(MOVE), BP)
          .addReg(SP)
          .addReg(ZERO);
      }
    }
  }
}

void LoongArchFrameLowering::emitEpilogue(MachineFunction &MF,
                                          MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator MBBI = MBB.getFirstTerminator();
  MachineFrameInfo &MFI            = MF.getFrameInfo();
  LoongArchFunctionInfo *LoongArchFI = MF.getInfo<LoongArchFunctionInfo>();

  const LoongArchInstrInfo &TII =
      *static_cast<const LoongArchInstrInfo *>(STI.getInstrInfo());
  const LoongArchRegisterInfo &RegInfo =
      *static_cast<const LoongArchRegisterInfo *>(STI.getRegisterInfo());

  DebugLoc DL = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();
  LoongArchABIInfo ABI = STI.getABI();
  unsigned SP = ABI.GetStackPtr();
  unsigned FP = ABI.GetFramePtr();
  unsigned ZERO = ABI.GetNullPtr();
  unsigned MOVE = ABI.GetGPRMoveOp();

  // if framepointer enabled, restore the stack pointer.
  if (hasFP(MF)) {
    // Find the first instruction that restores a callee-saved register.
    MachineBasicBlock::iterator I = MBBI;

    for (unsigned i = 0; i < MFI.getCalleeSavedInfo().size(); ++i)
      --I;

    // Insert instruction "move $sp, $fp" at this location.
    BuildMI(MBB, I, DL, TII.get(MOVE), SP).addReg(FP).addReg(ZERO);
  }

  if (LoongArchFI->callsEhReturn()) {
    const TargetRegisterClass *RC =
        ABI.ArePtrs64bit() ? &LoongArch::GPR64RegClass : &LoongArch::GPR32RegClass;

    // Find first instruction that restores a callee-saved register.
    MachineBasicBlock::iterator I = MBBI;
    for (unsigned i = 0; i < MFI.getCalleeSavedInfo().size(); ++i)
      --I;

    // Insert instructions that restore eh data registers.
    for (int J = 0; J < 4; ++J) {
      TII.loadRegFromStackSlot(MBB, I, ABI.GetEhDataReg(J),
                               LoongArchFI->getEhDataRegFI(J), RC, &RegInfo);
    }
  }

  // Get the number of bytes from FrameInfo
  uint64_t StackSize = MFI.getStackSize();

  if (!StackSize)
    return;

  // Adjust stack.
  TII.adjustStackPtr(SP, StackSize, MBB, MBBI);
}

int
LoongArchFrameLowering::getFrameIndexReference(const MachineFunction &MF,
                                               int FI,
                                               Register &FrameReg) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  LoongArchABIInfo ABI = STI.getABI();

  if (MFI.isFixedObjectIndex(FI))
    FrameReg = hasFP(MF) ? ABI.GetFramePtr() : ABI.GetStackPtr();
  else
    FrameReg = hasBP(MF) ? ABI.GetBasePtr() : ABI.GetStackPtr();

  return MFI.getObjectOffset(FI) + MFI.getStackSize() -
         getOffsetOfLocalArea() + MFI.getOffsetAdjustment();
}

bool LoongArchFrameLowering::spillCalleeSavedRegisters(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
    ArrayRef<CalleeSavedInfo> CSI, const TargetRegisterInfo *TRI) const {
  MachineFunction *MF = MBB.getParent();
  const TargetInstrInfo &TII = *STI.getInstrInfo();

  for (unsigned i = 0, e = CSI.size(); i != e; ++i) {
    // Add the callee-saved register as live-in. Do not add if the register is
    // RA and return address is taken, because it has already been added in
    // method LoongArchTargetLowering::lowerRETURNADDR.
    // It's killed at the spill, unless the register is RA and return address
    // is taken.
    unsigned Reg = CSI[i].getReg();
    bool IsRAAndRetAddrIsTaken = (Reg == LoongArch::RA || Reg == LoongArch::RA_64)
        && MF->getFrameInfo().isReturnAddressTaken();
    if (!IsRAAndRetAddrIsTaken)
      MBB.addLiveIn(Reg);

    // Insert the spill to the stack frame.
    bool IsKill = !IsRAAndRetAddrIsTaken;
    const TargetRegisterClass *RC = TRI->getMinimalPhysRegClass(Reg);
    TII.storeRegToStackSlot(MBB, MI, Reg, IsKill,
                            CSI[i].getFrameIdx(), RC, TRI);
  }

  return true;
}

bool
LoongArchFrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  // Reserve call frame if the size of the maximum call frame fits into 12-bit
  // immediate field and there are no variable sized objects on the stack.
  // Make sure the second register scavenger spill slot can be accessed with one
  // instruction.
  return isInt<12>(MFI.getMaxCallFrameSize() + getStackAlignment()) &&
    !MFI.hasVarSizedObjects();
}

/// Mark \p Reg and all registers aliasing it in the bitset.
static void setAliasRegs(MachineFunction &MF, BitVector &SavedRegs,
                         unsigned Reg) {
  const TargetRegisterInfo *TRI = MF.getSubtarget().getRegisterInfo();
  for (MCRegAliasIterator AI(Reg, TRI, true); AI.isValid(); ++AI)
    SavedRegs.set(*AI);
}

void LoongArchFrameLowering::determineCalleeSaves(MachineFunction &MF,
                                                  BitVector &SavedRegs,
                                                  RegScavenger *RS) const {
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
  const TargetRegisterInfo *TRI = MF.getSubtarget().getRegisterInfo();
  LoongArchFunctionInfo *LoongArchFI = MF.getInfo<LoongArchFunctionInfo>();
  LoongArchABIInfo ABI = STI.getABI();
  unsigned FP = ABI.GetFramePtr();
  unsigned BP = ABI.IsLP64() ? LoongArch::S7_64 : LoongArch::S7;

  // Mark $fp as used if function has dedicated frame pointer.
  if (hasFP(MF))
    setAliasRegs(MF, SavedRegs, FP);
  // Mark $s7 as used if function has dedicated base pointer.
  if (hasBP(MF))
    setAliasRegs(MF, SavedRegs, BP);

  // Create spill slots for eh data registers if function calls eh_return.
  if (LoongArchFI->callsEhReturn())
    LoongArchFI->createEhDataRegsFI();

  // Set scavenging frame index if necessary.
  uint64_t MaxSPOffset = estimateStackSize(MF);

  // If there is a variable
  // sized object on the stack, the estimation cannot account for it.
  if (isIntN(12, MaxSPOffset) &&
      !MF.getFrameInfo().hasVarSizedObjects())
    return;

  const TargetRegisterClass &RC =
      ABI.ArePtrs64bit() ? LoongArch::GPR64RegClass : LoongArch::GPR32RegClass;
  int FI = MF.getFrameInfo().CreateStackObject(TRI->getSpillSize(RC),
                                               TRI->getSpillAlign(RC), false);
  RS->addScavengingFrameIndex(FI);
}

// hasFP - Return true if the specified function should have a dedicated frame
// pointer register.  This is true if the function has variable sized allocas,
// if it needs dynamic stack realignment, if frame pointer elimination is
// disabled, or if the frame address is taken.
bool LoongArchFrameLowering::hasFP(const MachineFunction &MF) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetRegisterInfo *TRI = STI.getRegisterInfo();

  return MF.getTarget().Options.DisableFramePointerElim(MF) ||
      MFI.hasVarSizedObjects() || MFI.isFrameAddressTaken() ||
      TRI->needsStackRealignment(MF);
}

bool LoongArchFrameLowering::hasBP(const MachineFunction &MF) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetRegisterInfo *TRI = STI.getRegisterInfo();

  return MFI.hasVarSizedObjects() && TRI->needsStackRealignment(MF);
}

// Estimate the size of the stack, including the incoming arguments. We need to
// account for register spills, local objects, reserved call frame and incoming
// arguments. This is required to determine the largest possible positive offset
// from $sp so that it can be determined if an emergency spill slot for stack
// addresses is required.
uint64_t LoongArchFrameLowering::
estimateStackSize(const MachineFunction &MF) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetRegisterInfo &TRI = *STI.getRegisterInfo();

  int64_t Size = 0;

  // Iterate over fixed sized objects which are incoming arguments.
  for (int I = MFI.getObjectIndexBegin(); I != 0; ++I)
    if (MFI.getObjectOffset(I) > 0)
      Size += MFI.getObjectSize(I);

  // Conservatively assume all callee-saved registers will be saved.
  for (const MCPhysReg *R = TRI.getCalleeSavedRegs(&MF); *R; ++R) {
    unsigned RegSize = TRI.getSpillSize(*TRI.getMinimalPhysRegClass(*R));
    Size = alignTo(Size + RegSize, RegSize);
  }

  // Get the size of the rest of the frame objects and any possible reserved
  // call frame, accounting for alignment.
  return Size + MFI.estimateStackSize(MF);
}

// Eliminate ADJCALLSTACKDOWN, ADJCALLSTACKUP pseudo instructions
MachineBasicBlock::iterator LoongArchFrameLowering::
eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const {
  unsigned SP = STI.getABI().IsLP64() ? LoongArch::SP_64 : LoongArch::SP;

  if (!hasReservedCallFrame(MF)) {
    int64_t Amount = I->getOperand(0).getImm();
    if (I->getOpcode() == LoongArch::ADJCALLSTACKDOWN)
      Amount = -Amount;

    STI.getInstrInfo()->adjustStackPtr(SP, Amount, MBB, I);
  }

  return MBB.erase(I);
}
