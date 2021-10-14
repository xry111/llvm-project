//===-- LoongArchFrameLowering.cpp - LoongArch Frame Information ----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the LoongArch implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#include "LoongArchFrameLowering.h"
#include "LoongArchMachineFunctionInfo.h"
#include "LoongArchSubtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/MC/MCDwarf.h"

using namespace llvm;

// For now we use x18, a.k.a s2, as pointer to shadow call stack.
// User should explicitly set -ffixed-x18 and not use x18 in their asm.
static void emitSCSPrologue(MachineFunction &MF, MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MI,
                            const DebugLoc &DL) {
  if (!MF.getFunction().hasFnAttribute(Attribute::ShadowCallStack))
    return;

  const auto &STI = MF.getSubtarget<LoongArchSubtarget>();
  Register RAReg = STI.getRegisterInfo()->getRARegister();

  // Do not save RA to the SCS if it's not saved to the regular stack,
  // i.e. RA is not at risk of being overwritten.
  std::vector<CalleeSavedInfo> &CSI = MF.getFrameInfo().getCalleeSavedInfo();
  if (std::none_of(CSI.begin(), CSI.end(),
                   [&](CalleeSavedInfo &CSR) { return CSR.getReg() == RAReg; }))
    return;

  Register SCSPReg = LoongArchABI::getSCSPReg();

  auto &Ctx = MF.getFunction().getContext();
  if (!STI.isRegisterReservedByUser(SCSPReg)) {
    Ctx.diagnose(DiagnosticInfoUnsupported{
        MF.getFunction(), "x18 not reserved by user for Shadow Call Stack."});
    return;
  }

  const LoongArchInstrInfo *TII = STI.getInstrInfo();
  bool IsLA64 = STI.hasFeature(LoongArch::Feature64Bit);
  int64_t SlotSize = STI.getXLen() / 8;
  // Store return address to shadow call stack
  // s[w|d]  ra, 0(s2)
  // addi    s2, s2, [4|8]
  BuildMI(MBB, MI, DL, TII->get(IsLA64 ? LoongArch::ST_D : LoongArch::ST_W))
      .addReg(RAReg)
      .addReg(SCSPReg)
      .addImm(0);
  BuildMI(MBB, MI, DL, TII->get(STI.getADDI()))
      .addReg(SCSPReg, RegState::Define)
      .addReg(SCSPReg)
      .addImm(SlotSize);
}

static void emitSCSEpilogue(MachineFunction &MF, MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MI,
                            const DebugLoc &DL) {
  if (!MF.getFunction().hasFnAttribute(Attribute::ShadowCallStack))
    return;

  const auto &STI = MF.getSubtarget<LoongArchSubtarget>();
  Register RAReg = STI.getRegisterInfo()->getRARegister();

  // See emitSCSPrologue() above.
  std::vector<CalleeSavedInfo> &CSI = MF.getFrameInfo().getCalleeSavedInfo();
  if (std::none_of(CSI.begin(), CSI.end(),
                   [&](CalleeSavedInfo &CSR) { return CSR.getReg() == RAReg; }))
    return;

  Register SCSPReg = LoongArchABI::getSCSPReg();

  auto &Ctx = MF.getFunction().getContext();
  if (!STI.isRegisterReservedByUser(SCSPReg)) {
    Ctx.diagnose(DiagnosticInfoUnsupported{
        MF.getFunction(), "x18 not reserved by user for Shadow Call Stack."});
    return;
  }

  const LoongArchInstrInfo *TII = STI.getInstrInfo();
  bool IsLA64 = STI.hasFeature(LoongArch::Feature64Bit);
  int64_t SlotSize = STI.getXLen() / 8;
  // Load return address from shadow call stack
  // l[w|d]  ra, -[4|8](s2)
  // addi    s2, s2, -[4|8]
  BuildMI(MBB, MI, DL, TII->get(IsLA64 ? LoongArch::LD_D : LoongArch::LD_W))
      .addReg(RAReg, RegState::Define)
      .addReg(SCSPReg)
      .addImm(-SlotSize);
  BuildMI(MBB, MI, DL, TII->get(STI.getADDI()))
      .addReg(SCSPReg, RegState::Define)
      .addReg(SCSPReg)
      .addImm(-SlotSize);
}

bool LoongArchFrameLowering::hasFP(const MachineFunction &MF) const {
  const TargetRegisterInfo *RegInfo = MF.getSubtarget().getRegisterInfo();

  const MachineFrameInfo &MFI = MF.getFrameInfo();
  return MF.getTarget().Options.DisableFramePointerElim(MF) ||
         RegInfo->hasStackRealignment(MF) || MFI.hasVarSizedObjects() ||
         MFI.isFrameAddressTaken();
}

bool LoongArchFrameLowering::hasBP(const MachineFunction &MF) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetRegisterInfo *TRI = STI.getRegisterInfo();

  return MFI.hasVarSizedObjects() && TRI->hasStackRealignment(MF);
}

// Determines the size of the frame and maximum call frame size.
void LoongArchFrameLowering::determineFrameLayout(MachineFunction &MF) const {
  MachineFrameInfo &MFI = MF.getFrameInfo();

  // Get the number of bytes to allocate from the FrameInfo.
  uint64_t FrameSize = MFI.getStackSize();

  // Get the alignment.
  Align StackAlign = getStackAlign();

  // Set Max Call Frame Size
  uint64_t MaxCallSize = alignTo(MFI.getMaxCallFrameSize(), StackAlign);
  MFI.setMaxCallFrameSize(MaxCallSize);

  // Make sure the frame is aligned.
  FrameSize = alignTo(FrameSize, StackAlign);

  // Update frame info.
  MFI.setStackSize(FrameSize);
}

void LoongArchFrameLowering::adjustReg(MachineBasicBlock &MBB,
                                       MachineBasicBlock::iterator MBBI,
                                       const DebugLoc &DL, Register DestReg,
                                       Register SrcReg, int64_t Val,
                                       MachineInstr::MIFlag Flag) const {
  MachineRegisterInfo &MRI = MBB.getParent()->getRegInfo();
  const LoongArchInstrInfo *TII = STI.getInstrInfo();

  if (DestReg == SrcReg && Val == 0)
    return;

  if (isInt<12>(Val)) {
    BuildMI(MBB, MBBI, DL, TII->get(STI.getADDI()), DestReg)
        .addReg(SrcReg)
        .addImm(Val)
        .setMIFlag(Flag);
  } else {
    unsigned Opc = STI.getADD();
    bool isSub = Val < 0;
    if (isSub) {
      Val = -Val;
      Opc = STI.getSUB();
    }

    Register ScratchReg = MRI.createVirtualRegister(&LoongArch::GPRRegClass);
    TII->movImm(MBB, MBBI, DL, ScratchReg, Val, Flag);
    BuildMI(MBB, MBBI, DL, TII->get(Opc), DestReg)
        .addReg(SrcReg)
        .addReg(ScratchReg, RegState::Kill)
        .setMIFlag(Flag);
  }
}

// Returns the register used to hold the frame pointer.
static Register getFPReg(const LoongArchSubtarget &STI) {
  return LoongArch::R22;
}

// Returns the register used to hold the stack pointer.
static Register getSPReg(const LoongArchSubtarget &STI) {
  return LoongArch::R3;
}

static SmallVector<CalleeSavedInfo, 8>
getNonLibcallCSI(const std::vector<CalleeSavedInfo> &CSI) {
  SmallVector<CalleeSavedInfo, 8> NonLibcallCSI;

  for (auto &CS : CSI)
    if (CS.getFrameIdx() >= 0)
      NonLibcallCSI.push_back(CS);

  return NonLibcallCSI;
}

void LoongArchFrameLowering::emitPrologue(MachineFunction &MF,
                                          MachineBasicBlock &MBB) const {
  MachineFrameInfo &MFI = MF.getFrameInfo();
  auto *RVFI = MF.getInfo<LoongArchMachineFunctionInfo>();
  const LoongArchRegisterInfo *RI = STI.getRegisterInfo();
  const LoongArchInstrInfo *TII = STI.getInstrInfo();
  MachineBasicBlock::iterator MBBI = MBB.begin();

  Register FPReg = getFPReg(STI);
  Register SPReg = getSPReg(STI);
  Register BPReg = LoongArchABI::getBPReg();

  // Debug location must be unknown since the first debug location is used
  // to determine the end of the prologue.
  DebugLoc DL;

  // All calls are tail calls in GHC calling conv, and functions have no
  // prologue/epilogue.
  if (MF.getFunction().getCallingConv() == CallingConv::GHC)
    return;

  // Emit prologue for shadow call stack.
  emitSCSPrologue(MF, MBB, MBBI, DL);

  // Since spillCalleeSavedRegisters may have inserted a libcall, skip past
  // any instructions marked as FrameSetup
  while (MBBI != MBB.end() && MBBI->getFlag(MachineInstr::FrameSetup))
    ++MBBI;

  // Determine the correct frame layout
  determineFrameLayout(MF);

  // FIXME (note copied from Lanai): This appears to be overallocating.  Needs
  // investigation. Get the number of bytes to allocate from the FrameInfo.
  uint64_t StackSize = MFI.getStackSize();
  uint64_t RealStackSize = StackSize + RVFI->getLibCallStackSize();

  // Early exit if there is no need to allocate on the stack
  if (RealStackSize == 0 && !MFI.adjustsStack())
    return;

  // If the stack pointer has been marked as reserved, then produce an error if
  // the frame requires stack allocation
  if (STI.isRegisterReservedByUser(SPReg))
    MF.getFunction().getContext().diagnose(DiagnosticInfoUnsupported{
        MF.getFunction(), "Stack pointer required, but has been reserved."});

  uint64_t FirstSPAdjustAmount = getFirstSPAdjustAmount(MF);
  // Split the SP adjustment to reduce the offsets of callee saved spill.
  if (FirstSPAdjustAmount) {
    StackSize = FirstSPAdjustAmount;
    RealStackSize = FirstSPAdjustAmount;
  }

  // Allocate space on the stack if necessary.
  adjustReg(MBB, MBBI, DL, SPReg, SPReg, -StackSize, MachineInstr::FrameSetup);

  // Emit ".cfi_def_cfa_offset RealStackSize"
  unsigned CFIIndex = MF.addFrameInst(
      MCCFIInstruction::cfiDefCfaOffset(nullptr, RealStackSize));
  BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);

  const auto &CSI = MFI.getCalleeSavedInfo();

  // The frame pointer is callee-saved, and code has been generated for us to
  // save it to the stack. We need to skip over the storing of callee-saved
  // registers as the frame pointer must be modified after it has been saved
  // to the stack, not before.
  // FIXME: assumes exactly one instruction is used to save each callee-saved
  // register.
  std::advance(MBBI, getNonLibcallCSI(CSI).size());

  // Iterate over list of callee-saved registers and emit .cfi_offset
  // directives.
  for (const auto &Entry : CSI) {
    int FrameIdx = Entry.getFrameIdx();
    int64_t Offset;
    // Offsets for objects with fixed locations (IE: those saved by libcall) are
    // simply calculated from the frame index.
    if (FrameIdx < 0)
      Offset = FrameIdx * (int64_t)STI.getXLen() / 8;
    else
      Offset = MFI.getObjectOffset(Entry.getFrameIdx()) -
               RVFI->getLibCallStackSize();
    Register Reg = Entry.getReg();
    unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createOffset(
        nullptr, RI->getDwarfRegNum(Reg, true), Offset));
    BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
        .addCFIIndex(CFIIndex);
  }

  // Generate new FP.
  if (hasFP(MF)) {
    if (STI.isRegisterReservedByUser(FPReg))
      MF.getFunction().getContext().diagnose(DiagnosticInfoUnsupported{
          MF.getFunction(), "Frame pointer required, but has been reserved."});

    adjustReg(MBB, MBBI, DL, FPReg, SPReg,
              RealStackSize - RVFI->getVarArgsSaveSize(),
              MachineInstr::FrameSetup);

    // Emit ".cfi_def_cfa $fp, RVFI->getVarArgsSaveSize()"
    unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::cfiDefCfa(
        nullptr, RI->getDwarfRegNum(FPReg, true), RVFI->getVarArgsSaveSize()));
    BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
        .addCFIIndex(CFIIndex);
  }

  // Emit the second SP adjustment after saving callee saved registers.
  if (FirstSPAdjustAmount) {
    uint64_t SecondSPAdjustAmount = MFI.getStackSize() - FirstSPAdjustAmount;
    assert(SecondSPAdjustAmount > 0 &&
           "SecondSPAdjustAmount should be greater than zero");
    adjustReg(MBB, MBBI, DL, SPReg, SPReg, -SecondSPAdjustAmount,
              MachineInstr::FrameSetup);

    // If we are using a frame-pointer, and thus emitted ".cfi_def_cfa fp, 0",
    // don't emit an sp-based .cfi_def_cfa_offset
    if (!hasFP(MF)) {
      // Emit ".cfi_def_cfa_offset StackSize"
      unsigned CFIIndex = MF.addFrameInst(
          MCCFIInstruction::cfiDefCfaOffset(nullptr, MFI.getStackSize()));
      BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
          .addCFIIndex(CFIIndex);
    }
  }

  if (hasFP(MF)) {
    // Realign Stack
    const LoongArchRegisterInfo *RI = STI.getRegisterInfo();
    if (RI->hasStackRealignment(MF)) {
      Align MaxAlignment = MFI.getMaxAlign();

      const LoongArchInstrInfo *TII = STI.getInstrInfo();
      // On LoongArch you can't use ANDI to align stack: the immediate
      // operand of ANDI is zero-extended.
      if (false) {
        BuildMI(MBB, MBBI, DL, TII->get(LoongArch::ANDI), SPReg)
            .addReg(SPReg)
            .addImm(-(int)MaxAlignment.value());
      } else {
        unsigned ShiftAmount = Log2(MaxAlignment);
        Register VR =
            MF.getRegInfo().createVirtualRegister(&LoongArch::GPRRegClass);
        BuildMI(MBB, MBBI, DL, TII->get(STI.getSRLI()), VR)
            .addReg(SPReg)
            .addImm(ShiftAmount);
        BuildMI(MBB, MBBI, DL, TII->get(STI.getSLLI()), SPReg)
            .addReg(VR)
            .addImm(ShiftAmount);
      }
      // FP will be used to restore the frame in the epilogue, so we need
      // another base register BP to record SP after re-alignment. SP will
      // track the current stack after allocating variable sized objects.
      if (hasBP(MF)) {
        // move BP, SP
        BuildMI(MBB, MBBI, DL, TII->get(STI.getADDI()), BPReg)
            .addReg(SPReg)
            .addImm(0);
      }
    }
  }
}

void LoongArchFrameLowering::emitEpilogue(MachineFunction &MF,
                                          MachineBasicBlock &MBB) const {
  const LoongArchRegisterInfo *RI = STI.getRegisterInfo();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  auto *RVFI = MF.getInfo<LoongArchMachineFunctionInfo>();
  Register FPReg = getFPReg(STI);
  Register SPReg = getSPReg(STI);

  // All calls are tail calls in GHC calling conv, and functions have no
  // prologue/epilogue.
  if (MF.getFunction().getCallingConv() == CallingConv::GHC)
    return;

  // Get the insert location for the epilogue. If there were no terminators in
  // the block, get the last instruction.
  MachineBasicBlock::iterator MBBI = MBB.end();
  DebugLoc DL;
  if (!MBB.empty()) {
    MBBI = MBB.getFirstTerminator();
    if (MBBI == MBB.end())
      MBBI = MBB.getLastNonDebugInstr();
    DL = MBBI->getDebugLoc();

    // If this is not a terminator, the actual insert location should be after
    // the last instruction.
    if (!MBBI->isTerminator())
      MBBI = std::next(MBBI);

    // If callee-saved registers are saved via libcall, place stack adjustment
    // before this call.
    while (MBBI != MBB.begin() &&
           std::prev(MBBI)->getFlag(MachineInstr::FrameDestroy))
      --MBBI;
  }

  const auto &CSI = getNonLibcallCSI(MFI.getCalleeSavedInfo());

  // Skip to before the restores of callee-saved registers
  // FIXME: assumes exactly one instruction is used to restore each
  // callee-saved register.
  auto LastFrameDestroy = MBBI;
  if (!CSI.empty())
    LastFrameDestroy = std::prev(MBBI, CSI.size());

  uint64_t StackSize = MFI.getStackSize();
  uint64_t RealStackSize = StackSize + RVFI->getLibCallStackSize();
  uint64_t FPOffset = RealStackSize - RVFI->getVarArgsSaveSize();

  // Restore the stack pointer using the value of the frame pointer. Only
  // necessary if the stack pointer was modified, meaning the stack size is
  // unknown.
  if (RI->hasStackRealignment(MF) || MFI.hasVarSizedObjects()) {
    assert(hasFP(MF) && "frame pointer should not have been eliminated");
    adjustReg(MBB, LastFrameDestroy, DL, SPReg, FPReg, -FPOffset,
              MachineInstr::FrameDestroy);
  }

  uint64_t FirstSPAdjustAmount = getFirstSPAdjustAmount(MF);
  if (FirstSPAdjustAmount) {
    uint64_t SecondSPAdjustAmount = MFI.getStackSize() - FirstSPAdjustAmount;
    assert(SecondSPAdjustAmount > 0 &&
           "SecondSPAdjustAmount should be greater than zero");

    adjustReg(MBB, LastFrameDestroy, DL, SPReg, SPReg, SecondSPAdjustAmount,
              MachineInstr::FrameDestroy);
  }

  if (FirstSPAdjustAmount)
    StackSize = FirstSPAdjustAmount;

  // Deallocate stack
  adjustReg(MBB, MBBI, DL, SPReg, SPReg, StackSize, MachineInstr::FrameDestroy);

  // Emit epilogue for shadow call stack.
  emitSCSEpilogue(MF, MBB, MBBI, DL);
}

StackOffset LoongArchFrameLowering::getFrameIndexReference(
    const MachineFunction &MF, int FI, Register &FrameReg) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetRegisterInfo *RI = MF.getSubtarget().getRegisterInfo();
  const auto *RVFI = MF.getInfo<LoongArchMachineFunctionInfo>();

  // Callee-saved registers should be referenced relative to the stack
  // pointer (positive offset), otherwise use the frame pointer (negative
  // offset).
  const auto &CSI = getNonLibcallCSI(MFI.getCalleeSavedInfo());
  int MinCSFI = 0;
  int MaxCSFI = -1;

  int Offset = MFI.getObjectOffset(FI) - getOffsetOfLocalArea() +
               MFI.getOffsetAdjustment();

  uint64_t FirstSPAdjustAmount = getFirstSPAdjustAmount(MF);

  if (CSI.size()) {
    MinCSFI = CSI[0].getFrameIdx();
    MaxCSFI = CSI[CSI.size() - 1].getFrameIdx();
  }

  if (FI >= MinCSFI && FI <= MaxCSFI) {
    FrameReg = LoongArch::R3;

    if (FirstSPAdjustAmount)
      Offset += FirstSPAdjustAmount;
    else
      Offset += MFI.getStackSize();
  } else if (RI->hasStackRealignment(MF) && !MFI.isFixedObjectIndex(FI)) {
    // If the stack was realigned, the frame pointer is set in order to allow
    // SP to be restored, so we need another base register to record the stack
    // after realignment.
    if (hasBP(MF))
      FrameReg = LoongArchABI::getBPReg();
    else
      FrameReg = LoongArch::R3;
    Offset += MFI.getStackSize();
    if (FI < 0)
      Offset += RVFI->getLibCallStackSize();
  } else {
    FrameReg = RI->getFrameRegister(MF);
    if (hasFP(MF)) {
      Offset += RVFI->getVarArgsSaveSize();
      if (FI >= 0)
        Offset -= RVFI->getLibCallStackSize();
    } else {
      Offset += MFI.getStackSize();
      if (FI < 0)
        Offset += RVFI->getLibCallStackSize();
    }
  }
  return StackOffset::getFixed(Offset);
}

void LoongArchFrameLowering::determineCalleeSaves(MachineFunction &MF,
                                                  BitVector &SavedRegs,
                                                  RegScavenger *RS) const {
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
  // Unconditionally spill RA and FP only if the function uses a frame
  // pointer.
  if (hasFP(MF)) {
    SavedRegs.set(LoongArch::R1);
    SavedRegs.set(LoongArch::R22);
  }
  // Mark BP as used if function has dedicated base pointer.
  if (hasBP(MF))
    SavedRegs.set(LoongArchABI::getBPReg());

  // If interrupt is enabled and there are calls in the handler,
  // unconditionally save all Caller-saved registers and
  // all FP registers, regardless whether they are used.
  MachineFrameInfo &MFI = MF.getFrameInfo();

  if (MF.getFunction().hasFnAttribute("interrupt") && MFI.hasCalls()) {

    static const MCPhysReg CSRegs[] = {LoongArch::R1, /* ra */
                                       LoongArch::R4,  LoongArch::R5,
                                       LoongArch::R6,  LoongArch::R7,
                                       LoongArch::R8,  LoongArch::R9,
                                       LoongArch::R10, LoongArch::R11, /* a* */
                                       LoongArch::R12, LoongArch::R13,
                                       LoongArch::R14, LoongArch::R15,
                                       LoongArch::R16, LoongArch::R17,
                                       LoongArch::R18, LoongArch::R19,
                                       LoongArch::R20, 0};

    for (unsigned i = 0; CSRegs[i]; ++i)
      SavedRegs.set(CSRegs[i]);

    if (MF.getSubtarget<LoongArchSubtarget>().hasStdExtF()) {

      // If interrupt is enabled, this list contains all FP registers.
      const MCPhysReg *Regs = MF.getRegInfo().getCalleeSavedRegs();

      for (unsigned i = 0; Regs[i]; ++i)
        if (LoongArch::FPR32RegClass.contains(Regs[i]) ||
            LoongArch::FPR64RegClass.contains(Regs[i]))
          SavedRegs.set(Regs[i]);
    }
  }
}

void LoongArchFrameLowering::processFunctionBeforeFrameFinalized(
    MachineFunction &MF, RegScavenger *RS) const {
  const TargetRegisterInfo *RegInfo = MF.getSubtarget().getRegisterInfo();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetRegisterClass *RC = &LoongArch::GPRRegClass;
  // estimateStackSize has been observed to under-estimate the final stack
  // size, so give ourselves wiggle-room by checking for stack size
  // representable an 11-bit signed field rather than 12-bits.
  // FIXME: It may be possible to craft a function with a small stack that
  // still needs an emergency spill slot for branch relaxation. This case
  // would currently be missed.
  if (!isInt<11>(MFI.estimateStackSize(MF))) {
    int RegScavFI = MFI.CreateStackObject(RegInfo->getSpillSize(*RC),
                                          RegInfo->getSpillAlign(*RC), false);
    RS->addScavengingFrameIndex(RegScavFI);
  }
}

// Not preserve stack space within prologue for outgoing variables when the
// function contains variable size objects and let eliminateCallFramePseudoInstr
// preserve stack space for it.
bool LoongArchFrameLowering::hasReservedCallFrame(
    const MachineFunction &MF) const {
  return !MF.getFrameInfo().hasVarSizedObjects();
}

// Eliminate ADJCALLSTACKDOWN, ADJCALLSTACKUP pseudo instructions.
MachineBasicBlock::iterator
LoongArchFrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator MI) const {
  Register SPReg = LoongArch::R3;
  DebugLoc DL = MI->getDebugLoc();

  if (!hasReservedCallFrame(MF)) {
    // If space has not been reserved for a call frame, ADJCALLSTACKDOWN and
    // ADJCALLSTACKUP must be converted to instructions manipulating the stack
    // pointer. This is necessary when there is a variable length stack
    // allocation (e.g. alloca), which means it's not possible to allocate
    // space for outgoing arguments from within the function prologue.
    int64_t Amount = MI->getOperand(0).getImm();

    if (Amount != 0) {
      // Ensure the stack remains aligned after adjustment.
      Amount = alignSPAdjust(Amount);

      if (MI->getOpcode() == LoongArch::ADJCALLSTACKDOWN)
        Amount = -Amount;

      adjustReg(MBB, MI, DL, SPReg, SPReg, Amount, MachineInstr::NoFlags);
    }
  }

  return MBB.erase(MI);
}

// We would like to split the SP adjustment to reduce prologue/epilogue
// as following instructions. In this way, the offset of the callee saved
// register could fit in a single store.
//   add     sp,sp,-2032
//   sw      ra,2028(sp)
//   sw      s0,2024(sp)
//   sw      s1,2020(sp)
//   sw      s3,2012(sp)
//   sw      s4,2008(sp)
//   add     sp,sp,-64
uint64_t LoongArchFrameLowering::getFirstSPAdjustAmount(
    const MachineFunction &MF) const {
  const auto *RVFI = MF.getInfo<LoongArchMachineFunctionInfo>();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
  uint64_t StackSize = MFI.getStackSize();

  // Disable SplitSPAdjust if save-restore libcall used. The callee saved
  // registers will be pushed by the save-restore libcalls, so we don't have to
  // split the SP adjustment in this case.
  if (RVFI->getLibCallStackSize())
    return 0;

  // Return the FirstSPAdjustAmount if the StackSize can not fit in signed
  // 12-bit and there exists a callee saved register need to be pushed.
  if (!isInt<12>(StackSize) && (CSI.size() > 0)) {
    // FirstSPAdjustAmount is choosed as (2048 - StackAlign)
    // because 2048 will cause sp = sp + 2048 in epilogue split into
    // multi-instructions. The offset smaller than 2048 can fit in signle
    // load/store instruction and we have to stick with the stack alignment.
    // 2048 is 16-byte alignment. The stack alignment for LA64 is 16,
    // for LA32 is 8. So (2048 - StackAlign) will satisfy the stack alignment.
    return 2048 - getStackAlign().value();
  }
  return 0;
}

bool LoongArchFrameLowering::spillCalleeSavedRegisters(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
    ArrayRef<CalleeSavedInfo> CSI, const TargetRegisterInfo *TRI) const {
  if (CSI.empty())
    return true;

  MachineFunction *MF = MBB.getParent();
  const TargetInstrInfo &TII = *MF->getSubtarget().getInstrInfo();
  DebugLoc DL;
  if (MI != MBB.end() && !MI->isDebugInstr())
    DL = MI->getDebugLoc();

  // Manually spill values not spilled by libcall.
  const auto &NonLibcallCSI = getNonLibcallCSI(CSI);
  for (auto &CS : NonLibcallCSI) {
    // Insert the spill to the stack frame.
    Register Reg = CS.getReg();
    const TargetRegisterClass *RC = TRI->getMinimalPhysRegClass(Reg);
    TII.storeRegToStackSlot(MBB, MI, Reg, true, CS.getFrameIdx(), RC, TRI);
  }

  return true;
}

bool LoongArchFrameLowering::restoreCalleeSavedRegisters(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
    MutableArrayRef<CalleeSavedInfo> CSI, const TargetRegisterInfo *TRI) const {
  if (CSI.empty())
    return true;

  MachineFunction *MF = MBB.getParent();
  const TargetInstrInfo &TII = *MF->getSubtarget().getInstrInfo();
  DebugLoc DL;
  if (MI != MBB.end() && !MI->isDebugInstr())
    DL = MI->getDebugLoc();

  // Manually restore values not restored by libcall. Insert in reverse order.
  // loadRegFromStackSlot can insert multiple instructions.
  const auto &NonLibcallCSI = getNonLibcallCSI(CSI);
  for (auto &CS : reverse(NonLibcallCSI)) {
    Register Reg = CS.getReg();
    const TargetRegisterClass *RC = TRI->getMinimalPhysRegClass(Reg);
    TII.loadRegFromStackSlot(MBB, MI, Reg, CS.getFrameIdx(), RC, TRI);
    assert(MI != MBB.begin() && "loadRegFromStackSlot didn't insert any code!");
  }

  return true;
}

bool LoongArchFrameLowering::canUseAsPrologue(
    const MachineBasicBlock &MBB) const {
  return true;
}

bool LoongArchFrameLowering::canUseAsEpilogue(
    const MachineBasicBlock &MBB) const {
  return true;
}
