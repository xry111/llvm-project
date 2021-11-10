//=== LoongArchExpandAtomicPseudoInsts.cpp - Expand atomic pseudo instrs. -===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains a pass that expands atomic pseudo instructions into
// target instructions. This pass should be run at the last possible moment,
// avoiding the possibility for other passes to break the requirements for
// forward progress in the LR/SC block.
//
//===----------------------------------------------------------------------===//

#include "LoongArch.h"
#include "LoongArchInstrInfo.h"
#include "LoongArchTargetMachine.h"

#include "llvm/CodeGen/LivePhysRegs.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"

using namespace llvm;

#define LoongArch_EXPAND_ATOMIC_PSEUDO_NAME                                    \
  "LoongArch atomic pseudo instruction expansion pass"

namespace {

class LoongArchExpandAtomicPseudo : public MachineFunctionPass {
public:
  const LoongArchInstrInfo *TII;
  static char ID;

  LoongArchExpandAtomicPseudo() : MachineFunctionPass(ID) {
    initializeLoongArchExpandAtomicPseudoPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  StringRef getPassName() const override {
    return LoongArch_EXPAND_ATOMIC_PSEUDO_NAME;
  }

private:
  bool expandMBB(MachineBasicBlock &MBB);
  bool expandMI(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
                MachineBasicBlock::iterator &NextMBBI);
  bool expandAtomicBinOp(MachineBasicBlock &MBB,
                         MachineBasicBlock::iterator MBBI, AtomicRMWInst::BinOp,
                         bool IsMasked, int Width,
                         MachineBasicBlock::iterator &NextMBBI);
  bool expandAtomicMinMaxOp(MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MBBI,
                            AtomicRMWInst::BinOp, bool IsMasked, int Width,
                            MachineBasicBlock::iterator &NextMBBI);
  bool expandAtomicCmpXchg(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator MBBI, bool IsMasked,
                           int Width, MachineBasicBlock::iterator &NextMBBI);
};

char LoongArchExpandAtomicPseudo::ID = 0;

bool LoongArchExpandAtomicPseudo::runOnMachineFunction(MachineFunction &MF) {
  TII =
      static_cast<const LoongArchInstrInfo *>(MF.getSubtarget().getInstrInfo());
  bool Modified = false;
  for (auto &MBB : MF)
    Modified |= expandMBB(MBB);
  return Modified;
}

bool LoongArchExpandAtomicPseudo::expandMBB(MachineBasicBlock &MBB) {
  bool Modified = false;

  MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();
  while (MBBI != E) {
    MachineBasicBlock::iterator NMBBI = std::next(MBBI);
    Modified |= expandMI(MBB, MBBI, NMBBI);
    MBBI = NMBBI;
  }

  return Modified;
}

bool LoongArchExpandAtomicPseudo::expandMI(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
    MachineBasicBlock::iterator &NextMBBI) {
  // LoongArchInstrInfo::getInstSizeInBytes hard-codes the number of expanded
  // instructions for each pseudo, and must be updated when adding new pseudos
  // or changing existing ones.
  switch (MBBI->getOpcode()) {
  case LoongArch::PseudoAtomicLoadNand32:
    return expandAtomicBinOp(MBB, MBBI, AtomicRMWInst::Nand, false, 32,
                             NextMBBI);
  case LoongArch::PseudoAtomicLoadNand64:
    return expandAtomicBinOp(MBB, MBBI, AtomicRMWInst::Nand, false, 64,
                             NextMBBI);
  case LoongArch::PseudoMaskedAtomicSwap32:
    return expandAtomicBinOp(MBB, MBBI, AtomicRMWInst::Xchg, true, 32,
                             NextMBBI);
  case LoongArch::PseudoMaskedAtomicLoadAdd32:
    return expandAtomicBinOp(MBB, MBBI, AtomicRMWInst::Add, true, 32, NextMBBI);
  case LoongArch::PseudoMaskedAtomicLoadSub32:
    return expandAtomicBinOp(MBB, MBBI, AtomicRMWInst::Sub, true, 32, NextMBBI);
  case LoongArch::PseudoMaskedAtomicLoadNand32:
    return expandAtomicBinOp(MBB, MBBI, AtomicRMWInst::Nand, true, 32,
                             NextMBBI);
  case LoongArch::PseudoMaskedAtomicLoadMax32:
    return expandAtomicMinMaxOp(MBB, MBBI, AtomicRMWInst::Max, true, 32,
                                NextMBBI);
  case LoongArch::PseudoMaskedAtomicLoadMin32:
    return expandAtomicMinMaxOp(MBB, MBBI, AtomicRMWInst::Min, true, 32,
                                NextMBBI);
  case LoongArch::PseudoMaskedAtomicLoadUMax32:
    return expandAtomicMinMaxOp(MBB, MBBI, AtomicRMWInst::UMax, true, 32,
                                NextMBBI);
  case LoongArch::PseudoMaskedAtomicLoadUMin32:
    return expandAtomicMinMaxOp(MBB, MBBI, AtomicRMWInst::UMin, true, 32,
                                NextMBBI);
  case LoongArch::PseudoCmpXchg32:
    return expandAtomicCmpXchg(MBB, MBBI, false, 32, NextMBBI);
  case LoongArch::PseudoCmpXchg64:
    return expandAtomicCmpXchg(MBB, MBBI, false, 64, NextMBBI);
  case LoongArch::PseudoMaskedCmpXchg32:
    return expandAtomicCmpXchg(MBB, MBBI, true, 32, NextMBBI);
  }

  return false;
}

static unsigned getLRForRMW(AtomicOrdering Ordering, int Width) {
  if (Width == 32)
    return LoongArch::LL_W;
  if (Width == 64)
    return LoongArch::LL_D;
  llvm_unreachable("Unexpected LR width\n");
}

static unsigned getSCForRMW(AtomicOrdering Ordering, int Width) {
  if (Width == 32)
    return LoongArch::SC_W;
  if (Width == 64)
    return LoongArch::SC_D;
  llvm_unreachable("Unexpected SC width\n");
}

static void doAtomicBinOpExpansion(const LoongArchInstrInfo *TII,
                                   MachineInstr &MI, DebugLoc DL,
                                   MachineBasicBlock *ThisMBB,
                                   MachineBasicBlock *LoopMBB,
                                   MachineBasicBlock *DoneMBB,
                                   AtomicRMWInst::BinOp BinOp, int Width) {
  Register DestReg = MI.getOperand(0).getReg();
  Register ScratchReg = MI.getOperand(1).getReg();
  Register AddrReg = MI.getOperand(2).getReg();
  Register IncrReg = MI.getOperand(3).getReg();
  AtomicOrdering Ordering =
      static_cast<AtomicOrdering>(MI.getOperand(4).getImm());

  // .loop:
  //   ll.[w|d] dest, addr, 0
  //   binop scratch, dest, val
  //   sc.[w|d] scratch, scratch, (addr)
  //   beq $zero, scratch, loop
  BuildMI(LoopMBB, DL, TII->get(getLRForRMW(Ordering, Width)), DestReg)
      .addReg(AddrReg)
      .addImm(0);
  switch (BinOp) {
  default:
    llvm_unreachable("Unexpected AtomicRMW BinOp");
  case AtomicRMWInst::Nand:
    BuildMI(LoopMBB, DL, TII->get(LoongArch::AND), ScratchReg)
        .addReg(DestReg)
        .addReg(IncrReg);
    BuildMI(LoopMBB, DL, TII->get(LoongArch::NOR), ScratchReg)
        .addReg(ScratchReg)
        .addReg(LoongArch::R0);
    break;
  }
  BuildMI(LoopMBB, DL, TII->get(getSCForRMW(Ordering, Width)), ScratchReg)
      .addReg(ScratchReg)
      .addReg(AddrReg)
      .addImm(0);
  BuildMI(LoopMBB, DL, TII->get(LoongArch::BEQ))
      .addReg(ScratchReg)
      .addReg(LoongArch::R0)
      .addMBB(LoopMBB);
}

static void insertMaskedMerge(const LoongArchInstrInfo *TII, DebugLoc DL,
                              MachineBasicBlock *MBB, Register DestReg,
                              Register OldValReg, Register NewValReg,
                              Register MaskReg, Register ScratchReg) {
  assert(OldValReg != ScratchReg && "OldValReg and ScratchReg must be unique");
  assert(OldValReg != MaskReg && "OldValReg and MaskReg must be unique");
  assert(ScratchReg != MaskReg && "ScratchReg and MaskReg must be unique");

  // We select bits from newval and oldval using:
  // https://graphics.stanford.edu/~seander/bithacks.html#MaskedMerge
  // r = oldval ^ ((oldval ^ newval) & masktargetdata);
  BuildMI(MBB, DL, TII->get(LoongArch::XOR), ScratchReg)
      .addReg(OldValReg)
      .addReg(NewValReg);
  BuildMI(MBB, DL, TII->get(LoongArch::AND), ScratchReg)
      .addReg(ScratchReg)
      .addReg(MaskReg);
  BuildMI(MBB, DL, TII->get(LoongArch::XOR), DestReg)
      .addReg(OldValReg)
      .addReg(ScratchReg);
}

static void doMaskedAtomicBinOpExpansion(
    const LoongArchInstrInfo *TII, MachineInstr &MI, DebugLoc DL,
    MachineBasicBlock *ThisMBB, MachineBasicBlock *LoopMBB,
    MachineBasicBlock *DoneMBB, AtomicRMWInst::BinOp BinOp, int Width) {
  assert(Width == 32 && "Should never need to expand masked 64-bit operations");
  Register DestReg = MI.getOperand(0).getReg();
  Register ScratchReg = MI.getOperand(1).getReg();
  Register AddrReg = MI.getOperand(2).getReg();
  Register IncrReg = MI.getOperand(3).getReg();
  Register MaskReg = MI.getOperand(4).getReg();

  // .loop:
  //   ll.w destreg, (alignedaddr)
  //   binop scratch, destreg, incr
  //   xor scratch, destreg, scratch
  //   and scratch, scratch, masktargetdata
  //   xor scratch, destreg, scratch
  //   sc.w scratch, scratch, (alignedaddr)
  //   beq $zero, scratch, loop
  BuildMI(LoopMBB, DL, TII->get(LoongArch::LL_W), DestReg)
      .addReg(AddrReg)
      .addImm(0);
  switch (BinOp) {
  default:
    llvm_unreachable("Unexpected AtomicRMW BinOp");
  case AtomicRMWInst::Xchg:
    BuildMI(LoopMBB, DL, TII->get(LoongArch::OR), ScratchReg)
        .addReg(IncrReg)
        .addReg(LoongArch::R0);
    break;
  case AtomicRMWInst::Add:
    BuildMI(LoopMBB, DL, TII->get(LoongArch::ADD_W), ScratchReg)
        .addReg(DestReg)
        .addReg(IncrReg);
    break;
  case AtomicRMWInst::Sub:
    BuildMI(LoopMBB, DL, TII->get(LoongArch::SUB_W), ScratchReg)
        .addReg(DestReg)
        .addReg(IncrReg);
    break;
  case AtomicRMWInst::Nand:
    BuildMI(LoopMBB, DL, TII->get(LoongArch::AND), ScratchReg)
        .addReg(DestReg)
        .addReg(IncrReg);
    BuildMI(LoopMBB, DL, TII->get(LoongArch::NOR), ScratchReg)
        .addReg(ScratchReg)
        .addReg(LoongArch::R0);
    break;
  }

  insertMaskedMerge(TII, DL, LoopMBB, ScratchReg, DestReg, ScratchReg, MaskReg,
                    ScratchReg);

  BuildMI(LoopMBB, DL, TII->get(LoongArch::SC_W), ScratchReg)
      .addReg(ScratchReg)
      .addReg(AddrReg)
      .addImm(0);
  BuildMI(LoopMBB, DL, TII->get(LoongArch::BEQ))
      .addReg(ScratchReg)
      .addReg(LoongArch::R0)
      .addMBB(LoopMBB);
}

bool LoongArchExpandAtomicPseudo::expandAtomicBinOp(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
    AtomicRMWInst::BinOp BinOp, bool IsMasked, int Width,
    MachineBasicBlock::iterator &NextMBBI) {
  MachineInstr &MI = *MBBI;
  DebugLoc DL = MI.getDebugLoc();

  MachineFunction *MF = MBB.getParent();
  auto LoopMBB = MF->CreateMachineBasicBlock(MBB.getBasicBlock());
  auto DoneMBB = MF->CreateMachineBasicBlock(MBB.getBasicBlock());

  // Insert new MBBs.
  MF->insert(++MBB.getIterator(), LoopMBB);
  MF->insert(++LoopMBB->getIterator(), DoneMBB);

  // Set up successors and transfer remaining instructions to DoneMBB.
  LoopMBB->addSuccessor(LoopMBB);
  LoopMBB->addSuccessor(DoneMBB);
  DoneMBB->splice(DoneMBB->end(), &MBB, MI, MBB.end());
  DoneMBB->transferSuccessors(&MBB);
  MBB.addSuccessor(LoopMBB);

  if (!IsMasked)
    doAtomicBinOpExpansion(TII, MI, DL, &MBB, LoopMBB, DoneMBB, BinOp, Width);
  else
    doMaskedAtomicBinOpExpansion(TII, MI, DL, &MBB, LoopMBB, DoneMBB, BinOp,
                                 Width);

  BuildMI(LoopMBB, DL, TII->get(LoongArch::B)).addImm(0x8);
  BuildMI(LoopMBB, DL, TII->get(LoongArch::DBAR)).addImm(0x700);

  NextMBBI = MBB.end();
  MI.eraseFromParent();

  LivePhysRegs LiveRegs;
  computeAndAddLiveIns(LiveRegs, *LoopMBB);
  computeAndAddLiveIns(LiveRegs, *DoneMBB);

  return true;
}

static void insertSext(const LoongArchInstrInfo *TII, DebugLoc DL,
                       MachineBasicBlock *MBB, Register ValReg,
                       Register ShamtReg) {
  MachineFunction *MF = MBB->getParent();
  const auto &STI = MF->getSubtarget<LoongArchSubtarget>();
  BuildMI(MBB, DL, TII->get(STI.getSLL()), ValReg)
      .addReg(ValReg)
      .addReg(ShamtReg);
  BuildMI(MBB, DL, TII->get(STI.getSRA()), ValReg)
      .addReg(ValReg)
      .addReg(ShamtReg);
}

bool LoongArchExpandAtomicPseudo::expandAtomicMinMaxOp(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
    AtomicRMWInst::BinOp BinOp, bool IsMasked, int Width,
    MachineBasicBlock::iterator &NextMBBI) {
  assert(IsMasked == true &&
         "Should only need to expand masked atomic max/min");
  assert(Width == 32 && "Should never need to expand masked 64-bit operations");

  MachineInstr &MI = *MBBI;
  DebugLoc DL = MI.getDebugLoc();
  MachineFunction *MF = MBB.getParent();
  auto LoopHeadMBB = MF->CreateMachineBasicBlock(MBB.getBasicBlock());
  auto LoopIfBodyMBB = MF->CreateMachineBasicBlock(MBB.getBasicBlock());
  auto LoopTailMBB = MF->CreateMachineBasicBlock(MBB.getBasicBlock());
  auto DoneMBB = MF->CreateMachineBasicBlock(MBB.getBasicBlock());

  // Insert new MBBs.
  MF->insert(++MBB.getIterator(), LoopHeadMBB);
  MF->insert(++LoopHeadMBB->getIterator(), LoopIfBodyMBB);
  MF->insert(++LoopIfBodyMBB->getIterator(), LoopTailMBB);
  MF->insert(++LoopTailMBB->getIterator(), DoneMBB);

  // Set up successors and transfer remaining instructions to DoneMBB.
  LoopHeadMBB->addSuccessor(LoopIfBodyMBB);
  LoopHeadMBB->addSuccessor(LoopTailMBB);
  LoopIfBodyMBB->addSuccessor(LoopTailMBB);
  LoopTailMBB->addSuccessor(LoopHeadMBB);
  LoopTailMBB->addSuccessor(DoneMBB);
  DoneMBB->splice(DoneMBB->end(), &MBB, MI, MBB.end());
  DoneMBB->transferSuccessors(&MBB);
  MBB.addSuccessor(LoopHeadMBB);

  Register DestReg = MI.getOperand(0).getReg();
  Register Scratch1Reg = MI.getOperand(1).getReg();
  Register Scratch2Reg = MI.getOperand(2).getReg();
  Register AddrReg = MI.getOperand(3).getReg();
  Register IncrReg = MI.getOperand(4).getReg();
  Register MaskReg = MI.getOperand(5).getReg();

  // .loophead:
  //   ll.w destreg, (alignedaddr)
  //   and scratch2, destreg, mask
  //   move scratch1, destreg
  //   [sext scratch2 if signed min/max]
  //   ifnochangeneeded scratch2, incr, .looptail
  BuildMI(LoopHeadMBB, DL, TII->get(LoongArch::LL_W), DestReg)
      .addReg(AddrReg)
      .addImm(0);
  BuildMI(LoopHeadMBB, DL, TII->get(LoongArch::AND), Scratch2Reg)
      .addReg(DestReg)
      .addReg(MaskReg);
  BuildMI(LoopHeadMBB, DL, TII->get(LoongArch::OR), Scratch1Reg)
      .addReg(DestReg)
      .addReg(LoongArch::R0);

  switch (BinOp) {
  default:
    llvm_unreachable("Unexpected AtomicRMW BinOp");
  case AtomicRMWInst::Max: {
    insertSext(TII, DL, LoopHeadMBB, Scratch2Reg, MI.getOperand(6).getReg());
    BuildMI(LoopHeadMBB, DL, TII->get(LoongArch::BGE))
        .addReg(Scratch2Reg)
        .addReg(IncrReg)
        .addMBB(LoopTailMBB);
    break;
  }
  case AtomicRMWInst::Min: {
    insertSext(TII, DL, LoopHeadMBB, Scratch2Reg, MI.getOperand(6).getReg());
    BuildMI(LoopHeadMBB, DL, TII->get(LoongArch::BGE))
        .addReg(IncrReg)
        .addReg(Scratch2Reg)
        .addMBB(LoopTailMBB);
    break;
  }
  case AtomicRMWInst::UMax:
    BuildMI(LoopHeadMBB, DL, TII->get(LoongArch::BGEU))
        .addReg(Scratch2Reg)
        .addReg(IncrReg)
        .addMBB(LoopTailMBB);
    break;
  case AtomicRMWInst::UMin:
    BuildMI(LoopHeadMBB, DL, TII->get(LoongArch::BGEU))
        .addReg(IncrReg)
        .addReg(Scratch2Reg)
        .addMBB(LoopTailMBB);
    break;
  }

  // .loopifbody:
  //   xor scratch1, destreg, incr
  //   and scratch1, scratch1, mask
  //   xor scratch1, destreg, scratch1
  insertMaskedMerge(TII, DL, LoopIfBodyMBB, Scratch1Reg, DestReg, IncrReg,
                    MaskReg, Scratch1Reg);

  // .looptail:
  //   sc.w scratch1, addr, 0
  //   beq $zero, scratch1, loop
  //   b done     // skip the dbar insn because ...
  //   dbar 0x700 // ... it is only a workaround for LA464 uarch
  BuildMI(LoopTailMBB, DL, TII->get(LoongArch::SC_W), Scratch1Reg)
      .addReg(Scratch1Reg)
      .addReg(AddrReg)
      .addImm(0);
  BuildMI(LoopTailMBB, DL, TII->get(LoongArch::BEQ))
      .addReg(Scratch1Reg)
      .addReg(LoongArch::R0)
      .addMBB(LoopHeadMBB);

  BuildMI(LoopTailMBB, DL, TII->get(LoongArch::B)).addImm(0x8);
  BuildMI(LoopTailMBB, DL, TII->get(LoongArch::DBAR)).addImm(0x700);

  NextMBBI = MBB.end();
  MI.eraseFromParent();

  LivePhysRegs LiveRegs;
  computeAndAddLiveIns(LiveRegs, *LoopHeadMBB);
  computeAndAddLiveIns(LiveRegs, *LoopIfBodyMBB);
  computeAndAddLiveIns(LiveRegs, *LoopTailMBB);
  computeAndAddLiveIns(LiveRegs, *DoneMBB);

  return true;
}

bool LoongArchExpandAtomicPseudo::expandAtomicCmpXchg(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI, bool IsMasked,
    int Width, MachineBasicBlock::iterator &NextMBBI) {
  MachineInstr &MI = *MBBI;
  DebugLoc DL = MI.getDebugLoc();
  MachineFunction *MF = MBB.getParent();
  auto LoopHeadMBB = MF->CreateMachineBasicBlock(MBB.getBasicBlock());
  auto LoopTailMBB = MF->CreateMachineBasicBlock(MBB.getBasicBlock());
  auto DoneMBB = MF->CreateMachineBasicBlock(MBB.getBasicBlock());

  // Insert new MBBs.
  MF->insert(++MBB.getIterator(), LoopHeadMBB);
  MF->insert(++LoopHeadMBB->getIterator(), LoopTailMBB);
  MF->insert(++LoopTailMBB->getIterator(), DoneMBB);

  // Set up successors and transfer remaining instructions to DoneMBB.
  LoopHeadMBB->addSuccessor(LoopTailMBB);
  LoopHeadMBB->addSuccessor(DoneMBB);
  LoopTailMBB->addSuccessor(DoneMBB);
  LoopTailMBB->addSuccessor(LoopHeadMBB);
  DoneMBB->splice(DoneMBB->end(), &MBB, MI, MBB.end());
  DoneMBB->transferSuccessors(&MBB);
  MBB.addSuccessor(LoopHeadMBB);

  Register DestReg = MI.getOperand(0).getReg();
  Register ScratchReg = MI.getOperand(1).getReg();
  Register AddrReg = MI.getOperand(2).getReg();
  Register CmpValReg = MI.getOperand(3).getReg();
  Register NewValReg = MI.getOperand(4).getReg();
  AtomicOrdering Ordering =
      static_cast<AtomicOrdering>(MI.getOperand(IsMasked ? 6 : 5).getImm());

  if (!IsMasked) {
    // .loophead:
    //   ll.[w|d] dest, addr, 0
    //   bne dest, cmpval, done
    BuildMI(LoopHeadMBB, DL, TII->get(getLRForRMW(Ordering, Width)), DestReg)
        .addReg(AddrReg)
        .addImm(0);
    BuildMI(LoopHeadMBB, DL, TII->get(LoongArch::BNE))
        .addReg(DestReg)
        .addReg(CmpValReg)
        .addMBB(DoneMBB);
    // .looptail:
    //   or scratch, newval, $zero
    //   sc.[w|d] scratch, addr, 0
    //   beq $zero, scratch, loophead
    //   b done     // skip the dbar insn because ...
    //   dbar 0x700 // ... it is only a workaround for LA464 uarch
    BuildMI(LoopTailMBB, DL, TII->get(LoongArch::OR), ScratchReg)
        .addReg(NewValReg)
        .addReg(LoongArch::R0);
    BuildMI(LoopTailMBB, DL, TII->get(getSCForRMW(Ordering, Width)), ScratchReg)
        .addReg(ScratchReg)
        .addReg(AddrReg)
        .addImm(0);
    BuildMI(LoopTailMBB, DL, TII->get(LoongArch::BEQ))
        .addReg(ScratchReg)
        .addReg(LoongArch::R0)
        .addMBB(LoopHeadMBB);
    BuildMI(LoopTailMBB, DL, TII->get(LoongArch::B)).addImm(0x8);
    BuildMI(LoopTailMBB, DL, TII->get(LoongArch::DBAR)).addImm(0x700);
  } else {
    // .loophead:
    //   ll.[w|d] dest, addr, 0
    //   and scratch, dest, mask
    //   bne scratch, cmpval, done
    Register MaskReg = MI.getOperand(5).getReg();
    BuildMI(LoopHeadMBB, DL, TII->get(getLRForRMW(Ordering, Width)), DestReg)
        .addReg(AddrReg)
        .addImm(0);
    BuildMI(LoopHeadMBB, DL, TII->get(LoongArch::AND), ScratchReg)
        .addReg(DestReg)
        .addReg(MaskReg);
    BuildMI(LoopHeadMBB, DL, TII->get(LoongArch::BNE))
        .addReg(ScratchReg)
        .addReg(CmpValReg)
        .addMBB(DoneMBB);

    // .looptail:
    //   xor scratch, dest, newval
    //   and scratch, scratch, mask
    //   xor scratch, dest, scratch
    //   sc.[w|d] scratch, addr, 0
    //   beq $zero, scratch, loophead
    //   b done     // skip the dbar insn because ...
    //   dbar 0x700 // ... it is only a workaround for LA464 uarch
    insertMaskedMerge(TII, DL, LoopTailMBB, ScratchReg, DestReg, NewValReg,
                      MaskReg, ScratchReg);
    BuildMI(LoopTailMBB, DL, TII->get(getSCForRMW(Ordering, Width)), ScratchReg)
        .addReg(ScratchReg)
        .addReg(AddrReg)
        .addImm(0);
    BuildMI(LoopTailMBB, DL, TII->get(LoongArch::BEQ))
        .addReg(ScratchReg)
        .addReg(LoongArch::R0)
        .addMBB(LoopHeadMBB);
    BuildMI(LoopTailMBB, DL, TII->get(LoongArch::B)).addImm(0x8);
    BuildMI(LoopTailMBB, DL, TII->get(LoongArch::DBAR)).addImm(0x700);
  }

  NextMBBI = MBB.end();
  MI.eraseFromParent();

  LivePhysRegs LiveRegs;
  computeAndAddLiveIns(LiveRegs, *LoopHeadMBB);
  computeAndAddLiveIns(LiveRegs, *LoopTailMBB);
  computeAndAddLiveIns(LiveRegs, *DoneMBB);

  return true;
}

} // end of anonymous namespace

INITIALIZE_PASS(LoongArchExpandAtomicPseudo, "loongarch-expand-atomic-pseudo",
                LoongArch_EXPAND_ATOMIC_PSEUDO_NAME, false, false)

namespace llvm {

FunctionPass *createLoongArchExpandAtomicPseudoPass() {
  return new LoongArchExpandAtomicPseudo();
}

} // end of namespace llvm
