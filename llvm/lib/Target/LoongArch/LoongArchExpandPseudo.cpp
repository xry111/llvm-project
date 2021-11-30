//===-- LoongArchExpandPseudoInsts.cpp - Expand pseudo instructions ------------===//
//
// The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a pass that expands pseudo instructions into target
// instructions to allow proper scheduling, if-conversion, and other late
// optimizations. This pass should be run after register allocation but before
// the post-regalloc scheduling pass.
//
// This is currently only used for expanding atomic pseudos after register
// allocation. We do this to avoid the fast register allocator introducing
// spills between ll and sc. These stores cause some LoongArch implementations to
// abort the atomic RMW sequence.
//
//===----------------------------------------------------------------------===//

#include "LoongArch.h"
#include "LoongArchInstrInfo.h"
#include "LoongArchSubtarget.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "llvm/CodeGen/LivePhysRegs.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"

using namespace llvm;

static cl::opt<bool>
EnableLoongson3FixLLSC("loongarch-fix-loongson3-llsc", cl::Hidden,
                cl::desc("Work around loongson3 llsc erratum"),
                cl::init(true));

#define DEBUG_TYPE "loongarch-pseudo"

namespace {
  class LoongArchExpandPseudo : public MachineFunctionPass {
  public:
    static char ID;
    LoongArchExpandPseudo() : MachineFunctionPass(ID) {}

    const LoongArchInstrInfo *TII;
    const LoongArchSubtarget *STI;

    bool runOnMachineFunction(MachineFunction &Fn) override;

    MachineFunctionProperties getRequiredProperties() const override {
      return MachineFunctionProperties().set(
          MachineFunctionProperties::Property::NoVRegs);
    }

    StringRef getPassName() const override {
      return "LoongArch pseudo instruction expansion pass";
    }

  private:
    bool expandAtomicCmpSwap(MachineBasicBlock &MBB,
                             MachineBasicBlock::iterator MBBI,
                             MachineBasicBlock::iterator &NextMBBI);
    bool expandAtomicCmpSwapSubword(MachineBasicBlock &MBB,
                                    MachineBasicBlock::iterator MBBI,
                                    MachineBasicBlock::iterator &NextMBBI);

    bool expandAtomicBinOp(MachineBasicBlock &BB,
                           MachineBasicBlock::iterator I,
                           MachineBasicBlock::iterator &NMBBI, unsigned Size);
    bool expandAtomicBinOpSubword(MachineBasicBlock &BB,
                                  MachineBasicBlock::iterator I,
                                  MachineBasicBlock::iterator &NMBBI);

    bool expandPseudoCall(MachineBasicBlock &BB,
                          MachineBasicBlock::iterator I,
                          MachineBasicBlock::iterator &NMBBI);
    bool expandPseudoTEQ(MachineBasicBlock &BB,
                         MachineBasicBlock::iterator I,
                         MachineBasicBlock::iterator &NMBBI);

    bool expandLoadAddr(MachineBasicBlock &BB,
                        MachineBasicBlock::iterator I,
                        MachineBasicBlock::iterator &NMBBI);

    bool expandMI(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
                  MachineBasicBlock::iterator &NMBB);
    bool expandMBB(MachineBasicBlock &MBB);
   };
  char LoongArchExpandPseudo::ID = 0;
}

bool LoongArchExpandPseudo::expandAtomicCmpSwapSubword(
    MachineBasicBlock &BB, MachineBasicBlock::iterator I,
    MachineBasicBlock::iterator &NMBBI) {

  MachineFunction *MF = BB.getParent();

  DebugLoc DL = I->getDebugLoc();
  unsigned LL, SC;
  unsigned ZERO = LoongArch::ZERO;
  unsigned BNE = LoongArch::BNE32;
  unsigned BEQ = LoongArch::BEQ32;
  unsigned SEOp =
      I->getOpcode() == LoongArch::ATOMIC_CMP_SWAP_I8_POSTRA ? LoongArch::EXT_W_B32 : LoongArch::EXT_W_H32;

  LL = LoongArch::LL_W;
  SC = LoongArch::SC_W;

  unsigned Dest = I->getOperand(0).getReg();
  unsigned Ptr = I->getOperand(1).getReg();
  unsigned Mask = I->getOperand(2).getReg();
  unsigned ShiftCmpVal = I->getOperand(3).getReg();
  unsigned Mask2 = I->getOperand(4).getReg();
  unsigned ShiftNewVal = I->getOperand(5).getReg();
  unsigned ShiftAmnt = I->getOperand(6).getReg();
  unsigned Scratch = I->getOperand(7).getReg();
  unsigned Scratch2 = I->getOperand(8).getReg();

  // insert new blocks after the current block
  const BasicBlock *LLVM_BB = BB.getBasicBlock();
  MachineBasicBlock *loop1MBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *loop2MBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *sinkMBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *exitMBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineFunction::iterator It = ++BB.getIterator();
  MF->insert(It, loop1MBB);
  MF->insert(It, loop2MBB);
  MF->insert(It, sinkMBB);
  MF->insert(It, exitMBB);

  // Transfer the remainder of BB and its successor edges to exitMBB.
  exitMBB->splice(exitMBB->begin(), &BB,
                  std::next(MachineBasicBlock::iterator(I)), BB.end());
  exitMBB->transferSuccessorsAndUpdatePHIs(&BB);

  //  thisMBB:
  //    ...
  //    fallthrough --> loop1MBB
  BB.addSuccessor(loop1MBB, BranchProbability::getOne());
  loop1MBB->addSuccessor(sinkMBB);
  loop1MBB->addSuccessor(loop2MBB);
  loop1MBB->normalizeSuccProbs();
  loop2MBB->addSuccessor(loop1MBB);
  loop2MBB->addSuccessor(sinkMBB);
  loop2MBB->normalizeSuccProbs();
  sinkMBB->addSuccessor(exitMBB, BranchProbability::getOne());

  // loop1MBB:
  //   ll dest, 0(ptr)
  //   and Mask', dest, Mask
  //   bne Mask', ShiftCmpVal, exitMBB
  BuildMI(loop1MBB, DL, TII->get(LL), Scratch).addReg(Ptr).addImm(0);
  BuildMI(loop1MBB, DL, TII->get(LoongArch::AND32), Scratch2)
      .addReg(Scratch)
      .addReg(Mask);
  BuildMI(loop1MBB, DL, TII->get(BNE))
    .addReg(Scratch2).addReg(ShiftCmpVal).addMBB(sinkMBB);

  // loop2MBB:
  //   and dest, dest, mask2
  //   or dest, dest, ShiftNewVal
  //   sc dest, dest, 0(ptr)
  //   beq dest, $0, loop1MBB
  BuildMI(loop2MBB, DL, TII->get(LoongArch::AND32), Scratch)
      .addReg(Scratch, RegState::Kill)
      .addReg(Mask2);
  BuildMI(loop2MBB, DL, TII->get(LoongArch::OR32), Scratch)
      .addReg(Scratch, RegState::Kill)
      .addReg(ShiftNewVal);
  BuildMI(loop2MBB, DL, TII->get(SC), Scratch)
      .addReg(Scratch, RegState::Kill)
      .addReg(Ptr)
      .addImm(0);
  BuildMI(loop2MBB, DL, TII->get(BEQ))
      .addReg(Scratch, RegState::Kill)
      .addReg(ZERO)
      .addMBB(loop1MBB);

  //  sinkMBB:
  //    srl     srlres, Mask', shiftamt
  //    sign_extend dest,srlres
  BuildMI(sinkMBB, DL, TII->get(LoongArch::SRL_W), Dest)
      .addReg(Scratch2)
      .addReg(ShiftAmnt);

  BuildMI(sinkMBB, DL, TII->get(SEOp), Dest).addReg(Dest);

  if (EnableLoongson3FixLLSC) {
    bool Has_sync = false;
    for (MachineBasicBlock::iterator MBBb = sinkMBB->begin(), MBBe = sinkMBB->end();
         MBBb != MBBe; ++MBBb) {
      Has_sync |= MBBb->getOpcode() == LoongArch::DBAR ? true : false;
      if (MBBb->mayLoad() || MBBb->mayStore())
        break;
    }

    if (!Has_sync) {
      MachineBasicBlock::iterator Pos = sinkMBB->begin();
      BuildMI(*sinkMBB, Pos, DL, TII->get(LoongArch::DBAR)).addImm(0);
    }
  }

  LivePhysRegs LiveRegs;
  computeAndAddLiveIns(LiveRegs, *loop1MBB);
  computeAndAddLiveIns(LiveRegs, *loop2MBB);
  computeAndAddLiveIns(LiveRegs, *sinkMBB);
  computeAndAddLiveIns(LiveRegs, *exitMBB);

  NMBBI = BB.end();
  I->eraseFromParent();
  return true;
}

bool LoongArchExpandPseudo::expandAtomicCmpSwap(MachineBasicBlock &BB,
                                           MachineBasicBlock::iterator I,
                                           MachineBasicBlock::iterator &NMBBI) {

  const unsigned Size =
      I->getOpcode() == LoongArch::ATOMIC_CMP_SWAP_I32_POSTRA ? 4 : 8;
  MachineFunction *MF = BB.getParent();

  DebugLoc DL = I->getDebugLoc();

  unsigned LL, SC, ZERO, BNE, BEQ, MOVE;

  if (Size == 4) {
    LL = LoongArch::LL_W;
    SC = LoongArch::SC_W;
    BNE = LoongArch::BNE32;
    BEQ = LoongArch::BEQ32;

    ZERO = LoongArch::ZERO;
    MOVE = LoongArch::OR32;
  } else {
    LL = LoongArch::LL_D;
    SC = LoongArch::SC_D;
    ZERO = LoongArch::ZERO_64;
    BNE = LoongArch::BNE;
    BEQ = LoongArch::BEQ;
    MOVE = LoongArch::OR;
  }

  unsigned Dest = I->getOperand(0).getReg();
  unsigned Ptr = I->getOperand(1).getReg();
  unsigned OldVal = I->getOperand(2).getReg();
  unsigned NewVal = I->getOperand(3).getReg();
  unsigned Scratch = I->getOperand(4).getReg();

  // insert new blocks after the current block
  const BasicBlock *LLVM_BB = BB.getBasicBlock();
  MachineBasicBlock *loop1MBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *loop2MBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *exitMBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineFunction::iterator It = ++BB.getIterator();
  MF->insert(It, loop1MBB);
  MF->insert(It, loop2MBB);
  MF->insert(It, exitMBB);

  // Transfer the remainder of BB and its successor edges to exitMBB.
  exitMBB->splice(exitMBB->begin(), &BB,
                  std::next(MachineBasicBlock::iterator(I)), BB.end());
  exitMBB->transferSuccessorsAndUpdatePHIs(&BB);

  //  thisMBB:
  //    ...
  //    fallthrough --> loop1MBB
  BB.addSuccessor(loop1MBB, BranchProbability::getOne());
  loop1MBB->addSuccessor(exitMBB);
  loop1MBB->addSuccessor(loop2MBB);
  loop1MBB->normalizeSuccProbs();
  loop2MBB->addSuccessor(loop1MBB);
  loop2MBB->addSuccessor(exitMBB);
  loop2MBB->normalizeSuccProbs();

  // loop1MBB:
  //   ll dest, 0(ptr)
  //   bne dest, oldval, exitMBB
  BuildMI(loop1MBB, DL, TII->get(LL), Dest).addReg(Ptr).addImm(0);
  BuildMI(loop1MBB, DL, TII->get(BNE))
    .addReg(Dest, RegState::Kill).addReg(OldVal).addMBB(exitMBB);

  // loop2MBB:
  //   move scratch, NewVal
  //   sc Scratch, Scratch, 0(ptr)
  //   beq Scratch, $0, loop1MBB
  BuildMI(loop2MBB, DL, TII->get(MOVE), Scratch).addReg(NewVal).addReg(ZERO);
  BuildMI(loop2MBB, DL, TII->get(SC), Scratch)
    .addReg(Scratch).addReg(Ptr).addImm(0);
  BuildMI(loop2MBB, DL, TII->get(BEQ))
    .addReg(Scratch, RegState::Kill).addReg(ZERO).addMBB(loop1MBB);

  if (EnableLoongson3FixLLSC) {
    bool Has_sync = false;
    for (MachineBasicBlock::iterator MBBb = exitMBB->begin(), MBBe = exitMBB->end();
         MBBb != MBBe; ++MBBb) {
      Has_sync |= MBBb->getOpcode() == LoongArch::DBAR ? true : false;
      if (MBBb->mayLoad() || MBBb->mayStore())
        break;
    }
    if (!Has_sync) {
      MachineBasicBlock::iterator Pos = exitMBB->begin();
      BuildMI(*exitMBB, Pos, DL, TII->get(LoongArch::DBAR)).addImm(0);
    }
  }

  LivePhysRegs LiveRegs;
  computeAndAddLiveIns(LiveRegs, *loop1MBB);
  computeAndAddLiveIns(LiveRegs, *loop2MBB);
  computeAndAddLiveIns(LiveRegs, *exitMBB);

  NMBBI = BB.end();
  I->eraseFromParent();
  return true;
}

bool LoongArchExpandPseudo::expandAtomicBinOpSubword(
    MachineBasicBlock &BB, MachineBasicBlock::iterator I,
    MachineBasicBlock::iterator &NMBBI) {

  MachineFunction *MF = BB.getParent();

  DebugLoc DL = I->getDebugLoc();
  unsigned LL, SC;
  unsigned BEQ = LoongArch::BEQ32;
  unsigned SEOp = LoongArch::EXT_W_H32;

  LL = LoongArch::LL_W;
  SC = LoongArch::SC_W;

  bool IsSwap = false;
  bool IsNand = false;

  unsigned Opcode = 0;
  switch (I->getOpcode()) {
  case LoongArch::ATOMIC_LOAD_NAND_I8_POSTRA:
    SEOp = LoongArch::EXT_W_B32;
    LLVM_FALLTHROUGH;
  case LoongArch::ATOMIC_LOAD_NAND_I16_POSTRA:
    IsNand = true;
    break;
  case LoongArch::ATOMIC_SWAP_I8_POSTRA:
    SEOp = LoongArch::EXT_W_B32;
    LLVM_FALLTHROUGH;
  case LoongArch::ATOMIC_SWAP_I16_POSTRA:
    IsSwap = true;
    break;
  case LoongArch::ATOMIC_LOAD_ADD_I8_POSTRA:
    SEOp = LoongArch::EXT_W_B32;
    LLVM_FALLTHROUGH;
  case LoongArch::ATOMIC_LOAD_ADD_I16_POSTRA:
    Opcode = LoongArch::ADD_W;
    break;
  case LoongArch::ATOMIC_LOAD_MAX_I8_POSTRA:
    SEOp = LoongArch::EXT_W_B32;
    LLVM_FALLTHROUGH;
  case LoongArch::ATOMIC_LOAD_MAX_I16_POSTRA:
    Opcode = LoongArch::AMMAX_DB_W;
    break;
  case LoongArch::ATOMIC_LOAD_MIN_I8_POSTRA:
    SEOp = LoongArch::EXT_W_B32;
    LLVM_FALLTHROUGH;
  case LoongArch::ATOMIC_LOAD_MIN_I16_POSTRA:
    Opcode = LoongArch::AMMIN_DB_W;
    break;
  case LoongArch::ATOMIC_LOAD_UMAX_I8_POSTRA:
    SEOp = LoongArch::EXT_W_B32;
    LLVM_FALLTHROUGH;
  case LoongArch::ATOMIC_LOAD_UMAX_I16_POSTRA:
    Opcode = LoongArch::AMMAX_DB_WU;
    break;
  case LoongArch::ATOMIC_LOAD_UMIN_I8_POSTRA:
    SEOp = LoongArch::EXT_W_B32;
    LLVM_FALLTHROUGH;
  case LoongArch::ATOMIC_LOAD_UMIN_I16_POSTRA:
    Opcode = LoongArch::AMMIN_DB_WU;
    break;
  case LoongArch::ATOMIC_LOAD_SUB_I8_POSTRA:
    SEOp = LoongArch::EXT_W_B32;
    LLVM_FALLTHROUGH;
  case LoongArch::ATOMIC_LOAD_SUB_I16_POSTRA:
    Opcode = LoongArch::SUB_W;
    break;
  case LoongArch::ATOMIC_LOAD_AND_I8_POSTRA:
    SEOp = LoongArch::EXT_W_B32;
    LLVM_FALLTHROUGH;
  case LoongArch::ATOMIC_LOAD_AND_I16_POSTRA:
    Opcode = LoongArch::AND32;
    break;
  case LoongArch::ATOMIC_LOAD_OR_I8_POSTRA:
    SEOp = LoongArch::EXT_W_B32;
    LLVM_FALLTHROUGH;
  case LoongArch::ATOMIC_LOAD_OR_I16_POSTRA:
    Opcode = LoongArch::OR32;
    break;
  case LoongArch::ATOMIC_LOAD_XOR_I8_POSTRA:
    SEOp = LoongArch::EXT_W_B32;
    LLVM_FALLTHROUGH;
  case LoongArch::ATOMIC_LOAD_XOR_I16_POSTRA:
    Opcode = LoongArch::XOR32;
    break;
  default:
    llvm_unreachable("Unknown subword atomic pseudo for expansion!");
  }

  unsigned Dest = I->getOperand(0).getReg();
  unsigned Ptr = I->getOperand(1).getReg();
  unsigned Incr = I->getOperand(2).getReg();
  unsigned Mask = I->getOperand(3).getReg();
  unsigned Mask2 = I->getOperand(4).getReg();
  unsigned ShiftAmnt = I->getOperand(5).getReg();
  unsigned OldVal = I->getOperand(6).getReg();
  unsigned BinOpRes = I->getOperand(7).getReg();
  unsigned StoreVal = I->getOperand(8).getReg();

  const BasicBlock *LLVM_BB = BB.getBasicBlock();
  MachineBasicBlock *loopMBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *sinkMBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *exitMBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineFunction::iterator It = ++BB.getIterator();
  MF->insert(It, loopMBB);
  MF->insert(It, sinkMBB);
  MF->insert(It, exitMBB);

  exitMBB->splice(exitMBB->begin(), &BB, std::next(I), BB.end());
  exitMBB->transferSuccessorsAndUpdatePHIs(&BB);

  BB.addSuccessor(loopMBB, BranchProbability::getOne());
  loopMBB->addSuccessor(sinkMBB);
  loopMBB->addSuccessor(loopMBB);
  loopMBB->normalizeSuccProbs();

  BuildMI(loopMBB, DL, TII->get(LL), OldVal).addReg(Ptr).addImm(0);
  if (IsNand) {
    //  and andres, oldval, incr2
    //  nor binopres, $0, andres
    //  and newval, binopres, mask
    BuildMI(loopMBB, DL, TII->get(LoongArch::AND32), BinOpRes)
        .addReg(OldVal)
        .addReg(Incr);
    BuildMI(loopMBB, DL, TII->get(LoongArch::NOR32), BinOpRes)
        .addReg(LoongArch::ZERO)
        .addReg(BinOpRes);
    BuildMI(loopMBB, DL, TII->get(LoongArch::AND32), BinOpRes)
        .addReg(BinOpRes)
        .addReg(Mask);
  } else if (!IsSwap) {
    //  <binop> binopres, oldval, incr2
    //  and newval, binopres, mask
    BuildMI(loopMBB, DL, TII->get(Opcode), BinOpRes)
        .addReg(OldVal)
        .addReg(Incr);
    BuildMI(loopMBB, DL, TII->get(LoongArch::AND32), BinOpRes)
        .addReg(BinOpRes)
        .addReg(Mask);
  } else { // atomic.swap
    //  and newval, incr2, mask
    BuildMI(loopMBB, DL, TII->get(LoongArch::AND32), BinOpRes)
        .addReg(Incr)
        .addReg(Mask);
  }

  // and StoreVal, OlddVal, Mask2
  // or StoreVal, StoreVal, BinOpRes
  // StoreVal<tied1> = sc StoreVal, 0(Ptr)
  // beq StoreVal, zero, loopMBB
  BuildMI(loopMBB, DL, TII->get(LoongArch::AND32), StoreVal)
    .addReg(OldVal).addReg(Mask2);
  BuildMI(loopMBB, DL, TII->get(LoongArch::OR32), StoreVal)
    .addReg(StoreVal).addReg(BinOpRes);
  BuildMI(loopMBB, DL, TII->get(SC), StoreVal)
    .addReg(StoreVal).addReg(Ptr).addImm(0);
  BuildMI(loopMBB, DL, TII->get(BEQ))
    .addReg(StoreVal).addReg(LoongArch::ZERO).addMBB(loopMBB);

  //  sinkMBB:
  //    and     maskedoldval1,oldval,mask
  //    srl     srlres,maskedoldval1,shiftamt
  //    sign_extend dest,srlres

  sinkMBB->addSuccessor(exitMBB, BranchProbability::getOne());

  BuildMI(sinkMBB, DL, TII->get(LoongArch::AND32), Dest)
    .addReg(OldVal).addReg(Mask);
  BuildMI(sinkMBB, DL, TII->get(LoongArch::SRL_W), Dest)
      .addReg(Dest).addReg(ShiftAmnt);

  BuildMI(sinkMBB, DL, TII->get(SEOp), Dest).addReg(Dest);

  LivePhysRegs LiveRegs;
  computeAndAddLiveIns(LiveRegs, *loopMBB);
  computeAndAddLiveIns(LiveRegs, *sinkMBB);
  computeAndAddLiveIns(LiveRegs, *exitMBB);

  NMBBI = BB.end();
  I->eraseFromParent();

  return true;
}

bool LoongArchExpandPseudo::expandAtomicBinOp(MachineBasicBlock &BB,
                                         MachineBasicBlock::iterator I,
                                         MachineBasicBlock::iterator &NMBBI,
                                         unsigned Size) {
  MachineFunction *MF = BB.getParent();

  DebugLoc DL = I->getDebugLoc();

  unsigned LL, SC, ZERO, BEQ, SUB;
  if (Size == 4) {
    LL = LoongArch::LL_W;
    SC = LoongArch::SC_W;
    BEQ = LoongArch::BEQ32;
    ZERO = LoongArch::ZERO;
    SUB = LoongArch::SUB_W;
  } else {
    LL = LoongArch::LL_D;
    SC = LoongArch::SC_D;
    ZERO = LoongArch::ZERO_64;
    BEQ = LoongArch::BEQ;
    SUB = LoongArch::SUB_D;
  }

  unsigned OldVal = I->getOperand(0).getReg();
  unsigned Ptr = I->getOperand(1).getReg();
  unsigned Incr = I->getOperand(2).getReg();
  unsigned Scratch = I->getOperand(3).getReg();

  unsigned Opcode = 0;
  unsigned OR = 0;
  unsigned AND = 0;
  unsigned NOR = 0;
  bool IsNand = false;
  bool IsSub = false;
  switch (I->getOpcode()) {
  case LoongArch::ATOMIC_LOAD_ADD_I32_POSTRA:
    Opcode = LoongArch::AMADD_DB_W;
    break;
  case LoongArch::ATOMIC_LOAD_SUB_I32_POSTRA:
    IsSub = true;
    Opcode = LoongArch::AMADD_DB_W;
    break;
  case LoongArch::ATOMIC_LOAD_AND_I32_POSTRA:
    Opcode = LoongArch::AMAND_DB_W;
    break;
  case LoongArch::ATOMIC_LOAD_OR_I32_POSTRA:
    Opcode = LoongArch::AMOR_DB_W;
    break;
  case LoongArch::ATOMIC_LOAD_XOR_I32_POSTRA:
    Opcode = LoongArch::AMXOR_DB_W;
    break;
  case LoongArch::ATOMIC_LOAD_NAND_I32_POSTRA:
    IsNand = true;
    AND = LoongArch::AND32;
    NOR = LoongArch::NOR32;
    break;
  case LoongArch::ATOMIC_SWAP_I32_POSTRA:
    OR = LoongArch::AMSWAP_DB_W;
    break;
  case LoongArch::ATOMIC_LOAD_MAX_I32_POSTRA:
    Opcode = LoongArch::AMMAX_DB_W;
    break;
  case LoongArch::ATOMIC_LOAD_MIN_I32_POSTRA:
    Opcode = LoongArch::AMMIN_DB_W;
    break;
  case LoongArch::ATOMIC_LOAD_UMAX_I32_POSTRA:
    Opcode = LoongArch::AMMAX_DB_WU;
    break;
  case LoongArch::ATOMIC_LOAD_UMIN_I32_POSTRA:
    Opcode = LoongArch::AMMIN_DB_WU;
    break;
  case LoongArch::ATOMIC_LOAD_ADD_I64_POSTRA:
    Opcode = LoongArch::AMADD_DB_D;
    break;
  case LoongArch::ATOMIC_LOAD_SUB_I64_POSTRA:
    IsSub = true;
    Opcode = LoongArch::AMADD_DB_D;
    break;
  case LoongArch::ATOMIC_LOAD_AND_I64_POSTRA:
    Opcode = LoongArch::AMAND_DB_D;
    break;
  case LoongArch::ATOMIC_LOAD_OR_I64_POSTRA:
    Opcode = LoongArch::AMOR_DB_D;
    break;
  case LoongArch::ATOMIC_LOAD_XOR_I64_POSTRA:
    Opcode = LoongArch::AMXOR_DB_D;
    break;
  case LoongArch::ATOMIC_LOAD_NAND_I64_POSTRA:
    IsNand = true;
    AND = LoongArch::AND;
    NOR = LoongArch::NOR;
    break;
  case LoongArch::ATOMIC_SWAP_I64_POSTRA:
    OR = LoongArch::AMSWAP_DB_D;
    break;
  case LoongArch::ATOMIC_LOAD_MAX_I64_POSTRA:
    Opcode = LoongArch::AMMAX_DB_D;
    break;
  case LoongArch::ATOMIC_LOAD_MIN_I64_POSTRA:
    Opcode = LoongArch::AMMIN_DB_D;
    break;
  case LoongArch::ATOMIC_LOAD_UMAX_I64_POSTRA:
    Opcode = LoongArch::AMMAX_DB_DU;
    break;
  case LoongArch::ATOMIC_LOAD_UMIN_I64_POSTRA:
    Opcode = LoongArch::AMMIN_DB_DU;
    break;
  default:
    llvm_unreachable("Unknown pseudo atomic!");
  }

  const BasicBlock *LLVM_BB = BB.getBasicBlock();
  MachineBasicBlock *loopMBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *exitMBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineFunction::iterator It = ++BB.getIterator();
  MF->insert(It, loopMBB);
  MF->insert(It, exitMBB);

  exitMBB->splice(exitMBB->begin(), &BB, std::next(I), BB.end());
  exitMBB->transferSuccessorsAndUpdatePHIs(&BB);

  BB.addSuccessor(loopMBB, BranchProbability::getOne());
  loopMBB->addSuccessor(exitMBB);
  loopMBB->addSuccessor(loopMBB);
  loopMBB->normalizeSuccProbs();

  assert((OldVal != Ptr) && "Clobbered the wrong ptr reg!");
  assert((OldVal != Incr) && "Clobbered the wrong reg!");
  if (Opcode) {
    if(IsSub){
      BuildMI(loopMBB, DL, TII->get(SUB), Scratch).addReg(ZERO).addReg(Incr);
      BuildMI(loopMBB, DL, TII->get(Opcode), OldVal).addReg(Scratch).addReg(Ptr).addImm(0);
    }
    else{
      BuildMI(loopMBB, DL, TII->get(Opcode), OldVal).addReg(Incr).addReg(Ptr).addImm(0);
    }
  } else if (IsNand) {
    assert(AND && NOR &&
           "Unknown nand instruction for atomic pseudo expansion");
    BuildMI(loopMBB, DL, TII->get(LL), OldVal).addReg(Ptr).addImm(0);
    BuildMI(loopMBB, DL, TII->get(AND), Scratch).addReg(OldVal).addReg(Incr);
    BuildMI(loopMBB, DL, TII->get(NOR), Scratch).addReg(ZERO).addReg(Scratch);
    BuildMI(loopMBB, DL, TII->get(SC), Scratch).addReg(Scratch).addReg(Ptr).addImm(0);
    BuildMI(loopMBB, DL, TII->get(BEQ)).addReg(Scratch).addReg(ZERO).addMBB(loopMBB);
  } else {
    assert(OR && "Unknown instruction for atomic pseudo expansion!");
    BuildMI(loopMBB, DL, TII->get(OR), OldVal).addReg(Incr).addReg(Ptr).addImm(0);
  }


  NMBBI = BB.end();
  I->eraseFromParent();

  LivePhysRegs LiveRegs;
  computeAndAddLiveIns(LiveRegs, *loopMBB);
  computeAndAddLiveIns(LiveRegs, *exitMBB);

  return true;
}

bool LoongArchExpandPseudo::expandLoadAddr(MachineBasicBlock &BB,
                                           MachineBasicBlock::iterator I,
                                           MachineBasicBlock::iterator &NMBBI) {
  MachineInstr &MI = *I;
  DebugLoc DL = MI.getDebugLoc();

  unsigned Op = MI.getOpcode();
  unsigned DestReg = MI.getOperand(0).getReg();
  unsigned TmpReg;
  const MachineOperand &MO = MI.getOperand(1);

  MachineInstrBuilder MIB1, MIB2, MIB3, MIB4, MIB5;
  unsigned HiFlag, LoFlag, HigherFlag, HighestFlag;
  unsigned HiOp, LoOp, HigherOp, HighestOp, LastOp;
  bool UseGot = false;

  HiOp = LoongArch::PCADDU12I_ri;
  LoOp = LoongArch::ORI_rri;
  HigherOp = LoongArch::LU32I_D_ri;
  HighestOp = LoongArch::LU52I_D_rri;

  switch (Op) {
  case LoongArch::LoadAddrLocal:
    // pcaddu12i + addi.d
    LoFlag = LoongArchII::MO_PCREL_LO;
    HiFlag = LoongArchII::MO_PCREL_HI;
    LoOp = LoongArch::ADDI_D_rri;
    break;
  case LoongArch::LoadAddrLocalRR:
    // pcaddu12i + ori + lu32i.d + lu52i.d + add.d
    LoFlag = LoongArchII::MO_PCREL_RRLO;
    HiFlag = LoongArchII::MO_PCREL_RRHI;
    HigherFlag = LoongArchII::MO_PCREL_RRHIGHER;
    HighestFlag = LoongArchII::MO_PCREL_RRHIGHEST;
    LastOp = LoongArch::ADD_D_rrr;
    break;
  case LoongArch::LoadAddrGlobal:
  case LoongArch::LoadAddrGlobal_Alias:
    // pcaddu12i + ld.d
    LoFlag = LoongArchII::MO_GOT_LO;
    HiFlag = LoongArchII::MO_GOT_HI;
    HiOp = LoongArch::PCADDU12I_rii;
    LoOp = LoongArch::LD_D_rrii;
    UseGot = true;
    break;
  case LoongArch::LoadAddrGlobalRR:
    // pcaddu12i + ori + lu32i.d + lu52i.d +ldx.d
    LoFlag = LoongArchII::MO_GOT_RRLO;
    HiFlag = LoongArchII::MO_GOT_RRHI;
    HigherFlag = LoongArchII::MO_GOT_RRHIGHER;
    HighestFlag = LoongArchII::MO_GOT_RRHIGHEST;
    HiOp = LoongArch::PCADDU12I_rii;
    LoOp = LoongArch::ORI_rrii;
    HigherOp = LoongArch::LU32I_D_rii;
    HighestOp = LoongArch::LU52I_D_rrii;
    LastOp = LoongArch::LDX_D_rrr;
    UseGot = true;
    break;
  case LoongArch::LoadAddrTLS_LE:
    // lu12i.w + ori + lu32i.d + lu52i.d
    LoFlag = LoongArchII::MO_TLSLE_LO;
    HiFlag = LoongArchII::MO_TLSLE_HI;
    HigherFlag = LoongArchII::MO_TLSLE_HIGHER;
    HighestFlag = LoongArchII::MO_TLSLE_HIGHEST;
    HiOp = LoongArch::LU12I_W_ri;
    break;
  case LoongArch::LoadAddrTLS_IE:
    // pcaddu12i + ld.d
    LoFlag = LoongArchII::MO_TLSIE_LO;
    HiFlag = LoongArchII::MO_TLSIE_HI;
    HiOp = LoongArch::PCADDU12I_rii;
    LoOp = LoongArch::LD_D_rrii;
    UseGot = true;
    break;
  case LoongArch::LoadAddrTLS_IE_RR:
    // pcaddu12i + ori + lu32i.d + lu52i.d +ldx.d
    LoFlag = LoongArchII::MO_TLSIE_RRLO;
    HiFlag = LoongArchII::MO_TLSIE_RRHI;
    HigherFlag = LoongArchII::MO_TLSIE_RRHIGHER;
    HighestFlag = LoongArchII::MO_TLSIE_RRHIGHEST;
    HiOp = LoongArch::PCADDU12I_rii;
    LoOp = LoongArch::ORI_rrii;
    HigherOp = LoongArch::LU32I_D_rii;
    HighestOp = LoongArch::LU52I_D_rrii;
    LastOp = LoongArch::LDX_D_rrr;
    UseGot = true;
    break;
  case LoongArch::LoadAddrTLS_LD:
  case LoongArch::LoadAddrTLS_GD:
    // pcaddu12i + addi.d
    LoFlag = LoongArchII::MO_TLSGD_LO;
    HiFlag = LoongArchII::MO_TLSGD_HI;
    HiOp = LoongArch::PCADDU12I_rii;
    LoOp = LoongArch::ADDI_D_rrii;
    UseGot = true;
    break;
  case LoongArch::LoadAddrTLS_LD_RR:
  case LoongArch::LoadAddrTLS_GD_RR:
    // pcaddu12i + ori + lu32i.d + lu52i.d + add.d
    LoFlag = LoongArchII::MO_TLSGD_RRLO;
    HiFlag = LoongArchII::MO_TLSGD_RRHI;
    HigherFlag = LoongArchII::MO_TLSGD_RRHIGHER;
    HighestFlag = LoongArchII::MO_TLSGD_RRHIGHEST;
    HiOp = LoongArch::PCADDU12I_rii;
    LoOp = LoongArch::ORI_rrii;
    HigherOp = LoongArch::LU32I_D_rii;
    HighestOp = LoongArch::LU52I_D_rrii;
    LastOp = LoongArch::ADD_D_rrr;
    UseGot = true;
    break;
  default:
    break;
  }

  MIB1 = BuildMI(BB, I, DL, TII->get(HiOp), DestReg);

  switch (Op) {
  case LoongArch::LoadAddrLocal: // la.local rd, symbol
  case LoongArch::LoadAddrGlobal: // la.global rd, symbol
  case LoongArch::LoadAddrGlobal_Alias: // la rd, symbol
  case LoongArch::LoadAddrTLS_IE: // la.tls.ie rd, symbol
  case LoongArch::LoadAddrTLS_LD: // la.tls.ld rd, symbol
  case LoongArch::LoadAddrTLS_GD: // la.tls.gd rd, symbol
    MIB2 = BuildMI(BB, I, DL, TII->get(LoOp), DestReg)
      .addReg(DestReg);
    if (MO.isJTI()) {
      MIB1.addJumpTableIndex(MO.getIndex(), HiFlag);
      MIB2.addJumpTableIndex(MO.getIndex(), LoFlag);
    } else if (MO.isBlockAddress()) {
      MIB1.addBlockAddress(MO.getBlockAddress(), 0, HiFlag);
      MIB2.addBlockAddress(MO.getBlockAddress(), 0, LoFlag);
    } else {
      MIB1.addDisp(MO, 0, HiFlag);
      MIB2.addDisp(MO, 0, LoFlag);
    }
    if (UseGot == true) {
      MIB1.addExternalSymbol("_GLOBAL_OFFSET_TABLE_");
      MIB2.addExternalSymbol("_GLOBAL_OFFSET_TABLE_");
    }
    break;

  case LoongArch::LoadAddrLocalRR: //la.local rd, rs, symbol
  case LoongArch::LoadAddrGlobalRR: // la.global rd, rs, symbol
  case LoongArch::LoadAddrTLS_IE_RR: // la.tls.ie rd, rs, symbol
  case LoongArch::LoadAddrTLS_LD_RR: // la.tls.ld rd, rs, symbol
  case LoongArch::LoadAddrTLS_GD_RR: // la.tls.gd rd, rs, symbol
    TmpReg = MI.getOperand(MI.getNumOperands()-1).getReg();
    MIB2 = BuildMI(BB, I, DL, TII->get(LoOp), TmpReg)
                  .addReg(TmpReg);
    MIB3 = BuildMI(BB, I, DL, TII->get(HigherOp), TmpReg);
    MIB4 = BuildMI(BB, I, DL, TII->get(HighestOp), TmpReg)
                  .addReg(TmpReg);
    MIB5 = BuildMI(BB, I, DL, TII->get(LastOp), DestReg)
                  .addReg(DestReg)
                  .addReg(TmpReg);
    if (MO.isJTI()) {
      MIB1.addJumpTableIndex(MO.getIndex(), HiFlag);
      MIB2.addJumpTableIndex(MO.getIndex(), LoFlag);
      MIB3.addJumpTableIndex(MO.getIndex(), HigherFlag);
      MIB4.addJumpTableIndex(MO.getIndex(), HighestFlag);
    } else if (MO.isBlockAddress()) {
      MIB1.addBlockAddress(MO.getBlockAddress(), 0, HiFlag);
      MIB2.addBlockAddress(MO.getBlockAddress(), 0, LoFlag);
      MIB3.addBlockAddress(MO.getBlockAddress(), 0, HigherFlag);
      MIB4.addBlockAddress(MO.getBlockAddress(), 0, HighestFlag);
    } else {
      MIB1.addDisp(MO, 0, HiFlag);
      MIB2.addDisp(MO, 0, LoFlag);
      MIB3.addDisp(MO, 0, HigherFlag);
      MIB4.addDisp(MO, 0, HighestFlag);
    }
    if (UseGot == true) {
      MIB1.addExternalSymbol("_GLOBAL_OFFSET_TABLE_");
      MIB2.addExternalSymbol("_GLOBAL_OFFSET_TABLE_");
      MIB3.addExternalSymbol("_GLOBAL_OFFSET_TABLE_");
      MIB4.addExternalSymbol("_GLOBAL_OFFSET_TABLE_");
    }
    break;
  case LoongArch::LoadAddrTLS_LE: // la.tls.le rd, symbol
    MIB2 = BuildMI(BB, I, DL, TII->get(LoOp), DestReg)
                  .addReg(DestReg);
    MIB3 = BuildMI(BB, I, DL, TII->get(HigherOp), DestReg);
    MIB4 = BuildMI(BB, I, DL, TII->get(HighestOp), DestReg)
                  .addReg(DestReg);
    if (MO.isJTI()) {
      MIB1.addJumpTableIndex(MO.getIndex(), HiFlag);
      MIB2.addJumpTableIndex(MO.getIndex(), LoFlag);
      MIB3.addJumpTableIndex(MO.getIndex(), HigherFlag);
      MIB4.addJumpTableIndex(MO.getIndex(), HighestFlag);
    } else if (MO.isBlockAddress()) {
      MIB1.addBlockAddress(MO.getBlockAddress(), 0, HiFlag);
      MIB2.addBlockAddress(MO.getBlockAddress(), 0, LoFlag);
      MIB3.addBlockAddress(MO.getBlockAddress(), 0, HigherFlag);
      MIB4.addBlockAddress(MO.getBlockAddress(), 0, HighestFlag);
    } else {
      MIB1.addDisp(MO, 0, HiFlag);
      MIB2.addDisp(MO, 0, LoFlag);
      MIB3.addDisp(MO, 0, HigherFlag);
      MIB4.addDisp(MO, 0, HighestFlag);
    }
    break;
  default:
    break;
  }

  MI.eraseFromParent();

  return true;
}

bool LoongArchExpandPseudo::expandPseudoCall(MachineBasicBlock &BB,
                                           MachineBasicBlock::iterator I,
                                           MachineBasicBlock::iterator &NMBBI) {
  MachineFunction *MF = BB.getParent();
  MachineInstr &MI = *I;
  DebugLoc DL = MI.getDebugLoc();
  CodeModel::Model M = MF->getTarget().getCodeModel();
  Reloc::Model RM = MF->getTarget().getRelocationModel();

  unsigned Ra = LoongArch::RA_64;
  const MachineOperand &MO = MI.getOperand(0);
  unsigned HiFlag, LoFlag, HigherFlag, HighestFlag, NoFlag;

  HiFlag = LoongArchII::MO_CALL_HI;
  LoFlag = LoongArchII::MO_CALL_LO;
  NoFlag = LoongArchII::MO_NO_FLAG;

  if (RM == Reloc::Static) { // for jit
    MachineInstrBuilder MIB1, MIB2, MIB3, MIB4, MIB5;

    HiFlag = LoongArchII::MO_ABS_HI;
    LoFlag = LoongArchII::MO_ABS_LO;
    HigherFlag = LoongArchII::MO_ABS_HIGHER;
    HighestFlag = LoongArchII::MO_ABS_HIGHEST;
    // lu12i.w + ori + lu32i.d + lu52i.d + jirl

    MIB1 = BuildMI(BB, I, DL, TII->get(LoongArch::LU12I_W), Ra);
    MIB2 = BuildMI(BB, I, DL, TII->get(LoongArch::ORI), Ra)
                  .addReg(Ra);
    MIB3 = BuildMI(BB, I, DL, TII->get(LoongArch::LU32I_D), Ra);
    MIB4 = BuildMI(BB, I, DL, TII->get(LoongArch::LU52I_D), Ra)
                  .addReg(Ra);
    MIB5 = BuildMI(BB, I, DL, TII->get(LoongArch::JIRL), Ra)
                  .addReg(Ra)
      .addImm(0);
    if (MO.isSymbol()) {
      MIB1.addExternalSymbol(MO.getSymbolName(), HiFlag);
      MIB2.addExternalSymbol(MO.getSymbolName(), LoFlag);
      MIB3.addExternalSymbol(MO.getSymbolName(), HigherFlag);
      MIB4.addExternalSymbol(MO.getSymbolName(), HighestFlag);
    } else {
      MIB1.addDisp(MO, 0, HiFlag);
      MIB2.addDisp(MO, 0, LoFlag);
      MIB3.addDisp(MO, 0, HigherFlag);
      MIB4.addDisp(MO, 0, HighestFlag);
    }
  } else if (M == CodeModel::Large) {
    // pcaddu18i + jirl
    MachineInstrBuilder MIB1;
    MachineInstrBuilder MIB2;

    MIB1 = BuildMI(BB, I, DL, TII->get(LoongArch::PCADDU18I), Ra);
    MIB2 = BuildMI(BB, I, DL, TII->get(LoongArch::JIRL_CALL), Ra)
                  .addReg(Ra);
    if (MO.isSymbol()) {
      MIB1.addExternalSymbol(MO.getSymbolName(), HiFlag);
      MIB2.addExternalSymbol(MO.getSymbolName(), LoFlag);
    } else {
      MIB1.addDisp(MO, 0, HiFlag);
      MIB2.addDisp(MO, 0, LoFlag);
    }
  } else {
    // bl
    MachineInstrBuilder MIB1;
    MIB1 = BuildMI(BB, I, DL, TII->get(LoongArch::BL));
    if (MO.isSymbol()) {
      MIB1.addExternalSymbol(MO.getSymbolName(), NoFlag);
    } else {
      MIB1.addDisp(MO, 0, NoFlag);
    }
  }

  MI.eraseFromParent();

  return true;
}

bool LoongArchExpandPseudo::expandPseudoTEQ(MachineBasicBlock &BB,
                                           MachineBasicBlock::iterator I,
                                           MachineBasicBlock::iterator &NMBBI) {
  MachineInstr &MI = *I;
  DebugLoc DL = MI.getDebugLoc();

  unsigned Divisor = MI.getOperand(0).getReg();
  unsigned BneOp = LoongArch::BNE;
  unsigned Zero = LoongArch::ZERO_64;

  // beq $Divisor, $zero, 8
  BuildMI(BB, I, DL, TII->get(BneOp), Divisor)
    .addReg(Zero)
    .addImm(8);
  // break 7
  BuildMI(BB, I, DL, TII->get(LoongArch::BREAK))
    .addImm(7);;

  MI.eraseFromParent();

  return true;
}
bool LoongArchExpandPseudo::expandMI(MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator MBBI,
                                MachineBasicBlock::iterator &NMBB) {

  bool Modified = false;

  switch (MBBI->getOpcode()) {
  case LoongArch::PseudoTEQ:
    return expandPseudoTEQ(MBB, MBBI, NMBB);
  case LoongArch::PseudoCall:
    return expandPseudoCall(MBB, MBBI, NMBB);
  case LoongArch::LoadAddrLocal:
  case LoongArch::LoadAddrLocalRR:
  case LoongArch::LoadAddrGlobal:
  case LoongArch::LoadAddrGlobalRR:
  case LoongArch::LoadAddrGlobal_Alias:
  case LoongArch::LoadAddrTLS_LD:
  case LoongArch::LoadAddrTLS_LD_RR:
  case LoongArch::LoadAddrTLS_GD:
  case LoongArch::LoadAddrTLS_GD_RR:
  case LoongArch::LoadAddrTLS_IE:
  case LoongArch::LoadAddrTLS_IE_RR:
  case LoongArch::LoadAddrTLS_LE:
    return expandLoadAddr(MBB, MBBI, NMBB);
  case LoongArch::ATOMIC_CMP_SWAP_I32_POSTRA:
  case LoongArch::ATOMIC_CMP_SWAP_I64_POSTRA:
    return expandAtomicCmpSwap(MBB, MBBI, NMBB);
  case LoongArch::ATOMIC_CMP_SWAP_I8_POSTRA:
  case LoongArch::ATOMIC_CMP_SWAP_I16_POSTRA:
    return expandAtomicCmpSwapSubword(MBB, MBBI, NMBB);
  case LoongArch::ATOMIC_SWAP_I8_POSTRA:
  case LoongArch::ATOMIC_SWAP_I16_POSTRA:
  case LoongArch::ATOMIC_LOAD_NAND_I8_POSTRA:
  case LoongArch::ATOMIC_LOAD_NAND_I16_POSTRA:
  case LoongArch::ATOMIC_LOAD_ADD_I8_POSTRA:
  case LoongArch::ATOMIC_LOAD_ADD_I16_POSTRA:
  case LoongArch::ATOMIC_LOAD_SUB_I8_POSTRA:
  case LoongArch::ATOMIC_LOAD_SUB_I16_POSTRA:
  case LoongArch::ATOMIC_LOAD_AND_I8_POSTRA:
  case LoongArch::ATOMIC_LOAD_AND_I16_POSTRA:
  case LoongArch::ATOMIC_LOAD_OR_I8_POSTRA:
  case LoongArch::ATOMIC_LOAD_OR_I16_POSTRA:
  case LoongArch::ATOMIC_LOAD_XOR_I8_POSTRA:
  case LoongArch::ATOMIC_LOAD_XOR_I16_POSTRA:
  case LoongArch::ATOMIC_LOAD_MAX_I8_POSTRA:
  case LoongArch::ATOMIC_LOAD_MAX_I16_POSTRA:
  case LoongArch::ATOMIC_LOAD_MIN_I8_POSTRA:
  case LoongArch::ATOMIC_LOAD_MIN_I16_POSTRA:
  case LoongArch::ATOMIC_LOAD_UMAX_I8_POSTRA:
  case LoongArch::ATOMIC_LOAD_UMAX_I16_POSTRA:
  case LoongArch::ATOMIC_LOAD_UMIN_I8_POSTRA:
  case LoongArch::ATOMIC_LOAD_UMIN_I16_POSTRA:
    return expandAtomicBinOpSubword(MBB, MBBI, NMBB);
  case LoongArch::ATOMIC_LOAD_ADD_I32_POSTRA:
  case LoongArch::ATOMIC_LOAD_SUB_I32_POSTRA:
  case LoongArch::ATOMIC_LOAD_AND_I32_POSTRA:
  case LoongArch::ATOMIC_LOAD_OR_I32_POSTRA:
  case LoongArch::ATOMIC_LOAD_XOR_I32_POSTRA:
  case LoongArch::ATOMIC_LOAD_NAND_I32_POSTRA:
  case LoongArch::ATOMIC_SWAP_I32_POSTRA:
  case LoongArch::ATOMIC_LOAD_MAX_I32_POSTRA:
  case LoongArch::ATOMIC_LOAD_MIN_I32_POSTRA:
  case LoongArch::ATOMIC_LOAD_UMAX_I32_POSTRA:
  case LoongArch::ATOMIC_LOAD_UMIN_I32_POSTRA:
    return expandAtomicBinOp(MBB, MBBI, NMBB, 4);
  case LoongArch::ATOMIC_LOAD_ADD_I64_POSTRA:
  case LoongArch::ATOMIC_LOAD_SUB_I64_POSTRA:
  case LoongArch::ATOMIC_LOAD_AND_I64_POSTRA:
  case LoongArch::ATOMIC_LOAD_OR_I64_POSTRA:
  case LoongArch::ATOMIC_LOAD_XOR_I64_POSTRA:
  case LoongArch::ATOMIC_LOAD_NAND_I64_POSTRA:
  case LoongArch::ATOMIC_SWAP_I64_POSTRA:
  case LoongArch::ATOMIC_LOAD_MAX_I64_POSTRA:
  case LoongArch::ATOMIC_LOAD_MIN_I64_POSTRA:
  case LoongArch::ATOMIC_LOAD_UMAX_I64_POSTRA:
  case LoongArch::ATOMIC_LOAD_UMIN_I64_POSTRA:
    return expandAtomicBinOp(MBB, MBBI, NMBB, 8);
  default:
    return Modified;
  }
}

bool LoongArchExpandPseudo::expandMBB(MachineBasicBlock &MBB) {
  bool Modified = false;

  MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();
  while (MBBI != E) {
    MachineBasicBlock::iterator NMBBI = std::next(MBBI);
    Modified |= expandMI(MBB, MBBI, NMBBI);
    MBBI = NMBBI;
  }

  return Modified;
}

bool LoongArchExpandPseudo::runOnMachineFunction(MachineFunction &MF) {
  STI = &static_cast<const LoongArchSubtarget &>(MF.getSubtarget());
  TII = STI->getInstrInfo();

  bool Modified = false;
  for (MachineFunction::iterator MFI = MF.begin(), E = MF.end(); MFI != E;
       ++MFI)
    Modified |= expandMBB(*MFI);

  if (Modified)
    MF.RenumberBlocks();

  return Modified;
}

/// createLoongArchExpandPseudoPass - returns an instance of the pseudo instruction
/// expansion pass.
FunctionPass *llvm::createLoongArchExpandPseudoPass() {
  return new LoongArchExpandPseudo();
}
