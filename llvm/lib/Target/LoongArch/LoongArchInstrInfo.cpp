//===-- LoongArchInstrInfo.cpp - LoongArch Instruction Information --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the LoongArch implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "LoongArchInstrInfo.h"
#include "LoongArch.h"
#include "LoongArchSubtarget.h"
#include "LoongArchTargetMachine.h"
#include "MCTargetDesc/LoongArchMatInt.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

#define GET_INSTRINFO_CTOR_DTOR
#include "LoongArchGenInstrInfo.inc"

LoongArchInstrInfo::LoongArchInstrInfo(LoongArchSubtarget &STI)
    : LoongArchGenInstrInfo(LoongArch::ADJCALLSTACKDOWN,
                            LoongArch::ADJCALLSTACKUP),
      STI(STI) {}

unsigned LoongArchInstrInfo::isLoadFromStackSlot(const MachineInstr &MI,
                                                 int &FrameIndex) const {
  switch (MI.getOpcode()) {
  default:
    return 0;
  case LoongArch::LD_B:
  case LoongArch::LD_BU:
  case LoongArch::LD_H:
  case LoongArch::LD_HU:
  case LoongArch::LD_W:
  case LoongArch::FLD_S:
  case LoongArch::LD_WU:
  case LoongArch::LD_D:
  case LoongArch::FLD_D:
    break;
  }

  if (MI.getOperand(1).isFI() && MI.getOperand(2).isImm() &&
      MI.getOperand(2).getImm() == 0) {
    FrameIndex = MI.getOperand(1).getIndex();
    return MI.getOperand(0).getReg();
  }

  return 0;
}

unsigned LoongArchInstrInfo::isStoreToStackSlot(const MachineInstr &MI,
                                                int &FrameIndex) const {
  switch (MI.getOpcode()) {
  default:
    return 0;
  case LoongArch::ST_B:
  case LoongArch::ST_H:
  case LoongArch::ST_W:
  case LoongArch::FST_S:
  case LoongArch::ST_D:
  case LoongArch::FST_D:
    break;
  }

  if (MI.getOperand(1).isFI() && MI.getOperand(2).isImm() &&
      MI.getOperand(2).getImm() == 0) {
    FrameIndex = MI.getOperand(1).getIndex();
    return MI.getOperand(0).getReg();
  }

  return 0;
}

void LoongArchInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator MBBI,
                                     const DebugLoc &DL, MCRegister DstReg,
                                     MCRegister SrcReg, bool KillSrc) const {
  if (LoongArch::GPRRegClass.contains(DstReg, SrcReg)) {
    BuildMI(MBB, MBBI, DL, get(LoongArch::OR), DstReg)
        .addReg(SrcReg, getKillRegState(KillSrc))
        .addReg(LoongArch::R0);
    return;
  }

  // FPR->FPR copies.
  unsigned Opc;
  if (LoongArch::FPR32RegClass.contains(DstReg, SrcReg))
    Opc = LoongArch::FMOV_S;
  else if (LoongArch::FPR64RegClass.contains(DstReg, SrcReg))
    Opc = LoongArch::FMOV_D;
  else
    llvm_unreachable("Impossible reg-to-reg copy");

  BuildMI(MBB, MBBI, DL, get(Opc), DstReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
}

void LoongArchInstrInfo::storeRegToStackSlot(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator I, Register SrcReg,
    bool IsKill, int FI, const TargetRegisterClass *RC,
    const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end())
    DL = I->getDebugLoc();

  MachineFunction *MF = MBB.getParent();
  const MachineFrameInfo &MFI = MF->getFrameInfo();
  MachineMemOperand *MMO = MF->getMachineMemOperand(
      MachinePointerInfo::getFixedStack(*MF, FI), MachineMemOperand::MOStore,
      MFI.getObjectSize(FI), MFI.getObjectAlign(FI));

  unsigned Opcode;
  if (LoongArch::GPRRegClass.hasSubClassEq(RC))
    Opcode = TRI->getRegSizeInBits(LoongArch::GPRRegClass) == 32
                 ? LoongArch::ST_W
                 : LoongArch::ST_D;
  else if (LoongArch::FPR32RegClass.hasSubClassEq(RC))
    Opcode = LoongArch::FST_S;
  else if (LoongArch::FPR64RegClass.hasSubClassEq(RC))
    Opcode = LoongArch::FST_D;
  else
    llvm_unreachable("Can't store this register to stack slot");

  BuildMI(MBB, I, DL, get(Opcode))
      .addReg(SrcReg, getKillRegState(IsKill))
      .addFrameIndex(FI)
      .addImm(0)
      .addMemOperand(MMO);
}

void LoongArchInstrInfo::loadRegFromStackSlot(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator I, Register DstReg,
    int FI, const TargetRegisterClass *RC,
    const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end())
    DL = I->getDebugLoc();

  MachineFunction *MF = MBB.getParent();
  const MachineFrameInfo &MFI = MF->getFrameInfo();
  MachineMemOperand *MMO = MF->getMachineMemOperand(
      MachinePointerInfo::getFixedStack(*MF, FI), MachineMemOperand::MOLoad,
      MFI.getObjectSize(FI), MFI.getObjectAlign(FI));

  unsigned Opcode;
  if (LoongArch::GPRRegClass.hasSubClassEq(RC))
    Opcode = TRI->getRegSizeInBits(LoongArch::GPRRegClass) == 32
                 ? LoongArch::LD_W
                 : LoongArch::LD_D;
  else if (LoongArch::FPR32RegClass.hasSubClassEq(RC))
    Opcode = LoongArch::FLD_S;
  else if (LoongArch::FPR64RegClass.hasSubClassEq(RC))
    Opcode = LoongArch::FLD_D;
  else
    llvm_unreachable("Can't load this register from stack slot");

  BuildMI(MBB, I, DL, get(Opcode), DstReg)
      .addFrameIndex(FI)
      .addImm(0)
      .addMemOperand(MMO);
}

void LoongArchInstrInfo::movImm(MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator MBBI,
                                const DebugLoc &DL, Register DstReg,
                                uint64_t Val, MachineInstr::MIFlag Flag) const {
  MachineFunction *MF = MBB.getParent();
  MachineRegisterInfo &MRI = MF->getRegInfo();
  bool IsLA64 = MF->getSubtarget<LoongArchSubtarget>().is64Bit();
  Register SrcReg = LoongArch::R0;
  Register Result = MRI.createVirtualRegister(&LoongArch::GPRRegClass);
  unsigned Num = 0;

  if (!IsLA64 && !isInt<32>(Val))
    report_fatal_error("Should only materialize 32-bit constants for LA32");

  LoongArchMatInt::InstSeq Seq;
  LoongArchMatInt::generateInstSeq(Val, IsLA64, Seq);
  assert(Seq.size() > 0);

  for (LoongArchMatInt::Inst &Inst : Seq) {
    // Write the final result to DstReg if it's the last instruction in the Seq.
    // Otherwise, write the result to the temp register.
    if (++Num == Seq.size())
      Result = DstReg;

    if (Inst.Opc == LoongArch::LU12I_W) {
      BuildMI(MBB, MBBI, DL, get(LoongArch::LU12I_W), Result)
          .addImm(Inst.Imm)
          .setMIFlag(Flag);
    } else {
      BuildMI(MBB, MBBI, DL, get(Inst.Opc), Result)
          .addReg(SrcReg, RegState::Kill)
          .addImm(Inst.Imm)
          .setMIFlag(Flag);
    }
    // Only the first instruction has R0 as its source.
    SrcReg = Result;
  }
}

static bool isBranchEQNZ(unsigned code) {
  switch (code) {
  default:
    return false;
  case LoongArch::BCEQZ:
  case LoongArch::BCNEZ:
    return true;
  }
}

// The contents of values added to Cond are not examined outside of
// LoongArchInstrInfo, giving us flexibility in what to push to it. For
// LoongArch, we push BranchOpcode, Reg1, Reg2.
static void parseCondBranch(MachineInstr &LastInst, MachineBasicBlock *&Target,
                            SmallVectorImpl<MachineOperand> &Cond) {
  // Block ends with fall-through condbranch.
  assert(LastInst.getDesc().isConditionalBranch() &&
         "Unknown conditional branch");
  unsigned n = isBranchEQNZ(LastInst.getOpcode()) ? 1 : 2;
  Target = LastInst.getOperand(n).getMBB();
  Cond.push_back(MachineOperand::CreateImm(LastInst.getOpcode()));
  for (unsigned i = 0; i < n; i++)
    Cond.push_back(LastInst.getOperand(i));
}

static unsigned getOppositeBranchOpcode(int Opc) {
  switch (Opc) {
  default:
    llvm_unreachable("Unrecognized conditional branch");
  case LoongArch::BEQ:
    return LoongArch::BNE;
  case LoongArch::BNE:
    return LoongArch::BEQ;
  case LoongArch::BLT:
    return LoongArch::BGE;
  case LoongArch::BGE:
    return LoongArch::BLT;
  case LoongArch::BLTU:
    return LoongArch::BGEU;
  case LoongArch::BGEU:
    return LoongArch::BLTU;
  case LoongArch::BCEQZ:
    return LoongArch::BCNEZ;
  case LoongArch::BCNEZ:
    return LoongArch::BCEQZ;
  case LoongArch::BEQZ:
    return LoongArch::BNEZ;
  case LoongArch::BNEZ:
    return LoongArch::BEQZ;
  }
}

bool LoongArchInstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                       MachineBasicBlock *&TBB,
                                       MachineBasicBlock *&FBB,
                                       SmallVectorImpl<MachineOperand> &Cond,
                                       bool AllowModify) const {
  TBB = FBB = nullptr;
  Cond.clear();

  // If the block has no terminators, it just falls into the block after it.
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
  if (I == MBB.end() || !isUnpredicatedTerminator(*I))
    return false;

  // Count the number of terminators and find the first unconditional or
  // indirect branch.
  MachineBasicBlock::iterator FirstUncondOrIndirectBr = MBB.end();
  int NumTerminators = 0;
  for (auto J = I.getReverse(); J != MBB.rend() && isUnpredicatedTerminator(*J);
       J++) {
    NumTerminators++;
    if (J->getDesc().isUnconditionalBranch() ||
        J->getDesc().isIndirectBranch()) {
      FirstUncondOrIndirectBr = J.getReverse();
    }
  }

  // If AllowModify is true, we can erase any terminators after
  // FirstUncondOrIndirectBR.
  if (AllowModify && FirstUncondOrIndirectBr != MBB.end()) {
    while (std::next(FirstUncondOrIndirectBr) != MBB.end()) {
      std::next(FirstUncondOrIndirectBr)->eraseFromParent();
      NumTerminators--;
    }
    I = FirstUncondOrIndirectBr;
  }

  // We can't handle blocks that end in an indirect branch.
  if (I->getDesc().isIndirectBranch())
    return true;

  // We can't handle blocks with more than 2 terminators.
  if (NumTerminators > 2)
    return true;

  // Handle a single unconditional branch.
  if (NumTerminators == 1 && I->getDesc().isUnconditionalBranch()) {
    TBB = getBranchDestBlock(*I);
    return false;
  }

  // Handle a single conditional branch.
  if (NumTerminators == 1 && I->getDesc().isConditionalBranch()) {
    parseCondBranch(*I, TBB, Cond);
    return false;
  }

  // Handle a conditional branch followed by an unconditional branch.
  if (NumTerminators == 2 && std::prev(I)->getDesc().isConditionalBranch() &&
      I->getDesc().isUnconditionalBranch()) {
    parseCondBranch(*std::prev(I), TBB, Cond);
    FBB = getBranchDestBlock(*I);
    return false;
  }

  // Otherwise, we can't handle this.
  return true;
}

unsigned LoongArchInstrInfo::removeBranch(MachineBasicBlock &MBB,
                                          int *BytesRemoved) const {
  if (BytesRemoved)
    *BytesRemoved = 0;
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();
  if (I == MBB.end())
    return 0;

  if (!I->getDesc().isUnconditionalBranch() &&
      !I->getDesc().isConditionalBranch())
    return 0;

  // Remove the branch.
  if (BytesRemoved)
    *BytesRemoved += getInstSizeInBytes(*I);
  I->eraseFromParent();

  I = MBB.end();

  if (I == MBB.begin())
    return 1;
  --I;
  if (!I->getDesc().isConditionalBranch())
    return 1;

  // Remove the branch.
  if (BytesRemoved)
    *BytesRemoved += getInstSizeInBytes(*I);
  I->eraseFromParent();
  return 2;
}

// Inserts a branch into the end of the specific MachineBasicBlock, returning
// the number of instructions inserted.
unsigned LoongArchInstrInfo::insertBranch(
    MachineBasicBlock &MBB, MachineBasicBlock *TBB, MachineBasicBlock *FBB,
    ArrayRef<MachineOperand> Cond, const DebugLoc &DL, int *BytesAdded) const {
  if (BytesAdded)
    *BytesAdded = 0;

  // Shouldn't be a fall through.
  assert(TBB && "insertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 3 || Cond.size() == 0 ||
          (Cond.size() == 2 && isBranchEQNZ(Cond[0].getImm()))) &&
         "LoongArch branch conditions have two components!");

  // Unconditional branch.
  if (Cond.empty()) {
    MachineInstr &MI = *BuildMI(&MBB, DL, get(LoongArch::B)).addMBB(TBB);
    if (BytesAdded)
      *BytesAdded += getInstSizeInBytes(MI);
    return 1;
  }

  // Either a one or two-way conditional branch.
  unsigned Opc = Cond[0].getImm();
  MachineInstr &CondMI =
      isBranchEQNZ(Opc)
          ? *BuildMI(&MBB, DL, get(Opc)).add(Cond[1]).addMBB(TBB)
          : *BuildMI(&MBB, DL, get(Opc)).add(Cond[1]).add(Cond[2]).addMBB(TBB);
  if (BytesAdded)
    *BytesAdded += getInstSizeInBytes(CondMI);

  // One-way conditional branch.
  if (!FBB)
    return 1;

  // Two-way conditional branch.
  MachineInstr &MI = *BuildMI(&MBB, DL, get(LoongArch::B)).addMBB(FBB);
  if (BytesAdded)
    *BytesAdded += getInstSizeInBytes(MI);
  return 2;
}

bool LoongArchInstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  assert((Cond.size() == (isBranchEQNZ(Cond[0].getImm()) ? 2 : 3)) &&
         "Invalid branch condition!");
  Cond[0].setImm(getOppositeBranchOpcode(Cond[0].getImm()));
  return false;
}

MachineBasicBlock *
LoongArchInstrInfo::getBranchDestBlock(const MachineInstr &MI) const {
  assert(MI.getDesc().isBranch() && "Unexpected opcode!");
  // The branch target is always the last operand.
  int NumOp = MI.getNumExplicitOperands();
  return MI.getOperand(NumOp - 1).getMBB();
}

bool LoongArchInstrInfo::isBranchOffsetInRange(unsigned BranchOp,
                                               int64_t BrOffset) const {
  // Ideally we could determine the supported branch offset from the
  // LoongArchII::FormMask, but this can't be used for Pseudo instructions like
  // PseudoBR.
  switch (BranchOp) {
  default:
    llvm_unreachable("Unexpected opcode!");
  case LoongArch::BEQ:
  case LoongArch::BNE:
  case LoongArch::BLT:
  case LoongArch::BGE:
  case LoongArch::BLTU:
  case LoongArch::BGEU:
    return isIntN(18, BrOffset);
  case LoongArch::BEQZ:
  case LoongArch::BNEZ:
  case LoongArch::BCEQZ:
  case LoongArch::BCNEZ:
    return isIntN(23, BrOffset);
  case LoongArch::B:
    return isIntN(28, BrOffset);
  }
}

unsigned LoongArchInstrInfo::getInstSizeInBytes(const MachineInstr &MI) const {
  unsigned Opcode = MI.getOpcode();
  unsigned LLSCFixup = 8; // b 0x8; dbar 0x700

  switch (Opcode) {
  default:
    return get(Opcode).getSize();
  case TargetOpcode::EH_LABEL:
  case TargetOpcode::IMPLICIT_DEF:
  case TargetOpcode::KILL:
  case TargetOpcode::DBG_VALUE:
    return 0;
  // These values are determined based on LoongArchExpandAtomicPseudoInsts,
  // LoongArchExpandPseudoInsts and LoongArchMCCodeEmitter, depending on where
  // the pseudos are expanded.
  case LoongArch::PseudoLLA:
  case LoongArch::PseudoLA:
  case LoongArch::PseudoLA_TLS_IE:
  case LoongArch::PseudoLA_TLS_LD:
  case LoongArch::PseudoLA_TLS_GD:
    return 8;
  case LoongArch::PseudoLA_TLS_LE:
    return STI.getXLen() == 64 ? 16 : 8;
  case LoongArch::PseudoAtomicLoadNand32:
  case LoongArch::PseudoAtomicLoadNand64:
  case LoongArch::PseudoCmpXchg32:
  case LoongArch::PseudoCmpXchg64:
    return 20 + LLSCFixup;
  case LoongArch::PseudoMaskedAtomicSwap32:
  case LoongArch::PseudoMaskedAtomicLoadAdd32:
  case LoongArch::PseudoMaskedAtomicLoadSub32:
    return 28 + LLSCFixup;
  case LoongArch::PseudoMaskedAtomicLoadNand32:
    return 32 + LLSCFixup;
  case LoongArch::PseudoMaskedAtomicLoadMax32:
  case LoongArch::PseudoMaskedAtomicLoadMin32:
    return 44 + LLSCFixup;
  case LoongArch::PseudoMaskedAtomicLoadUMax32:
  case LoongArch::PseudoMaskedAtomicLoadUMin32:
    return 36 + LLSCFixup;
  case LoongArch::PseudoMaskedCmpXchg32:
    return 32 + LLSCFixup;
  case TargetOpcode::INLINEASM:
  case TargetOpcode::INLINEASM_BR: {
    const MachineFunction &MF = *MI.getParent()->getParent();
    const auto &TM =
        static_cast<const LoongArchTargetMachine &>(MF.getTarget());
    return getInlineAsmLength(MI.getOperand(0).getSymbolName(),
                              *TM.getMCAsmInfo());
  }
  }
}

bool LoongArchInstrInfo::isAsCheapAsAMove(const MachineInstr &MI) const {
  const unsigned Opcode = MI.getOpcode();
  switch (Opcode) {
  default:
    break;
  // There are only "move" insns mentioned in LoongArch Vol1 included here.
  // Something like "ORI $rd, $rs, 0" may also be used as move, but it's
  // hard to tell if they are as cheap as the canonical move in the uarch
  // implementation.
  case LoongArch::FMOV_D:
  case LoongArch::FMOV_S:
    return MI.getOperand(1).isReg();
  case LoongArch::OR:
    return MI.getOperand(1).isReg() && MI.getOperand(2).isReg() &&
           MI.getOperand(2).getReg() == LoongArch::R0;
  }
  return MI.isAsCheapAsAMove();
}

Optional<DestSourcePair>
LoongArchInstrInfo::isCopyInstrImpl(const MachineInstr &MI) const {
  if (MI.isMoveReg())
    return DestSourcePair{MI.getOperand(0), MI.getOperand(1)};
  switch (MI.getOpcode()) {
  default:
    break;
  case LoongArch::OR:
    // Operand 1 can be a frameindex but callers expect registers
    if (MI.getOperand(1).isReg() && MI.getOperand(2).isReg() &&
        MI.getOperand(2).getReg() == LoongArch::R0)
      return DestSourcePair{MI.getOperand(0), MI.getOperand(1)};
    break;
  case LoongArch::FMOV_D:
  case LoongArch::FMOV_S:
    if (MI.getOperand(1).isReg())
      return DestSourcePair{MI.getOperand(0), MI.getOperand(1)};
    break;
  }
  return None;
}

bool LoongArchInstrInfo::verifyInstruction(const MachineInstr &MI,
                                           StringRef &ErrInfo) const {
  const MCInstrInfo *MCII = STI.getInstrInfo();
  MCInstrDesc const &Desc = MCII->get(MI.getOpcode());

  for (auto &OI : enumerate(Desc.operands())) {
    unsigned OpType = OI.value().OperandType;
    if (OpType >= LoongArchOp::OPERAND_FIRST_LoongArch_IMM &&
        OpType <= LoongArchOp::OPERAND_LAST_LoongArch_IMM) {
      const MachineOperand &MO = MI.getOperand(OI.index());
      if (MO.isImm()) {
        int64_t Imm = MO.getImm();
        bool Ok;
        switch (OpType) {
        default:
          llvm_unreachable("Unexpected operand type");
        case LoongArchOp::OPERAND_UIMM2:
          Ok = isUInt<2>(Imm);
          break;
        case LoongArchOp::OPERAND_UIMM2_ALSL:
          Ok = (1 <= Imm && Imm <= 4);
          break;
        case LoongArchOp::OPERAND_UIMM4:
          Ok = isUInt<4>(Imm);
          break;
        case LoongArchOp::OPERAND_UIMM5:
          Ok = isUInt<5>(Imm);
          break;
        case LoongArchOp::OPERAND_UIMM6:
          Ok = isUInt<6>(Imm);
          break;
        case LoongArchOp::OPERAND_UIMM12:
          Ok = isUInt<12>(Imm);
          break;
        case LoongArchOp::OPERAND_SIMM12:
          Ok = isInt<12>(Imm);
          break;
        case LoongArchOp::OPERAND_SIMM14:
          Ok = isInt<14>(Imm);
          break;
        case LoongArchOp::OPERAND_UIMM15:
          Ok = isUInt<15>(Imm);
          break;
        case LoongArchOp::OPERAND_SIMM16:
          Ok = isInt<16>(Imm);
          break;
        case LoongArchOp::OPERAND_UIMM20:
          Ok = isUInt<20>(Imm);
          break;
        case LoongArchOp::OPERAND_SIMM20:
          Ok = isInt<20>(Imm);
          break;
        case LoongArchOp::OPERAND_UIMMLOG2XLEN:
          if (STI.getTargetTriple().isArch64Bit())
            Ok = isUInt<6>(Imm);
          else
            Ok = isUInt<5>(Imm);
          break;
        }
        if (!Ok) {
          ErrInfo = "Invalid immediate";
          return false;
        }
      }
    }
  }

  return true;
}

// Return true if get the base operand, byte offset of an instruction and the
// memory width. Width is the size of memory that is being loaded/stored.
bool LoongArchInstrInfo::getMemOperandWithOffsetWidth(
    const MachineInstr &LdSt, const MachineOperand *&BaseReg, int64_t &Offset,
    unsigned &Width, const TargetRegisterInfo *TRI) const {
  if (!LdSt.mayLoadOrStore())
    return false;

  // Here we assume the standard RISC-V ISA, which uses a base+offset
  // addressing mode. You'll need to relax these conditions to support custom
  // load/stores instructions.
  if (LdSt.getNumExplicitOperands() != 3)
    return false;
  if (!LdSt.getOperand(1).isReg() || !LdSt.getOperand(2).isImm())
    return false;

  if (!LdSt.hasOneMemOperand())
    return false;

  Width = (*LdSt.memoperands_begin())->getSize();
  BaseReg = &LdSt.getOperand(1);
  Offset = LdSt.getOperand(2).getImm();
  return true;
}

bool LoongArchInstrInfo::areMemAccessesTriviallyDisjoint(
    const MachineInstr &MIa, const MachineInstr &MIb) const {
  assert(MIa.mayLoadOrStore() && "MIa must be a load or store.");
  assert(MIb.mayLoadOrStore() && "MIb must be a load or store.");

  if (MIa.hasUnmodeledSideEffects() || MIb.hasUnmodeledSideEffects() ||
      MIa.hasOrderedMemoryRef() || MIb.hasOrderedMemoryRef())
    return false;

  // Retrieve the base register, offset from the base register and width. Width
  // is the size of memory that is being loaded/stored (e.g. 1, 2, 4).  If
  // base registers are identical, and the offset of a lower memory access +
  // the width doesn't overlap the offset of a higher memory access,
  // then the memory accesses are different.
  const TargetRegisterInfo *TRI = STI.getRegisterInfo();
  const MachineOperand *BaseOpA = nullptr, *BaseOpB = nullptr;
  int64_t OffsetA = 0, OffsetB = 0;
  unsigned int WidthA = 0, WidthB = 0;
  if (getMemOperandWithOffsetWidth(MIa, BaseOpA, OffsetA, WidthA, TRI) &&
      getMemOperandWithOffsetWidth(MIb, BaseOpB, OffsetB, WidthB, TRI)) {
    if (BaseOpA->isIdenticalTo(*BaseOpB)) {
      int LowOffset = std::min(OffsetA, OffsetB);
      int HighOffset = std::max(OffsetA, OffsetB);
      int LowWidth = (LowOffset == OffsetA) ? WidthA : WidthB;
      if (LowOffset + LowWidth <= HighOffset)
        return true;
    }
  }
  return false;
}

std::pair<unsigned, unsigned>
LoongArchInstrInfo::decomposeMachineOperandsTargetFlags(unsigned TF) const {
  const unsigned Mask = LoongArchII::MO_DIRECT_FLAG_MASK;
  return std::make_pair(TF & Mask, TF & ~Mask);
}

ArrayRef<std::pair<unsigned, const char *>>
LoongArchInstrInfo::getSerializableDirectMachineOperandTargetFlags() const {
  using namespace LoongArchII;
  static const std::pair<unsigned, const char *> TargetFlags[] = {
      {MO_CALL, "larch-call"}, {MO_PLT, "larch-plt"}};
  return makeArrayRef(TargetFlags);
}
