//===- LoongArchInstrInfo.cpp - LoongArch Instruction Information -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the LoongArch implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "LoongArchInstrInfo.h"
#include "MCTargetDesc/LoongArchBaseInfo.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "LoongArchAnalyzeImmediate.h"
#include "LoongArchSubtarget.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetOpcodes.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/Target/TargetMachine.h"
#include <cassert>

using namespace llvm;

#define GET_INSTRINFO_CTOR_DTOR
#include "LoongArchGenInstrInfo.inc"

// Pin the vtable to this file.
void LoongArchInstrInfo::anchor() {}
LoongArchInstrInfo::LoongArchInstrInfo(const LoongArchSubtarget &STI)
    : LoongArchGenInstrInfo(LoongArch::ADJCALLSTACKDOWN,
                            LoongArch::ADJCALLSTACKUP),
    RI(), Subtarget(STI) {}

const LoongArchRegisterInfo &LoongArchInstrInfo::getRegisterInfo() const {
  return RI;
}

/// isLoadFromStackSlot - If the specified machine instruction is a direct
/// load from a stack slot, return the virtual or physical register number of
/// the destination along with the FrameIndex of the loaded stack slot.  If
/// not, return 0.  This predicate must return 0 if the instruction has
/// any side effects other than loading from the stack slot.
unsigned LoongArchInstrInfo::isLoadFromStackSlot(const MachineInstr &MI,
                                                 int &FrameIndex) const {
  unsigned Opc = MI.getOpcode();
  if ((Opc == LoongArch::LD_W)   || (Opc == LoongArch::LD_D) ||
      (Opc == LoongArch::FLD_S) || (Opc == LoongArch::FLD_D)) {
    if ((MI.getOperand(1).isFI()) &&  // is a stack slot
        (MI.getOperand(2).isImm()) && // the imm is zero
        (isZeroImm(MI.getOperand(2)))) {
      FrameIndex = MI.getOperand(1).getIndex();
      return MI.getOperand(0).getReg();
    }
  }
  return 0;
}

/// isStoreToStackSlot - If the specified machine instruction is a direct
/// store to a stack slot, return the virtual or physical register number of
/// the source reg along with the FrameIndex of the loaded stack slot.  If
/// not, return 0.  This predicate must return 0 if the instruction has
/// any side effects other than storing to the stack slot.
unsigned LoongArchInstrInfo::isStoreToStackSlot(const MachineInstr &MI,
                                                int &FrameIndex) const {
  unsigned Opc = MI.getOpcode();
  if ((Opc == LoongArch::ST_D) || (Opc == LoongArch::ST_W) ||
      (Opc == LoongArch::FST_S) ||(Opc == LoongArch::FST_D)) {
    if ((MI.getOperand(1).isFI()) &&  // is a stack slot
        (MI.getOperand(2).isImm()) && // the imm is zero
        (isZeroImm(MI.getOperand(2)))) {
      FrameIndex = MI.getOperand(1).getIndex();
      return MI.getOperand(0).getReg();
    }
  }
  return 0;
}

void LoongArchInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator I,
                                     const DebugLoc &DL, MCRegister DestReg,
                                     MCRegister SrcReg, bool KillSrc) const {
  unsigned Opc = 0, ZeroReg = 0;
  if (LoongArch::GPR32RegClass.contains(DestReg)) { // Copy to CPU Reg.
    if (LoongArch::GPR32RegClass.contains(SrcReg)) {
      Opc = LoongArch::OR32, ZeroReg = LoongArch::ZERO;
    }
    else if (LoongArch::FGR32RegClass.contains(SrcReg))
      Opc = LoongArch::MOVFR2GR_S;
    else if (LoongArch::FCFRRegClass.contains(SrcReg))
      Opc = LoongArch::MOVCF2GR;
  }
  else if (LoongArch::GPR32RegClass.contains(SrcReg)) { // Copy from CPU Reg.
    if (LoongArch::FGR32RegClass.contains(DestReg))
      Opc = LoongArch::MOVGR2FR_W;
    else if (LoongArch::FCFRRegClass.contains(DestReg))
      Opc = LoongArch::MOVGR2CF;
  }
  else if (LoongArch::FGR32RegClass.contains(DestReg, SrcReg))
    Opc = LoongArch::FMOV_S;
  else if (LoongArch::FGR64RegClass.contains(DestReg, SrcReg))
    Opc = LoongArch::FMOV_D;
  else if (LoongArch::GPR64RegClass.contains(DestReg)) { // Copy to CPU64 Reg.
    if (LoongArch::GPR64RegClass.contains(SrcReg))
      Opc = LoongArch::OR, ZeroReg = LoongArch::ZERO_64;
    else if (LoongArch::FGR64RegClass.contains(SrcReg))
      Opc = LoongArch::MOVFR2GR_D;
    else if (LoongArch::FCFRRegClass.contains(SrcReg))
      Opc = LoongArch::MOVCF2GR;
  }
  else if (LoongArch::GPR64RegClass.contains(SrcReg)) { // Copy from CPU64 Reg.
    if (LoongArch::FGR64RegClass.contains(DestReg))
      Opc = LoongArch::MOVGR2FR_D;
    else if (LoongArch::FCFRRegClass.contains(DestReg))
      Opc = LoongArch::MOVGR2CF;
  }
  else if (LoongArch::FGR32RegClass.contains(DestReg)) // Copy to FGR32 Reg
      Opc = LoongArch::MOVCF2FR;
  else if (LoongArch::FGR32RegClass.contains(SrcReg))  // Copy from FGR32 Reg
      Opc = LoongArch::MOVFR2CF;
  else if (LoongArch::FGR64RegClass.contains(DestReg)) // Copy to FGR64 Reg
      Opc = LoongArch::MOVCF2FR;
  else if (LoongArch::FGR64RegClass.contains(SrcReg))  // Copy from FGR64 Reg
      Opc = LoongArch::MOVFR2CF;

  assert(Opc && "Cannot copy registers");

  MachineInstrBuilder MIB = BuildMI(MBB, I, DL, get(Opc));

  if (DestReg)
    MIB.addReg(DestReg, RegState::Define);

  if (SrcReg)
    MIB.addReg(SrcReg, getKillRegState(KillSrc));

  if (ZeroReg)
    MIB.addReg(ZeroReg);
}

static bool isORCopyInst(const MachineInstr &MI) {
  switch (MI.getOpcode()) {
  default:
    break;
  case LoongArch::OR:
    if (MI.getOperand(2).getReg() == LoongArch::ZERO_64)
      return true;
    break;
  case LoongArch::OR32:
    if (MI.getOperand(2).getReg() == LoongArch::ZERO)
      return true;
    break;
  }
  return false;
}

/// We check for the common case of 'or', as it's LoongArch' preferred instruction
/// for GPRs but we have to check the operands to ensure that is the case.
/// Other move instructions for LoongArch are directly identifiable.
Optional<DestSourcePair>
LoongArchInstrInfo::isCopyInstrImpl(const MachineInstr &MI) const {
  if (MI.isMoveReg() || isORCopyInst(MI)) {
    return DestSourcePair{MI.getOperand(0), MI.getOperand(1)};
  }
  return None;
}

void LoongArchInstrInfo::
storeRegToStack(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                Register SrcReg, bool isKill, int FI,
                const TargetRegisterClass *RC, const TargetRegisterInfo *TRI,
                int64_t Offset) const {
  DebugLoc DL;
  MachineMemOperand *MMO = GetMemOperand(MBB, FI, MachineMemOperand::MOStore);

  unsigned Opc = 0;
  if (LoongArch::GPR32RegClass.hasSubClassEq(RC))
    Opc = LoongArch::ST_W;
  else if (LoongArch::GPR64RegClass.hasSubClassEq(RC))
    Opc = LoongArch::ST_D;
  else if (LoongArch::FGR64RegClass.hasSubClassEq(RC))
    Opc = LoongArch::FST_D;
  else if (LoongArch::FGR32RegClass.hasSubClassEq(RC))
    Opc = LoongArch::FST_S;

  assert(Opc && "Register class not handled!");
  BuildMI(MBB, I, DL, get(Opc)).addReg(SrcReg, getKillRegState(isKill))
    .addFrameIndex(FI).addImm(Offset).addMemOperand(MMO);
}

void LoongArchInstrInfo::
loadRegFromStack(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                 Register DestReg, int FI, const TargetRegisterClass *RC,
                 const TargetRegisterInfo *TRI, int64_t Offset) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();
  MachineMemOperand *MMO = GetMemOperand(MBB, FI, MachineMemOperand::MOLoad);
  unsigned Opc = 0;

  if (LoongArch::GPR32RegClass.hasSubClassEq(RC))
    Opc = LoongArch::LD_W;
  else if (LoongArch::GPR64RegClass.hasSubClassEq(RC))
    Opc = LoongArch::LD_D;
  else if (LoongArch::FGR32RegClass.hasSubClassEq(RC))
    Opc = LoongArch::FLD_S;
  else if (LoongArch::FGR64RegClass.hasSubClassEq(RC))
    Opc = LoongArch::FLD_D;

  assert(Opc && "Register class not handled!");

  BuildMI(MBB, I, DL, get(Opc), DestReg)
      .addFrameIndex(FI)
      .addImm(Offset)
      .addMemOperand(MMO);
}

bool LoongArchInstrInfo::expandPostRAPseudo(MachineInstr &MI) const {
  MachineBasicBlock &MBB = *MI.getParent();
  switch (MI.getDesc().getOpcode()) {
  default:
    return false;
  case LoongArch::RetRA:
    expandRetRA(MBB, MI);
    break;
  case LoongArch::ERet:
    expandERet(MBB, MI);
    break;
  case LoongArch::PseudoFFINT_S_W:
    expandCvtFPInt(MBB, MI, LoongArch::FFINT_S_W, LoongArch::MOVGR2FR_W, false);
    break;
  case LoongArch::PseudoFFINT_S_L:
    expandCvtFPInt(MBB, MI, LoongArch::FFINT_S_L, LoongArch::MOVGR2FR_D, true);
    break;
  case LoongArch::PseudoFFINT_D_W:
    expandCvtFPInt(MBB, MI, LoongArch::FFINT_D_W, LoongArch::MOVGR2FR_W, true);
    break;
  case LoongArch::PseudoFFINT_D_L:
    expandCvtFPInt(MBB, MI, LoongArch::FFINT_D_L, LoongArch::MOVGR2FR_D, true);
    break;
  case LoongArch::LoongArcheh_return32:
  case LoongArch::LoongArcheh_return64:
    expandEhReturn(MBB, MI);
    break;
  }

  MBB.erase(MI);
  return true;
}

/// getOppositeBranchOpc - Return the inverse of the specified
/// opcode, e.g. turning BEQ to BNE.
unsigned LoongArchInstrInfo::getOppositeBranchOpc(unsigned Opc) const {
  switch (Opc) {
  default:                 llvm_unreachable("Illegal opcode!");
  case LoongArch::BEQ32:   return LoongArch::BNE32;
  case LoongArch::BEQ:     return LoongArch::BNE;
  case LoongArch::BNE32:   return LoongArch::BEQ32;
  case LoongArch::BNE:     return LoongArch::BEQ;
  case LoongArch::BEQZ32:  return LoongArch::BNEZ32;
  case LoongArch::BEQZ:    return LoongArch::BNEZ;
  case LoongArch::BNEZ32:  return LoongArch::BEQZ32;
  case LoongArch::BNEZ:    return LoongArch::BEQZ;
  case LoongArch::BCEQZ:   return LoongArch::BCNEZ;
  case LoongArch::BCNEZ:   return LoongArch::BCEQZ;
  case LoongArch::BLT32:   return LoongArch::BGE32;
  case LoongArch::BLT:     return LoongArch::BGE;
  case LoongArch::BGE32:   return LoongArch::BLT32;
  case LoongArch::BGE:     return LoongArch::BLT;
  case LoongArch::BLTU32:  return LoongArch::BGEU32;
  case LoongArch::BLTU:    return LoongArch::BGEU;
  case LoongArch::BGEU32:  return LoongArch::BLTU32;
  case LoongArch::BGEU:    return LoongArch::BLTU;
  }
}

/// Adjust SP by Amount bytes.
void LoongArchInstrInfo::adjustStackPtr(unsigned SP, int64_t Amount,
                                          MachineBasicBlock &MBB,
                                          MachineBasicBlock::iterator I) const {
  LoongArchABIInfo ABI = Subtarget.getABI();
  DebugLoc DL;
  unsigned ADDI = ABI.GetPtrAddiOp();

  if (Amount == 0)
    return;

  if (isInt<12>(Amount)) {
    // addi sp, sp, amount
    BuildMI(MBB, I, DL, get(ADDI), SP).addReg(SP).addImm(Amount)
      .setMIFlag(MachineInstr::FrameSetup);
  } else {
    // For numbers which are not 12bit integers we synthesize Amount inline
    // then add or subtract it from sp.
    unsigned Opc = ABI.GetPtrAddOp();
    if (Amount < 0) {
      Opc = ABI.GetPtrSubOp();
      Amount = -Amount;
    }
    unsigned Reg = loadImmediate(Amount, MBB, I, DL, nullptr);
    BuildMI(MBB, I, DL, get(Opc), SP).addReg(SP).addReg(Reg, RegState::Kill)
      .setMIFlag(MachineInstr::FrameSetup);
  }
}

/// This function generates the sequence of instructions needed to get the
/// result of adding register REG and immediate IMM.
unsigned LoongArchInstrInfo::loadImmediate(int64_t Imm, MachineBasicBlock &MBB,
                                             MachineBasicBlock::iterator II,
                                             const DebugLoc &DL,
                                             unsigned *NewImm) const {
  LoongArchAnalyzeImmediate AnalyzeImm;
  const LoongArchSubtarget &STI = Subtarget;
  MachineRegisterInfo &RegInfo = MBB.getParent()->getRegInfo();
  unsigned Size = STI.isABI_LP64() ? 64 : 32;
  unsigned ZEROReg = STI.isABI_LP64() ? LoongArch::ZERO_64 : LoongArch::ZERO;
  const TargetRegisterClass *RC = STI.isABI_LP64() ?
    &LoongArch::GPR64RegClass : &LoongArch::GPR32RegClass;
  bool LastInstrIsADDI = NewImm;

  const LoongArchAnalyzeImmediate::InstSeq &Seq =
    AnalyzeImm.Analyze(Imm, Size, LastInstrIsADDI);
  LoongArchAnalyzeImmediate::InstSeq::const_iterator Inst = Seq.begin();

  assert(Seq.size() && (!LastInstrIsADDI || (Seq.size() > 1)));

  unsigned Reg = RegInfo.createVirtualRegister(RC);

  BuildMI(MBB, II, DL, get(Inst->Opc), Reg).addReg(ZEROReg)
    .addImm(SignExtend64<12>(Inst->ImmOpnd));

  // Build the remaining instructions in Seq.
  for (++Inst; Inst != Seq.end() - LastInstrIsADDI; ++Inst)
    BuildMI(MBB, II, DL, get(Inst->Opc), Reg).addReg(Reg, RegState::Kill)
      .addImm(SignExtend64<12>(Inst->ImmOpnd));

  if (LastInstrIsADDI)
    *NewImm = Inst->ImmOpnd;

  return Reg;
}

unsigned LoongArchInstrInfo::getAnalyzableBrOpc(unsigned Opc) const {
    return (Opc == LoongArch::B      || Opc == LoongArch::B32      ||
            Opc == LoongArch::BEQZ   || Opc == LoongArch::BEQZ32   ||
            Opc == LoongArch::BNEZ   || Opc == LoongArch::BNEZ32   ||
            Opc == LoongArch::BCEQZ ||
            Opc == LoongArch::BCNEZ ||
            Opc == LoongArch::BEQ    || Opc == LoongArch::BEQ32    ||
            Opc == LoongArch::BNE    || Opc == LoongArch::BNE32    ||
            Opc == LoongArch::BLT    || Opc == LoongArch::BLT32    ||
            Opc == LoongArch::BGE    || Opc == LoongArch::BGE32    ||
            Opc == LoongArch::BLTU   || Opc == LoongArch::BLTU32   ||
            Opc == LoongArch::BGEU   || Opc == LoongArch::BGEU32) ? Opc : 0;
}

void LoongArchInstrInfo::expandRetRA(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator I) const {

  MachineInstrBuilder MIB;

  if (Subtarget.is64Bit())
    MIB = BuildMI(MBB, I, I->getDebugLoc(), get(LoongArch::PseudoReturn64))
              .addReg(LoongArch::RA_64, RegState::Undef);
  else
    MIB = BuildMI(MBB, I, I->getDebugLoc(), get(LoongArch::PseudoReturn))
              .addReg(LoongArch::RA, RegState::Undef);

  // Retain any imp-use flags.
  for (auto & MO : I->operands()) {
    if (MO.isImplicit())
      MIB.add(MO);
  }
}

void LoongArchInstrInfo::expandERet(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator I) const {
    BuildMI(MBB, I, I->getDebugLoc(), get(LoongArch::ERTN));
}

std::pair<bool, bool>
LoongArchInstrInfo::compareOpndSize(unsigned Opc,
                                 const MachineFunction &MF) const {
  const MCInstrDesc &Desc = get(Opc);
  assert(Desc.NumOperands == 2 && "Unary instruction expected.");
  const LoongArchRegisterInfo *RI = &getRegisterInfo();
  unsigned DstRegSize = RI->getRegSizeInBits(*getRegClass(Desc, 0, RI, MF));
  unsigned SrcRegSize = RI->getRegSizeInBits(*getRegClass(Desc, 1, RI, MF));

  return std::make_pair(DstRegSize > SrcRegSize, DstRegSize < SrcRegSize);
}

void LoongArchInstrInfo::expandCvtFPInt(MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator I,
                                     unsigned CvtOpc, unsigned MovOpc,
                                     bool IsI64) const {
  const MCInstrDesc &CvtDesc = get(CvtOpc), &MovDesc = get(MovOpc);
  const MachineOperand &Dst = I->getOperand(0), &Src = I->getOperand(1);
  unsigned DstReg = Dst.getReg(), SrcReg = Src.getReg(), TmpReg = DstReg;
  unsigned KillSrc =  getKillRegState(Src.isKill());
  DebugLoc DL = I->getDebugLoc();
  bool DstIsLarger, SrcIsLarger;

  std::tie(DstIsLarger, SrcIsLarger) =
      compareOpndSize(CvtOpc, *MBB.getParent());

  if (DstIsLarger)
    TmpReg = getRegisterInfo().getSubReg(DstReg, LoongArch::sub_lo);

  if (SrcIsLarger)
    DstReg = getRegisterInfo().getSubReg(DstReg, LoongArch::sub_lo);

  BuildMI(MBB, I, DL, MovDesc, TmpReg).addReg(SrcReg, KillSrc);
  BuildMI(MBB, I, DL, CvtDesc, DstReg).addReg(TmpReg, RegState::Kill);
}

void LoongArchInstrInfo::expandEhReturn(MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator I) const {
  // This pseudo instruction is generated as part of the lowering of
  // ISD::EH_RETURN. We convert it to a stack increment by OffsetReg, and
  // indirect jump to TargetReg
  LoongArchABIInfo ABI = Subtarget.getABI();
  unsigned ADD = ABI.GetPtrAddOp();
  unsigned SP = Subtarget.is64Bit() ? LoongArch::SP_64 : LoongArch::SP;
  unsigned RA = Subtarget.is64Bit() ? LoongArch::RA_64 : LoongArch::RA;
  unsigned T8 = Subtarget.is64Bit() ? LoongArch::T8_64 : LoongArch::T8;
  unsigned ZERO = Subtarget.is64Bit() ? LoongArch::ZERO_64 : LoongArch::ZERO;
  unsigned OffsetReg = I->getOperand(0).getReg();
  unsigned TargetReg = I->getOperand(1).getReg();

  // add $ra, $v0, $zero
  // add $sp, $sp, $v1
  // jr   $ra (via RetRA)
  const TargetMachine &TM = MBB.getParent()->getTarget();
  if (TM.isPositionIndependent())
    BuildMI(MBB, I, I->getDebugLoc(), get(ADD), T8)
        .addReg(TargetReg)
        .addReg(ZERO);
  BuildMI(MBB, I, I->getDebugLoc(), get(ADD), RA)
      .addReg(TargetReg)
      .addReg(ZERO);
  BuildMI(MBB, I, I->getDebugLoc(), get(ADD), SP).addReg(SP).addReg(OffsetReg);
  expandRetRA(MBB, I);
}


bool LoongArchInstrInfo::isZeroImm(const MachineOperand &op) const {
  return op.isImm() && op.getImm() == 0;
}

/// insertNoop - If data hazard condition is found insert the target nop
/// instruction.
// FIXME: This appears to be dead code.
void LoongArchInstrInfo::
insertNoop(MachineBasicBlock &MBB, MachineBasicBlock::iterator MI) const
{
  DebugLoc DL;
  BuildMI(MBB, MI, DL, get(LoongArch::NOP));
}

MachineMemOperand *
LoongArchInstrInfo::GetMemOperand(MachineBasicBlock &MBB, int FI,
                             MachineMemOperand::Flags Flags) const {
  MachineFunction &MF = *MBB.getParent();
  MachineFrameInfo &MFI = MF.getFrameInfo();

  return MF.getMachineMemOperand(MachinePointerInfo::getFixedStack(MF, FI),
                                 Flags, MFI.getObjectSize(FI),
                                 MFI.getObjectAlign(FI));
}

//===----------------------------------------------------------------------===//
// Branch Analysis
//===----------------------------------------------------------------------===//

void LoongArchInstrInfo::AnalyzeCondBr(const MachineInstr *Inst, unsigned Opc,
                                  MachineBasicBlock *&BB,
                                  SmallVectorImpl<MachineOperand> &Cond) const {
  assert(getAnalyzableBrOpc(Opc) && "Not an analyzable branch");
  int NumOp = Inst->getNumExplicitOperands();

  // for both int and fp branches, the last explicit operand is the
  // MBB.
  BB = Inst->getOperand(NumOp-1).getMBB();
  Cond.push_back(MachineOperand::CreateImm(Opc));

  for (int i = 0; i < NumOp-1; i++)
    Cond.push_back(Inst->getOperand(i));
}

bool LoongArchInstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                  MachineBasicBlock *&TBB,
                                  MachineBasicBlock *&FBB,
                                  SmallVectorImpl<MachineOperand> &Cond,
                                  bool AllowModify) const {
  SmallVector<MachineInstr*, 2> BranchInstrs;
  BranchType BT = analyzeBranch(MBB, TBB, FBB, Cond, AllowModify, BranchInstrs);

  return (BT == BT_None) || (BT == BT_Indirect);
}

MachineInstr *
LoongArchInstrInfo::BuildCondBr(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                                const DebugLoc &DL,
                                ArrayRef<MachineOperand> Cond) const {
  unsigned Opc = Cond[0].getImm();
  const MCInstrDesc &MCID = get(Opc);
  MachineInstrBuilder MIB = BuildMI(&MBB, DL, MCID);

  for (unsigned i = 1; i < Cond.size(); ++i) {
    assert((Cond[i].isImm() || Cond[i].isReg()) &&
           "Cannot copy operand for conditional branch!");
    MIB.add(Cond[i]);
  }
  MIB.addMBB(TBB);
  return MIB.getInstr();
}

unsigned LoongArchInstrInfo::insertBranch(MachineBasicBlock &MBB,
                                          MachineBasicBlock *TBB,
                                          MachineBasicBlock *FBB,
                                          ArrayRef<MachineOperand> Cond,
                                          const DebugLoc &DL,
                                          int *BytesAdded) const {
  unsigned UncondBrOpc = LoongArch::B;
  // Shouldn't be a fall through.
  assert(TBB && "insertBranch must not be told to insert a fallthrough");
  if (BytesAdded)
    *BytesAdded = 0;

  // # of condition operands:
  //  Unconditional branches: 0
  //  Floating point branches: 1 (opc)
  //  Int BranchZero: 2 (opc, reg)
  //  Int Branch: 3 (opc, reg0, reg1)
  assert((Cond.size() <= 3) &&
         "# of LoongArch branch conditions must be <= 3!");

  // Two-way Conditional branch.
  if (FBB) {
    MachineInstr &MI1 = *BuildCondBr(MBB, TBB, DL, Cond);
    if (BytesAdded)
      *BytesAdded += getInstSizeInBytes(MI1);
    MachineInstr &MI2 = *BuildMI(&MBB, DL, get(UncondBrOpc)).addMBB(FBB);
    if (BytesAdded)
      *BytesAdded += getInstSizeInBytes(MI2);
    return 2;
  }

  // One way branch.
  // Unconditional branch.
  if (Cond.empty()) {
    MachineInstr &MI = *BuildMI(&MBB, DL, get(UncondBrOpc)).addMBB(TBB);
    if (BytesAdded)
      *BytesAdded += getInstSizeInBytes(MI);
  }
  else {// Conditional branch.
    MachineInstr &MI = *BuildCondBr(MBB, TBB, DL, Cond);
    if (BytesAdded)
      *BytesAdded += getInstSizeInBytes(MI);
  }
  return 1;
}

unsigned LoongArchInstrInfo::insertIndirectBranch(MachineBasicBlock &MBB,
                                                  MachineBasicBlock &DestBB,
                                                  const DebugLoc &DL,
                                                  int64_t BrOffset,
                                                  RegScavenger *RS) const {
  assert(RS && "RegScavenger required for long branching");
  assert(MBB.empty() &&
         "new block should be inserted for expanding unconditional branch");
  assert(MBB.pred_size() == 1);

  MachineFunction *MF = MBB.getParent();
  MachineRegisterInfo &MRI = MF->getRegInfo();
  const LoongArchSubtarget &Subtarget = MF->getSubtarget<LoongArchSubtarget>();
  bool is64 = Subtarget.isABI_LP64();
  const TargetRegisterClass *RC =
    is64 ? &LoongArch::GPR64RegClass : &LoongArch::GPR32RegClass;

  if (!is64 && !isInt<32>(BrOffset))
    report_fatal_error(
        "Branch offsets outside of the signed 32-bit range not supported");

  unsigned ScratchReg = MRI.createVirtualRegister(RC);
  unsigned ZeroReg = is64 ? LoongArch::ZERO_64 : LoongArch::ZERO;
  auto II = MBB.end();

  MachineInstr &Pcaddu12iMI =
      *BuildMI(MBB, II, DL, get(LoongArch::LONG_BRANCH_PCADDU12I), ScratchReg)
          .addMBB(&DestBB, LoongArchII::MO_PCREL_HI);
  BuildMI(MBB, II, DL, get(LoongArch::LONG_BRANCH_ADDID2Op), ScratchReg)
      .addReg(ScratchReg)
      .addMBB(&DestBB, LoongArchII::MO_PCREL_LO);
  BuildMI(MBB, II, DL, get(LoongArch::JIRL))
      .addReg(ZeroReg)
      .addReg(ScratchReg, RegState::Kill)
      .addImm(0);
  RS->enterBasicBlockEnd(MBB);
  unsigned Scav = RS->scavengeRegisterBackwards(
      *RC, MachineBasicBlock::iterator(Pcaddu12iMI), false, 0);
  MRI.replaceRegWith(ScratchReg, Scav);
  MRI.clearVirtRegs();
  RS->setRegUsed(Scav);

  return 12;
}

unsigned LoongArchInstrInfo::removeBranch(MachineBasicBlock &MBB,
                                          int *BytesRemoved) const {
  if (BytesRemoved)
    *BytesRemoved = 0;

  MachineBasicBlock::reverse_iterator I = MBB.rbegin(), REnd = MBB.rend();
  unsigned removed = 0;

  // Up to 2 branches are removed.
  // Note that indirect branches are not removed.
  while (I != REnd && removed < 2) {
    // Skip past debug instructions.
    if (I->isDebugInstr()) {
      ++I;
      continue;
    }
    if (!getAnalyzableBrOpc(I->getOpcode()))
      break;
    // Remove the branch.
    I->eraseFromParent();
    if (BytesRemoved)
      *BytesRemoved += getInstSizeInBytes(*I);
    I = MBB.rbegin();
    ++removed;
  }

  return removed;
}

/// reverseBranchCondition - Return the inverse opcode of the
/// specified Branch instruction.
bool LoongArchInstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  assert( (Cond.size() && Cond.size() <= 3) &&
          "Invalid LoongArch branch condition!");
  Cond[0].setImm(getOppositeBranchOpc(Cond[0].getImm()));
  return false;
}

LoongArchInstrInfo::BranchType LoongArchInstrInfo::analyzeBranch(
    MachineBasicBlock &MBB, MachineBasicBlock *&TBB, MachineBasicBlock *&FBB,
    SmallVectorImpl<MachineOperand> &Cond, bool AllowModify,
    SmallVectorImpl<MachineInstr *> &BranchInstrs) const {
  MachineBasicBlock::reverse_iterator I = MBB.rbegin(), REnd = MBB.rend();

  // Skip all the debug instructions.
  while (I != REnd && I->isDebugInstr())
    ++I;

  if (I == REnd || !isUnpredicatedTerminator(*I)) {
    // This block ends with no branches (it just falls through to its succ).
    // Leave TBB/FBB null.
    TBB = FBB = nullptr;
    return BT_NoBranch;
  }

  MachineInstr *LastInst = &*I;
  unsigned LastOpc = LastInst->getOpcode();
  BranchInstrs.push_back(LastInst);

  // Not an analyzable branch (e.g., indirect jump).
  if (!getAnalyzableBrOpc(LastOpc))
    return LastInst->isIndirectBranch() ? BT_Indirect : BT_None;

  // Get the second to last instruction in the block.
  unsigned SecondLastOpc = 0;
  MachineInstr *SecondLastInst = nullptr;

  // Skip past any debug instruction to see if the second last actual
  // is a branch.
  ++I;
  while (I != REnd && I->isDebugInstr())
    ++I;

  if (I != REnd) {
    SecondLastInst = &*I;
    SecondLastOpc = getAnalyzableBrOpc(SecondLastInst->getOpcode());

    // Not an analyzable branch (must be an indirect jump).
    if (isUnpredicatedTerminator(*SecondLastInst) && !SecondLastOpc)
      return BT_None;
  }

  // If there is only one terminator instruction, process it.
  if (!SecondLastOpc) {
    // Unconditional branch.
    if (LastInst->isUnconditionalBranch()) {
      TBB = LastInst->getOperand(0).getMBB();
      return BT_Uncond;
    }

    // Conditional branch
    AnalyzeCondBr(LastInst, LastOpc, TBB, Cond);
    return BT_Cond;
  }

  // If we reached here, there are two branches.
  // If there are three terminators, we don't know what sort of block this is.
  if (++I != REnd && isUnpredicatedTerminator(*I))
    return BT_None;

  BranchInstrs.insert(BranchInstrs.begin(), SecondLastInst);

  // If second to last instruction is an unconditional branch,
  // analyze it and remove the last instruction.
  if (SecondLastInst->isUnconditionalBranch()) {
    // Return if the last instruction cannot be removed.
    if (!AllowModify)
      return BT_None;

    TBB = SecondLastInst->getOperand(0).getMBB();
    LastInst->eraseFromParent();
    BranchInstrs.pop_back();
    return BT_Uncond;
  }

  // Conditional branch followed by an unconditional branch.
  // The last one must be unconditional.
  if (!LastInst->isUnconditionalBranch())
    return BT_None;

  AnalyzeCondBr(SecondLastInst, SecondLastOpc, TBB, Cond);
  FBB = LastInst->getOperand(0).getMBB();

  return BT_CondUncond;
}

MachineBasicBlock *
LoongArchInstrInfo::getBranchDestBlock(const MachineInstr &MI) const {
  assert(MI.getDesc().isBranch() && "Unexpected opcode!");
  // The branch target is always the last operand.
  int NumOp = MI.getNumExplicitOperands();
  return MI.getOperand(NumOp - 1).getMBB();
}

bool LoongArchInstrInfo::isBranchOffsetInRange(unsigned BranchOpc, int64_t BrOffset) const {
/*
      	switch (BranchOpc) {
  case LoongArch::B:
  case LoongArch::BAL:
  case LoongArch::BAL_BR:
  case LoongArch::BC1F:
  case LoongArch::BC1FL:
  case LoongArch::BC1T:
  case LoongArch::BC1TL:
  case LoongArch::BEQ:     case LoongArch::BEQ64:
  case LoongArch::BEQL:
  case LoongArch::BGEZ:    case LoongArch::BGEZ64:
  case LoongArch::BGEZL:
  case LoongArch::BGEZAL:
  case LoongArch::BGEZALL:
  case LoongArch::BGTZ:    case LoongArch::BGTZ64:
  case LoongArch::BGTZL:
  case LoongArch::BLEZ:    case LoongArch::BLEZ64:
  case LoongArch::BLEZL:
  case LoongArch::BLTZ:    case LoongArch::BLTZ64:
  case LoongArch::BLTZL:
  case LoongArch::BLTZAL:
  case LoongArch::BLTZALL:
  case LoongArch::BNE:     case LoongArch::BNE64:
  case LoongArch::BNEL:
    return isInt<18>(BrOffset);

  case LoongArch::BC1EQZ:
  case LoongArch::BC1NEZ:
  case LoongArch::BC2EQZ:
  case LoongArch::BC2NEZ:
  case LoongArch::BEQC:   case LoongArch::BEQC64:
  case LoongArch::BNEC:   case LoongArch::BNEC64:
  case LoongArch::BGEC:   case LoongArch::BGEC64:
  case LoongArch::BGEUC:  case LoongArch::BGEUC64:
  case LoongArch::BGEZC:  case LoongArch::BGEZC64:
  case LoongArch::BGTZC:  case LoongArch::BGTZC64:
  case LoongArch::BLEZC:  case LoongArch::BLEZC64:
  case LoongArch::BLTC:   case LoongArch::BLTC64:
  case LoongArch::BLTUC:  case LoongArch::BLTUC64:
  case LoongArch::BLTZC:  case LoongArch::BLTZC64:
  case LoongArch::BNVC:
  case LoongArch::BOVC:
  case LoongArch::BGEZALC:
  case LoongArch::BEQZALC:
  case LoongArch::BGTZALC:
  case LoongArch::BLEZALC:
  case LoongArch::BLTZALC:
  case LoongArch::BNEZALC:
    return isInt<18>(BrOffset);

  case LoongArch::BEQZC:  case LoongArch::BEQZC64:
  case LoongArch::BNEZC:  case LoongArch::BNEZC64:
    return isInt<23>(BrOffset);
  }
    */
  switch (BranchOpc) {
  case LoongArch::B: case LoongArch::B32:
    return isInt<28>(BrOffset);

  case LoongArch::BEQZ: case LoongArch::BEQZ32:
  case LoongArch::BNEZ: case LoongArch::BNEZ32:
  case LoongArch::BCEQZ:
  case LoongArch::BCNEZ:
    return isInt<23>(BrOffset);

  case LoongArch::BEQ: case LoongArch::BEQ32:
  case LoongArch::BNE: case LoongArch::BNE32:
  case LoongArch::BLT: case LoongArch::BLT32:
  case LoongArch::BGE: case LoongArch::BGE32:
  case LoongArch::BLTU: case LoongArch::BLTU32:
  case LoongArch::BGEU: case LoongArch::BGEU32:
    return isInt<18>(BrOffset);
  }

  llvm_unreachable("Unknown branch instruction!");
}


/// Predicate for distingushing between control transfer instructions and all
/// other instructions for handling forbidden slots. Consider inline assembly
/// as unsafe as well.
bool LoongArchInstrInfo::SafeInForbiddenSlot(const MachineInstr &MI) const {
  if (MI.isInlineAsm())
    return false;

  return (MI.getDesc().TSFlags & LoongArchII::IsCTI) == 0;
}

/// Predicate for distingushing instructions that have forbidden slots.
bool LoongArchInstrInfo::HasForbiddenSlot(const MachineInstr &MI) const {
  return (MI.getDesc().TSFlags & LoongArchII::HasForbiddenSlot) != 0;
}

/// Return the number of bytes of code the specified instruction may be.
unsigned LoongArchInstrInfo::getInstSizeInBytes(const MachineInstr &MI) const {
  switch (MI.getOpcode()) {
  default:
    return MI.getDesc().getSize();
  case  TargetOpcode::INLINEASM: {       // Inline Asm: Variable size.
    const MachineFunction *MF = MI.getParent()->getParent();
    const char *AsmStr = MI.getOperand(0).getSymbolName();
    return getInlineAsmLength(AsmStr, *MF->getTarget().getMCAsmInfo());
  }
  }
}

MachineInstrBuilder
LoongArchInstrInfo::genInstrWithNewOpc(unsigned NewOpc,
                                  MachineBasicBlock::iterator I) const {
  MachineInstrBuilder MIB;

  int ZeroOperandPosition = -1;
  bool BranchWithZeroOperand = false;
  if (I->isBranch() && !I->isPseudo()) {
    auto TRI = I->getParent()->getParent()->getSubtarget().getRegisterInfo();
    ZeroOperandPosition = I->findRegisterUseOperandIdx(LoongArch::ZERO, false, TRI);
    BranchWithZeroOperand = ZeroOperandPosition != -1;
  }

  MIB = BuildMI(*I->getParent(), I, I->getDebugLoc(), get(NewOpc));

  if (NewOpc == LoongArch::JIRL) {
    MIB->RemoveOperand(0);
    for (unsigned J = 0, E = I->getDesc().getNumOperands(); J < E; ++J) {
      MIB.add(I->getOperand(J));
    }
    MIB.addImm(0);
  } else {
    for (unsigned J = 0, E = I->getDesc().getNumOperands(); J < E; ++J) {
      if (BranchWithZeroOperand && (unsigned)ZeroOperandPosition == J)
        continue;

      MIB.add(I->getOperand(J));
    }
  }

  MIB.copyImplicitOps(*I);
  MIB.cloneMemRefs(*I);
  return MIB;
}

bool LoongArchInstrInfo::findCommutedOpIndices(const MachineInstr &MI,
                                               unsigned &SrcOpIdx1,
                                               unsigned &SrcOpIdx2) const {
  assert(!MI.isBundle() &&
         "TargetInstrInfo::findCommutedOpIndices() can't handle bundles");

  const MCInstrDesc &MCID = MI.getDesc();
  if (!MCID.isCommutable())
    return false;

  return TargetInstrInfo::findCommutedOpIndices(MI, SrcOpIdx1, SrcOpIdx2);
}

// bstrins, bstrpick have the following constraints:
// 0 <= lsb <= msb <= High
static bool verifyBstrInstruction(const MachineInstr &MI, StringRef &ErrInfo,
                                  const int64_t High) {
  MachineOperand MOMsb = MI.getOperand(2);
  if (!MOMsb.isImm()) {
    ErrInfo = "Msb operand is not an immediate!";
    return false;
  }
  MachineOperand MOLsb = MI.getOperand(3);
  if (!MOLsb.isImm()) {
    ErrInfo = "Lsb operand is not an immediate!";
    return false;
  }

  int64_t Lsb = MOLsb.getImm();
  if (!((0 <= Lsb) && (Lsb <= High))) {
    ErrInfo = "Lsb operand is out of range!";
    return false;
  }

  int64_t Msb = MOMsb.getImm();
  if (!((0 <= Msb) && (Msb <= High))) {
    ErrInfo = "Msb operand is out of range!";
    return false;
  }

  if (!(Lsb <= Msb)) {
    ErrInfo = "Lsb operand is not less than or equal to msb operand!";
    return false;
  }

  return true;
}

//  Perform target specific instruction verification.
bool LoongArchInstrInfo::verifyInstruction(const MachineInstr &MI,
                                      StringRef &ErrInfo) const {
  // Verify that bstrins and bstrpick instructions are well formed.
   switch (MI.getOpcode()) {
    case LoongArch::BSTRINS_W:
    case LoongArch::BSTRPICK_W:
      return verifyBstrInstruction(MI, ErrInfo, 31);
    case LoongArch::BSTRINS_D:
    case LoongArch::BSTRPICK_D:
      return verifyBstrInstruction(MI, ErrInfo, 63);
    default:
      return true;
  }

  return true;
}

std::pair<unsigned, unsigned>
LoongArchInstrInfo::decomposeMachineOperandsTargetFlags(unsigned TF) const {
  return std::make_pair(TF, 0u);
}

ArrayRef<std::pair<unsigned, const char*>>
LoongArchInstrInfo::getSerializableDirectMachineOperandTargetFlags() const {
 using namespace LoongArchII;

 static const std::pair<unsigned, const char*> Flags[] = {
    {MO_PCREL_HI,        "larch-pcrel-hi"},
    {MO_PCREL_LO,        "larch-pcrel-lo"},
    {MO_TLSGD_HI,        "larch-tlsgd-hi"},
    {MO_TLSGD_LO,        "larch-tlsgd-lo"},
    {MO_TLSIE_HI,        "larch-tlsie-hi"},
    {MO_TLSIE_LO,        "larch-tlsie-lo"},
    {MO_TLSLE_HI,        "larch-tlsle-hi"},
    {MO_TLSLE_LO,        "larch-tlsle-lo"},
    {MO_ABS_HI,          "larch-abs-hi"},
    {MO_ABS_LO,          "larch-abs-lo"},
    {MO_ABS_HIGHER,      "larch-abs-higher"},
    {MO_ABS_HIGHEST,     "larch-abs-highest"},
    {MO_GOT_HI,          "larch-got-hi"},
    {MO_GOT_LO,          "larch-got-lo"},
    {MO_CALL_HI,         "larch-call-hi"},
    {MO_CALL_LO,         "larch-call-lo"}
  };
  return makeArrayRef(Flags);
}
