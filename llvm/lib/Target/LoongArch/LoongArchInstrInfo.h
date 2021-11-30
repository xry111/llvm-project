//===- LoongArchInstrInfo.h - LoongArch Instruction Information -----------*- C++ -*-===//
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
// FIXME: We need to override TargetInstrInfo::getInlineAsmLength method in
// order for LoongArchLongBranch pass to work correctly when the code has inline
// assembly.  The returned value doesn't have to be the asm instruction's exact
// size in bytes; LoongArchLongBranch only expects it to be the correct upper bound.
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_LOONGARCHINSTRINFO_H
#define LLVM_LIB_TARGET_LOONGARCH_LOONGARCHINSTRINFO_H

#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "LoongArch.h"
#include "LoongArchRegisterInfo.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include <cstdint>

#define GET_INSTRINFO_HEADER
#include "LoongArchGenInstrInfo.inc"

namespace llvm {

class MachineInstr;
class MachineOperand;
class LoongArchSubtarget;
class TargetRegisterClass;
class TargetRegisterInfo;

class LoongArchInstrInfo : public LoongArchGenInstrInfo {
  virtual void anchor();
  const LoongArchRegisterInfo RI;
  const LoongArchSubtarget &Subtarget;

public:
  enum BranchType {
    BT_None,       // Couldn't analyze branch.
    BT_NoBranch,   // No branches found.
    BT_Uncond,     // One unconditional branch.
    BT_Cond,       // One conditional branch.
    BT_CondUncond, // A conditional branch followed by an unconditional branch.
    BT_Indirect    // One indirct branch.
  };

  explicit LoongArchInstrInfo(const LoongArchSubtarget &STI);

  /// isLoadFromStackSlot - If the specified machine instruction is a direct
  /// load from a stack slot, return the virtual or physical register number of
  /// the destination along with the FrameIndex of the loaded stack slot.  If
  /// not, return 0.  This predicate must return 0 if the instruction has
  /// any side effects other than loading from the stack slot.
  unsigned isLoadFromStackSlot(const MachineInstr &MI,
                               int &FrameIndex) const override;

  /// isStoreToStackSlot - If the specified machine instruction is a direct
  /// store to a stack slot, return the virtual or physical register number of
  /// the source reg along with the FrameIndex of the loaded stack slot.  If
  /// not, return 0.  This predicate must return 0 if the instruction has
  /// any side effects other than storing to the stack slot.
  unsigned isStoreToStackSlot(const MachineInstr &MI,
                              int &FrameIndex) const override;

  void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                   const DebugLoc &DL, MCRegister DestReg, MCRegister SrcReg,
                   bool KillSrc) const override;

  /// Branch Analysis
  bool analyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                     MachineBasicBlock *&FBB,
                     SmallVectorImpl<MachineOperand> &Cond,
                     bool AllowModify) const override;

  unsigned removeBranch(MachineBasicBlock &MBB,
                        int *BytesRemoved = nullptr) const override;

  unsigned insertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                        MachineBasicBlock *FBB, ArrayRef<MachineOperand> Cond,
                        const DebugLoc &DL,
                        int *BytesAdded = nullptr) const override;

  unsigned insertIndirectBranch(MachineBasicBlock &MBB,
                                MachineBasicBlock &NewDestBB,
                                const DebugLoc &DL, int64_t BrOffset,
                                RegScavenger *RS = nullptr) const override;
  bool
  reverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const override;

  BranchType analyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                           MachineBasicBlock *&FBB,
                           SmallVectorImpl<MachineOperand> &Cond,
                           bool AllowModify,
                           SmallVectorImpl<MachineInstr *> &BranchInstrs) const;

  /// Get the block that branch instruction jumps to.
  MachineBasicBlock *getBranchDestBlock(const MachineInstr &MI) const override;

  /// Determine if the branch target is in range.
  bool isBranchOffsetInRange(unsigned BranchOpc,
                             int64_t BrOffset) const override;

  /// Predicate to determine if an instruction can go in a forbidden slot.
  bool SafeInForbiddenSlot(const MachineInstr &MI) const;

  /// Predicate to determine if an instruction has a forbidden slot.
  bool HasForbiddenSlot(const MachineInstr &MI) const;

  /// Insert nop instruction when hazard condition is found
  void insertNoop(MachineBasicBlock &MBB,
                  MachineBasicBlock::iterator MI) const override;

  /// getRegisterInfo - TargetInstrInfo is a superset of MRegister info.  As
  /// such, whenever a client has an instance of instruction info, it should
  /// always be able to get register info as well (through this method).
  const LoongArchRegisterInfo &getRegisterInfo() const;

  bool expandPostRAPseudo(MachineInstr &MI) const override;

  unsigned getOppositeBranchOpc(unsigned Opc) const;

  /// Emit a series of instructions to load an immediate. If NewImm is a
  /// non-NULL parameter, the last instruction is not emitted, but instead
  /// its immediate operand is returned in NewImm.
  unsigned loadImmediate(int64_t Imm, MachineBasicBlock &MBB,
                         MachineBasicBlock::iterator II, const DebugLoc &DL,
                         unsigned *NewImm) const;

  /// Return the number of bytes of code the specified instruction may be.
  unsigned getInstSizeInBytes(const MachineInstr &MI) const override;

  void storeRegToStackSlot(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator MBBI,
                           Register SrcReg, bool isKill, int FrameIndex,
                           const TargetRegisterClass *RC,
                           const TargetRegisterInfo *TRI) const override {
    storeRegToStack(MBB, MBBI, SrcReg, isKill, FrameIndex, RC, TRI, 0);
  }

  void loadRegFromStackSlot(MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MBBI,
                            Register DestReg, int FrameIndex,
                            const TargetRegisterClass *RC,
                            const TargetRegisterInfo *TRI) const override {
    loadRegFromStack(MBB, MBBI, DestReg, FrameIndex, RC, TRI, 0);
  }

  void storeRegToStack(MachineBasicBlock &MBB,
                       MachineBasicBlock::iterator MI,
                       Register SrcReg, bool isKill, int FrameIndex,
                       const TargetRegisterClass *RC,
                       const TargetRegisterInfo *TRI,
                       int64_t Offset) const;

  void loadRegFromStack(MachineBasicBlock &MBB,
                        MachineBasicBlock::iterator MI,
                        Register DestReg, int FrameIndex,
                        const TargetRegisterClass *RC,
                        const TargetRegisterInfo *TRI,
                        int64_t Offset) const;

  /// Adjust SP by Amount bytes.
  void adjustStackPtr(unsigned SP, int64_t Amount,
                      MachineBasicBlock &MBB,
                      MachineBasicBlock::iterator I) const;

  /// Create an instruction which has the same operands and memory operands
  /// as MI but has a new opcode.
  MachineInstrBuilder genInstrWithNewOpc(unsigned NewOpc,
                                         MachineBasicBlock::iterator I) const;

  bool findCommutedOpIndices(const MachineInstr &MI, unsigned &SrcOpIdx1,
                             unsigned &SrcOpIdx2) const override;

  /// Perform target specific instruction verification.
  bool verifyInstruction(const MachineInstr &MI,
                         StringRef &ErrInfo) const override;

  std::pair<unsigned, unsigned>
  decomposeMachineOperandsTargetFlags(unsigned TF) const override;

  ArrayRef<std::pair<unsigned, const char *>>
  getSerializableDirectMachineOperandTargetFlags() const override;

protected:
  /// If the specific machine instruction is a instruction that moves/copies
  /// value from one register to another register return true along with
  /// @Source machine operand and @Destination machine operand.
  Optional<DestSourcePair>
  isCopyInstrImpl(const MachineInstr &MI) const override;

private:

  bool isZeroImm(const MachineOperand &op) const;

  MachineMemOperand *GetMemOperand(MachineBasicBlock &MBB, int FI,
                                   MachineMemOperand::Flags Flags) const;

  unsigned getAnalyzableBrOpc(unsigned Opc) const;

  void AnalyzeCondBr(const MachineInstr *Inst, unsigned Opc,
                     MachineBasicBlock *&BB,
                     SmallVectorImpl<MachineOperand> &Cond) const;

  MachineInstr *
  BuildCondBr(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
              const DebugLoc &DL, ArrayRef<MachineOperand> Cond) const;

  void expandRetRA(MachineBasicBlock &MBB, MachineBasicBlock::iterator I) const;

  void expandERet(MachineBasicBlock &MBB, MachineBasicBlock::iterator I) const;

  std::pair<bool, bool> compareOpndSize(unsigned Opc,
                                        const MachineFunction &MF) const;

  /// Expand pseudo Int-to-FP conversion instructions.
  ///
  /// For example, the following pseudo instruction
  ///  PseudoCVT_D32_W D2, A5
  /// gets expanded into these two instructions:
  ///  MTC1 F4, A5
  ///  CVT_D32_W D2, F4
  ///
  /// We do this expansion post-RA to avoid inserting a floating point copy
  /// instruction between MTC1 and CVT_D32_W.
  void expandCvtFPInt(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                      unsigned CvtOpc, unsigned MovOpc, bool IsI64) const;

  void expandEhReturn(MachineBasicBlock &MBB,
                      MachineBasicBlock::iterator I) const;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_LOONGARCH_LOONGARCHINSTRINFO_H
