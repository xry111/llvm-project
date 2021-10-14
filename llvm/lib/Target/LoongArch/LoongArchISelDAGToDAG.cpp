//=== LoongArchISelDAGToDAG.cpp - A dag to dag inst selector for LoongArch ===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines an instruction selector for the LoongArch target.
//
//===----------------------------------------------------------------------===//

#include "LoongArchISelDAGToDAG.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "MCTargetDesc/LoongArchMatInt.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/IR/IntrinsicsLoongArch.h"
#include "llvm/Support/Alignment.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "loongarch-isel"

void LoongArchDAGToDAGISel::PostprocessISelDAG() { doPeepholeLoadStoreADDI(); }

static SDNode *selectImm(SelectionDAG *CurDAG, const SDLoc &DL, int64_t Imm,
                         MVT XLenVT) {
  LoongArchMatInt::InstSeq Seq;
  LoongArchMatInt::generateInstSeq(Imm, XLenVT == MVT::i64, Seq);

  SDNode *Result = nullptr;
  SDValue SrcReg = CurDAG->getRegister(LoongArch::R0, XLenVT);
  for (LoongArchMatInt::Inst &Inst : Seq) {
    SDValue SDImm = CurDAG->getTargetConstant(Inst.Imm, DL, XLenVT);
    if (Inst.Opc == LoongArch::LU12I_W)
      Result = CurDAG->getMachineNode(Inst.Opc, DL, XLenVT, SDImm);
    else
      Result = CurDAG->getMachineNode(Inst.Opc, DL, XLenVT, SrcReg, SDImm);

    // Only the first instruction has R0 as its source.
    SrcReg = SDValue(Result, 0);
  }

  return Result;
}

void LoongArchDAGToDAGISel::Select(SDNode *Node) {
  // If we have a custom node, we have already selected.
  if (Node->isMachineOpcode()) {
    LLVM_DEBUG(dbgs() << "== "; Node->dump(CurDAG); dbgs() << "\n");
    Node->setNodeId(-1);
    return;
  }

  // Instruction Selection not handled by the auto-generated tablegen selection
  // should be handled here.
  unsigned Opcode = Node->getOpcode();
  MVT XLenVT = Subtarget->getXLenVT();
  SDLoc DL(Node);
  EVT VT = Node->getValueType(0);

  switch (Opcode) {
  case ISD::ADD: {
    // Optimize (add r, imm) to (addi (addi r, imm0) imm1) if applicable. The
    // immediate must be in specific ranges and have a single use.
    if (auto *ConstOp = dyn_cast<ConstantSDNode>(Node->getOperand(1))) {
      if (!(ConstOp->hasOneUse()))
        break;
      // The imm must be in range [-4096,-2049] or [2048,4094].
      int64_t Imm = ConstOp->getSExtValue();
      if (!(-4096 <= Imm && Imm <= -2049) && !(2048 <= Imm && Imm <= 4094))
        break;
      // Break the imm to imm0+imm1.
      EVT VT = Node->getValueType(0);
      const SDValue ImmOp0 = CurDAG->getTargetConstant(Imm - Imm / 2, DL, VT);
      const SDValue ImmOp1 = CurDAG->getTargetConstant(Imm / 2, DL, VT);
      auto *NodeAddi0 = CurDAG->getMachineNode(Subtarget->getADDI(), DL, VT,
                                               Node->getOperand(0), ImmOp0);
      auto *NodeAddi1 = CurDAG->getMachineNode(Subtarget->getADDI(), DL, VT,
                                               SDValue(NodeAddi0, 0), ImmOp1);
      ReplaceNode(Node, NodeAddi1);
      return;
    }
    break;
  }
  case ISD::Constant: {
    auto ConstNode = cast<ConstantSDNode>(Node);
    if (VT == XLenVT && ConstNode->isNullValue()) {
      SDValue New = CurDAG->getCopyFromReg(CurDAG->getEntryNode(), DL,
                                           LoongArch::R0, XLenVT);
      ReplaceNode(Node, New.getNode());
      return;
    }
    int64_t Imm = ConstNode->getSExtValue();
    if (XLenVT == MVT::i64) {
      ReplaceNode(Node, selectImm(CurDAG, DL, Imm, XLenVT));
      return;
    }
    break;
  }
  case ISD::FrameIndex: {
    SDValue Imm = CurDAG->getTargetConstant(0, DL, XLenVT);
    int FI = cast<FrameIndexSDNode>(Node)->getIndex();
    SDValue TFI = CurDAG->getTargetFrameIndex(FI, VT);
    ReplaceNode(Node,
                CurDAG->getMachineNode(Subtarget->getADDI(), DL, VT, TFI, Imm));
    return;
  }
  case ISD::INTRINSIC_W_CHAIN: {
    unsigned IntNo = cast<ConstantSDNode>(Node->getOperand(1))->getZExtValue();
    switch (IntNo) {
      // By default we do not custom select any intrinsic.
    default:
      break;
    }
    break;
  }
  case ISD::INTRINSIC_VOID: {
    break;
  }
  }

  // Select the default instruction.
  SelectCode(Node);
}

bool LoongArchDAGToDAGISel::SelectInlineAsmMemoryOperand(
    const SDValue &Op, unsigned ConstraintID, std::vector<SDValue> &OutOps) {
  switch (ConstraintID) {
  case InlineAsm::Constraint_m:
    // We just support simple memory operands that have a single address
    // operand and need no special handling.
    OutOps.push_back(Op);
    return false;
  case InlineAsm::Constraint_A:
    OutOps.push_back(Op);
    return false;
  default:
    break;
  }

  return true;
}

bool LoongArchDAGToDAGISel::SelectAddrFI(SDValue Addr, SDValue &Base) {
  if (auto FIN = dyn_cast<FrameIndexSDNode>(Addr)) {
    Base = CurDAG->getTargetFrameIndex(FIN->getIndex(), Subtarget->getXLenVT());
    return true;
  }
  return false;
}

// Match (srl (and val, mask), imm) where the result would be a
// zero-extended 32-bit integer. i.e. the mask is 0xffffffff or the result
// is equivalent to this (SimplifyDemandedBits may have removed lower bits
// from the mask that aren't necessary due to the right-shifting).
bool LoongArchDAGToDAGISel::MatchSRLIW(SDNode *N) const {
  assert(N->getOpcode() == ISD::SRL);
  assert(N->getOperand(0).getOpcode() == ISD::AND);
  assert(isa<ConstantSDNode>(N->getOperand(1)));
  assert(isa<ConstantSDNode>(N->getOperand(0).getOperand(1)));

  // The IsLA64 predicate is checked after PatFrag predicates so we can get
  // here even on LA32.
  if (!Subtarget->is64Bit())
    return false;

  SDValue And = N->getOperand(0);
  uint64_t ShAmt = N->getConstantOperandVal(1);
  uint64_t Mask = And.getConstantOperandVal(1);
  return (Mask | maskTrailingOnes<uint64_t>(ShAmt)) == 0xffffffff;
}

// Check that it is a SLLIUW (Shift Logical Left Immediate Unsigned i32
// on LA64).
// SLLIUW is the same as SLLI except for the fact that it clears the bits
// XLEN-1:32 of the input RS1 before shifting.
// A PatFrag has already checked that it has the right structure:
//
//  (AND (SHL RS1, VC2), VC1)
//
// We check that VC2, the shamt is less than 32, otherwise the pattern is
// exactly the same as SLLI and we give priority to that.
// Eventually we check that VC1, the mask used to clear the upper 32 bits
// of RS1, is correct:
//
//  VC1 == (0xFFFFFFFF << VC2)
//
bool LoongArchDAGToDAGISel::MatchSLLIUW(SDNode *N) const {
  assert(N->getOpcode() == ISD::AND);
  assert(N->getOperand(0).getOpcode() == ISD::SHL);
  assert(isa<ConstantSDNode>(N->getOperand(1)));
  assert(isa<ConstantSDNode>(N->getOperand(0).getOperand(1)));

  // The IsLA64 predicate is checked after PatFrag predicates so we can get
  // here even on LA32.
  if (!Subtarget->is64Bit())
    return false;

  SDValue Shl = N->getOperand(0);
  uint64_t VC1 = N->getConstantOperandVal(1);
  uint64_t VC2 = Shl.getConstantOperandVal(1);

  // Immediate range should be enforced by uimm5 predicate.
  assert(VC2 < 32 && "Unexpected immediate");
  return (VC1 >> VC2) == UINT64_C(0xFFFFFFFF);
}

// X0 has special meaning for vsetvl/vsetvli.
//  rd | rs1 |   AVL value | Effect on vl
//--------------------------------------------------------------
// !X0 |  X0 |       VLMAX | Set vl to VLMAX
//  X0 |  X0 | Value in vl | Keep current vl, just change vtype.
bool LoongArchDAGToDAGISel::selectVLOp(SDValue N, SDValue &VL) {
  // If the VL value is a constant 0, manually select it to an ADDI with 0
  // immediate to prevent the default selection path from matching it to X0.
  auto *C = dyn_cast<ConstantSDNode>(N);
  if (C && C->isNullValue())
    VL = SDValue(selectImm(CurDAG, SDLoc(N), 0, Subtarget->getXLenVT()), 0);
  else
    VL = N;

  return true;
}

// Merge an ADDI into the offset of a load/store instruction where possible.
// (load (addi base, off1), off2) -> (load base, off1+off2)
// (store val, (addi base, off1), off2) -> (store val, base, off1+off2)
// This is possible when off1+off2 fits a 12-bit immediate.
void LoongArchDAGToDAGISel::doPeepholeLoadStoreADDI() {
  SelectionDAG::allnodes_iterator Position(CurDAG->getRoot().getNode());
  ++Position;

  while (Position != CurDAG->allnodes_begin()) {
    SDNode *N = &*--Position;
    // Skip dead nodes and any non-machine opcodes.
    if (N->use_empty() || !N->isMachineOpcode())
      continue;

    int OffsetOpIdx;
    int BaseOpIdx;

    // Only attempt this optimisation for I-type loads and S-type stores.
    switch (N->getMachineOpcode()) {
    default:
      continue;
    case LoongArch::LD_B:
    case LoongArch::LD_H:
    case LoongArch::LD_W:
    case LoongArch::LD_BU:
    case LoongArch::LD_HU:
    case LoongArch::LD_WU:
    case LoongArch::LD_D:
    case LoongArch::FLD_S:
    case LoongArch::FLD_D:
      BaseOpIdx = 0;
      OffsetOpIdx = 1;
      break;
    case LoongArch::ST_B:
    case LoongArch::ST_H:
    case LoongArch::ST_W:
    case LoongArch::ST_D:
    case LoongArch::FST_S:
    case LoongArch::FST_D:
      BaseOpIdx = 1;
      OffsetOpIdx = 2;
      break;
    }

    if (!isa<ConstantSDNode>(N->getOperand(OffsetOpIdx)))
      continue;

    SDValue Base = N->getOperand(BaseOpIdx);

    // If the base is an ADDI, we can merge it in to the load/store.
    if (!Base.isMachineOpcode() ||
        (Base.getMachineOpcode() != LoongArch::ADDI_W &&
         Base.getMachineOpcode() != LoongArch::ADDI_D))
      continue;

    SDValue ImmOperand = Base.getOperand(1);
    uint64_t Offset2 = N->getConstantOperandVal(OffsetOpIdx);

    if (auto Const = dyn_cast<ConstantSDNode>(ImmOperand)) {
      int64_t Offset1 = Const->getSExtValue();
      int64_t CombinedOffset = Offset1 + Offset2;
      if (!isInt<12>(CombinedOffset))
        continue;
      ImmOperand = CurDAG->getTargetConstant(CombinedOffset, SDLoc(ImmOperand),
                                             ImmOperand.getValueType());
    } else if (auto GA = dyn_cast<GlobalAddressSDNode>(ImmOperand)) {
      // If the off1 in (addi base, off1) is a global variable's address (its
      // low part, really), then we can rely on the alignment of that variable
      // to provide a margin of safety before off1 can overflow the 12 bits.
      // Check if off2 falls within that margin; if so off1+off2 can't overflow.
      const DataLayout &DL = CurDAG->getDataLayout();
      Align Alignment = GA->getGlobal()->getPointerAlignment(DL);
      if (Offset2 != 0 && Alignment <= Offset2)
        continue;
      int64_t Offset1 = GA->getOffset();
      int64_t CombinedOffset = Offset1 + Offset2;
      ImmOperand = CurDAG->getTargetGlobalAddress(
          GA->getGlobal(), SDLoc(ImmOperand), ImmOperand.getValueType(),
          CombinedOffset, GA->getTargetFlags());
    } else if (auto CP = dyn_cast<ConstantPoolSDNode>(ImmOperand)) {
      // Ditto.
      Align Alignment = CP->getAlign();
      if (Offset2 != 0 && Alignment <= Offset2)
        continue;
      int64_t Offset1 = CP->getOffset();
      int64_t CombinedOffset = Offset1 + Offset2;
      ImmOperand = CurDAG->getTargetConstantPool(
          CP->getConstVal(), ImmOperand.getValueType(), CP->getAlign(),
          CombinedOffset, CP->getTargetFlags());
    } else {
      continue;
    }

    LLVM_DEBUG(dbgs() << "Folding add-immediate into mem-op:\nBase:    ");
    LLVM_DEBUG(Base->dump(CurDAG));
    LLVM_DEBUG(dbgs() << "\nN: ");
    LLVM_DEBUG(N->dump(CurDAG));
    LLVM_DEBUG(dbgs() << "\n");

    // Modify the offset operand of the load/store.
    if (BaseOpIdx == 0) // Load
      CurDAG->UpdateNodeOperands(N, Base.getOperand(0), ImmOperand,
                                 N->getOperand(2));
    else // Store
      CurDAG->UpdateNodeOperands(N, N->getOperand(0), Base.getOperand(0),
                                 ImmOperand, N->getOperand(3));

    // The add-immediate may now be dead, in which case remove it.
    if (Base.getNode()->use_empty())
      CurDAG->RemoveDeadNode(Base.getNode());
  }
}

// This pass converts a legalized DAG into a LoongArch-specific DAG, ready
// for instruction scheduling.
FunctionPass *llvm::createLoongArchISelDag(LoongArchTargetMachine &TM) {
  return new LoongArchDAGToDAGISel(TM);
}
