//===- LoongArchISelLowering.h - LoongArch DAG Lowering Interface ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that LoongArch uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_LOONGARCHISELLOWERING_H
#define LLVM_LIB_TARGET_LOONGARCH_LOONGARCHISELLOWERING_H

#include "MCTargetDesc/LoongArchABIInfo.h"
#include "MCTargetDesc/LoongArchBaseInfo.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "LoongArch.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/ISDOpcodes.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/MachineValueType.h"
#include "llvm/Target/TargetMachine.h"
#include <algorithm>
#include <cassert>
#include <deque>
#include <string>
#include <utility>
#include <vector>

namespace llvm {

class Argument;
class CCState;
class CCValAssign;
class FastISel;
class FunctionLoweringInfo;
class MachineBasicBlock;
class MachineFrameInfo;
class MachineInstr;
class LoongArchCCState;
class LoongArchFunctionInfo;
class LoongArchSubtarget;
class LoongArchTargetMachine;
class SelectionDAG;
class TargetLibraryInfo;
class TargetRegisterClass;

  namespace LoongArchISD {

    enum NodeType : unsigned {
      // Start the numbering from where ISD NodeType finishes.
      FIRST_NUMBER = ISD::BUILTIN_OP_END,

      // Jump and link (call)
      JmpLink,

      // Tail call
      TailCall,

      // global address
      GlobalAddress,

      // Floating Point Branch Conditional
      FPBrcond,

      // Floating Point Compare
      FPCmp,

      // Floating Point Conditional Moves
      CMovFP_T,
      CMovFP_F,

      // FP-to-int truncation node.
      TruncIntFP,

      // Return
      Ret,

      // error trap Return
      ERet,

      // Software Exception Return.
      EH_RETURN,

      DBAR,

      BSTRPICK,
      BSTRINS,
    };

  } // ene namespace LoongArchISD

  //===--------------------------------------------------------------------===//
  // TargetLowering Implementation
  //===--------------------------------------------------------------------===//

  class LoongArchTargetLowering : public TargetLowering  {
  public:
    explicit LoongArchTargetLowering(const LoongArchTargetMachine &TM,
                                const LoongArchSubtarget &STI);

    bool allowsMisalignedMemoryAccesses(
        EVT VT, unsigned AS, Align A,
        MachineMemOperand::Flags Flags,
        bool *Fast) const override;

    MVT getScalarShiftAmountTy(const DataLayout &, EVT) const override {
      return MVT::i32;
    }

    EVT getTypeForExtReturn(LLVMContext &Context, EVT VT,
                            ISD::NodeType) const override;

    bool isCheapToSpeculateCttz() const override;
    bool isCheapToSpeculateCtlz() const override;

    /// Return the correct alignment for the current calling convention.
    Align getABIAlignmentForCallingConv(Type *ArgTy,
                                        const DataLayout &DL) const override {
      const Align ABIAlign = DL.getABITypeAlign(ArgTy);
      if (ArgTy->isVectorTy())
        return std::min(ABIAlign, Align(8));
      return ABIAlign;
    }

    ISD::NodeType getExtendForAtomicOps() const override {
      return ISD::SIGN_EXTEND;
    }

    void LowerOperationWrapper(SDNode *N,
                               SmallVectorImpl<SDValue> &Results,
                               SelectionDAG &DAG) const override;

    /// LowerOperation - Provide custom lowering hooks for some operations.
    SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const override;

    bool isFMAFasterThanFMulAndFAdd(const MachineFunction &MF,
                                    EVT VT) const override;

    /// ReplaceNodeResults - Replace the results of node with an illegal result
    /// type with new values built out of custom code.
    ///
    void ReplaceNodeResults(SDNode *N, SmallVectorImpl<SDValue>&Results,
                            SelectionDAG &DAG) const override;

    /// getTargetNodeName - This method returns the name of a target specific
    //  DAG node.
    const char *getTargetNodeName(unsigned Opcode) const override;

    /// getSetCCResultType - get the ISD::SETCC result ValueType
    EVT getSetCCResultType(const DataLayout &DL, LLVMContext &Context,
                           EVT VT) const override;

    SDValue PerformDAGCombine(SDNode *N, DAGCombinerInfo &DCI) const override;

    MachineBasicBlock *
    EmitInstrWithCustomInserter(MachineInstr &MI,
                                MachineBasicBlock *MBB) const override;

    bool isShuffleMaskLegal(ArrayRef<int> Mask, EVT VT) const override {
      return false;
    }

    const TargetRegisterClass *getRepRegClassFor(MVT VT) const override;

    void AdjustInstrPostInstrSelection(MachineInstr &MI,
                                       SDNode *Node) const override;

    void HandleByVal(CCState *, unsigned &, Align) const override;

    /// If a physical register, this returns the register that receives the
    /// exception address on entry to an EH pad.
    Register
    getExceptionPointerRegister(const Constant *PersonalityFn) const override {
      return ABI.IsLP64D() ? LoongArch::A0_64 : LoongArch::A0;
    }

    /// If a physical register, this returns the register that receives the
    /// exception typeid on entry to a landing pad.
    Register
    getExceptionSelectorRegister(const Constant *PersonalityFn) const override {
      return ABI.IsLP64D() ? LoongArch::A1_64 : LoongArch::A1;
    }

    bool isJumpTableRelative() const override {
      return getTargetMachine().isPositionIndependent();
    }

   CCAssignFn *CCAssignFnForCall() const;

   CCAssignFn *CCAssignFnForReturn() const;

  private:
    template <class NodeTy>
    SDValue getAddr(NodeTy *N, SelectionDAG &DAG, bool IsLocal = true) const;

    /// This function fills Ops, which is the list of operands that will later
    /// be used when a function call node is created. It also generates
    /// copyToReg nodes to set up argument registers.
    void
    getOpndList(SmallVectorImpl<SDValue> &Ops,
                std::deque<std::pair<unsigned, SDValue>> &RegsToPass,
                bool IsPICCall, bool GlobalOrExternal, bool InternalLinkage,
                bool IsCallReloc, CallLoweringInfo &CLI, SDValue Callee,
                SDValue Chain) const;

    SDValue lowerLOAD(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerSTORE(SDValue Op, SelectionDAG &DAG) const;

    // Subtarget Info
    const LoongArchSubtarget &Subtarget;
    // Cache the ABI from the TargetMachine, we use it everywhere.
    const LoongArchABIInfo &ABI;

    // Create a TargetGlobalAddress node.
    SDValue getTargetNode(GlobalAddressSDNode *N, EVT Ty, SelectionDAG &DAG,
                          unsigned Flag) const;

    // Create a TargetExternalSymbol node.
    SDValue getTargetNode(ExternalSymbolSDNode *N, EVT Ty, SelectionDAG &DAG,
                          unsigned Flag) const;

    // Create a TargetBlockAddress node.
    SDValue getTargetNode(BlockAddressSDNode *N, EVT Ty, SelectionDAG &DAG,
                          unsigned Flag) const;

    // Create a TargetJumpTable node.
    SDValue getTargetNode(JumpTableSDNode *N, EVT Ty, SelectionDAG &DAG,
                          unsigned Flag) const;

    // Create a TargetConstantPool node.
    SDValue getTargetNode(ConstantPoolSDNode *N, EVT Ty, SelectionDAG &DAG,
                          unsigned Flag) const;

    // Lower Operand helpers
    SDValue LowerCallResult(SDValue Chain, SDValue InFlag,
                            CallingConv::ID CallConv, bool isVarArg,
                            const SmallVectorImpl<ISD::InputArg> &Ins,
                            const SDLoc &dl, SelectionDAG &DAG,
                            SmallVectorImpl<SDValue> &InVals,
                            TargetLowering::CallLoweringInfo &CLI) const;

    // Lower Operand specifics
    SDValue lowerINTRINSIC_WO_CHAIN(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerINTRINSIC_W_CHAIN(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerINTRINSIC_VOID(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerUINT_TO_FP(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerSINT_TO_FP(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerFP_TO_UINT(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerFP_TO_SINT(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerBRCOND(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerConstantPool(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerBlockAddress(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerGlobalTLSAddress(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerJumpTable(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerSELECT(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerSETCC(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerVASTART(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerVAARG(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerFCOPYSIGN(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerFABS(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerFRAMEADDR(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerRETURNADDR(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerEH_RETURN(SDValue Op, SelectionDAG &DAG) const;
    SDValue lowerATOMIC_FENCE(SDValue Op, SelectionDAG& DAG) const;
    SDValue lowerShiftLeftParts(SDValue Op, SelectionDAG& DAG) const;
    SDValue lowerShiftRightParts(SDValue Op, SelectionDAG& DAG,
                                 bool IsSRA) const;
    SDValue lowerEH_DWARF_CFA(SDValue Op, SelectionDAG &DAG) const;

    /// isEligibleForTailCallOptimization - Check whether the call is eligible
    /// for tail call optimization.
    bool
    isEligibleForTailCallOptimization(const CCState &CCInfo,
                                      unsigned NextStackOffset,
                                      const LoongArchFunctionInfo &FI) const;

    /// copyByValArg - Copy argument registers which were used to pass a byval
    /// argument to the stack. Create a stack frame object for the byval
    /// argument.
    void copyByValRegs(SDValue Chain, const SDLoc &DL,
                       std::vector<SDValue> &OutChains, SelectionDAG &DAG,
                       const ISD::ArgFlagsTy &Flags,
                       SmallVectorImpl<SDValue> &InVals,
                       const Argument *FuncArg, unsigned FirstReg,
                       unsigned LastReg, const CCValAssign &VA,
                       LoongArchCCState &State) const;

    /// passByValArg - Pass a byval argument in registers or on stack.
    void passByValArg(SDValue Chain, const SDLoc &DL,
                      std::deque<std::pair<unsigned, SDValue>> &RegsToPass,
                      SmallVectorImpl<SDValue> &MemOpChains, SDValue StackPtr,
                      MachineFrameInfo &MFI, SelectionDAG &DAG, SDValue Arg,
                      unsigned FirstReg, unsigned LastReg,
                      const ISD::ArgFlagsTy &Flags,
                      const CCValAssign &VA) const;

    /// writeVarArgRegs - Write variable function arguments passed in registers
    /// to the stack. Also create a stack frame object for the first variable
    /// argument.
    void writeVarArgRegs(std::vector<SDValue> &OutChains, SDValue Chain,
                         const SDLoc &DL, SelectionDAG &DAG,
                         CCState &State) const;

    SDValue
    LowerFormalArguments(SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
                         const SmallVectorImpl<ISD::InputArg> &Ins,
                         const SDLoc &dl, SelectionDAG &DAG,
                         SmallVectorImpl<SDValue> &InVals) const override;

    SDValue passArgOnStack(SDValue StackPtr, unsigned Offset, SDValue Chain,
                           SDValue Arg, const SDLoc &DL, bool IsTailCall,
                           SelectionDAG &DAG) const;

    SDValue LowerCall(TargetLowering::CallLoweringInfo &CLI,
                      SmallVectorImpl<SDValue> &InVals) const override;

    bool CanLowerReturn(CallingConv::ID CallConv, MachineFunction &MF,
                        bool isVarArg,
                        const SmallVectorImpl<ISD::OutputArg> &Outs,
                        LLVMContext &Context) const override;

    SDValue LowerReturn(SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
                        const SmallVectorImpl<ISD::OutputArg> &Outs,
                        const SmallVectorImpl<SDValue> &OutVals,
                        const SDLoc &dl, SelectionDAG &DAG) const override;

    bool shouldSignExtendTypeInLibCall(EVT Type, bool IsSigned) const override;

    // Inline asm support
    ConstraintType getConstraintType(StringRef Constraint) const override;

    /// Examine constraint string and operand type and determine a weight value.
    /// The operand object must already have been set up with the operand type.
    ConstraintWeight getSingleConstraintMatchWeight(
      AsmOperandInfo &info, const char *constraint) const override;

    /// This function parses registers that appear in inline-asm constraints.
    /// It returns pair (0, 0) on failure.
    std::pair<unsigned, const TargetRegisterClass *>
    parseRegForInlineAsmConstraint(StringRef C, MVT VT) const;

    std::pair<unsigned, const TargetRegisterClass *>
    getRegForInlineAsmConstraint(const TargetRegisterInfo *TRI,
                                 StringRef Constraint, MVT VT) const override;

    /// LowerAsmOperandForConstraint - Lower the specified operand into the Ops
    /// vector.  If it is invalid, don't add anything to Ops. If hasMemory is
    /// true it means one of the asm constraint of the inline asm instruction
    /// being processed is 'm'.
    void LowerAsmOperandForConstraint(SDValue Op,
                                      std::string &Constraint,
                                      std::vector<SDValue> &Ops,
                                      SelectionDAG &DAG) const override;

    unsigned
    getInlineAsmMemConstraint(StringRef ConstraintCode) const override {
      if (ConstraintCode == "R")
        return InlineAsm::Constraint_R;
      else if (ConstraintCode == "ZC")
        return InlineAsm::Constraint_ZC;
      else if (ConstraintCode == "ZB")
        return InlineAsm::Constraint_ZB;
      return TargetLowering::getInlineAsmMemConstraint(ConstraintCode);
    }

    bool isLegalAddressingMode(const DataLayout &DL, const AddrMode &AM,
                               Type *Ty, unsigned AS,
                               Instruction *I = nullptr) const override;

    bool isOffsetFoldingLegal(const GlobalAddressSDNode *GA) const override;

    EVT getOptimalMemOpType(const MemOp &Op,
                            const AttributeList &FuncAttributes) const override;

    /// isFPImmLegal - Returns true if the target can instruction select the
    /// specified FP immediate natively. If false, the legalizer will
    /// materialize the FP immediate as a load from a constant pool.
    bool isFPImmLegal(const APFloat &Imm, EVT VT,
                      bool ForCodeSize) const override;

    bool useSoftFloat() const override;

    bool shouldInsertFencesForAtomic(const Instruction *I) const override {
      return isa<LoadInst>(I) || isa<StoreInst>(I);
    }

    /// Emit a sign-extension using sll/sra, seb, or seh appropriately.
    MachineBasicBlock *emitSignExtendToI32InReg(MachineInstr &MI,
                                                MachineBasicBlock *BB,
                                                unsigned Size, unsigned DstReg,
                                                unsigned SrcRec) const;

    MachineBasicBlock *emitLoadAddress(MachineInstr &MI,
                                       MachineBasicBlock *BB) const;
    MachineBasicBlock *emitAtomicBinary(MachineInstr &MI,
                                        MachineBasicBlock *BB) const;
    MachineBasicBlock *emitAtomicBinaryPartword(MachineInstr &MI,
                                                MachineBasicBlock *BB,
                                                unsigned Size) const;
    MachineBasicBlock *emitAtomicCmpSwap(MachineInstr &MI,
                                         MachineBasicBlock *BB) const;
    MachineBasicBlock *emitAtomicCmpSwapPartword(MachineInstr &MI,
                                                 MachineBasicBlock *BB,
                                                 unsigned Size) const;
    MachineBasicBlock *emitSEL_D(MachineInstr &MI, MachineBasicBlock *BB) const;

    MachineBasicBlock *emitPseudoSELECT(MachineInstr &MI, MachineBasicBlock *BB,
                                        bool isFPCmp, unsigned Opc) const;
  };

} // end namespace llvm

#endif // LLVM_LIB_TARGET_LOONGARCH_LOONGARCHISELLOWERING_H
