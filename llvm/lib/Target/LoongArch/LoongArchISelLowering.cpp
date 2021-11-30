//===- LoongArchISelLowering.cpp - LoongArch DAG Lowering Implementation ------------===//
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

#include "LoongArchISelLowering.h"
#include "MCTargetDesc/LoongArchBaseInfo.h"
#include "MCTargetDesc/LoongArchInstPrinter.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "LoongArchCCState.h"
#include "LoongArchInstrInfo.h"
#include "LoongArchMachineFunction.h"
#include "LoongArchRegisterInfo.h"
#include "LoongArchSubtarget.h"
#include "LoongArchTargetMachine.h"
#include "LoongArchTargetObjectFile.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Triple.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/FunctionLoweringInfo.h"
#include "llvm/CodeGen/ISDOpcodes.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RuntimeLibcalls.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicsLoongArch.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MachineValueType.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <deque>
#include <iterator>
#include <utility>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "loongarch-lower"

STATISTIC(NumTailCalls, "Number of tail calls");

static cl::opt<bool>
NoZeroDivCheck("mnocheck-zero-division", cl::Hidden,
               cl::desc("LoongArch: Don't trap on integer division by zero."),
               cl::init(false));

static cl::opt<bool>
UseLoongArchTailCalls("loongarch-tail-calls", cl::Hidden,
                      cl::desc("LoongArch: permit tail calls."), cl::init(false));

static const MCPhysReg LoongArch64DPRegs[8] = {
  LoongArch::F0_64, LoongArch::F1_64, LoongArch::F2_64, LoongArch::F3_64,
  LoongArch::F4_64, LoongArch::F5_64, LoongArch::F6_64, LoongArch::F7_64
};

// If I is a shifted mask, set the size (SMSize) and the first bit of the
// mask (SMLsb), and return true.
// For example, if I is 0x003ff800, (SMLsb, SMSize) = (11, 11).
static bool isShiftedMask(uint64_t I, uint64_t &SMLsb, uint64_t &SMSize) {
  if (!isShiftedMask_64(I))
    return false;

  SMSize = countPopulation(I);
  SMLsb = countTrailingZeros(I);
  return true;
}

SDValue LoongArchTargetLowering::getTargetNode(GlobalAddressSDNode *N, EVT Ty,
                                          SelectionDAG &DAG,
                                          unsigned Flag) const {
  return DAG.getTargetGlobalAddress(N->getGlobal(), SDLoc(N), Ty, 0, Flag);
}

SDValue LoongArchTargetLowering::getTargetNode(ExternalSymbolSDNode *N, EVT Ty,
                                          SelectionDAG &DAG,
                                          unsigned Flag) const {
  return DAG.getTargetExternalSymbol(N->getSymbol(), Ty, Flag);
}

SDValue LoongArchTargetLowering::getTargetNode(BlockAddressSDNode *N, EVT Ty,
                                          SelectionDAG &DAG,
                                          unsigned Flag) const {
  return DAG.getTargetBlockAddress(N->getBlockAddress(), Ty, N->getOffset(), Flag);
}

SDValue LoongArchTargetLowering::getTargetNode(JumpTableSDNode *N, EVT Ty,
                                          SelectionDAG &DAG,
                                          unsigned Flag) const {
  return DAG.getTargetJumpTable(N->getIndex(), Ty, Flag);
}

SDValue LoongArchTargetLowering::getTargetNode(ConstantPoolSDNode *N, EVT Ty,
                                          SelectionDAG &DAG,
                                          unsigned Flag) const {
  return DAG.getTargetConstantPool(N->getConstVal(), Ty, N->getAlign(),
                                   N->getOffset(), Flag);
}

const char *LoongArchTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch ((LoongArchISD::NodeType)Opcode) {
  case LoongArchISD::FIRST_NUMBER:      break;
  case LoongArchISD::JmpLink:           return "LoongArchISD::JmpLink";
  case LoongArchISD::TailCall:          return "LoongArchISD::TailCall";
  case LoongArchISD::GlobalAddress:     return "LoongArchISD::GlobalAddress";
  case LoongArchISD::Ret:               return "LoongArchISD::Ret";
  case LoongArchISD::ERet:              return "LoongArchISD::ERet";
  case LoongArchISD::EH_RETURN:         return "LoongArchISD::EH_RETURN";
  case LoongArchISD::FPBrcond:          return "LoongArchISD::FPBrcond";
  case LoongArchISD::FPCmp:             return "LoongArchISD::FPCmp";
  case LoongArchISD::CMovFP_T:          return "LoongArchISD::CMovFP_T";
  case LoongArchISD::CMovFP_F:          return "LoongArchISD::CMovFP_F";
  case LoongArchISD::TruncIntFP:        return "LoongArchISD::TruncIntFP";
  case LoongArchISD::DBAR:              return "LoongArchISD::DBAR";
  case LoongArchISD::BSTRPICK:          return "LoongArchISD::BSTRPICK";
  case LoongArchISD::BSTRINS:           return "LoongArchISD::BSTRINS";
  }
  return nullptr;
}

LoongArchTargetLowering::LoongArchTargetLowering(const LoongArchTargetMachine &TM,
                                       const LoongArchSubtarget &STI)
    : TargetLowering(TM), Subtarget(STI), ABI(TM.getABI()) {
  // Set up the register classes
  addRegisterClass(MVT::i32, &LoongArch::GPR32RegClass);

  if (Subtarget.is64Bit())
    addRegisterClass(MVT::i64, &LoongArch::GPR64RegClass);

  // LoongArch does not have i1 type, so use i32 for
  // setcc operations results (slt, sgt, ...).
  setBooleanContents(ZeroOrOneBooleanContent);
  setBooleanVectorContents(ZeroOrNegativeOneBooleanContent);

  // Load extented operations for i1 types must be promoted
  for (MVT VT : MVT::integer_valuetypes()) {
    setLoadExtAction(ISD::EXTLOAD,  VT, MVT::i1,  Promote);
    setLoadExtAction(ISD::ZEXTLOAD, VT, MVT::i1,  Promote);
    setLoadExtAction(ISD::SEXTLOAD, VT, MVT::i1,  Promote);
  }

  // LoongArch doesn't have extending float->double load/store.  Set LoadExtAction
  // for f32, f16
  for (MVT VT : MVT::fp_valuetypes()) {
    setLoadExtAction(ISD::EXTLOAD, VT, MVT::f32, Expand);
    setLoadExtAction(ISD::EXTLOAD, VT, MVT::f16, Expand);
  }

  // Set LoadExtAction for f16 vectors to Expand
  for (MVT VT : MVT::fp_fixedlen_vector_valuetypes()) {
    MVT F16VT = MVT::getVectorVT(MVT::f16, VT.getVectorNumElements());
    if (F16VT.isValid())
      setLoadExtAction(ISD::EXTLOAD, VT, F16VT, Expand);
  }

  setTruncStoreAction(MVT::f32, MVT::f16, Expand);
  setTruncStoreAction(MVT::f64, MVT::f16, Expand);

  setTruncStoreAction(MVT::f64, MVT::f32, Expand);

  // Used by legalize types to correctly generate the setcc result.
  // Without this, every float setcc comes with a AND/OR with the result,
  // we don't want this, since the fpcmp result goes to a flag register,
  // which is used implicitly by brcond and select operations.
  AddPromotedToType(ISD::SETCC, MVT::i1, MVT::i32);

  // LoongArch Custom Operations
  setOperationAction(ISD::BR_JT,              MVT::Other, Expand);
  setOperationAction(ISD::GlobalAddress,      MVT::i32,   Custom);
  setOperationAction(ISD::BlockAddress,       MVT::i32,   Custom);
  setOperationAction(ISD::GlobalTLSAddress,   MVT::i32,   Custom);
  setOperationAction(ISD::JumpTable,          MVT::i32,   Custom);
  setOperationAction(ISD::ConstantPool,       MVT::i32,   Custom);
  setOperationAction(ISD::SELECT,             MVT::f32,   Custom);
  setOperationAction(ISD::SELECT,             MVT::f64,   Custom);
  setOperationAction(ISD::SELECT,             MVT::i32,   Custom);
  setOperationAction(ISD::SETCC,              MVT::f32,   Custom);
  setOperationAction(ISD::SETCC,              MVT::f64,   Custom);
  setOperationAction(ISD::BRCOND,             MVT::Other, Custom);
  // fcopysign does not use 'custom' in the instruction legalization phase
  // when using llvm's Intrinsic-'copysign'.
  //setOperationAction(ISD::FCOPYSIGN,          MVT::f32,   Custom);
  //setOperationAction(ISD::FCOPYSIGN,          MVT::f64,   Custom);
  setOperationAction(ISD::FP_TO_SINT,         MVT::i32,   Custom);

  if (Subtarget.is64Bit()) {
    setOperationAction(ISD::GlobalAddress,      MVT::i64,   Custom);
    setOperationAction(ISD::BlockAddress,       MVT::i64,   Custom);
    setOperationAction(ISD::GlobalTLSAddress,   MVT::i64,   Custom);
    setOperationAction(ISD::JumpTable,          MVT::i64,   Custom);
    setOperationAction(ISD::ConstantPool,       MVT::i64,   Custom);
    setOperationAction(ISD::SELECT,             MVT::i64,   Custom);
    setOperationAction(ISD::LOAD,               MVT::i64,   Legal);
    setOperationAction(ISD::STORE,              MVT::i64,   Custom);
    setOperationAction(ISD::FP_TO_SINT,         MVT::i64,   Custom);
    setOperationAction(ISD::SHL_PARTS,          MVT::i64,   Custom);
    setOperationAction(ISD::SRA_PARTS,          MVT::i64,   Custom);
    setOperationAction(ISD::SRL_PARTS,          MVT::i64,   Custom);
  }

  if (!Subtarget.is64Bit()) {
    setOperationAction(ISD::SHL_PARTS,          MVT::i32,   Custom);
    setOperationAction(ISD::SRA_PARTS,          MVT::i32,   Custom);
    setOperationAction(ISD::SRL_PARTS,          MVT::i32,   Custom);
  }

  setOperationAction(ISD::EH_DWARF_CFA,         MVT::i32,   Custom);
  if (Subtarget.is64Bit())
    setOperationAction(ISD::EH_DWARF_CFA,       MVT::i64,   Custom);

  setOperationAction(ISD::SDIV, MVT::i32, Expand);
  setOperationAction(ISD::SREM, MVT::i32, Expand);
  setOperationAction(ISD::UDIV, MVT::i32, Expand);
  setOperationAction(ISD::UREM, MVT::i32, Expand);
  setOperationAction(ISD::SDIV, MVT::i64, Expand);
  setOperationAction(ISD::SREM, MVT::i64, Expand);
  setOperationAction(ISD::UDIV, MVT::i64, Expand);
  setOperationAction(ISD::UREM, MVT::i64, Expand);

  // Operations not directly supported by LoongArch.
  setOperationAction(ISD::BR_CC,             MVT::f32,   Expand);
  setOperationAction(ISD::BR_CC,             MVT::f64,   Expand);
  setOperationAction(ISD::BR_CC,             MVT::i32,   Expand);
  setOperationAction(ISD::BR_CC,             MVT::i64,   Expand);
  setOperationAction(ISD::SELECT_CC,         MVT::i32,   Expand);
  setOperationAction(ISD::SELECT_CC,         MVT::i64,   Expand);
  setOperationAction(ISD::SELECT_CC,         MVT::f32,   Expand);
  setOperationAction(ISD::SELECT_CC,         MVT::f64,   Expand);
  setOperationAction(ISD::UINT_TO_FP,        MVT::i32,   Expand);
  setOperationAction(ISD::UINT_TO_FP,        MVT::i64,   Expand);
  setOperationAction(ISD::FP_TO_UINT,        MVT::i32,   Expand);
  setOperationAction(ISD::FP_TO_UINT,        MVT::i64,   Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1,    Expand);
  setOperationAction(ISD::CTPOP,           MVT::i32,   Expand);
  setOperationAction(ISD::CTPOP,           MVT::i64,   Expand);
  setOperationAction(ISD::ROTL,              MVT::i32,   Expand);
  setOperationAction(ISD::ROTL,              MVT::i64,   Expand);
  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i32,  Expand);
  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i64,  Expand);

  setOperationAction(ISD::FSIN,              MVT::f32,   Expand);
  setOperationAction(ISD::FSIN,              MVT::f64,   Expand);
  setOperationAction(ISD::FCOS,              MVT::f32,   Expand);
  setOperationAction(ISD::FCOS,              MVT::f64,   Expand);
  setOperationAction(ISD::FSINCOS,           MVT::f32,   Expand);
  setOperationAction(ISD::FSINCOS,           MVT::f64,   Expand);
  setOperationAction(ISD::FPOW,              MVT::f32,   Expand);
  setOperationAction(ISD::FPOW,              MVT::f64,   Expand);
  setOperationAction(ISD::FLOG,              MVT::f32,   Expand);
  setOperationAction(ISD::FRINT,             MVT::f32,   Legal);
  setOperationAction(ISD::FRINT,             MVT::f64,   Legal);

  setOperationAction(ISD::FLOG10,            MVT::f32,   Expand);
  setOperationAction(ISD::FEXP,              MVT::f32,   Expand);
  setOperationAction(ISD::FMA,               MVT::f32,   Legal);
  setOperationAction(ISD::FMA,               MVT::f64,   Legal);
  setOperationAction(ISD::FREM,              MVT::f32,   Expand);
  setOperationAction(ISD::FREM,              MVT::f64,   Expand);

  // Lower f16 conversion operations into library calls
  setOperationAction(ISD::FP16_TO_FP,        MVT::f32,   Expand);
  setOperationAction(ISD::FP_TO_FP16,        MVT::f32,   Expand);
  setOperationAction(ISD::FP16_TO_FP,        MVT::f64,   Expand);
  setOperationAction(ISD::FP_TO_FP16,        MVT::f64,   Expand);

  setOperationAction(ISD::EH_RETURN, MVT::Other, Custom);

  setOperationAction(ISD::VASTART,           MVT::Other, Custom);
  setOperationAction(ISD::VAARG,             MVT::Other, Custom);
  setOperationAction(ISD::VACOPY,            MVT::Other, Expand);
  setOperationAction(ISD::VAEND,             MVT::Other, Expand);

  // Use the default for now
  setOperationAction(ISD::STACKSAVE,         MVT::Other, Expand);
  setOperationAction(ISD::STACKRESTORE,      MVT::Other, Expand);

  if (!Subtarget.is64Bit()) {
    setOperationAction(ISD::ATOMIC_LOAD,     MVT::i64,   Expand);
    setOperationAction(ISD::ATOMIC_STORE,    MVT::i64,   Expand);
  }

  if (Subtarget.is64Bit()) {
    setLoadExtAction(ISD::SEXTLOAD, MVT::i64, MVT::i32, Custom);
    setLoadExtAction(ISD::ZEXTLOAD, MVT::i64, MVT::i32, Custom);
    setLoadExtAction(ISD::EXTLOAD, MVT::i64, MVT::i32, Custom);
    setTruncStoreAction(MVT::i64, MVT::i32, Custom);
  }

  setOperationAction(ISD::TRAP, MVT::Other, Legal);
  setOperationAction(ISD::BITREVERSE, MVT::i32, Legal);
  setOperationAction(ISD::BITREVERSE, MVT::i64, Legal);

  setTargetDAGCombine(ISD::SELECT);
  setTargetDAGCombine(ISD::AND);
  setTargetDAGCombine(ISD::OR);
  setTargetDAGCombine(ISD::AssertZext);
  setTargetDAGCombine(ISD::SHL);

  if (ABI.IsLP32()) {
    // These libcalls are not available in 32-bit.
    setLibcallName(RTLIB::SHL_I128, nullptr);
    setLibcallName(RTLIB::SRL_I128, nullptr);
    setLibcallName(RTLIB::SRA_I128, nullptr);
  }

  if (!Subtarget.useSoftFloat()) {
    addRegisterClass(MVT::f32, &LoongArch::FGR32RegClass);

    // When dealing with single precision only, use libcalls
    if (!Subtarget.isSingleFloat()) {
      if (Subtarget.isFP64bit())
        addRegisterClass(MVT::f64, &LoongArch::FGR64RegClass);
    }
  }

  setOperationAction(ISD::SMUL_LOHI,          MVT::i32, Custom);
  setOperationAction(ISD::UMUL_LOHI,          MVT::i32, Custom);

  if (Subtarget.is64Bit())
    setOperationAction(ISD::MUL,              MVT::i64, Custom);

  if (Subtarget.is64Bit()) {
    setOperationAction(ISD::SMUL_LOHI,        MVT::i64, Custom);
    setOperationAction(ISD::UMUL_LOHI,        MVT::i64, Custom);
    setOperationAction(ISD::SDIVREM,          MVT::i64, Custom);
    setOperationAction(ISD::UDIVREM,          MVT::i64, Custom);
  }

  setOperationAction(ISD::INTRINSIC_WO_CHAIN, MVT::i64, Custom);
  setOperationAction(ISD::INTRINSIC_W_CHAIN,  MVT::i64, Custom);

  setOperationAction(ISD::SDIVREM, MVT::i32, Custom);
  setOperationAction(ISD::UDIVREM, MVT::i32, Custom);
  setOperationAction(ISD::ATOMIC_FENCE,       MVT::Other, Custom);
  setOperationAction(ISD::LOAD,               MVT::i32, Legal);
  setOperationAction(ISD::STORE,              MVT::i32, Custom);

  setTargetDAGCombine(ISD::MUL);

  setOperationAction(ISD::INTRINSIC_WO_CHAIN, MVT::Other, Custom);
  setOperationAction(ISD::INTRINSIC_W_CHAIN, MVT::Other, Custom);
  setOperationAction(ISD::INTRINSIC_VOID, MVT::Other, Custom);

  // Replace the accumulator-based multiplies with a
  // three register instruction.
  setOperationAction(ISD::SMUL_LOHI, MVT::i32, Expand);
  setOperationAction(ISD::UMUL_LOHI, MVT::i32, Expand);
  setOperationAction(ISD::MUL, MVT::i32, Legal);
  setOperationAction(ISD::MULHS, MVT::i32, Legal);
  setOperationAction(ISD::MULHU, MVT::i32, Legal);

  // Replace the accumulator-based division/remainder with separate
  // three register division and remainder instructions.
  setOperationAction(ISD::SDIVREM, MVT::i32, Expand);
  setOperationAction(ISD::UDIVREM, MVT::i32, Expand);
  setOperationAction(ISD::SDIV, MVT::i32, Legal);
  setOperationAction(ISD::UDIV, MVT::i32, Legal);
  setOperationAction(ISD::SREM, MVT::i32, Legal);
  setOperationAction(ISD::UREM, MVT::i32, Legal);

  // Replace the accumulator-based multiplies with a
  // three register instruction.
  setOperationAction(ISD::SMUL_LOHI, MVT::i64, Expand);
  setOperationAction(ISD::UMUL_LOHI, MVT::i64, Expand);
  setOperationAction(ISD::MUL, MVT::i64, Legal);
  setOperationAction(ISD::MULHS, MVT::i64, Legal);
  setOperationAction(ISD::MULHU, MVT::i64, Legal);

  // Replace the accumulator-based division/remainder with separate
  // three register division and remainder instructions.
  setOperationAction(ISD::SDIVREM, MVT::i64, Expand);
  setOperationAction(ISD::UDIVREM, MVT::i64, Expand);
  setOperationAction(ISD::SDIV, MVT::i64, Legal);
  setOperationAction(ISD::UDIV, MVT::i64, Legal);
  setOperationAction(ISD::SREM, MVT::i64, Legal);
  setOperationAction(ISD::UREM, MVT::i64, Legal);

  MaxGluedStoresPerMemcpy = 4;

  setMinFunctionAlignment(Subtarget.is64Bit() ? Align(8) : Align(4));

  // The arguments on the stack are defined in terms of 4-byte slots on LP32
  // and 8-byte slots on LPX32/LP64.
  setMinStackArgumentAlignment((ABI.IsLPX32() || ABI.IsLP64()) ? Align(8)
                                                               : Align(4));

  setStackPointerRegisterToSaveRestore(ABI.IsLP64() ? LoongArch::SP_64 : LoongArch::SP);

  MaxStoresPerMemcpy = 16;

  computeRegisterProperties(Subtarget.getRegisterInfo());
}

bool
LoongArchTargetLowering::allowsMisalignedMemoryAccesses(
    EVT VT, unsigned, unsigned, MachineMemOperand::Flags, bool *Fast) const {
  if (Fast)
    *Fast = true;
  return true;
}

EVT LoongArchTargetLowering::getSetCCResultType(const DataLayout &, LLVMContext &,
                                           EVT VT) const {
  if (!VT.isVector())
    return MVT::i32;
  return VT.changeVectorElementTypeToInteger();
}

static LoongArch::CondCode condCodeToFCC(ISD::CondCode CC) {
  switch (CC) {
  default: llvm_unreachable("Unknown fp condition code!");
  case ISD::SETEQ:
  case ISD::SETOEQ: return LoongArch::FCOND_OEQ;
  case ISD::SETUNE: return LoongArch::FCOND_UNE;
  case ISD::SETLT:
  case ISD::SETOLT: return LoongArch::FCOND_OLT;
  case ISD::SETGT:
  case ISD::SETOGT: return LoongArch::FCOND_OGT;
  case ISD::SETLE:
  case ISD::SETOLE: return LoongArch::FCOND_OLE;
  case ISD::SETGE:
  case ISD::SETOGE: return LoongArch::FCOND_OGE;
  case ISD::SETULT: return LoongArch::FCOND_ULT;
  case ISD::SETULE: return LoongArch::FCOND_ULE;
  case ISD::SETUGT: return LoongArch::FCOND_UGT;
  case ISD::SETUGE: return LoongArch::FCOND_UGE;
  case ISD::SETUO:  return LoongArch::FCOND_UN;
  case ISD::SETO:   return LoongArch::FCOND_OR;
  case ISD::SETNE:
  case ISD::SETONE: return LoongArch::FCOND_ONE;
  case ISD::SETUEQ: return LoongArch::FCOND_UEQ;
  }
}

/// This function returns true if the floating point conditional branches and
/// conditional moves which use condition code CC should be inverted.
static bool invertFPCondCodeUser(LoongArch::CondCode CC) {
  if (CC >= LoongArch::FCOND_F && CC <= LoongArch::FCOND_SUNE)
    return false;

  assert((CC >= LoongArch::FCOND_T && CC <= LoongArch::FCOND_GT) &&
         "Illegal Condition Code");

  return true;
}

// Creates and returns an FPCmp node from a setcc node.
// Returns Op if setcc is not a floating point comparison.
static SDValue createFPCmp(SelectionDAG &DAG, const SDValue &Op) {
  // must be a SETCC node
  if (Op.getOpcode() != ISD::SETCC)
    return Op;

  SDValue LHS = Op.getOperand(0);

  if (!LHS.getValueType().isFloatingPoint())
    return Op;

  SDValue RHS = Op.getOperand(1);
  SDLoc DL(Op);

  // Assume the 3rd operand is a CondCodeSDNode. Add code to check the type of
  // node if necessary.
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(2))->get();

  return DAG.getNode(LoongArchISD::FPCmp, DL, MVT::Glue, LHS, RHS,
                     DAG.getConstant(condCodeToFCC(CC), DL, MVT::i32));
}

// Creates and returns a CMovFPT/F node.
static SDValue createCMovFP(SelectionDAG &DAG, SDValue Cond, SDValue True,
                            SDValue False, const SDLoc &DL) {
  ConstantSDNode *CC = cast<ConstantSDNode>(Cond.getOperand(2));
  bool invert = invertFPCondCodeUser((LoongArch::CondCode)CC->getSExtValue());
  SDValue FCC0 = DAG.getRegister(LoongArch::FCC0, MVT::i32);

  return DAG.getNode((invert ? LoongArchISD::CMovFP_F : LoongArchISD::CMovFP_T), DL,
                 True.getValueType(), True, FCC0, False, Cond);

}

static SDValue performSELECTCombine(SDNode *N, SelectionDAG &DAG,
                                    TargetLowering::DAGCombinerInfo &DCI,
                                    const LoongArchSubtarget &Subtarget) {
  if (DCI.isBeforeLegalizeOps())
    return SDValue();

  SDValue SetCC = N->getOperand(0);

  if ((SetCC.getOpcode() != ISD::SETCC) ||
      !SetCC.getOperand(0).getValueType().isInteger())
    return SDValue();

  SDValue False = N->getOperand(2);
  EVT FalseTy = False.getValueType();

  if (!FalseTy.isInteger())
    return SDValue();

  ConstantSDNode *FalseC = dyn_cast<ConstantSDNode>(False);

  // If the RHS (False) is 0, we swap the order of the operands
  // of ISD::SELECT (obviously also inverting the condition) so that we can
  // take advantage of conditional moves using the $0 register.
  // Example:
  //   return (a != 0) ? x : 0;
  //     load $reg, x
  //     movz $reg, $0, a
  if (!FalseC)
    return SDValue();

  const SDLoc DL(N);

  if (!FalseC->getZExtValue()) {
    ISD::CondCode CC = cast<CondCodeSDNode>(SetCC.getOperand(2))->get();
    SDValue True = N->getOperand(1);

    SetCC = DAG.getSetCC(DL, SetCC.getValueType(), SetCC.getOperand(0),
                         SetCC.getOperand(1),
                         ISD::getSetCCInverse(CC, SetCC.getValueType()));

    return DAG.getNode(ISD::SELECT, DL, FalseTy, SetCC, False, True);
  }

  // If both operands are integer constants there's a possibility that we
  // can do some interesting optimizations.
  SDValue True = N->getOperand(1);
  ConstantSDNode *TrueC = dyn_cast<ConstantSDNode>(True);

  if (!TrueC || !True.getValueType().isInteger())
    return SDValue();

  // We'll also ignore MVT::i64 operands as this optimizations proves
  // to be ineffective because of the required sign extensions as the result
  // of a SETCC operator is always MVT::i32 for non-vector types.
  if (True.getValueType() == MVT::i64)
    return SDValue();

  int64_t Diff = TrueC->getSExtValue() - FalseC->getSExtValue();

  // 1)  (a < x) ? y : y-1
  //  slti $reg1, a, x
  //  addiu $reg2, $reg1, y-1
  if (Diff == 1)
    return DAG.getNode(ISD::ADD, DL, SetCC.getValueType(), SetCC, False);

  // 2)  (a < x) ? y-1 : y
  //  slti $reg1, a, x
  //  xor $reg1, $reg1, 1
  //  addiu $reg2, $reg1, y-1
  if (Diff == -1) {
    ISD::CondCode CC = cast<CondCodeSDNode>(SetCC.getOperand(2))->get();
    SetCC = DAG.getSetCC(DL, SetCC.getValueType(), SetCC.getOperand(0),
                         SetCC.getOperand(1),
                         ISD::getSetCCInverse(CC, SetCC.getValueType()));
    return DAG.getNode(ISD::ADD, DL, SetCC.getValueType(), SetCC, True);
  }

  // Could not optimize.
  return SDValue();
}

static SDValue performANDCombine(SDNode *N, SelectionDAG &DAG,
                                 TargetLowering::DAGCombinerInfo &DCI,
                                 const LoongArchSubtarget &Subtarget) {
  if (DCI.isBeforeLegalizeOps())
    return SDValue();

  SDValue FirstOperand = N->getOperand(0);
  unsigned FirstOperandOpc = FirstOperand.getOpcode();
  SDValue Mask = N->getOperand(1);
  EVT ValTy = N->getValueType(0);
  SDLoc DL(N);

  uint64_t Lsb = 0, SMLsb, SMSize;
  ConstantSDNode *CN;
  SDValue NewOperand;
  unsigned Opc;

  // Op's second operand must be a shifted mask.
  if (!(CN = dyn_cast<ConstantSDNode>(Mask)) ||
      !isShiftedMask(CN->getZExtValue(), SMLsb, SMSize))
    return SDValue();

  if (FirstOperandOpc == ISD::SRA || FirstOperandOpc == ISD::SRL) {
    // Pattern match BSTRPICK.
    //  $dst = and ((sra or srl) $src , lsb), (2**size - 1)
    //  => bstrpick $dst, $src, lsb+size-1, lsb

    // The second operand of the shift must be an immediate.
    if (!(CN = dyn_cast<ConstantSDNode>(FirstOperand.getOperand(1))))
      return SDValue();

    Lsb = CN->getZExtValue();

    // Return if the shifted mask does not start at bit 0 or the sum of its size
    // and Lsb exceeds the word's size.
    if (SMLsb != 0 || Lsb + SMSize > ValTy.getSizeInBits())
      return SDValue();

    Opc = LoongArchISD::BSTRPICK;
    NewOperand = FirstOperand.getOperand(0);
  } else {
    // Pattern match BSTRPICK.
    //  $dst = and $src, (2**size - 1) , if size > 12
    //  => bstrpick $dst, $src, lsb+size-1, lsb , lsb = 0

    // If the mask is <= 0xfff, andi can be used instead.
    if (CN->getZExtValue() <= 0xfff)
      return SDValue();
    // Return if the mask doesn't start at position 0.
    if (SMLsb)
      return SDValue();

    Opc = LoongArchISD::BSTRPICK;
    NewOperand = FirstOperand;
  }
  return DAG.getNode(Opc, DL, ValTy, NewOperand,
                     DAG.getConstant((Lsb + SMSize - 1), DL, MVT::i32),
                     DAG.getConstant(Lsb, DL, MVT::i32));
}

static SDValue performORCombine(SDNode *N, SelectionDAG &DAG,
                                TargetLowering::DAGCombinerInfo &DCI,
                                const LoongArchSubtarget &Subtarget) {
  // Pattern match BSTRINS.
  //  $dst = or (and $src1 , mask0), (and (shl $src, lsb), mask1),
  //  where mask1 = (2**size - 1) << lsb, mask0 = ~mask1
  //  => bstrins $dst, $src, lsb+size-1, lsb, $src1
  if (DCI.isBeforeLegalizeOps())
    return SDValue();

  SDValue And0 = N->getOperand(0), And1 = N->getOperand(1);
  uint64_t SMLsb0, SMSize0, SMLsb1, SMSize1;
  ConstantSDNode *CN, *CN1;

  // See if Op's first operand matches (and $src1 , mask0).
  if (And0.getOpcode() != ISD::AND)
    return SDValue();

  if (!(CN = dyn_cast<ConstantSDNode>(And0.getOperand(1))) ||
      !isShiftedMask(~CN->getSExtValue(), SMLsb0, SMSize0))
    return SDValue();

  // See if Op's second operand matches (and (shl $src, lsb), mask1).
  if (And1.getOpcode() == ISD::AND &&
      And1.getOperand(0).getOpcode() == ISD::SHL) {

    if (!(CN = dyn_cast<ConstantSDNode>(And1.getOperand(1))) ||
        !isShiftedMask(CN->getZExtValue(), SMLsb1, SMSize1))
      return SDValue();

    // The shift masks must have the same least significant bit and size.
    if (SMLsb0 != SMLsb1 || SMSize0 != SMSize1)
      return SDValue();

    SDValue Shl = And1.getOperand(0);

    if (!(CN = dyn_cast<ConstantSDNode>(Shl.getOperand(1))))
      return SDValue();

    unsigned Shamt = CN->getZExtValue();

    // Return if the shift amount and the first bit position of mask are not the
    // same.
    EVT ValTy = N->getValueType(0);
    if ((Shamt != SMLsb0) || (SMLsb0 + SMSize0 > ValTy.getSizeInBits()))
      return SDValue();

    SDLoc DL(N);
    return DAG.getNode(LoongArchISD::BSTRINS, DL, ValTy, Shl.getOperand(0),
                       DAG.getConstant((SMLsb0 + SMSize0 - 1), DL, MVT::i32),
                       DAG.getConstant(SMLsb0, DL, MVT::i32),
                       And0.getOperand(0));
  } else {
    // Pattern match BSTRINS.
    //  $dst = or (and $src, mask0), mask1
    //  where mask0 = ((1 << SMSize0) -1) << SMLsb0
    //  => bstrins $dst, $src, SMLsb0+SMSize0-1, SMLsb0
    if (~CN->getSExtValue() == ((((int64_t)1 << SMSize0) - 1) << SMLsb0) &&
        (SMSize0 + SMLsb0 <= 64)) {
      // Check if AND instruction has constant as argument
      bool isConstCase = And1.getOpcode() != ISD::AND;
      if (And1.getOpcode() == ISD::AND) {
        if (!(CN1 = dyn_cast<ConstantSDNode>(And1->getOperand(1))))
          return SDValue();
      } else {
        if (!(CN1 = dyn_cast<ConstantSDNode>(N->getOperand(1))))
          return SDValue();
      }
      // Don't generate BSTRINS if constant OR operand doesn't fit into bits
      // cleared by constant AND operand.
      if (CN->getSExtValue() & CN1->getSExtValue())
        return SDValue();

      SDLoc DL(N);
      EVT ValTy = N->getOperand(0)->getValueType(0);
      SDValue Const1;
      SDValue SrlX;
      if (!isConstCase) {
        Const1 = DAG.getConstant(SMLsb0, DL, MVT::i32);
        SrlX = DAG.getNode(ISD::SRL, DL, And1->getValueType(0), And1, Const1);
      }
      return DAG.getNode(
          LoongArchISD::BSTRINS, DL, N->getValueType(0),
          isConstCase
              ? DAG.getConstant(CN1->getSExtValue() >> SMLsb0, DL, ValTy)
              : SrlX,
          DAG.getConstant(ValTy.getSizeInBits() / 8 < 8 ? (SMLsb0 + (SMSize0 & 31) - 1)
                                                        : (SMLsb0 + SMSize0 - 1),
                          DL, MVT::i32),
          DAG.getConstant(SMLsb0, DL, MVT::i32),
          And0->getOperand(0));

    }
    return SDValue();
  }
}

static bool
shouldTransformMulToShiftsAddsSubs(APInt C, EVT VT,
                                   SelectionDAG &DAG,
                                   const LoongArchSubtarget &Subtarget) {
  // Estimate the number of operations the below transform will turn a
  // constant multiply into. The number is approximately equal to the minimal
  // number of powers of two that constant can be broken down to by adding
  // or subtracting them.
  //
  // If we have taken more than 17[1] / 8[2] steps to attempt the
  // optimization for a native sized value, it is more than likely that this
  // optimization will make things worse.
  //
  // [1] LoongArch64 requires 11 instructions at most to materialize any constant,
  //     multiplication requires at least 4 cycles, but another cycle (or two)
  //     to retrieve the result from corresponding registers.
  //
  // [2] LoongArch32 requires 2 instructions at most to materialize any constant,
  //     multiplication requires at least 4 cycles, but another cycle (or two)
  //     to retrieve the result from corresponding registers.
  //
  // TODO:
  // - MaxSteps needs to consider the `VT` of the constant for the current
  //   target.
  // - Consider to perform this optimization after type legalization.
  //   That allows to remove a workaround for types not supported natively.
  // - Take in account `-Os, -Oz` flags because this optimization
  //   increases code size.
  unsigned MaxSteps = Subtarget.isABI_LP32() ? 8 : 17;

  SmallVector<APInt, 16> WorkStack(1, C);
  unsigned Steps = 0;
  unsigned BitWidth = C.getBitWidth();

  while (!WorkStack.empty()) {
    APInt Val = WorkStack.pop_back_val();

    if (Val == 0 || Val == 1)
      continue;

    if (Steps >= MaxSteps)
      return false;

    if (Val.isPowerOf2()) {
      ++Steps;
      continue;
    }

    APInt Floor = APInt(BitWidth, 1) << Val.logBase2();
    APInt Ceil = Val.isNegative() ? APInt(BitWidth, 0)
                                  : APInt(BitWidth, 1) << C.ceilLogBase2();

    if ((Val - Floor).ule(Ceil - Val)) {
      WorkStack.push_back(Floor);
      WorkStack.push_back(Val - Floor);
    } else {
      WorkStack.push_back(Ceil);
      WorkStack.push_back(Ceil - Val);
    }

    ++Steps;
  }

  // If the value being multiplied is not supported natively, we have to pay
  // an additional legalization cost, conservatively assume an increase in the
  // cost of 3 instructions per step. This values for this heuristic were
  // determined experimentally.
  unsigned RegisterSize = DAG.getTargetLoweringInfo()
                              .getRegisterType(*DAG.getContext(), VT)
                              .getSizeInBits();
  Steps *= (VT.getSizeInBits() != RegisterSize) * 3;
  if (Steps > 27)
    return false;

  return true;
}

static SDValue genConstMult(SDValue X, APInt C, const SDLoc &DL, EVT VT,
                            EVT ShiftTy, SelectionDAG &DAG) {
  // Return 0.
  if (C == 0)
    return DAG.getConstant(0, DL, VT);

  // Return x.
  if (C == 1)
    return X;

  // If c is power of 2, return (shl x, log2(c)).
  if (C.isPowerOf2())
    return DAG.getNode(ISD::SHL, DL, VT, X,
                       DAG.getConstant(C.logBase2(), DL, ShiftTy));

  unsigned BitWidth = C.getBitWidth();
  APInt Floor = APInt(BitWidth, 1) << C.logBase2();
  APInt Ceil = C.isNegative() ? APInt(BitWidth, 0) :
                                APInt(BitWidth, 1) << C.ceilLogBase2();

  // If |c - floor_c| <= |c - ceil_c|,
  // where floor_c = pow(2, floor(log2(c))) and ceil_c = pow(2, ceil(log2(c))),
  // return (add constMult(x, floor_c), constMult(x, c - floor_c)).
  if ((C - Floor).ule(Ceil - C)) {
    SDValue Op0 = genConstMult(X, Floor, DL, VT, ShiftTy, DAG);
    SDValue Op1 = genConstMult(X, C - Floor, DL, VT, ShiftTy, DAG);
    return DAG.getNode(ISD::ADD, DL, VT, Op0, Op1);
  }

  // If |c - floor_c| > |c - ceil_c|,
  // return (sub constMult(x, ceil_c), constMult(x, ceil_c - c)).
  SDValue Op0 = genConstMult(X, Ceil, DL, VT, ShiftTy, DAG);
  SDValue Op1 = genConstMult(X, Ceil - C, DL, VT, ShiftTy, DAG);
  return DAG.getNode(ISD::SUB, DL, VT, Op0, Op1);
}

static SDValue performMULCombine(SDNode *N, SelectionDAG &DAG,
                                 const TargetLowering::DAGCombinerInfo &DCI,
                                 const LoongArchTargetLowering *TL,
                                 const LoongArchSubtarget &Subtarget) {
  EVT VT = N->getValueType(0);

  if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(N->getOperand(1)))
    if (!VT.isVector() && shouldTransformMulToShiftsAddsSubs(
                              C->getAPIntValue(), VT, DAG, Subtarget))
      return genConstMult(N->getOperand(0), C->getAPIntValue(), SDLoc(N), VT,
                          TL->getScalarShiftAmountTy(DAG.getDataLayout(), VT),
                          DAG);

  return SDValue(N, 0);
}

static SDValue performSRACombine(SDNode *N, SelectionDAG &DAG,
                                 TargetLowering::DAGCombinerInfo &DCI,
                                 const LoongArchSubtarget &Subtarget) {
  return SDValue();
}

static SDValue performXORCombine(SDNode *N, SelectionDAG &DAG,
                                 const LoongArchSubtarget &Subtarget) {
  return SDValue();
}

SDValue  LoongArchTargetLowering::
PerformDAGCombine(SDNode *N, DAGCombinerInfo &DCI) const {
  SelectionDAG &DAG = DCI.DAG;
  SDValue Val;

  switch (N->getOpcode()) {
  default: break;
  case ISD::AND:
    return performANDCombine(N, DAG, DCI, Subtarget);
  case ISD::OR:
    return performORCombine(N, DAG, DCI, Subtarget);
  case ISD::XOR:
    return performXORCombine(N, DAG, Subtarget);
  case ISD::MUL:
    return performMULCombine(N, DAG, DCI, this, Subtarget);
  case ISD::SRA:
    return performSRACombine(N, DAG, DCI, Subtarget);
  case ISD::SELECT:
    return performSELECTCombine(N, DAG, DCI, Subtarget);
  }
  return SDValue();
}

static SDValue LowerSUINT_TO_FP(unsigned ExtOpcode, SDValue Op, SelectionDAG &DAG) {

  EVT ResTy = Op->getValueType(0);
  SDValue Op0 = Op->getOperand(0);
  EVT ViaTy = Op0->getValueType(0);
  SDLoc DL(Op);

  if (!ResTy.isVector()) {
    if(ResTy.getScalarSizeInBits() == ViaTy.getScalarSizeInBits())
        return DAG.getNode(ISD::BITCAST, DL, ResTy, Op0);
    else if(ResTy.getScalarSizeInBits() > ViaTy.getScalarSizeInBits()) {
        Op0 = DAG.getNode(ISD::BITCAST, DL, MVT::f32, Op0);
        return DAG.getNode(ISD::FP_EXTEND, DL, MVT::f64, Op0);
    } else {
        Op0 = DAG.getNode(ISD::BITCAST, DL, MVT::f64, Op0);
        return DAG.getNode(ISD::TRUNCATE, DL, MVT::f32, Op0);
    }

  }

   return Op0;
}

bool LoongArchTargetLowering::isCheapToSpeculateCttz() const {
  return true;
}

bool LoongArchTargetLowering::isCheapToSpeculateCtlz() const {
  return true;
}

void
LoongArchTargetLowering::LowerOperationWrapper(SDNode *N,
                                          SmallVectorImpl<SDValue> &Results,
                                          SelectionDAG &DAG) const {
  SDValue Res = LowerOperation(SDValue(N, 0), DAG);

  for (unsigned I = 0, E = Res->getNumValues(); I != E; ++I)
    Results.push_back(Res.getValue(I));
}

void
LoongArchTargetLowering::ReplaceNodeResults(SDNode *N,
                                       SmallVectorImpl<SDValue> &Results,
                                       SelectionDAG &DAG) const {
  return LowerOperationWrapper(N, Results, DAG);
}

SDValue LoongArchTargetLowering::
LowerOperation(SDValue Op, SelectionDAG &DAG) const
{
  switch (Op.getOpcode())
  {
  case ISD::STORE:              return lowerSTORE(Op, DAG);
  case ISD::INTRINSIC_WO_CHAIN: return lowerINTRINSIC_WO_CHAIN(Op, DAG);
  case ISD::INTRINSIC_W_CHAIN:  return lowerINTRINSIC_W_CHAIN(Op, DAG);
  case ISD::INTRINSIC_VOID:     return lowerINTRINSIC_VOID(Op, DAG);
  case ISD::UINT_TO_FP:         return lowerUINT_TO_FP(Op, DAG);
  case ISD::SINT_TO_FP:         return lowerSINT_TO_FP(Op, DAG);
  case ISD::FP_TO_UINT:         return lowerFP_TO_UINT(Op, DAG);
  case ISD::FP_TO_SINT:         return lowerFP_TO_SINT(Op, DAG);
  case ISD::BRCOND:             return lowerBRCOND(Op, DAG);
  case ISD::ConstantPool:       return lowerConstantPool(Op, DAG);
  case ISD::GlobalAddress:      return lowerGlobalAddress(Op, DAG);
  case ISD::BlockAddress:       return lowerBlockAddress(Op, DAG);
  case ISD::GlobalTLSAddress:   return lowerGlobalTLSAddress(Op, DAG);
  case ISD::JumpTable:          return lowerJumpTable(Op, DAG);
  case ISD::SELECT:             return lowerSELECT(Op, DAG);
  case ISD::SETCC:              return lowerSETCC(Op, DAG);
  case ISD::VASTART:            return lowerVASTART(Op, DAG);
  case ISD::VAARG:              return lowerVAARG(Op, DAG);
  case ISD::FCOPYSIGN:          return lowerFCOPYSIGN(Op, DAG);
  case ISD::FRAMEADDR:          return lowerFRAMEADDR(Op, DAG);
  case ISD::RETURNADDR:         return lowerRETURNADDR(Op, DAG);
  case ISD::EH_RETURN:          return lowerEH_RETURN(Op, DAG);
  case ISD::ATOMIC_FENCE:       return lowerATOMIC_FENCE(Op, DAG);
  case ISD::SHL_PARTS:          return lowerShiftLeftParts(Op, DAG);
  case ISD::SRA_PARTS:          return lowerShiftRightParts(Op, DAG, true);
  case ISD::SRL_PARTS:          return lowerShiftRightParts(Op, DAG, false);
  case ISD::EH_DWARF_CFA:       return lowerEH_DWARF_CFA(Op, DAG);
  }
  return SDValue();
}

//===----------------------------------------------------------------------===//
//  Lower helper functions
//===----------------------------------------------------------------------===//

template <class NodeTy>
SDValue LoongArchTargetLowering::getAddr(NodeTy *N, SelectionDAG &DAG,
                                     bool IsLocal) const {
  SDLoc DL(N);
  EVT Ty = getPointerTy(DAG.getDataLayout());

  if (isPositionIndependent()) {
    SDValue Addr = getTargetNode(N, Ty, DAG, 0U);
    if (IsLocal)
      // Use PC-relative addressing to access the symbol.
      return SDValue(DAG.getMachineNode(LoongArch::LoadAddrLocal, DL, Ty, Addr), 0);

    // Use PC-relative addressing to access the GOT for this symbol, then load
    // the address from the GOT.
    return SDValue(DAG.getMachineNode(LoongArch::LoadAddrGlobal, DL, Ty, Addr), 0);
  }

  SDValue Addr = getTargetNode(N, Ty, DAG, 0U);
  return SDValue(DAG.getMachineNode(LoongArch::LoadAddrLocal, DL, Ty, Addr), 0);
}

// addLiveIn - This helper function adds the specified physical register to the
// MachineFunction as a live in value.  It also creates a corresponding
// virtual register for it.
static unsigned
addLiveIn(MachineFunction &MF, unsigned PReg, const TargetRegisterClass *RC)
{
  unsigned VReg = MF.getRegInfo().createVirtualRegister(RC);
  MF.getRegInfo().addLiveIn(PReg, VReg);
  return VReg;
}

static MachineBasicBlock *insertDivByZeroTrap(MachineInstr &MI,
                                              MachineBasicBlock &MBB,
                                              const TargetInstrInfo &TII,
                                              bool Is64Bit) {
  if (NoZeroDivCheck)
    return &MBB;

  // Insert pseudo instruction(PseudoTEQ), will expand:
  //   beq $divisor_reg, $zero, 8
  //   break 7
  MachineBasicBlock::iterator I(MI);
  MachineInstrBuilder MIB;
  MachineOperand &Divisor = MI.getOperand(2);
  unsigned TeqOp = LoongArch::PseudoTEQ;

  MIB = BuildMI(MBB, std::next(I), MI.getDebugLoc(), TII.get(TeqOp))
            .addReg(Divisor.getReg(), getKillRegState(Divisor.isKill()));

  // Use the 32-bit sub-register if this is a 64-bit division.
  //if (Is64Bit)
  //  MIB->getOperand(0).setSubReg(LoongArch::sub_32);

  // Clear Divisor's kill flag.
  Divisor.setIsKill(false);

  // We would normally delete the original instruction here but in this case
  // we only needed to inject an additional instruction rather than replace it.

  return &MBB;
}

MachineBasicBlock *
LoongArchTargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                                MachineBasicBlock *BB) const {
  switch (MI.getOpcode()) {
  default:
    llvm_unreachable("Unexpected instr type to insert");
  case LoongArch::ATOMIC_LOAD_ADD_I8:
    return emitAtomicBinaryPartword(MI, BB, 1);
  case LoongArch::ATOMIC_LOAD_ADD_I16:
    return emitAtomicBinaryPartword(MI, BB, 2);
  case LoongArch::ATOMIC_LOAD_ADD_I32:
    return emitAtomicBinary(MI, BB);
  case LoongArch::ATOMIC_LOAD_ADD_I64:
    return emitAtomicBinary(MI, BB);

  case LoongArch::ATOMIC_LOAD_AND_I8:
    return emitAtomicBinaryPartword(MI, BB, 1);
  case LoongArch::ATOMIC_LOAD_AND_I16:
    return emitAtomicBinaryPartword(MI, BB, 2);
  case LoongArch::ATOMIC_LOAD_AND_I32:
    return emitAtomicBinary(MI, BB);
  case LoongArch::ATOMIC_LOAD_AND_I64:
    return emitAtomicBinary(MI, BB);

  case LoongArch::ATOMIC_LOAD_OR_I8:
    return emitAtomicBinaryPartword(MI, BB, 1);
  case LoongArch::ATOMIC_LOAD_OR_I16:
    return emitAtomicBinaryPartword(MI, BB, 2);
  case LoongArch::ATOMIC_LOAD_OR_I32:
    return emitAtomicBinary(MI, BB);
  case LoongArch::ATOMIC_LOAD_OR_I64:
    return emitAtomicBinary(MI, BB);

  case LoongArch::ATOMIC_LOAD_XOR_I8:
    return emitAtomicBinaryPartword(MI, BB, 1);
  case LoongArch::ATOMIC_LOAD_XOR_I16:
    return emitAtomicBinaryPartword(MI, BB, 2);
  case LoongArch::ATOMIC_LOAD_XOR_I32:
    return emitAtomicBinary(MI, BB);
  case LoongArch::ATOMIC_LOAD_XOR_I64:
    return emitAtomicBinary(MI, BB);

  case LoongArch::ATOMIC_LOAD_NAND_I8:
    return emitAtomicBinaryPartword(MI, BB, 1);
  case LoongArch::ATOMIC_LOAD_NAND_I16:
    return emitAtomicBinaryPartword(MI, BB, 2);
  case LoongArch::ATOMIC_LOAD_NAND_I32:
    return emitAtomicBinary(MI, BB);
  case LoongArch::ATOMIC_LOAD_NAND_I64:
    return emitAtomicBinary(MI, BB);

  case LoongArch::ATOMIC_LOAD_SUB_I8:
    return emitAtomicBinaryPartword(MI, BB, 1);
  case LoongArch::ATOMIC_LOAD_SUB_I16:
    return emitAtomicBinaryPartword(MI, BB, 2);
  case LoongArch::ATOMIC_LOAD_SUB_I32:
    return emitAtomicBinary(MI, BB);
  case LoongArch::ATOMIC_LOAD_SUB_I64:
    return emitAtomicBinary(MI, BB);

  case LoongArch::ATOMIC_SWAP_I8:
    return emitAtomicBinaryPartword(MI, BB, 1);
  case LoongArch::ATOMIC_SWAP_I16:
    return emitAtomicBinaryPartword(MI, BB, 2);
  case LoongArch::ATOMIC_SWAP_I32:
    return emitAtomicBinary(MI, BB);
  case LoongArch::ATOMIC_SWAP_I64:
    return emitAtomicBinary(MI, BB);

  case LoongArch::ATOMIC_LOAD_MAX_I8:
    return emitAtomicBinaryPartword(MI, BB, 1);
  case LoongArch::ATOMIC_LOAD_MAX_I16:
    return emitAtomicBinaryPartword(MI, BB, 2);
  case LoongArch::ATOMIC_LOAD_MAX_I32:
    return emitAtomicBinary(MI, BB);
  case LoongArch::ATOMIC_LOAD_MAX_I64:
    return emitAtomicBinary(MI, BB);

  case LoongArch::ATOMIC_LOAD_MIN_I8:
    return emitAtomicBinaryPartword(MI, BB, 1);
  case LoongArch::ATOMIC_LOAD_MIN_I16:
    return emitAtomicBinaryPartword(MI, BB, 2);
  case LoongArch::ATOMIC_LOAD_MIN_I32:
    return emitAtomicBinary(MI, BB);
  case LoongArch::ATOMIC_LOAD_MIN_I64:
    return emitAtomicBinary(MI, BB);

  case LoongArch::ATOMIC_LOAD_UMAX_I8:
    return emitAtomicBinaryPartword(MI, BB, 1);
  case LoongArch::ATOMIC_LOAD_UMAX_I16:
    return emitAtomicBinaryPartword(MI, BB, 2);
  case LoongArch::ATOMIC_LOAD_UMAX_I32:
    return emitAtomicBinary(MI, BB);
  case LoongArch::ATOMIC_LOAD_UMAX_I64:
    return emitAtomicBinary(MI, BB);

  case LoongArch::ATOMIC_LOAD_UMIN_I8:
    return emitAtomicBinaryPartword(MI, BB, 1);
  case LoongArch::ATOMIC_LOAD_UMIN_I16:
    return emitAtomicBinaryPartword(MI, BB, 2);
  case LoongArch::ATOMIC_LOAD_UMIN_I32:
    return emitAtomicBinary(MI, BB);
  case LoongArch::ATOMIC_LOAD_UMIN_I64:
    return emitAtomicBinary(MI, BB);

  case LoongArch::ATOMIC_CMP_SWAP_I8:
    return emitAtomicCmpSwapPartword(MI, BB, 1);
  case LoongArch::ATOMIC_CMP_SWAP_I16:
    return emitAtomicCmpSwapPartword(MI, BB, 2);
  case LoongArch::ATOMIC_CMP_SWAP_I32:
    return emitAtomicCmpSwap(MI, BB);
  case LoongArch::ATOMIC_CMP_SWAP_I64:
    return emitAtomicCmpSwap(MI, BB);

  case LoongArch::PseudoSELECT_I:
  case LoongArch::PseudoSELECT_I64:
  case LoongArch::PseudoSELECT_S:
  case LoongArch::PseudoSELECT_D64:
    return emitPseudoSELECT(MI, BB, false, LoongArch::BNE32);

  case LoongArch::PseudoSELECTFP_T_I:
  case LoongArch::PseudoSELECTFP_T_I64:
  case LoongArch::PseudoSELECTFP_T_S:
  case LoongArch::PseudoSELECTFP_T_D64:
    return emitPseudoSELECT(MI, BB, true, LoongArch::BCNEZ);

  case LoongArch::PseudoSELECTFP_F_I:
  case LoongArch::PseudoSELECTFP_F_I64:
  case LoongArch::PseudoSELECTFP_F_S:
  case LoongArch::PseudoSELECTFP_F_D64:
    return emitPseudoSELECT(MI, BB, true, LoongArch::BCEQZ);
  case LoongArch::DIV_W:
  case LoongArch::DIV_WU:
  case LoongArch::MOD_W:
  case LoongArch::MOD_WU:
    return insertDivByZeroTrap(MI, *BB, *Subtarget.getInstrInfo(), false);
  case LoongArch::DIV_D:
  case LoongArch::DIV_DU:
  case LoongArch::MOD_D:
  case LoongArch::MOD_DU:
    return insertDivByZeroTrap(MI, *BB, *Subtarget.getInstrInfo(), true);
  }
}

const TargetRegisterClass *
LoongArchTargetLowering::getRepRegClassFor(MVT VT) const {
  return TargetLowering::getRepRegClassFor(VT);
}

// This function also handles LoongArch::ATOMIC_SWAP_I32 (when BinOpcode == 0), and
// LoongArch::ATOMIC_LOAD_NAND_I32 (when Nand == true)
MachineBasicBlock *
LoongArchTargetLowering::emitAtomicBinary(MachineInstr &MI,
                                     MachineBasicBlock *BB) const {

  MachineFunction *MF = BB->getParent();
  MachineRegisterInfo &RegInfo = MF->getRegInfo();
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();

  unsigned AtomicOp;
  switch (MI.getOpcode()) {
  case LoongArch::ATOMIC_LOAD_ADD_I32:
    AtomicOp = LoongArch::ATOMIC_LOAD_ADD_I32_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_SUB_I32:
    AtomicOp = LoongArch::ATOMIC_LOAD_SUB_I32_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_AND_I32:
    AtomicOp = LoongArch::ATOMIC_LOAD_AND_I32_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_OR_I32:
    AtomicOp = LoongArch::ATOMIC_LOAD_OR_I32_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_XOR_I32:
    AtomicOp = LoongArch::ATOMIC_LOAD_XOR_I32_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_NAND_I32:
    AtomicOp = LoongArch::ATOMIC_LOAD_NAND_I32_POSTRA;
    break;
  case LoongArch::ATOMIC_SWAP_I32:
    AtomicOp = LoongArch::ATOMIC_SWAP_I32_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_MAX_I32:
    AtomicOp = LoongArch::ATOMIC_LOAD_MAX_I32_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_MIN_I32:
    AtomicOp = LoongArch::ATOMIC_LOAD_MIN_I32_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_UMAX_I32:
    AtomicOp = LoongArch::ATOMIC_LOAD_UMAX_I32_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_UMIN_I32:
    AtomicOp = LoongArch::ATOMIC_LOAD_UMIN_I32_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_ADD_I64:
    AtomicOp = LoongArch::ATOMIC_LOAD_ADD_I64_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_SUB_I64:
    AtomicOp = LoongArch::ATOMIC_LOAD_SUB_I64_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_AND_I64:
    AtomicOp = LoongArch::ATOMIC_LOAD_AND_I64_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_OR_I64:
    AtomicOp = LoongArch::ATOMIC_LOAD_OR_I64_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_XOR_I64:
    AtomicOp = LoongArch::ATOMIC_LOAD_XOR_I64_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_NAND_I64:
    AtomicOp = LoongArch::ATOMIC_LOAD_NAND_I64_POSTRA;
    break;
  case LoongArch::ATOMIC_SWAP_I64:
    AtomicOp = LoongArch::ATOMIC_SWAP_I64_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_MAX_I64:
    AtomicOp = LoongArch::ATOMIC_LOAD_MAX_I64_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_MIN_I64:
    AtomicOp = LoongArch::ATOMIC_LOAD_MIN_I64_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_UMAX_I64:
    AtomicOp = LoongArch::ATOMIC_LOAD_UMAX_I64_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_UMIN_I64:
    AtomicOp = LoongArch::ATOMIC_LOAD_UMIN_I64_POSTRA;
    break;
  default:
    llvm_unreachable("Unknown pseudo atomic for replacement!");
  }

  unsigned OldVal = MI.getOperand(0).getReg();
  unsigned Ptr = MI.getOperand(1).getReg();
  unsigned Incr = MI.getOperand(2).getReg();
  unsigned Scratch = RegInfo.createVirtualRegister(RegInfo.getRegClass(OldVal));

  MachineBasicBlock::iterator II(MI);

  // The scratch registers here with the EarlyClobber | Define | Implicit
  // flags is used to persuade the register allocator and the machine
  // verifier to accept the usage of this register. This has to be a real
  // register which has an UNDEF value but is dead after the instruction which
  // is unique among the registers chosen for the instruction.

  // The EarlyClobber flag has the semantic properties that the operand it is
  // attached to is clobbered before the rest of the inputs are read. Hence it
  // must be unique among the operands to the instruction.
  // The Define flag is needed to coerce the machine verifier that an Undef
  // value isn't a problem.
  // The Dead flag is needed as the value in scratch isn't used by any other
  // instruction. Kill isn't used as Dead is more precise.
  // The implicit flag is here due to the interaction between the other flags
  // and the machine verifier.

  // For correctness purpose, a new pseudo is introduced here. We need this
  // new pseudo, so that FastRegisterAllocator does not see an ll/sc sequence
  // that is spread over >1 basic blocks. A register allocator which
  // introduces (or any codegen infact) a store, can violate the expectations
  // of the hardware.
  //
  // An atomic read-modify-write sequence starts with a linked load
  // instruction and ends with a store conditional instruction. The atomic
  // read-modify-write sequence fails if any of the following conditions
  // occur between the execution of ll and sc:
  //   * A coherent store is completed by another process or coherent I/O
  //     module into the block of synchronizable physical memory containing
  //     the word. The size and alignment of the block is
  //     implementation-dependent.
  //   * A coherent store is executed between an LL and SC sequence on the
  //     same processor to the block of synchornizable physical memory
  //     containing the word.
  //

  unsigned PtrCopy = RegInfo.createVirtualRegister(RegInfo.getRegClass(Ptr));
  unsigned IncrCopy = RegInfo.createVirtualRegister(RegInfo.getRegClass(Incr));

  if(MI.getOpcode() == LoongArch::ATOMIC_LOAD_NAND_I32
     || MI.getOpcode() == LoongArch::ATOMIC_LOAD_NAND_I64){
    BuildMI(*BB, II, DL, TII->get(LoongArch::DBAR)).addImm(0);
  }

  BuildMI(*BB, II, DL, TII->get(LoongArch::COPY), IncrCopy).addReg(Incr);
  BuildMI(*BB, II, DL, TII->get(LoongArch::COPY), PtrCopy).addReg(Ptr);

  BuildMI(*BB, II, DL, TII->get(AtomicOp))
      .addReg(OldVal, RegState::Define | RegState::EarlyClobber)
      .addReg(PtrCopy)
      .addReg(IncrCopy)
      .addReg(Scratch, RegState::Define | RegState::EarlyClobber |
                           RegState::Implicit | RegState::Dead);

  if(MI.getOpcode() == LoongArch::ATOMIC_LOAD_NAND_I32
     || MI.getOpcode() == LoongArch::ATOMIC_LOAD_NAND_I64){
    BuildMI(*BB, II, DL, TII->get(LoongArch::DBAR)).addImm(0);
  }

  MI.eraseFromParent();

  return BB;
}

MachineBasicBlock *LoongArchTargetLowering::emitSignExtendToI32InReg(
    MachineInstr &MI, MachineBasicBlock *BB, unsigned Size, unsigned DstReg,
    unsigned SrcReg) const {
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();
  const DebugLoc &DL = MI.getDebugLoc();
  if (Size == 1) {
    BuildMI(BB, DL, TII->get(LoongArch::EXT_W_B32), DstReg).addReg(SrcReg);
    return BB;
  }

  if (Size == 2) {
    BuildMI(BB, DL, TII->get(LoongArch::EXT_W_H32), DstReg).addReg(SrcReg);
    return BB;
  }

  MachineFunction *MF = BB->getParent();
  MachineRegisterInfo &RegInfo = MF->getRegInfo();
  const TargetRegisterClass *RC = getRegClassFor(MVT::i32);
  unsigned ScrReg = RegInfo.createVirtualRegister(RC);

  assert(Size < 32);
  int64_t ShiftImm = 32 - (Size * 8);

  BuildMI(BB, DL, TII->get(LoongArch::SLLI_W), ScrReg).addReg(SrcReg).addImm(ShiftImm);
  BuildMI(BB, DL, TII->get(LoongArch::SRAI_W), DstReg).addReg(ScrReg).addImm(ShiftImm);

  return BB;
}

MachineBasicBlock *LoongArchTargetLowering::emitAtomicBinaryPartword(
    MachineInstr &MI, MachineBasicBlock *BB, unsigned Size) const {
  assert((Size == 1 || Size == 2) &&
         "Unsupported size for EmitAtomicBinaryPartial.");

  MachineFunction *MF = BB->getParent();
  MachineRegisterInfo &RegInfo = MF->getRegInfo();
  const TargetRegisterClass *RC = getRegClassFor(MVT::i32);
  const bool ArePtrs64bit = ABI.ArePtrs64bit();
  const TargetRegisterClass *RCp =
    getRegClassFor(ArePtrs64bit ? MVT::i64 : MVT::i32);
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();

  unsigned Dest = MI.getOperand(0).getReg();
  unsigned Ptr = MI.getOperand(1).getReg();
  unsigned Incr = MI.getOperand(2).getReg();

  unsigned AlignedAddr = RegInfo.createVirtualRegister(RCp);
  unsigned ShiftAmt = RegInfo.createVirtualRegister(RC);
  unsigned Mask = RegInfo.createVirtualRegister(RC);
  unsigned Mask2 = RegInfo.createVirtualRegister(RC);
  unsigned Incr2 = RegInfo.createVirtualRegister(RC);
  unsigned MaskLSB2 = RegInfo.createVirtualRegister(RCp);
  unsigned PtrLSB2 = RegInfo.createVirtualRegister(RC);
  unsigned MaskUpper = RegInfo.createVirtualRegister(RC);
  unsigned MaskUppest = RegInfo.createVirtualRegister(RC);
  unsigned Scratch = RegInfo.createVirtualRegister(RC);
  unsigned Scratch2 = RegInfo.createVirtualRegister(RC);
  unsigned Scratch3 = RegInfo.createVirtualRegister(RC);

  unsigned AtomicOp = 0;
  switch (MI.getOpcode()) {
  case LoongArch::ATOMIC_LOAD_NAND_I8:
    AtomicOp = LoongArch::ATOMIC_LOAD_NAND_I8_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_NAND_I16:
    AtomicOp = LoongArch::ATOMIC_LOAD_NAND_I16_POSTRA;
    break;
  case LoongArch::ATOMIC_SWAP_I8:
    AtomicOp = LoongArch::ATOMIC_SWAP_I8_POSTRA;
    break;
  case LoongArch::ATOMIC_SWAP_I16:
    AtomicOp = LoongArch::ATOMIC_SWAP_I16_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_MAX_I8:
    AtomicOp = LoongArch::ATOMIC_LOAD_MAX_I8_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_MAX_I16:
    AtomicOp = LoongArch::ATOMIC_LOAD_MAX_I16_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_MIN_I8:
    AtomicOp = LoongArch::ATOMIC_LOAD_MIN_I8_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_MIN_I16:
    AtomicOp = LoongArch::ATOMIC_LOAD_MIN_I16_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_UMAX_I8:
    AtomicOp = LoongArch::ATOMIC_LOAD_UMAX_I8_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_UMAX_I16:
    AtomicOp = LoongArch::ATOMIC_LOAD_UMAX_I16_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_UMIN_I8:
    AtomicOp = LoongArch::ATOMIC_LOAD_UMIN_I8_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_UMIN_I16:
    AtomicOp = LoongArch::ATOMIC_LOAD_UMIN_I16_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_ADD_I8:
    AtomicOp = LoongArch::ATOMIC_LOAD_ADD_I8_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_ADD_I16:
    AtomicOp = LoongArch::ATOMIC_LOAD_ADD_I16_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_SUB_I8:
    AtomicOp = LoongArch::ATOMIC_LOAD_SUB_I8_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_SUB_I16:
    AtomicOp = LoongArch::ATOMIC_LOAD_SUB_I16_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_AND_I8:
    AtomicOp = LoongArch::ATOMIC_LOAD_AND_I8_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_AND_I16:
    AtomicOp = LoongArch::ATOMIC_LOAD_AND_I16_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_OR_I8:
    AtomicOp = LoongArch::ATOMIC_LOAD_OR_I8_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_OR_I16:
    AtomicOp = LoongArch::ATOMIC_LOAD_OR_I16_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_XOR_I8:
    AtomicOp = LoongArch::ATOMIC_LOAD_XOR_I8_POSTRA;
    break;
  case LoongArch::ATOMIC_LOAD_XOR_I16:
    AtomicOp = LoongArch::ATOMIC_LOAD_XOR_I16_POSTRA;
    break;
  default:
    llvm_unreachable("Unknown subword atomic pseudo for expansion!");
  }

  // insert new blocks after the current block
  const BasicBlock *LLVM_BB = BB->getBasicBlock();
  MachineBasicBlock *exitMBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineFunction::iterator It = ++BB->getIterator();
  MF->insert(It, exitMBB);

  // Transfer the remainder of BB and its successor edges to exitMBB.
  exitMBB->splice(exitMBB->begin(), BB,
                  std::next(MachineBasicBlock::iterator(MI)), BB->end());
  exitMBB->transferSuccessorsAndUpdatePHIs(BB);

  BB->addSuccessor(exitMBB, BranchProbability::getOne());

  //  thisMBB:
  //    addiu   masklsb2,$0,-4                # 0xfffffffc
  //    and     alignedaddr,ptr,masklsb2
  //    andi    ptrlsb2,ptr,3
  //    sll     shiftamt,ptrlsb2,3
  //    ori     maskupper,$0,255               # 0xff
  //    sll     mask,maskupper,shiftamt
  //    nor     mask2,$0,mask
  //    sll     incr2,incr,shiftamt

  BuildMI(BB, DL, TII->get(LoongArch::DBAR)).addImm(0);
  int64_t MaskImm = (Size == 1) ? 255 : 4095;
  BuildMI(BB, DL, TII->get(ABI.GetPtrAddiOp()), MaskLSB2)
    .addReg(ABI.GetNullPtr()).addImm(-4);
  BuildMI(BB, DL, TII->get(ABI.GetPtrAndOp()), AlignedAddr)
    .addReg(Ptr).addReg(MaskLSB2);
  BuildMI(BB, DL, TII->get(LoongArch::ANDI32), PtrLSB2)
      .addReg(Ptr, 0, ArePtrs64bit ? LoongArch::sub_32 : 0).addImm(3);
  BuildMI(BB, DL, TII->get(LoongArch::SLLI_W), ShiftAmt).addReg(PtrLSB2).addImm(3);

  if(MaskImm==4095){
  BuildMI(BB, DL, TII->get(LoongArch::LU12I_W32), MaskUppest).addImm(0xf);
  BuildMI(BB, DL, TII->get(LoongArch::ORI32), MaskUpper)
    .addReg(MaskUppest).addImm(MaskImm);
  }
  else{
  BuildMI(BB, DL, TII->get(LoongArch::ORI32), MaskUpper)
    .addReg(LoongArch::ZERO).addImm(MaskImm);
  }

  BuildMI(BB, DL, TII->get(LoongArch::SLL_W), Mask)
    .addReg(MaskUpper).addReg(ShiftAmt);
  BuildMI(BB, DL, TII->get(LoongArch::NOR32), Mask2).addReg(LoongArch::ZERO).addReg(Mask);
  BuildMI(BB, DL, TII->get(LoongArch::SLL_W), Incr2).addReg(Incr).addReg(ShiftAmt);


  // The purposes of the flags on the scratch registers is explained in
  // emitAtomicBinary. In summary, we need a scratch register which is going to
  // be undef, that is unique among registers chosen for the instruction.

  BuildMI(BB, DL, TII->get(AtomicOp))
      .addReg(Dest, RegState::Define | RegState::EarlyClobber)
      .addReg(AlignedAddr)
      .addReg(Incr2)
      .addReg(Mask)
      .addReg(Mask2)
      .addReg(ShiftAmt)
      .addReg(Scratch, RegState::EarlyClobber | RegState::Define |
                           RegState::Dead | RegState::Implicit)
      .addReg(Scratch2, RegState::EarlyClobber | RegState::Define |
                            RegState::Dead | RegState::Implicit)
      .addReg(Scratch3, RegState::EarlyClobber | RegState::Define |
                            RegState::Dead | RegState::Implicit);

  BuildMI(BB, DL, TII->get(LoongArch::DBAR)).addImm(0);

  MI.eraseFromParent(); // The instruction is gone now.

  return exitMBB;
}

// Lower atomic compare and swap to a pseudo instruction, taking care to
// define a scratch register for the pseudo instruction's expansion. The
// instruction is expanded after the register allocator as to prevent
// the insertion of stores between the linked load and the store conditional.

MachineBasicBlock *
LoongArchTargetLowering::emitAtomicCmpSwap(MachineInstr &MI,
                                      MachineBasicBlock *BB) const {
  assert((MI.getOpcode() == LoongArch::ATOMIC_CMP_SWAP_I32 ||
          MI.getOpcode() == LoongArch::ATOMIC_CMP_SWAP_I64) &&
         "Unsupported atomic psseudo for EmitAtomicCmpSwap.");

  const unsigned Size = MI.getOpcode() == LoongArch::ATOMIC_CMP_SWAP_I32 ? 4 : 8;

  MachineFunction *MF = BB->getParent();
  MachineRegisterInfo &MRI = MF->getRegInfo();
  const TargetRegisterClass *RC = getRegClassFor(MVT::getIntegerVT(Size * 8));
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();

  unsigned AtomicOp = MI.getOpcode() == LoongArch::ATOMIC_CMP_SWAP_I32
                          ? LoongArch::ATOMIC_CMP_SWAP_I32_POSTRA
                          : LoongArch::ATOMIC_CMP_SWAP_I64_POSTRA;
  unsigned Dest = MI.getOperand(0).getReg();
  unsigned Ptr = MI.getOperand(1).getReg();
  unsigned OldVal = MI.getOperand(2).getReg();
  unsigned NewVal = MI.getOperand(3).getReg();

  unsigned Scratch = MRI.createVirtualRegister(RC);
  MachineBasicBlock::iterator II(MI);

  // We need to create copies of the various registers and kill them at the
  // atomic pseudo. If the copies are not made, when the atomic is expanded
  // after fast register allocation, the spills will end up outside of the
  // blocks that their values are defined in, causing livein errors.

  unsigned DestCopy = MRI.createVirtualRegister(MRI.getRegClass(Dest));
  unsigned PtrCopy = MRI.createVirtualRegister(MRI.getRegClass(Ptr));
  unsigned OldValCopy = MRI.createVirtualRegister(MRI.getRegClass(OldVal));
  unsigned NewValCopy = MRI.createVirtualRegister(MRI.getRegClass(NewVal));

  BuildMI(*BB, II, DL, TII->get(LoongArch::DBAR)).addImm(0);
  BuildMI(*BB, II, DL, TII->get(LoongArch::COPY), DestCopy).addReg(Dest);
  BuildMI(*BB, II, DL, TII->get(LoongArch::COPY), PtrCopy).addReg(Ptr);
  BuildMI(*BB, II, DL, TII->get(LoongArch::COPY), OldValCopy).addReg(OldVal);
  BuildMI(*BB, II, DL, TII->get(LoongArch::COPY), NewValCopy).addReg(NewVal);

  // The purposes of the flags on the scratch registers is explained in
  // emitAtomicBinary. In summary, we need a scratch register which is going to
  // be undef, that is unique among registers chosen for the instruction.

  BuildMI(*BB, II, DL, TII->get(AtomicOp))
      .addReg(Dest, RegState::Define | RegState::EarlyClobber)
      .addReg(PtrCopy, RegState::Kill)
      .addReg(OldValCopy, RegState::Kill)
      .addReg(NewValCopy, RegState::Kill)
      .addReg(Scratch, RegState::EarlyClobber | RegState::Define |
                           RegState::Dead | RegState::Implicit);

  BuildMI(*BB, II, DL, TII->get(LoongArch::DBAR)).addImm(0);

  MI.eraseFromParent(); // The instruction is gone now.

  return BB;
}

MachineBasicBlock *LoongArchTargetLowering::emitAtomicCmpSwapPartword(
    MachineInstr &MI, MachineBasicBlock *BB, unsigned Size) const {
  assert((Size == 1 || Size == 2) &&
      "Unsupported size for EmitAtomicCmpSwapPartial.");

  MachineFunction *MF = BB->getParent();
  MachineRegisterInfo &RegInfo = MF->getRegInfo();
  const TargetRegisterClass *RC = getRegClassFor(MVT::i32);
  const bool ArePtrs64bit = ABI.ArePtrs64bit();
  const TargetRegisterClass *RCp =
    getRegClassFor(ArePtrs64bit ? MVT::i64 : MVT::i32);
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();

  unsigned Dest = MI.getOperand(0).getReg();
  unsigned Ptr = MI.getOperand(1).getReg();
  unsigned CmpVal = MI.getOperand(2).getReg();
  unsigned NewVal = MI.getOperand(3).getReg();

  unsigned AlignedAddr = RegInfo.createVirtualRegister(RCp);
  unsigned ShiftAmt = RegInfo.createVirtualRegister(RC);
  unsigned Mask = RegInfo.createVirtualRegister(RC);
  unsigned Mask2 = RegInfo.createVirtualRegister(RC);
  unsigned ShiftedCmpVal = RegInfo.createVirtualRegister(RC);
  unsigned ShiftedNewVal = RegInfo.createVirtualRegister(RC);
  unsigned MaskLSB2 = RegInfo.createVirtualRegister(RCp);
  unsigned PtrLSB2 = RegInfo.createVirtualRegister(RC);
  unsigned MaskUpper = RegInfo.createVirtualRegister(RC);
  unsigned MaskUppest = RegInfo.createVirtualRegister(RC);
  unsigned Mask3 = RegInfo.createVirtualRegister(RC);
  unsigned MaskedCmpVal = RegInfo.createVirtualRegister(RC);
  unsigned MaskedNewVal = RegInfo.createVirtualRegister(RC);
  unsigned AtomicOp = MI.getOpcode() == LoongArch::ATOMIC_CMP_SWAP_I8
                          ? LoongArch::ATOMIC_CMP_SWAP_I8_POSTRA
                          : LoongArch::ATOMIC_CMP_SWAP_I16_POSTRA;

  // The scratch registers here with the EarlyClobber | Define | Dead | Implicit
  // flags are used to coerce the register allocator and the machine verifier to
  // accept the usage of these registers.
  // The EarlyClobber flag has the semantic properties that the operand it is
  // attached to is clobbered before the rest of the inputs are read. Hence it
  // must be unique among the operands to the instruction.
  // The Define flag is needed to coerce the machine verifier that an Undef
  // value isn't a problem.
  // The Dead flag is needed as the value in scratch isn't used by any other
  // instruction. Kill isn't used as Dead is more precise.
  unsigned Scratch = RegInfo.createVirtualRegister(RC);
  unsigned Scratch2 = RegInfo.createVirtualRegister(RC);

  // insert new blocks after the current block
  const BasicBlock *LLVM_BB = BB->getBasicBlock();
  MachineBasicBlock *exitMBB = MF->CreateMachineBasicBlock(LLVM_BB);
  MachineFunction::iterator It = ++BB->getIterator();
  MF->insert(It, exitMBB);

  // Transfer the remainder of BB and its successor edges to exitMBB.
  exitMBB->splice(exitMBB->begin(), BB,
                  std::next(MachineBasicBlock::iterator(MI)), BB->end());
  exitMBB->transferSuccessorsAndUpdatePHIs(BB);

  BB->addSuccessor(exitMBB, BranchProbability::getOne());

  //  thisMBB:
  //    addiu   masklsb2,$0,-4                # 0xfffffffc
  //    and     alignedaddr,ptr,masklsb2
  //    andi    ptrlsb2,ptr,3
  //    xori    ptrlsb2,ptrlsb2,3              # Only for BE
  //    sll     shiftamt,ptrlsb2,3
  //    ori     maskupper,$0,255               # 0xff
  //    sll     mask,maskupper,shiftamt
  //    nor     mask2,$0,mask
  //    andi    maskedcmpval,cmpval,255
  //    sll     shiftedcmpval,maskedcmpval,shiftamt
  //    andi    maskednewval,newval,255
  //    sll     shiftednewval,maskednewval,shiftamt

  BuildMI(BB, DL, TII->get(LoongArch::DBAR)).addImm(0);

  int64_t MaskImm = (Size == 1) ? 255 : 4095;
  BuildMI(BB, DL, TII->get(ArePtrs64bit ? LoongArch::ADDI_D : LoongArch::ADDI_W), MaskLSB2)
    .addReg(ABI.GetNullPtr()).addImm(-4);
  BuildMI(BB, DL, TII->get(ArePtrs64bit ? LoongArch::AND : LoongArch::AND32), AlignedAddr)
    .addReg(Ptr).addReg(MaskLSB2);
  BuildMI(BB, DL, TII->get(LoongArch::ANDI32), PtrLSB2)
      .addReg(Ptr, 0, ArePtrs64bit ? LoongArch::sub_32 : 0).addImm(3);
  BuildMI(BB, DL, TII->get(LoongArch::SLLI_W), ShiftAmt).addReg(PtrLSB2).addImm(3);

  if(MaskImm==4095){
  BuildMI(BB, DL, TII->get(LoongArch::LU12I_W32), MaskUppest).addImm(0xf);
  BuildMI(BB, DL, TII->get(LoongArch::ORI32), MaskUpper)
    .addReg(MaskUppest).addImm(MaskImm);
  }
  else{
  BuildMI(BB, DL, TII->get(LoongArch::ORI32), MaskUpper)
    .addReg(LoongArch::ZERO).addImm(MaskImm);
  }

  BuildMI(BB, DL, TII->get(LoongArch::SLL_W), Mask)
    .addReg(MaskUpper).addReg(ShiftAmt);
  BuildMI(BB, DL, TII->get(LoongArch::NOR32), Mask2).addReg(LoongArch::ZERO).addReg(Mask);
  if(MaskImm==4095){
    BuildMI(BB, DL, TII->get(LoongArch::ORI32), Mask3)
    .addReg(MaskUppest).addImm(MaskImm);
    BuildMI(BB, DL, TII->get(LoongArch::AND32), MaskedCmpVal)
      .addReg(CmpVal).addReg(Mask3);
    BuildMI(BB, DL, TII->get(LoongArch::SLL_W), ShiftedCmpVal)
      .addReg(MaskedCmpVal).addReg(ShiftAmt);
    BuildMI(BB, DL, TII->get(LoongArch::AND32), MaskedNewVal)
      .addReg(NewVal).addReg(Mask3);
  }
  else{
    BuildMI(BB, DL, TII->get(LoongArch::ANDI32), MaskedCmpVal)
      .addReg(CmpVal).addImm(MaskImm);
    BuildMI(BB, DL, TII->get(LoongArch::SLL_W), ShiftedCmpVal)
      .addReg(MaskedCmpVal).addReg(ShiftAmt);
    BuildMI(BB, DL, TII->get(LoongArch::ANDI32), MaskedNewVal)
      .addReg(NewVal).addImm(MaskImm);
  }
  BuildMI(BB, DL, TII->get(LoongArch::SLL_W), ShiftedNewVal)
    .addReg(MaskedNewVal).addReg(ShiftAmt);

  // The purposes of the flags on the scratch registers are explained in
  // emitAtomicBinary. In summary, we need a scratch register which is going to
  // be undef, that is unique among the register chosen for the instruction.

  BuildMI(BB, DL, TII->get(AtomicOp))
      .addReg(Dest, RegState::Define | RegState::EarlyClobber)
      .addReg(AlignedAddr)
      .addReg(Mask)
      .addReg(ShiftedCmpVal)
      .addReg(Mask2)
      .addReg(ShiftedNewVal)
      .addReg(ShiftAmt)
      .addReg(Scratch, RegState::EarlyClobber | RegState::Define |
                           RegState::Dead | RegState::Implicit)
      .addReg(Scratch2, RegState::EarlyClobber | RegState::Define |
                            RegState::Dead | RegState::Implicit);

  BuildMI(BB, DL, TII->get(LoongArch::DBAR)).addImm(0);

  MI.eraseFromParent(); // The instruction is gone now.

  return exitMBB;
}

SDValue LoongArchTargetLowering::lowerBRCOND(SDValue Op, SelectionDAG &DAG) const {
  // The first operand is the chain, the second is the condition, the third is
  // the block to branch to if the condition is true.
  SDValue Chain = Op.getOperand(0);
  SDValue Dest = Op.getOperand(2);
  SDLoc DL(Op);

  SDValue CondRes = createFPCmp(DAG, Op.getOperand(1));

  // Return if flag is not set by a floating point comparison.
  if (CondRes.getOpcode() != LoongArchISD::FPCmp)
    return Op;

  SDValue CCNode  = CondRes.getOperand(2);
  LoongArch::CondCode CC =
    (LoongArch::CondCode)cast<ConstantSDNode>(CCNode)->getZExtValue();
  unsigned Opc = invertFPCondCodeUser(CC) ? LoongArch::BRANCH_F : LoongArch::BRANCH_T;
  SDValue BrCode = DAG.getConstant(Opc, DL, MVT::i32);
  SDValue FCC0 = DAG.getRegister(LoongArch::FCC0, MVT::i32);
  return DAG.getNode(LoongArchISD::FPBrcond, DL, Op.getValueType(), Chain, BrCode,
                     FCC0, Dest, CondRes);
}

SDValue LoongArchTargetLowering::
lowerSELECT(SDValue Op, SelectionDAG &DAG) const
{
  SDValue Cond = createFPCmp(DAG, Op.getOperand(0));

  // Return if flag is not set by a floating point comparison.
  if (Cond.getOpcode() != LoongArchISD::FPCmp)
    return Op;

  return createCMovFP(DAG, Cond, Op.getOperand(1), Op.getOperand(2),
                      SDLoc(Op));
}

SDValue LoongArchTargetLowering::lowerSETCC(SDValue Op, SelectionDAG &DAG) const {
  SDValue Cond = createFPCmp(DAG, Op);

  assert(Cond.getOpcode() == LoongArchISD::FPCmp &&
         "Floating point operand expected.");

  SDLoc DL(Op);
  SDValue True  = DAG.getConstant(1, DL, MVT::i32);
  SDValue False = DAG.getConstant(0, DL, MVT::i32);

  return createCMovFP(DAG, Cond, True, False, DL);
}

SDValue LoongArchTargetLowering::lowerGlobalAddress(SDValue Op,
                                               SelectionDAG &DAG) const {
  GlobalAddressSDNode *N = cast<GlobalAddressSDNode>(Op);

  const GlobalValue *GV = N->getGlobal();
  bool IsLocal = getTargetMachine().shouldAssumeDSOLocal(*GV->getParent(), GV);
  SDValue Addr = getAddr(N, DAG, IsLocal);

  return Addr;
}

SDValue LoongArchTargetLowering::lowerBlockAddress(SDValue Op,
                                              SelectionDAG &DAG) const {
  BlockAddressSDNode *N = cast<BlockAddressSDNode>(Op);

  return getAddr(N, DAG);
}

SDValue LoongArchTargetLowering::
lowerGlobalTLSAddress(SDValue Op, SelectionDAG &DAG) const
{
  GlobalAddressSDNode *GA = cast<GlobalAddressSDNode>(Op);
  if (DAG.getTarget().useEmulatedTLS())
    return LowerToTLSEmulatedModel(GA, DAG);

  SDLoc DL(GA);
  const GlobalValue *GV = GA->getGlobal();
  EVT PtrVT = getPointerTy(DAG.getDataLayout());

  TLSModel::Model model = getTargetMachine().getTLSModel(GV);

  if (model == TLSModel::GeneralDynamic || model == TLSModel::LocalDynamic) {
    // General Dynamic TLS Model && Local Dynamic TLS Model
    unsigned PtrSize = PtrVT.getSizeInBits();
    IntegerType *PtrTy = Type::getIntNTy(*DAG.getContext(), PtrSize);
    //  SDValue Addr = DAG.getTargetGlobalAddress(GV, DL, PtrTy, 0, 0);
    SDValue Addr = DAG.getTargetGlobalAddress(GV, DL, PtrVT, 0, 0U);
    SDValue Load = SDValue(DAG.getMachineNode(LoongArch::LoadAddrTLS_GD ,
                           DL, PtrVT, Addr), 0);
    SDValue TlsGetAddr = DAG.getExternalSymbol("__tls_get_addr", PtrVT);

    ArgListTy Args;
    ArgListEntry Entry;
    Entry.Node = Load;
    Entry.Ty = PtrTy;
    Args.push_back(Entry);

    TargetLowering::CallLoweringInfo CLI(DAG);
    CLI.setDebugLoc(DL)
       .setChain(DAG.getEntryNode())
       .setLibCallee(CallingConv::C, PtrTy, TlsGetAddr, std::move(Args));
    std::pair<SDValue, SDValue> CallResult = LowerCallTo(CLI);

    SDValue Ret = CallResult.first;

    return Ret;
  }

  SDValue Addr = DAG.getTargetGlobalAddress(GV, DL, PtrVT, 0, 0U);
  SDValue Offset;
  if (model == TLSModel::InitialExec) {
    // Initial Exec TLS Model
    Offset = SDValue(DAG.getMachineNode(LoongArch::LoadAddrTLS_IE, DL,
                     PtrVT, Addr), 0);
  } else {
    // Local Exec TLS Model
    assert(model == TLSModel::LocalExec);
    Offset = SDValue(DAG.getMachineNode(LoongArch::LoadAddrTLS_LE, DL,
                     PtrVT, Addr), 0);
  }

  SDValue ThreadPointer = DAG.getRegister((PtrVT == MVT::i32)
                                          ? LoongArch::TP
                                          : LoongArch::TP_64, PtrVT);
  return DAG.getNode(ISD::ADD, DL, PtrVT, ThreadPointer, Offset);
}

SDValue LoongArchTargetLowering::
lowerJumpTable(SDValue Op, SelectionDAG &DAG) const
{
  JumpTableSDNode *N = cast<JumpTableSDNode>(Op);

  return getAddr(N, DAG);
}

SDValue LoongArchTargetLowering::
lowerConstantPool(SDValue Op, SelectionDAG &DAG) const
{
  ConstantPoolSDNode *N = cast<ConstantPoolSDNode>(Op);

  return getAddr(N, DAG);
}

SDValue LoongArchTargetLowering::lowerVASTART(SDValue Op, SelectionDAG &DAG) const {
  MachineFunction &MF = DAG.getMachineFunction();
  LoongArchFunctionInfo *FuncInfo = MF.getInfo<LoongArchFunctionInfo>();

  SDLoc DL(Op);
  SDValue FI = DAG.getFrameIndex(FuncInfo->getVarArgsFrameIndex(),
                                 getPointerTy(MF.getDataLayout()));

  // vastart just stores the address of the VarArgsFrameIndex slot into the
  // memory location argument.
  const Value *SV = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();
  return DAG.getStore(Op.getOperand(0), DL, FI, Op.getOperand(1),
                      MachinePointerInfo(SV));
}

SDValue LoongArchTargetLowering::lowerVAARG(SDValue Op, SelectionDAG &DAG) const {
  SDNode *Node = Op.getNode();
  EVT VT = Node->getValueType(0);
  SDValue Chain = Node->getOperand(0);
  SDValue VAListPtr = Node->getOperand(1);
  const Align Align =
      llvm::MaybeAlign(Node->getConstantOperandVal(3)).valueOrOne();
  const Value *SV = cast<SrcValueSDNode>(Node->getOperand(2))->getValue();
  SDLoc DL(Node);
  unsigned ArgSlotSizeInBytes = (ABI.IsLPX32() || ABI.IsLP64()) ? 8 : 4;

  SDValue VAListLoad = DAG.getLoad(getPointerTy(DAG.getDataLayout()), DL, Chain,
                                   VAListPtr, MachinePointerInfo(SV));
  SDValue VAList = VAListLoad;

  // Re-align the pointer if necessary.
  // It should only ever be necessary for 64-bit types on LP32 since the minimum
  // argument alignment is the same as the maximum type alignment for LPX32/LP64.
  //
  // FIXME: We currently align too often. The code generator doesn't notice
  //        when the pointer is still aligned from the last va_arg (or pair of
  //        va_args for the i64 on LP32 case).
  if (Align > getMinStackArgumentAlignment()) {
    VAList = DAG.getNode(
        ISD::ADD, DL, VAList.getValueType(), VAList,
        DAG.getConstant(Align.value() - 1, DL, VAList.getValueType()));

    VAList = DAG.getNode(
        ISD::AND, DL, VAList.getValueType(), VAList,
        DAG.getConstant(-(int64_t)Align.value(), DL, VAList.getValueType()));
  }

  // Increment the pointer, VAList, to the next vaarg.
  auto &TD = DAG.getDataLayout();
  unsigned ArgSizeInBytes =
      TD.getTypeAllocSize(VT.getTypeForEVT(*DAG.getContext()));
  SDValue Tmp3 =
      DAG.getNode(ISD::ADD, DL, VAList.getValueType(), VAList,
                  DAG.getConstant(alignTo(ArgSizeInBytes, ArgSlotSizeInBytes),
                                  DL, VAList.getValueType()));
  // Store the incremented VAList to the legalized pointer
  Chain = DAG.getStore(VAListLoad.getValue(1), DL, Tmp3, VAListPtr,
                       MachinePointerInfo(SV));

  // Load the actual argument out of the pointer VAList
  return DAG.getLoad(VT, DL, Chain, VAList, MachinePointerInfo());
}

static SDValue lowerFCOPYSIGLPX32(SDValue Op, SelectionDAG &DAG) {
  // TODO:
  return SDValue();
}

static SDValue lowerFCOPYSIGLP64(SDValue Op, SelectionDAG &DAG) {
  unsigned WidthX = Op.getOperand(0).getValueSizeInBits();
  unsigned WidthY = Op.getOperand(1).getValueSizeInBits();
  EVT TyX = MVT::getIntegerVT(WidthX), TyY = MVT::getIntegerVT(WidthY);
  SDLoc DL(Op);

  // Bitcast to integer nodes.
  SDValue X = DAG.getNode(ISD::BITCAST, DL, TyX, Op.getOperand(0));
  SDValue Y = DAG.getNode(ISD::BITCAST, DL, TyY, Op.getOperand(1));

  // bstrpick  E, Y, width(Y) - 1, width(Y) - 1  ; extract bit width(Y)-1 of Y
  // bstrins  X, E, width(X) - 1, width(X) - 1  ; insert extracted bit at bit width(X)-1 of X
  SDValue E = DAG.getNode(LoongArchISD::BSTRPICK, DL, TyY, Y,
                          DAG.getConstant(WidthY - 1, DL, MVT::i32), DAG.getConstant(WidthY - 1, DL, MVT::i32));

  if (WidthX > WidthY)
    E = DAG.getNode(ISD::ZERO_EXTEND, DL, TyX, E);
  else if (WidthY > WidthX)
    E = DAG.getNode(ISD::TRUNCATE, DL, TyX, E);

  SDValue I = DAG.getNode(LoongArchISD::BSTRINS, DL, TyX, E,
                          DAG.getConstant(WidthX - 1, DL, MVT::i32), DAG.getConstant(WidthX - 1, DL, MVT::i32),
                          X);
  return DAG.getNode(ISD::BITCAST, DL, Op.getOperand(0).getValueType(), I);
}

SDValue
LoongArchTargetLowering::lowerFCOPYSIGN(SDValue Op, SelectionDAG &DAG) const {
  if (Subtarget.is64Bit())
    return lowerFCOPYSIGLP64(Op, DAG);

  return lowerFCOPYSIGLPX32(Op, DAG);
}

SDValue LoongArchTargetLowering::
lowerFRAMEADDR(SDValue Op, SelectionDAG &DAG) const {
  // check the depth
  assert((cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue() == 0) &&
         "Frame address can only be determined for current frame.");

  MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();
  MFI.setFrameAddressIsTaken(true);
  EVT VT = Op.getValueType();
  SDLoc DL(Op);
  SDValue FrameAddr = DAG.getCopyFromReg(
      DAG.getEntryNode(), DL, ABI.IsLP64() ? LoongArch::FP_64 : LoongArch::FP, VT);
  return FrameAddr;
}

SDValue LoongArchTargetLowering::lowerRETURNADDR(SDValue Op,
                                            SelectionDAG &DAG) const {
  if (verifyReturnAddressArgumentIsConstant(Op, DAG))
    return SDValue();

  // check the depth
  assert((cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue() == 0) &&
         "Return address can be determined only for current frame.");

  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  MVT VT = Op.getSimpleValueType();
  unsigned RA = ABI.IsLP64() ? LoongArch::RA_64 : LoongArch::RA;
  MFI.setReturnAddressIsTaken(true);

  // Return RA, which contains the return address. Mark it an implicit live-in.
  unsigned Reg = MF.addLiveIn(RA, getRegClassFor(VT));
  return DAG.getCopyFromReg(DAG.getEntryNode(), SDLoc(Op), Reg, VT);
}

// An EH_RETURN is the result of lowering llvm.eh.return which in turn is
// generated from __builtin_eh_return (offset, handler)
// The effect of this is to adjust the stack pointer by "offset"
// and then branch to "handler".
SDValue LoongArchTargetLowering::lowerEH_RETURN(SDValue Op, SelectionDAG &DAG)
                                                                     const {
  MachineFunction &MF = DAG.getMachineFunction();
  LoongArchFunctionInfo *LoongArchFI = MF.getInfo<LoongArchFunctionInfo>();

  LoongArchFI->setCallsEhReturn();
  SDValue Chain     = Op.getOperand(0);
  SDValue Offset    = Op.getOperand(1);
  SDValue Handler   = Op.getOperand(2);
  SDLoc DL(Op);
  EVT Ty = ABI.IsLP64() ? MVT::i64 : MVT::i32;

  // Store stack offset in A1, store jump target in A0. Glue CopyToReg and
  // EH_RETURN nodes, so that instructions are emitted back-to-back.
  unsigned OffsetReg = ABI.IsLP64() ? LoongArch::A1_64 : LoongArch::A1;
  unsigned AddrReg = ABI.IsLP64() ? LoongArch::A0_64 : LoongArch::A0;
  Chain = DAG.getCopyToReg(Chain, DL, OffsetReg, Offset, SDValue());
  Chain = DAG.getCopyToReg(Chain, DL, AddrReg, Handler, Chain.getValue(1));
  return DAG.getNode(LoongArchISD::EH_RETURN, DL, MVT::Other, Chain,
                     DAG.getRegister(OffsetReg, Ty),
                     DAG.getRegister(AddrReg, getPointerTy(MF.getDataLayout())),
                     Chain.getValue(1));
}

SDValue LoongArchTargetLowering::lowerATOMIC_FENCE(SDValue Op,
                                              SelectionDAG &DAG) const {
  // FIXME: Need pseudo-fence for 'singlethread' fences
  // FIXME: Set SType for weaker fences where supported/appropriate.
  unsigned SType = 0;
  SDLoc DL(Op);
  return DAG.getNode(LoongArchISD::DBAR, DL, MVT::Other, Op.getOperand(0),
                     DAG.getConstant(SType, DL, MVT::i32));
}

SDValue LoongArchTargetLowering::lowerShiftLeftParts(SDValue Op,
                                                SelectionDAG &DAG) const {
  SDLoc DL(Op);
  MVT VT = Subtarget.is64Bit() ? MVT::i64 : MVT::i32;

  SDValue Lo = Op.getOperand(0), Hi = Op.getOperand(1);
  SDValue Shamt = Op.getOperand(2);
  // if shamt < (VT.bits):
  //  lo = (shl lo, shamt)
  //  hi = (or (shl hi, shamt) (srl (srl lo, 1), ~shamt))
  // else:
  //  lo = 0
  //  hi = (shl lo, shamt[4:0])
  SDValue Not = DAG.getNode(ISD::XOR, DL, MVT::i32, Shamt,
                            DAG.getConstant(-1, DL, MVT::i32));
  SDValue ShiftRight1Lo = DAG.getNode(ISD::SRL, DL, VT, Lo,
                                      DAG.getConstant(1, DL, VT));
  SDValue ShiftRightLo = DAG.getNode(ISD::SRL, DL, VT, ShiftRight1Lo, Not);
  SDValue ShiftLeftHi = DAG.getNode(ISD::SHL, DL, VT, Hi, Shamt);
  SDValue Or = DAG.getNode(ISD::OR, DL, VT, ShiftLeftHi, ShiftRightLo);
  SDValue ShiftLeftLo = DAG.getNode(ISD::SHL, DL, VT, Lo, Shamt);
  SDValue Cond = DAG.getNode(ISD::AND, DL, MVT::i32, Shamt,
                             DAG.getConstant(VT.getSizeInBits(), DL, MVT::i32));
  Lo = DAG.getNode(ISD::SELECT, DL, VT, Cond,
                   DAG.getConstant(0, DL, VT), ShiftLeftLo);
  Hi = DAG.getNode(ISD::SELECT, DL, VT, Cond, ShiftLeftLo, Or);

  SDValue Ops[2] = {Lo, Hi};
  return DAG.getMergeValues(Ops, DL);
}

SDValue LoongArchTargetLowering::lowerShiftRightParts(SDValue Op, SelectionDAG &DAG,
                                                 bool IsSRA) const {
  SDLoc DL(Op);
  SDValue Lo = Op.getOperand(0), Hi = Op.getOperand(1);
  SDValue Shamt = Op.getOperand(2);
  MVT VT = Subtarget.is64Bit() ? MVT::i64 : MVT::i32;

  // if shamt < (VT.bits):
  //  lo = (or (shl (shl hi, 1), ~shamt) (srl lo, shamt))
  //  if isSRA:
  //    hi = (sra hi, shamt)
  //  else:
  //    hi = (srl hi, shamt)
  // else:
  //  if isSRA:
  //   lo = (sra hi, shamt[4:0])
  //   hi = (sra hi, 31)
  //  else:
  //   lo = (srl hi, shamt[4:0])
  //   hi = 0
  SDValue Not = DAG.getNode(ISD::XOR, DL, MVT::i32, Shamt,
                            DAG.getConstant(-1, DL, MVT::i32));
  SDValue ShiftLeft1Hi = DAG.getNode(ISD::SHL, DL, VT, Hi,
                                     DAG.getConstant(1, DL, VT));
  SDValue ShiftLeftHi = DAG.getNode(ISD::SHL, DL, VT, ShiftLeft1Hi, Not);
  SDValue ShiftRightLo = DAG.getNode(ISD::SRL, DL, VT, Lo, Shamt);
  SDValue Or = DAG.getNode(ISD::OR, DL, VT, ShiftLeftHi, ShiftRightLo);
  SDValue ShiftRightHi = DAG.getNode(IsSRA ? ISD::SRA : ISD::SRL,
                                     DL, VT, Hi, Shamt);
  SDValue Cond = DAG.getNode(ISD::AND, DL, MVT::i32, Shamt,
                             DAG.getConstant(VT.getSizeInBits(), DL, MVT::i32));
  SDValue Ext = DAG.getNode(ISD::SRA, DL, VT, Hi,
                            DAG.getConstant(VT.getSizeInBits() - 1, DL, VT));
  Lo = DAG.getNode(ISD::SELECT, DL, VT, Cond, ShiftRightHi, Or);
  Hi = DAG.getNode(ISD::SELECT, DL, VT, Cond,
                   IsSRA ? Ext : DAG.getConstant(0, DL, VT), ShiftRightHi);

  SDValue Ops[2] = {Lo, Hi};
  return DAG.getMergeValues(Ops, DL);
}

// Lower (store (fp_to_sint $fp) $ptr) to (store (TruncIntFP $fp), $ptr).
static SDValue lowerFP_TO_SINT_STORE(StoreSDNode *SD, SelectionDAG &DAG,
                                     bool SingleFloat) {
  SDValue Val = SD->getValue();

  if (Val.getOpcode() != ISD::FP_TO_SINT ||
      (Val.getValueSizeInBits() > 32 && SingleFloat))
    return SDValue();

  EVT FPTy = EVT::getFloatingPointVT(Val.getValueSizeInBits());
  SDValue Tr = DAG.getNode(LoongArchISD::TruncIntFP, SDLoc(Val), FPTy,
                           Val.getOperand(0));
  return DAG.getStore(SD->getChain(), SDLoc(SD), Tr, SD->getBasePtr(),
                      SD->getPointerInfo(), SD->getAlignment(),
                      SD->getMemOperand()->getFlags());
}

SDValue LoongArchTargetLowering::lowerSTORE(SDValue Op, SelectionDAG &DAG) const {
  StoreSDNode *SD = cast<StoreSDNode>(Op);
  return lowerFP_TO_SINT_STORE(SD, DAG, Subtarget.isSingleFloat());
}

SDValue LoongArchTargetLowering::lowerINTRINSIC_WO_CHAIN(SDValue Op,
                                                      SelectionDAG &DAG) const {
  SDLoc DL(Op);
  unsigned Intrinsic = cast<ConstantSDNode>(Op->getOperand(0))->getZExtValue();
  switch (Intrinsic) {
  default:
    return SDValue();
  case Intrinsic::thread_pointer: {
    EVT PtrVT = getPointerTy(DAG.getDataLayout());
    if (PtrVT ==  MVT::i64)
      return DAG.getRegister(LoongArch::TP_64, MVT::i64);
    return DAG.getRegister(LoongArch::TP, MVT::i32);
  }
  }
}

SDValue LoongArchTargetLowering::lowerINTRINSIC_W_CHAIN(SDValue Op,
                                                     SelectionDAG &DAG) const {
  return SDValue();
}

SDValue LoongArchTargetLowering::lowerINTRINSIC_VOID(SDValue Op,
                                                  SelectionDAG &DAG) const {
  unsigned Intr = cast<ConstantSDNode>(Op->getOperand(1))->getZExtValue();
  switch (Intr) {
  default:
    return SDValue();
  }
}

SDValue LoongArchTargetLowering::lowerUINT_TO_FP(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  EVT ResTy = Op->getValueType(0);
  Op = LowerSUINT_TO_FP(ISD::ZERO_EXTEND_VECTOR_INREG, Op, DAG);
  if (!ResTy.isVector())
    return Op;
  return DAG.getNode(ISD::UINT_TO_FP, DL, ResTy, Op);
}

SDValue LoongArchTargetLowering::lowerSINT_TO_FP(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  EVT ResTy = Op->getValueType(0);
  Op = LowerSUINT_TO_FP(ISD::SIGN_EXTEND_VECTOR_INREG, Op, DAG);
  if (!ResTy.isVector())
    return Op;
  return DAG.getNode(ISD::SINT_TO_FP, DL, ResTy, Op);
}

SDValue LoongArchTargetLowering::lowerFP_TO_UINT(SDValue Op, SelectionDAG &DAG) const {
  return SDValue();
}

SDValue LoongArchTargetLowering::lowerFP_TO_SINT(SDValue Op, SelectionDAG &DAG) const {
  if (Op.getValueSizeInBits() > 32 && Subtarget.isSingleFloat())
    return SDValue();

  EVT FPTy = EVT::getFloatingPointVT(Op.getValueSizeInBits());
  SDValue Trunc = DAG.getNode(LoongArchISD::TruncIntFP, SDLoc(Op), FPTy,
                              Op.getOperand(0));
  return DAG.getNode(ISD::BITCAST, SDLoc(Op), Op.getValueType(), Trunc);
}

SDValue LoongArchTargetLowering::lowerEH_DWARF_CFA(SDValue Op,
                                              SelectionDAG &DAG) const {

  // Return a fixed StackObject with offset 0 which points to the old stack
  // pointer.
  MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();
  EVT ValTy = Op->getValueType(0);
  int FI = MFI.CreateFixedObject(Op.getValueSizeInBits() / 8, 0, false);
  return DAG.getFrameIndex(FI, ValTy);
}

bool LoongArchTargetLowering::isEligibleForTailCallOptimization(
    const CCState &CCInfo, unsigned NextStackOffset,
    const LoongArchFunctionInfo &FI) const {
  if (!UseLoongArchTailCalls)
    return false;

  // Return false if either the callee or caller has a byval argument.
  if (CCInfo.getInRegsParamsCount() > 0 || FI.hasByvalArg())
    return false;

  // Return true if the callee's argument area is no larger than the
  // caller's.
  return NextStackOffset <= FI.getIncomingArgSize();
}

//===----------------------------------------------------------------------===//
//                      Calling Convention Implementation
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// TODO: Implement a generic logic using tblgen that can support this.
// LoongArch LP32 ABI rules:
// ---
// i32 - Passed in A0, A1, A2, A3 and stack
// f32 - Only passed in f32 registers if no int reg has been used yet to hold
//       an argument. Otherwise, passed in A1, A2, A3 and stack.
// f64 - Only passed in two aliased f32 registers if no int reg has been used
//       yet to hold an argument. Otherwise, use A2, A3 and stack. If A1 is
//       not used, it must be shadowed. If only A3 is available, shadow it and
//       go to stack.
// vXiX - Received as scalarized i32s, passed in A0 - A3 and the stack.
// vXf32 - Passed in either a pair of registers {A0, A1}, {A2, A3} or {A0 - A3}
//         with the remainder spilled to the stack.
// vXf64 - Passed in either {A0, A1, A2, A3} or {A2, A3} and in both cases
//         spilling the remainder to the stack.
//
//  For vararg functions, all arguments are passed in A0, A1, A2, A3 and stack.
//===----------------------------------------------------------------------===//

static bool CC_LoongArchLP32(unsigned ValNo, MVT ValVT, MVT LocVT,
                       CCValAssign::LocInfo LocInfo, ISD::ArgFlagsTy ArgFlags,
                       CCState &State, ArrayRef<MCPhysReg> F64Regs) {
  static const MCPhysReg IntRegs[] = { LoongArch::A0, LoongArch::A1, LoongArch::A2, LoongArch::A3 };

  const LoongArchCCState * LoongArchState = static_cast<LoongArchCCState *>(&State);

  static const MCPhysReg F32Regs[] = { LoongArch::F12, LoongArch::F14 };

  static const MCPhysReg FloatVectorIntRegs[] = { LoongArch::A0, LoongArch::A2 };

  // Do not process byval args here.
  if (ArgFlags.isByVal())
    return true;


  // Promote i8 and i16
  if (LocVT == MVT::i8 || LocVT == MVT::i16) {
    LocVT = MVT::i32;
    if (ArgFlags.isSExt())
      LocInfo = CCValAssign::SExt;
    else if (ArgFlags.isZExt())
      LocInfo = CCValAssign::ZExt;
    else
      LocInfo = CCValAssign::AExt;
  }

  unsigned Reg;

  // f32 and f64 are allocated in A0, A1, A2, A3 when either of the following
  // is true: function is vararg, argument is 3rd or higher, there is previous
  // argument which is not f32 or f64.
  bool AllocateFloatsInIntReg = State.isVarArg() || ValNo > 1 ||
                                State.getFirstUnallocated(F32Regs) != ValNo;
  Align OrigAlign = ArgFlags.getNonZeroOrigAlign();
  bool isI64 = (ValVT == MVT::i32 && OrigAlign == Align(8));
  bool isVectorFloat = LoongArchState->WasOriginalArgVectorFloat(ValNo);

  // The LoongArch vector ABI for floats passes them in a pair of registers
  if (ValVT == MVT::i32 && isVectorFloat) {
    // This is the start of an vector that was scalarized into an unknown number
    // of components. It doesn't matter how many there are. Allocate one of the
    // notional 8 byte aligned registers which map onto the argument stack, and
    // shadow the register lost to alignment requirements.
    if (ArgFlags.isSplit()) {
      Reg = State.AllocateReg(FloatVectorIntRegs);
      if (Reg == LoongArch::A2)
        State.AllocateReg(LoongArch::A1);
      else if (Reg == 0)
        State.AllocateReg(LoongArch::A3);
    } else {
      // If we're an intermediate component of the split, we can just attempt to
      // allocate a register directly.
      Reg = State.AllocateReg(IntRegs);
    }
  } else if (ValVT == MVT::i32 || (ValVT == MVT::f32 && AllocateFloatsInIntReg)) {
    Reg = State.AllocateReg(IntRegs);
    // If this is the first part of an i64 arg,
    // the allocated register must be either A0 or A2.
    if (isI64 && (Reg == LoongArch::A1 || Reg == LoongArch::A3))
      Reg = State.AllocateReg(IntRegs);
    LocVT = MVT::i32;
  } else if (ValVT == MVT::f64 && AllocateFloatsInIntReg) {
    // Allocate int register and shadow next int register. If first
    // available register is LoongArch::A1 or LoongArch::A3, shadow it too.
    Reg = State.AllocateReg(IntRegs);
    if (Reg == LoongArch::A1 || Reg == LoongArch::A3)
      Reg = State.AllocateReg(IntRegs);
    State.AllocateReg(IntRegs);
    LocVT = MVT::i32;
  } else if (ValVT.isFloatingPoint() && !AllocateFloatsInIntReg) {
    // we are guaranteed to find an available float register
    if (ValVT == MVT::f32) {
      Reg = State.AllocateReg(F32Regs);
      // Shadow int register
      State.AllocateReg(IntRegs);
    } else {
      Reg = State.AllocateReg(F64Regs);
      // Shadow int registers
      unsigned Reg2 = State.AllocateReg(IntRegs);
      if (Reg2 == LoongArch::A1 || Reg2 == LoongArch::A3)
        State.AllocateReg(IntRegs);
      State.AllocateReg(IntRegs);
    }
  } else
    llvm_unreachable("Cannot handle this ValVT.");

  if (!Reg) {
    unsigned Offset = State.AllocateStack(ValVT.getStoreSize(), OrigAlign);
    State.addLoc(CCValAssign::getMem(ValNo, ValVT, Offset, LocVT, LocInfo));
  } else
    State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));

  return false;
}

static bool CC_LoongArchLP32_FP32(unsigned ValNo, MVT ValVT,
                            MVT LocVT, CCValAssign::LocInfo LocInfo,
                            ISD::ArgFlagsTy ArgFlags, CCState &State) {
  static const MCPhysReg F64Regs[] = {LoongArch::F0_64, LoongArch::F1_64, LoongArch::F2_64, \
                                      LoongArch::F3_64, LoongArch::F4_64, LoongArch::F5_64, \
                                      LoongArch::F6_64, LoongArch::F7_64 };

  return CC_LoongArchLP32(ValNo, ValVT, LocVT, LocInfo, ArgFlags, State, F64Regs);
}

static bool CC_LoongArchLP32_FP64(unsigned ValNo, MVT ValVT,
                            MVT LocVT, CCValAssign::LocInfo LocInfo,
                            ISD::ArgFlagsTy ArgFlags, CCState &State) {
  static const MCPhysReg F64Regs[] = {LoongArch::F0_64, LoongArch::F1_64, LoongArch::F2_64, \
                                      LoongArch::F3_64, LoongArch::F4_64, LoongArch::F5_64, \
                                      LoongArch::F6_64, LoongArch::F7_64 };

  return CC_LoongArchLP32(ValNo, ValVT, LocVT, LocInfo, ArgFlags, State, F64Regs);
}

static bool CC_LoongArch_F128(unsigned ValNo, MVT ValVT,
                            MVT LocVT, CCValAssign::LocInfo LocInfo,
                            ISD::ArgFlagsTy ArgFlags, CCState &State) LLVM_ATTRIBUTE_UNUSED;

static bool CC_LoongArch_F128(unsigned ValNo, MVT ValVT,
                            MVT LocVT, CCValAssign::LocInfo LocInfo,
                            ISD::ArgFlagsTy ArgFlags, CCState &State) {

  static const MCPhysReg ArgRegs[8] = {
      LoongArch::A0_64, LoongArch::A1_64, LoongArch::A2_64, LoongArch::A3_64,
      LoongArch::A4_64, LoongArch::A5_64, LoongArch::A6_64, LoongArch::A7_64};

  unsigned Idx = State.getFirstUnallocated(ArgRegs);
  // Skip 'odd' register if necessary.
  if (!ArgFlags.isSplitEnd() && Idx != array_lengthof(ArgRegs) && Idx % 2 == 1)
    State.AllocateReg(ArgRegs);
  return true;
}

static bool CC_LoongArchLP32(unsigned ValNo, MVT ValVT, MVT LocVT,
                       CCValAssign::LocInfo LocInfo, ISD::ArgFlagsTy ArgFlags,
                       CCState &State) LLVM_ATTRIBUTE_UNUSED;

#include "LoongArchGenCallingConv.inc"

 CCAssignFn *LoongArchTargetLowering::CCAssignFnForCall() const{
   return CC_LoongArch;
 }

 CCAssignFn *LoongArchTargetLowering::CCAssignFnForReturn() const{
   return RetCC_LoongArch;
 }

//===----------------------------------------------------------------------===//
//                  Call Calling Convention Implementation
//===----------------------------------------------------------------------===//
SDValue LoongArchTargetLowering::passArgOnStack(SDValue StackPtr, unsigned Offset,
                                           SDValue Chain, SDValue Arg,
                                           const SDLoc &DL, bool IsTailCall,
                                           SelectionDAG &DAG) const {
  if (!IsTailCall) {
    SDValue PtrOff =
        DAG.getNode(ISD::ADD, DL, getPointerTy(DAG.getDataLayout()), StackPtr,
                    DAG.getIntPtrConstant(Offset, DL));
    return DAG.getStore(Chain, DL, Arg, PtrOff, MachinePointerInfo());
  }

  MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();
  int FI = MFI.CreateFixedObject(Arg.getValueSizeInBits() / 8, Offset, false);
  SDValue FIN = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
  return DAG.getStore(Chain, DL, Arg, FIN, MachinePointerInfo(),
                      /* Alignment = */ 0, MachineMemOperand::MOVolatile);
}

void LoongArchTargetLowering::
getOpndList(SmallVectorImpl<SDValue> &Ops,
            std::deque<std::pair<unsigned, SDValue>> &RegsToPass,
            bool IsPICCall, bool GlobalOrExternal, bool InternalLinkage,
            bool IsCallReloc, CallLoweringInfo &CLI, SDValue Callee,
            SDValue Chain) const {
  // Build a sequence of copy-to-reg nodes chained together with token
  // chain and flag operands which copy the outgoing args into registers.
  // The InFlag in necessary since all emitted instructions must be
  // stuck together.
  SDValue InFlag;

  Ops.push_back(Callee);

  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
    Chain = CLI.DAG.getCopyToReg(Chain, CLI.DL, RegsToPass[i].first,
                                 RegsToPass[i].second, InFlag);
    InFlag = Chain.getValue(1);
  }

  // Add argument registers to the end of the list so that they are
  // known live into the call.
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i)
    Ops.push_back(CLI.DAG.getRegister(RegsToPass[i].first,
                                      RegsToPass[i].second.getValueType()));

  // Add a register mask operand representing the call-preserved registers.
  const TargetRegisterInfo *TRI = Subtarget.getRegisterInfo();
  const uint32_t *Mask =
      TRI->getCallPreservedMask(CLI.DAG.getMachineFunction(), CLI.CallConv);
  assert(Mask && "Missing call preserved mask for calling convention");
  Ops.push_back(CLI.DAG.getRegisterMask(Mask));

  if (InFlag.getNode())
    Ops.push_back(InFlag);
}

void LoongArchTargetLowering::AdjustInstrPostInstrSelection(MachineInstr &MI,
                                                       SDNode *Node) const {
  switch (MI.getOpcode()) {
    default:
      return;
  }
}

/// LowerCall - functions arguments are copied from virtual regs to
/// (physical regs)/(stack frame), CALLSEQ_START and CALLSEQ_END are emitted.
SDValue
LoongArchTargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                              SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG                     = CLI.DAG;
  SDLoc DL                              = CLI.DL;
  SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
  SmallVectorImpl<SDValue> &OutVals     = CLI.OutVals;
  SmallVectorImpl<ISD::InputArg> &Ins   = CLI.Ins;
  SDValue Chain                         = CLI.Chain;
  SDValue Callee                        = CLI.Callee;
  bool &IsTailCall                      = CLI.IsTailCall;
  CallingConv::ID CallConv              = CLI.CallConv;
  bool IsVarArg                         = CLI.IsVarArg;

  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetFrameLowering *TFL = Subtarget.getFrameLowering();
  bool IsPIC = isPositionIndependent();

  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  LoongArchCCState CCInfo(
      CallConv, IsVarArg, DAG.getMachineFunction(), ArgLocs, *DAG.getContext(),
      LoongArchCCState::getSpecialCallingConvForCallee(Callee.getNode(), Subtarget));

  const ExternalSymbolSDNode *ES =
      dyn_cast_or_null<const ExternalSymbolSDNode>(Callee.getNode());

  // There is one case where CALLSEQ_START..CALLSEQ_END can be nested, which
  // is during the lowering of a call with a byval argument which produces
  // a call to memcpy. For the LP32 case, this causes the caller to allocate
  // stack space for the reserved argument area for the callee, then recursively
  // again for the memcpy call. In the NEWABI case, this doesn't occur as those
  // ABIs mandate that the callee allocates the reserved argument area. We do
  // still produce nested CALLSEQ_START..CALLSEQ_END with zero space though.
  //
  // If the callee has a byval argument and memcpy is used, we are mandated
  // to already have produced a reserved argument area for the callee for LP32.
  // Therefore, the reserved argument area can be reused for both calls.
  //
  // Other cases of calling memcpy cannot have a chain with a CALLSEQ_START
  // present, as we have yet to hook that node onto the chain.
  //
  // Hence, the CALLSEQ_START and CALLSEQ_END nodes can be eliminated in this
  // case. GCC does a similar trick, in that wherever possible, it calculates
  // the maximum out going argument area (including the reserved area), and
  // preallocates the stack space on entrance to the caller.
  //
  // FIXME: We should do the same for efficiency and space.

  // Note: The check on the calling convention below must match
  //       LoongArchABIInfo::GetCalleeAllocdArgSizeInBytes().
  bool MemcpyInByVal = ES &&
                       StringRef(ES->getSymbol()) == StringRef("memcpy") &&
                       CallConv != CallingConv::Fast &&
                       Chain.getOpcode() == ISD::CALLSEQ_START;

  // Allocate the reserved argument area. It seems strange to do this from the
  // caller side but removing it breaks the frame size calculation.
  unsigned ReservedArgArea =
      MemcpyInByVal ? 0 : ABI.GetCalleeAllocdArgSizeInBytes(CallConv);
  CCInfo.AllocateStack(ReservedArgArea, Align(1));

  CCInfo.AnalyzeCallOperands(Outs, CC_LoongArch, CLI.getArgs(),
                             ES ? ES->getSymbol() : nullptr);

  // Get a count of how many bytes are to be pushed on the stack.
  unsigned NextStackOffset = CCInfo.getNextStackOffset();

  // Check if it's really possible to do a tail call. Restrict it to functions
  // that are part of this compilation unit.
  bool InternalLinkage = false;
  if (IsTailCall) {
    IsTailCall = isEligibleForTailCallOptimization(
        CCInfo, NextStackOffset, *MF.getInfo<LoongArchFunctionInfo>());
     if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
      InternalLinkage = G->getGlobal()->hasInternalLinkage();
      IsTailCall &= (InternalLinkage || G->getGlobal()->hasLocalLinkage() ||
                     G->getGlobal()->hasPrivateLinkage() ||
                     G->getGlobal()->hasHiddenVisibility() ||
                     G->getGlobal()->hasProtectedVisibility());
     }
  }
  if (!IsTailCall && CLI.CB && CLI.CB->isMustTailCall())
    report_fatal_error("failed to perform tail call elimination on a call "
                       "site marked musttail");

  if (IsTailCall)
    ++NumTailCalls;

  // Chain is the output chain of the last Load/Store or CopyToReg node.
  // ByValChain is the output chain of the last Memcpy node created for copying
  // byval arguments to the stack.
  unsigned StackAlignment = TFL->getStackAlignment();
  NextStackOffset = alignTo(NextStackOffset, StackAlignment);
  SDValue NextStackOffsetVal = DAG.getIntPtrConstant(NextStackOffset, DL, true);

  if (!(IsTailCall || MemcpyInByVal))
    Chain = DAG.getCALLSEQ_START(Chain, NextStackOffset, 0, DL);

  SDValue StackPtr =
      DAG.getCopyFromReg(Chain, DL, ABI.IsLP64() ? LoongArch::SP_64 : LoongArch::SP,
                         getPointerTy(DAG.getDataLayout()));

  std::deque<std::pair<unsigned, SDValue>> RegsToPass;
  SmallVector<SDValue, 8> MemOpChains;

  CCInfo.rewindByValRegsInfo();

  // Walk the register/memloc assignments, inserting copies/loads.
  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    SDValue Arg = OutVals[i];
    CCValAssign &VA = ArgLocs[i];
    MVT ValVT = VA.getValVT(), LocVT = VA.getLocVT();
    ISD::ArgFlagsTy Flags = Outs[i].Flags;
    bool UseUpperBits = false;

    // ByVal Arg.
    if (Flags.isByVal()) {
      unsigned FirstByValReg, LastByValReg;
      unsigned ByValIdx = CCInfo.getInRegsParamsProcessed();
      CCInfo.getInRegsParamInfo(ByValIdx, FirstByValReg, LastByValReg);

      assert(Flags.getByValSize() &&
             "ByVal args of size 0 should have been ignored by front-end.");
      assert(ByValIdx < CCInfo.getInRegsParamsCount());
      assert(!IsTailCall &&
             "Do not tail-call optimize if there is a byval argument.");
      passByValArg(Chain, DL, RegsToPass, MemOpChains, StackPtr, MFI, DAG, Arg,
                   FirstByValReg, LastByValReg, Flags,
                   VA);
      CCInfo.nextInRegsParam();
      continue;
    }

    // Promote the value if needed.
    switch (VA.getLocInfo()) {
    default:
      llvm_unreachable("Unknown loc info!");
    case CCValAssign::Full:
      if (VA.isRegLoc()) {
        if ((ValVT == MVT::f32 && LocVT == MVT::i32) ||
            (ValVT == MVT::f64 && LocVT == MVT::i64) ||
            (ValVT == MVT::i64 && LocVT == MVT::f64))
          Arg = DAG.getNode(ISD::BITCAST, DL, LocVT, Arg);
      }
      break;
    case CCValAssign::BCvt:
      Arg = DAG.getNode(ISD::BITCAST, DL, LocVT, Arg);
      break;
    case CCValAssign::SExtUpper:
      UseUpperBits = true;
      LLVM_FALLTHROUGH;
    case CCValAssign::SExt:
      Arg = DAG.getNode(ISD::SIGN_EXTEND, DL, LocVT, Arg);
      break;
    case CCValAssign::ZExtUpper:
      UseUpperBits = true;
      LLVM_FALLTHROUGH;
    case CCValAssign::ZExt:
      Arg = DAG.getNode(ISD::ZERO_EXTEND, DL, LocVT, Arg);
      break;
    case CCValAssign::AExtUpper:
      UseUpperBits = true;
      LLVM_FALLTHROUGH;
    case CCValAssign::AExt:
      Arg = DAG.getNode(ISD::ANY_EXTEND, DL, LocVT, Arg);
      break;
    }

    if (UseUpperBits) {
      unsigned ValSizeInBits = Outs[i].ArgVT.getSizeInBits();
      unsigned LocSizeInBits = VA.getLocVT().getSizeInBits();
      Arg = DAG.getNode(
          ISD::SHL, DL, VA.getLocVT(), Arg,
          DAG.getConstant(LocSizeInBits - ValSizeInBits, DL, VA.getLocVT()));
    }

    // Arguments that can be passed on register must be kept at
    // RegsToPass vector
    if (VA.isRegLoc()) {
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
      continue;
    }

    // Register can't get to this point...
    assert(VA.isMemLoc());

    // emit ISD::STORE whichs stores the
    // parameter value to a stack Location
    MemOpChains.push_back(passArgOnStack(StackPtr, VA.getLocMemOffset(),
                                         Chain, Arg, DL, IsTailCall, DAG));
  }

  // Transform all store nodes into one single node because all store
  // nodes are independent of each other.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, MemOpChains);

  // If the callee is a GlobalAddress/ExternalSymbol node (quite common, every
  // direct call is) turn it into a TargetGlobalAddress/TargetExternalSymbol
  // node so that legalize doesn't hack it.

  bool GlobalOrExternal = false, IsCallReloc = false;

  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
    Callee = DAG.getTargetGlobalAddress(G->getGlobal(), DL,
                                        getPointerTy(DAG.getDataLayout()), 0,
                                        LoongArchII::MO_NO_FLAG);
    GlobalOrExternal = true;
  }
  else if (ExternalSymbolSDNode *S = dyn_cast<ExternalSymbolSDNode>(Callee)) {
    const char *Sym = S->getSymbol();
    Callee = DAG.getTargetExternalSymbol(
        Sym, getPointerTy(DAG.getDataLayout()), LoongArchII::MO_NO_FLAG);

    GlobalOrExternal = true;
  }

  SmallVector<SDValue, 8> Ops(1, Chain);
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);

  getOpndList(Ops, RegsToPass, IsPIC, GlobalOrExternal, InternalLinkage,
              IsCallReloc, CLI, Callee, Chain);

  if (IsTailCall) {
    MF.getFrameInfo().setHasTailCall();
    return DAG.getNode(LoongArchISD::TailCall, DL, MVT::Other, Ops);
  }

  Chain = DAG.getNode(LoongArchISD::JmpLink, DL, NodeTys, Ops);
  SDValue InFlag = Chain.getValue(1);

  // Create the CALLSEQ_END node in the case of where it is not a call to
  // memcpy.
  if (!(MemcpyInByVal)) {
    Chain = DAG.getCALLSEQ_END(Chain, NextStackOffsetVal,
                               DAG.getIntPtrConstant(0, DL, true), InFlag, DL);
    InFlag = Chain.getValue(1);
  }

  // Handle result values, copying them out of physregs into vregs that we
  // return.
  return LowerCallResult(Chain, InFlag, CallConv, IsVarArg, Ins, DL, DAG,
                         InVals, CLI);
}

/// LowerCallResult - Lower the result values of a call into the
/// appropriate copies out of appropriate physical registers.
SDValue LoongArchTargetLowering::LowerCallResult(
    SDValue Chain, SDValue InFlag, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals,
    TargetLowering::CallLoweringInfo &CLI) const {
  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  LoongArchCCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), RVLocs,
                     *DAG.getContext());

  const ExternalSymbolSDNode *ES =
      dyn_cast_or_null<const ExternalSymbolSDNode>(CLI.Callee.getNode());
  CCInfo.AnalyzeCallResult(Ins, RetCC_LoongArch, CLI.RetTy,
                           ES ? ES->getSymbol() : nullptr);

  // Copy all of the result registers out of their specified physreg.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    SDValue Val = DAG.getCopyFromReg(Chain, DL, RVLocs[i].getLocReg(),
                                     RVLocs[i].getLocVT(), InFlag);
    Chain = Val.getValue(1);
    InFlag = Val.getValue(2);

    if (VA.isUpperBitsInLoc()) {
      unsigned ValSizeInBits = Ins[i].ArgVT.getSizeInBits();
      unsigned LocSizeInBits = VA.getLocVT().getSizeInBits();
      unsigned Shift =
          VA.getLocInfo() == CCValAssign::ZExtUpper ? ISD::SRL : ISD::SRA;
      Val = DAG.getNode(
          Shift, DL, VA.getLocVT(), Val,
          DAG.getConstant(LocSizeInBits - ValSizeInBits, DL, VA.getLocVT()));
    }

    switch (VA.getLocInfo()) {
    default:
      llvm_unreachable("Unknown loc info!");
    case CCValAssign::Full:
      break;
    case CCValAssign::BCvt:
      Val = DAG.getNode(ISD::BITCAST, DL, VA.getValVT(), Val);
      break;
    case CCValAssign::AExt:
    case CCValAssign::AExtUpper:
      Val = DAG.getNode(ISD::TRUNCATE, DL, VA.getValVT(), Val);
      break;
    case CCValAssign::ZExt:
    case CCValAssign::ZExtUpper:
      Val = DAG.getNode(ISD::AssertZext, DL, VA.getLocVT(), Val,
                        DAG.getValueType(VA.getValVT()));
      Val = DAG.getNode(ISD::TRUNCATE, DL, VA.getValVT(), Val);
      break;
    case CCValAssign::SExt:
    case CCValAssign::SExtUpper:
      Val = DAG.getNode(ISD::AssertSext, DL, VA.getLocVT(), Val,
                        DAG.getValueType(VA.getValVT()));
      Val = DAG.getNode(ISD::TRUNCATE, DL, VA.getValVT(), Val);
      break;
    }

    InVals.push_back(Val);
  }

  return Chain;
}

static SDValue UnpackFromArgumentSlot(SDValue Val, const CCValAssign &VA,
                                      EVT ArgVT, const SDLoc &DL,
                                      SelectionDAG &DAG) {
  MVT LocVT = VA.getLocVT();
  EVT ValVT = VA.getValVT();

  // Shift into the upper bits if necessary.
  switch (VA.getLocInfo()) {
  default:
    break;
  case CCValAssign::AExtUpper:
  case CCValAssign::SExtUpper:
  case CCValAssign::ZExtUpper: {
    unsigned ValSizeInBits = ArgVT.getSizeInBits();
    unsigned LocSizeInBits = VA.getLocVT().getSizeInBits();
    unsigned Opcode =
        VA.getLocInfo() == CCValAssign::ZExtUpper ? ISD::SRL : ISD::SRA;
    Val = DAG.getNode(
        Opcode, DL, VA.getLocVT(), Val,
        DAG.getConstant(LocSizeInBits - ValSizeInBits, DL, VA.getLocVT()));
    break;
  }
  }

  // If this is an value smaller than the argument slot size (32-bit for LP32,
  // 64-bit for LPX32/LP64), it has been promoted in some way to the argument slot
  // size. Extract the value and insert any appropriate assertions regarding
  // sign/zero extension.
  switch (VA.getLocInfo()) {
  default:
    llvm_unreachable("Unknown loc info!");
  case CCValAssign::Full:
    break;
  case CCValAssign::AExtUpper:
  case CCValAssign::AExt:
    Val = DAG.getNode(ISD::TRUNCATE, DL, ValVT, Val);
    break;
  case CCValAssign::SExtUpper:
  case CCValAssign::SExt:
    Val = DAG.getNode(ISD::AssertSext, DL, LocVT, Val, DAG.getValueType(ValVT));
    Val = DAG.getNode(ISD::TRUNCATE, DL, ValVT, Val);
    break;
  case CCValAssign::ZExtUpper:
  case CCValAssign::ZExt:
    Val = DAG.getNode(ISD::AssertZext, DL, LocVT, Val, DAG.getValueType(ValVT));
    Val = DAG.getNode(ISD::TRUNCATE, DL, ValVT, Val);
    break;
  case CCValAssign::BCvt:
    Val = DAG.getNode(ISD::BITCAST, DL, ValVT, Val);
    break;
  }

  return Val;
}

//===----------------------------------------------------------------------===//
//             Formal Arguments Calling Convention Implementation
//===----------------------------------------------------------------------===//
/// LowerFormalArguments - transform physical registers into virtual registers
/// and generate load operations for arguments places on the stack.
SDValue LoongArchTargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  LoongArchFunctionInfo *LoongArchFI = MF.getInfo<LoongArchFunctionInfo>();

  LoongArchFI->setVarArgsFrameIndex(0);

  // Used with vargs to acumulate store chains.
  std::vector<SDValue> OutChains;

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  LoongArchCCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), ArgLocs,
                     *DAG.getContext());
  CCInfo.AllocateStack(ABI.GetCalleeAllocdArgSizeInBytes(CallConv), Align(1));
  const Function &Func = DAG.getMachineFunction().getFunction();
  Function::const_arg_iterator FuncArg = Func.arg_begin();

  CCInfo.AnalyzeFormalArguments(Ins, CC_LoongArch_FixedArg);
  LoongArchFI->setFormalArgInfo(CCInfo.getNextStackOffset(),
                           CCInfo.getInRegsParamsCount() > 0);

  unsigned CurArgIdx = 0;
  CCInfo.rewindByValRegsInfo();

  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];
    if (Ins[i].isOrigArg()) {
      std::advance(FuncArg, Ins[i].getOrigArgIndex() - CurArgIdx);
      CurArgIdx = Ins[i].getOrigArgIndex();
    }
    EVT ValVT = VA.getValVT();
    ISD::ArgFlagsTy Flags = Ins[i].Flags;
    bool IsRegLoc = VA.isRegLoc();

    if (Flags.isByVal()) {
      assert(Ins[i].isOrigArg() && "Byval arguments cannot be implicit");
      unsigned FirstByValReg, LastByValReg;
      unsigned ByValIdx = CCInfo.getInRegsParamsProcessed();
      CCInfo.getInRegsParamInfo(ByValIdx, FirstByValReg, LastByValReg);

      assert(Flags.getByValSize() &&
             "ByVal args of size 0 should have been ignored by front-end.");
      assert(ByValIdx < CCInfo.getInRegsParamsCount());
      copyByValRegs(Chain, DL, OutChains, DAG, Flags, InVals, &*FuncArg,
                    FirstByValReg, LastByValReg, VA, CCInfo);
      CCInfo.nextInRegsParam();
      continue;
    }

    // Arguments stored on registers
    if (IsRegLoc) {
      MVT RegVT = VA.getLocVT();
      unsigned ArgReg = VA.getLocReg();
      const TargetRegisterClass *RC = getRegClassFor(RegVT);

      // Transform the arguments stored on
      // physical registers into virtual ones
      unsigned Reg = addLiveIn(DAG.getMachineFunction(), ArgReg, RC);
      SDValue ArgValue = DAG.getCopyFromReg(Chain, DL, Reg, RegVT);

      ArgValue = UnpackFromArgumentSlot(ArgValue, VA, Ins[i].ArgVT, DL, DAG);

      // Handle floating point arguments passed in integer registers and
      // long double arguments passed in floating point registers.
      if ((RegVT == MVT::i32 && ValVT == MVT::f32) ||
          (RegVT == MVT::i64 && ValVT == MVT::f64) ||
          (RegVT == MVT::f64 && ValVT == MVT::i64))
        ArgValue = DAG.getNode(ISD::BITCAST, DL, ValVT, ArgValue);
      else if (ABI.IsLP32() && RegVT == MVT::i32 &&
               ValVT == MVT::f64) {
        // TODO: lp32
      }

      InVals.push_back(ArgValue);
    } else { // VA.isRegLoc()
      MVT LocVT = VA.getLocVT();

      if (ABI.IsLP32()) {
        // We ought to be able to use LocVT directly but LP32 sets it to i32
        // when allocating floating point values to integer registers.
        // This shouldn't influence how we load the value into registers unless
        // we are targeting softfloat.
        if (VA.getValVT().isFloatingPoint() && !Subtarget.useSoftFloat())
          LocVT = VA.getValVT();
      }

      // sanity check
      assert(VA.isMemLoc());

      // The stack pointer offset is relative to the caller stack frame.
      int FI = MFI.CreateFixedObject(LocVT.getSizeInBits() / 8,
                                     VA.getLocMemOffset(), true);

      // Create load nodes to retrieve arguments from the stack
      SDValue FIN = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
      SDValue ArgValue = DAG.getLoad(
          LocVT, DL, Chain, FIN,
          MachinePointerInfo::getFixedStack(DAG.getMachineFunction(), FI));
      OutChains.push_back(ArgValue.getValue(1));

      ArgValue = UnpackFromArgumentSlot(ArgValue, VA, Ins[i].ArgVT, DL, DAG);

      InVals.push_back(ArgValue);
    }
  }

  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    // The loongarch ABIs for returning structs by value requires that we copy
    // the sret argument into $v0 for the return. Save the argument into
    // a virtual register so that we can access it from the return points.
    if (Ins[i].Flags.isSRet()) {
      unsigned Reg = LoongArchFI->getSRetReturnReg();
      if (!Reg) {
        Reg = MF.getRegInfo().createVirtualRegister(
            getRegClassFor(ABI.IsLP64() ? MVT::i64 : MVT::i32));
        LoongArchFI->setSRetReturnReg(Reg);
      }
      SDValue Copy = DAG.getCopyToReg(DAG.getEntryNode(), DL, Reg, InVals[i]);
      Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, Copy, Chain);
      break;
    }
  }

  if (IsVarArg)
    writeVarArgRegs(OutChains, Chain, DL, DAG, CCInfo);

  // All stores are grouped in one node to allow the matching between
  // the size of Ins and InVals. This only happens when on varg functions
  if (!OutChains.empty()) {
    OutChains.push_back(Chain);
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, OutChains);
  }

  return Chain;
}

//===----------------------------------------------------------------------===//
//               Return Value Calling Convention Implementation
//===----------------------------------------------------------------------===//

bool
LoongArchTargetLowering::CanLowerReturn(CallingConv::ID CallConv,
                                   MachineFunction &MF, bool IsVarArg,
                                   const SmallVectorImpl<ISD::OutputArg> &Outs,
                                   LLVMContext &Context) const {
  SmallVector<CCValAssign, 16> RVLocs;
  LoongArchCCState CCInfo(CallConv, IsVarArg, MF, RVLocs, Context);
  return CCInfo.CheckReturn(Outs, RetCC_LoongArch);
}

bool
LoongArchTargetLowering::shouldSignExtendTypeInLibCall(EVT Type, bool IsSigned) const {
  if ((ABI.IsLPX32() || ABI.IsLP64()) && Type == MVT::i32)
      return true;

  return IsSigned;
}

SDValue
LoongArchTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
                                bool IsVarArg,
                                const SmallVectorImpl<ISD::OutputArg> &Outs,
                                const SmallVectorImpl<SDValue> &OutVals,
                                const SDLoc &DL, SelectionDAG &DAG) const {
  // CCValAssign - represent the assignment of
  // the return value to a location
  SmallVector<CCValAssign, 16> RVLocs;
  MachineFunction &MF = DAG.getMachineFunction();

  // CCState - Info about the registers and stack slot.
  LoongArchCCState CCInfo(CallConv, IsVarArg, MF, RVLocs, *DAG.getContext());

  // Analyze return values.
  CCInfo.AnalyzeReturn(Outs, RetCC_LoongArch);

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);

  // Copy the result values into the output registers.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    SDValue Val = OutVals[i];
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");
    bool UseUpperBits = false;

    switch (VA.getLocInfo()) {
    default:
      llvm_unreachable("Unknown loc info!");
    case CCValAssign::Full:
      break;
    case CCValAssign::BCvt:
      Val = DAG.getNode(ISD::BITCAST, DL, VA.getLocVT(), Val);
      break;
    case CCValAssign::AExtUpper:
      UseUpperBits = true;
      LLVM_FALLTHROUGH;
    case CCValAssign::AExt:
      Val = DAG.getNode(ISD::ANY_EXTEND, DL, VA.getLocVT(), Val);
      break;
    case CCValAssign::ZExtUpper:
      UseUpperBits = true;
      LLVM_FALLTHROUGH;
    case CCValAssign::ZExt:
      Val = DAG.getNode(ISD::ZERO_EXTEND, DL, VA.getLocVT(), Val);
      break;
    case CCValAssign::SExtUpper:
      UseUpperBits = true;
      LLVM_FALLTHROUGH;
    case CCValAssign::SExt:
      Val = DAG.getNode(ISD::SIGN_EXTEND, DL, VA.getLocVT(), Val);
      break;
    }

    if (UseUpperBits) {
      unsigned ValSizeInBits = Outs[i].ArgVT.getSizeInBits();
      unsigned LocSizeInBits = VA.getLocVT().getSizeInBits();
      Val = DAG.getNode(
          ISD::SHL, DL, VA.getLocVT(), Val,
          DAG.getConstant(LocSizeInBits - ValSizeInBits, DL, VA.getLocVT()));
    }

    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Val, Flag);

    // Guarantee that all emitted copies are stuck together with flags.
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  // The loongarch ABIs for returning structs by value requires that we copy
  // the sret argument into $v0 for the return. We saved the argument into
  // a virtual register in the entry block, so now we copy the value out
  // and into $v0.
  if (MF.getFunction().hasStructRetAttr()) {
    LoongArchFunctionInfo *LoongArchFI = MF.getInfo<LoongArchFunctionInfo>();
    unsigned Reg = LoongArchFI->getSRetReturnReg();

    if (!Reg)
      llvm_unreachable("sret virtual register not created in the entry block");
    SDValue Val =
        DAG.getCopyFromReg(Chain, DL, Reg, getPointerTy(DAG.getDataLayout()));
    unsigned A0 = ABI.IsLP64() ? LoongArch::A0_64 : LoongArch::A0;

    Chain = DAG.getCopyToReg(Chain, DL, A0, Val, Flag);
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(A0, getPointerTy(DAG.getDataLayout())));
  }

  RetOps[0] = Chain;  // Update chain.

  // Add the flag if we have it.
  if (Flag.getNode())
    RetOps.push_back(Flag);

  // Standard return on LoongArch is a "jr $ra"
  return DAG.getNode(LoongArchISD::Ret, DL, MVT::Other, RetOps);
}

//===----------------------------------------------------------------------===//
//                           LoongArch Inline Assembly Support
//===----------------------------------------------------------------------===//

/// getConstraintType - Given a constraint letter, return the type of
/// constraint it is for this target.
LoongArchTargetLowering::ConstraintType
LoongArchTargetLowering::getConstraintType(StringRef Constraint) const {
  // LoongArch specific constraints
  // GCC config/loongarch/constraints.md
  //
  // 'f': Floating Point register
  // 'G': Floating-point 0
  // 'l': Signed 16-bit constant
  // 'R': Memory address that can be used in a non-macro load or store
  // "ZC" Memory address with 16-bit and 4 bytes aligned offset
  // "ZB" Memory address with 0 offset

  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
      default : break;
      case 'f':
        return C_RegisterClass;
      case 'l':
      case 'G':
        return C_Other;
      case 'R':
        return C_Memory;
    }
  }

  if (Constraint == "ZC" || Constraint == "ZB")
    return C_Memory;

  return TargetLowering::getConstraintType(Constraint);
}

/// Examine constraint type and operand type and determine a weight value.
/// This object must already have been set up with the operand type
/// and the current alternative constraint selected.
TargetLowering::ConstraintWeight
LoongArchTargetLowering::getSingleConstraintMatchWeight(
    AsmOperandInfo &info, const char *constraint) const {
  ConstraintWeight weight = CW_Invalid;
  Value *CallOperandVal = info.CallOperandVal;
    // If we don't have a value, we can't do a match,
    // but allow it at the lowest weight.
  if (!CallOperandVal)
    return CW_Default;
  Type *type = CallOperandVal->getType();
  // Look at the constraint type.
  switch (*constraint) {
  default:
    weight = TargetLowering::getSingleConstraintMatchWeight(info, constraint);
    break;
  case 'f': // FPU
    if (type->isFloatTy())
      weight = CW_Register;
    break;
  case 'l': // signed 16 bit immediate
  case 'I': // signed 12 bit immediate
  case 'J': // integer zero
  case 'G': // floating-point zero
  case 'K': // unsigned 12 bit immediate
    if (isa<ConstantInt>(CallOperandVal))
      weight = CW_Constant;
    break;
  case 'm':
  case 'R':
    weight = CW_Memory;
    break;
  }
  return weight;
}

/// This is a helper function to parse a physical register string and split it
/// into non-numeric and numeric parts (Prefix and Reg). The first boolean flag
/// that is returned indicates whether parsing was successful. The second flag
/// is true if the numeric part exists.
static std::pair<bool, bool> parsePhysicalReg(StringRef C, StringRef &Prefix,
                                              unsigned long long &Reg) {
  if (C.empty() || C.front() != '{' || C.back() != '}')
    return std::make_pair(false, false);

  // Search for the first numeric character.
  StringRef::const_iterator I, B = C.begin() + 1, E = C.end() - 1;
  I = std::find_if(B, E, isdigit);

  Prefix = StringRef(B, I - B);

  // The second flag is set to false if no numeric characters were found.
  if (I == E)
    return std::make_pair(true, false);

  // Parse the numeric characters.
  return std::make_pair(!getAsUnsignedInteger(StringRef(I, E - I), 10, Reg),
                        true);
}

EVT LoongArchTargetLowering::getTypeForExtReturn(LLVMContext &Context, EVT VT,
                                            ISD::NodeType) const {
  bool Cond = !Subtarget.isABI_LP32() && VT.getSizeInBits() == 32;
  EVT MinVT = getRegisterType(Context, Cond ? MVT::i64 : MVT::i32);
  return VT.bitsLT(MinVT) ? MinVT : VT;
}

std::pair<unsigned, const TargetRegisterClass *> LoongArchTargetLowering::
parseRegForInlineAsmConstraint(StringRef C, MVT VT) const {
  const TargetRegisterInfo *TRI =
      Subtarget.getRegisterInfo();
  const TargetRegisterClass *RC;
  StringRef Prefix;
  unsigned long long Reg;

  std::pair<bool, bool> R = parsePhysicalReg(C, Prefix, Reg);

  if (!R.first)
    return std::make_pair(0U, nullptr);

  if (!R.second)
    return std::make_pair(0U, nullptr);

  if (Prefix == "$f") { // Parse $f0-$f31.
    // If the size of FP registers is 64-bit or Reg is an even number, select
    // the 64-bit register class. Otherwise, select the 32-bit register class.
    if (VT == MVT::Other)
      VT = (Subtarget.isFP64bit() || !(Reg % 2)) ? MVT::f64 : MVT::f32;

    RC = getRegClassFor(VT);
  }
  else if (Prefix == "$vr") { // Parse $vr0-$vr31.
    RC = getRegClassFor((VT == MVT::Other) ? MVT::v16i8 : VT);
  }
  else if (Prefix == "$xr") { // Parse $xr0-$xr31.
    RC = getRegClassFor((VT == MVT::Other) ? MVT::v16i8 : VT);
  }
  else if (Prefix == "$fcc") // Parse $fcc0-$fcc7.
    RC = TRI->getRegClass(LoongArch::FCFRRegClassID);
  else { // Parse $r0-$r31.
    assert(Prefix == "$r");
    RC = getRegClassFor((VT == MVT::Other) ? MVT::i32 : VT);
  }

  assert(Reg < RC->getNumRegs());

  if (RC == &LoongArch::GPR64RegClass || RC == &LoongArch::GPR32RegClass) {
    // Sync with the GPR32/GPR64 RegisterClass in LoongArchRegisterInfo.td
    // that just like LoongArchAsmParser.cpp
    switch (Reg) {
      case 0: return std::make_pair(*(RC->begin() + 0), RC); // r0
      case 1: return std::make_pair(*(RC->begin() + 27), RC); // r1
      case 2: return std::make_pair(*(RC->begin() + 28), RC); // r2
      case 3: return std::make_pair(*(RC->begin() + 29), RC); // r3
      case 4: return std::make_pair(*(RC->begin() + 1), RC); // r4
      case 5: return std::make_pair(*(RC->begin() + 2), RC); // r5
      case 6: return std::make_pair(*(RC->begin() + 3), RC); // r6
      case 7: return std::make_pair(*(RC->begin() + 4), RC); // r7
      case 8: return std::make_pair(*(RC->begin() + 5), RC); // r8
      case 9: return std::make_pair(*(RC->begin() + 6), RC); // r9
      case 10: return std::make_pair(*(RC->begin() + 7), RC); // r10
      case 11: return std::make_pair(*(RC->begin() + 8), RC); // r11
      case 12: return std::make_pair(*(RC->begin() + 9), RC); // r12
      case 13: return std::make_pair(*(RC->begin() + 10), RC); // r13
      case 14: return std::make_pair(*(RC->begin() + 11), RC); // r14
      case 15: return std::make_pair(*(RC->begin() + 12), RC); // r15
      case 16: return std::make_pair(*(RC->begin() + 13), RC); // r16
      case 17: return std::make_pair(*(RC->begin() + 14), RC); // r17
      case 18: return std::make_pair(*(RC->begin() + 15), RC); // r18
      case 19: return std::make_pair(*(RC->begin() + 16), RC); // r19
      case 20: return std::make_pair(*(RC->begin() + 17), RC); // r20
      case 21: return std::make_pair(*(RC->begin() + 30), RC); // r21
      case 22: return std::make_pair(*(RC->begin() + 31), RC); // r22
      case 23: return std::make_pair(*(RC->begin() + 18), RC); // r23
      case 24: return std::make_pair(*(RC->begin() + 19), RC); // r24
      case 25: return std::make_pair(*(RC->begin() + 20), RC); // r25
      case 26: return std::make_pair(*(RC->begin() + 21), RC); // r26
      case 27: return std::make_pair(*(RC->begin() + 22), RC); // r27
      case 28: return std::make_pair(*(RC->begin() + 23), RC); // r28
      case 29: return std::make_pair(*(RC->begin() + 24), RC); // r29
      case 30: return std::make_pair(*(RC->begin() + 25), RC); // r30
      case 31: return std::make_pair(*(RC->begin() + 26), RC); // r31
    }
  }
  return std::make_pair(*(RC->begin() + Reg), RC);
}

/// Given a register class constraint, like 'r', if this corresponds directly
/// to an LLVM register class, return a register of 0 and the register class
/// pointer.
std::pair<unsigned, const TargetRegisterClass *>
LoongArchTargetLowering::getRegForInlineAsmConstraint(const TargetRegisterInfo *TRI,
                                                 StringRef Constraint,
                                                 MVT VT) const {
    if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    case 'r':
      if (VT == MVT::i32 || VT == MVT::i16 || VT == MVT::i8) {
        return std::make_pair(0U, &LoongArch::GPR32RegClass);
      }
      if (VT == MVT::i64 && !Subtarget.is64Bit())
        return std::make_pair(0U, &LoongArch::GPR32RegClass);
      if (VT == MVT::i64 && Subtarget.is64Bit())
        return std::make_pair(0U, &LoongArch::GPR64RegClass);
      // This will generate an error message
      return std::make_pair(0U, nullptr);
    case 'f': // FPU
      if (VT == MVT::f32)
        return std::make_pair(0U, &LoongArch::FGR32RegClass);
      else if (VT == MVT::f64)
        return std::make_pair(0U, &LoongArch::FGR64RegClass);
      break;
    }
  }

  std::pair<unsigned, const TargetRegisterClass *> R;
  R = parseRegForInlineAsmConstraint(Constraint, VT);

  if (R.second)
    return R;

  return TargetLowering::getRegForInlineAsmConstraint(TRI, Constraint, VT);
}

/// LowerAsmOperandForConstraint - Lower the specified operand into the Ops
/// vector.  If it is invalid, don't add anything to Ops.
void LoongArchTargetLowering::LowerAsmOperandForConstraint(SDValue Op,
                                                     std::string &Constraint,
                                                     std::vector<SDValue>&Ops,
                                                     SelectionDAG &DAG) const {
  SDLoc DL(Op);
  SDValue Result;

  // Only support length 1 constraints for now.
  if (Constraint.length() > 1) return;

  char ConstraintLetter = Constraint[0];
  switch (ConstraintLetter) {
  default: break; // This will fall through to the generic implementation
  case 'l': // Signed 16 bit constant
    // If this fails, the parent routine will give an error
    if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
      EVT Type = Op.getValueType();
      int64_t Val = C->getSExtValue();
      if (isInt<16>(Val)) {
        Result = DAG.getTargetConstant(Val, DL, Type);
        break;
      }
    }
    return;
  case 'I': // Signed 12 bit constant
    // If this fails, the parent routine will give an error
    if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
      EVT Type = Op.getValueType();
      int64_t Val = C->getSExtValue();
      if (isInt<12>(Val)) {
        Result = DAG.getTargetConstant(Val, DL, Type);
        break;
      }
    }
    return;
  case 'J': // integer zero
    if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
      EVT Type = Op.getValueType();
      int64_t Val = C->getZExtValue();
      if (Val == 0) {
        Result = DAG.getTargetConstant(0, DL, Type);
        break;
      }
    }
    return;
  case 'G': // floating-point zero
    if (ConstantFPSDNode *C = dyn_cast<ConstantFPSDNode>(Op)) {
      if (C->isZero()) {
        EVT Type = Op.getValueType();
        Result = DAG.getTargetConstantFP(0, DL, Type);
        break;
      }
    }
    return;
  case 'K': // unsigned 12 bit immediate
    if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
      EVT Type = Op.getValueType();
      uint64_t Val = (uint64_t)C->getZExtValue();
      if (isUInt<12>(Val)) {
        Result = DAG.getTargetConstant(Val, DL, Type);
        break;
      }
    }
    return;
  }

  if (Result.getNode()) {
    Ops.push_back(Result);
    return;
  }

  TargetLowering::LowerAsmOperandForConstraint(Op, Constraint, Ops, DAG);
}

bool LoongArchTargetLowering::isLegalAddressingMode(const DataLayout &DL,
                                               const AddrMode &AM, Type *Ty,
                                               unsigned AS, Instruction *I) const {
  // No global is ever allowed as a base.
  if (AM.BaseGV)
    return false;

  switch (AM.Scale) {
  case 0: // "r+i" or just "i", depending on HasBaseReg.
    break;
  case 1:
    if (!AM.HasBaseReg) // allow "r+i".
      break;
    return false; // disallow "r+r" or "r+r+i".
  default:
    return false;
  }

  return true;
}

bool
LoongArchTargetLowering::isOffsetFoldingLegal(const GlobalAddressSDNode *GA) const {
  // The LoongArch target isn't yet aware of offsets.
  return false;
}

EVT LoongArchTargetLowering::getOptimalMemOpType(
    const MemOp &Op, const AttributeList &FuncAttributes) const {
  if (Subtarget.is64Bit())
    return MVT::i64;

  return MVT::i32;
}

/// isFPImmLegal - Returns true if the target can instruction select the
/// specified FP immediate natively. If false, the legalizer will
/// materialize the FP immediate as a load from a constant pool.
bool LoongArchTargetLowering::isFPImmLegal(const APFloat &Imm, EVT VT,
                                           bool ForCodeSize) const {
  if (VT != MVT::f32 && VT != MVT::f64)
    return false;
  if (Imm.isNegZero())
    return false;
  return (Imm.isZero() || Imm.isExactlyValue(+1.0));
}

bool LoongArchTargetLowering::useSoftFloat() const {
  return Subtarget.useSoftFloat();
}

void LoongArchTargetLowering::copyByValRegs(
    SDValue Chain, const SDLoc &DL, std::vector<SDValue> &OutChains,
    SelectionDAG &DAG, const ISD::ArgFlagsTy &Flags,
    SmallVectorImpl<SDValue> &InVals, const Argument *FuncArg,
    unsigned FirstReg, unsigned LastReg, const CCValAssign &VA,
    LoongArchCCState &State) const {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  unsigned GPRSizeInBytes = Subtarget.getGPRSizeInBytes();
  unsigned NumRegs = LastReg - FirstReg;
  unsigned RegAreaSize = NumRegs * GPRSizeInBytes;
  unsigned FrameObjSize = std::max(Flags.getByValSize(), RegAreaSize);
  int FrameObjOffset;
  ArrayRef<MCPhysReg> ByValArgRegs = ABI.GetByValArgRegs();

  if (RegAreaSize)
    FrameObjOffset =
        (int)ABI.GetCalleeAllocdArgSizeInBytes(State.getCallingConv()) -
        (int)((ByValArgRegs.size() - FirstReg) * GPRSizeInBytes);
  else
    FrameObjOffset = VA.getLocMemOffset();

  // Create frame object.
  EVT PtrTy = getPointerTy(DAG.getDataLayout());
  // Make the fixed object stored to mutable so that the load instructions
  // referencing it have their memory dependencies added.
  // Set the frame object as isAliased which clears the underlying objects
  // vector in ScheduleDAGInstrs::buildSchedGraph() resulting in addition of all
  // stores as dependencies for loads referencing this fixed object.
  int FI = MFI.CreateFixedObject(FrameObjSize, FrameObjOffset, false, true);
  SDValue FIN = DAG.getFrameIndex(FI, PtrTy);
  InVals.push_back(FIN);

  if (!NumRegs)
    return;

  // Copy arg registers.
  MVT RegTy = MVT::getIntegerVT(GPRSizeInBytes * 8);
  const TargetRegisterClass *RC = getRegClassFor(RegTy);

  for (unsigned I = 0; I < NumRegs; ++I) {
    unsigned ArgReg = ByValArgRegs[FirstReg + I];
    unsigned VReg = addLiveIn(MF, ArgReg, RC);
    unsigned Offset = I * GPRSizeInBytes;
    SDValue StorePtr = DAG.getNode(ISD::ADD, DL, PtrTy, FIN,
                                   DAG.getConstant(Offset, DL, PtrTy));
    SDValue Store = DAG.getStore(Chain, DL, DAG.getRegister(VReg, RegTy),
                                 StorePtr, MachinePointerInfo(FuncArg, Offset));
    OutChains.push_back(Store);
  }
}

// Copy byVal arg to registers and stack.
void LoongArchTargetLowering::passByValArg(
    SDValue Chain, const SDLoc &DL,
    std::deque<std::pair<unsigned, SDValue>> &RegsToPass,
    SmallVectorImpl<SDValue> &MemOpChains, SDValue StackPtr,
    MachineFrameInfo &MFI, SelectionDAG &DAG, SDValue Arg, unsigned FirstReg,
    unsigned LastReg, const ISD::ArgFlagsTy &Flags,
    const CCValAssign &VA) const {
  unsigned ByValSizeInBytes = Flags.getByValSize();
  unsigned OffsetInBytes = 0; // From beginning of struct
  unsigned RegSizeInBytes = Subtarget.getGPRSizeInBytes();
  Align Alignment =
      std::min(Flags.getNonZeroByValAlign(), Align(RegSizeInBytes));
  EVT PtrTy = getPointerTy(DAG.getDataLayout()),
      RegTy = MVT::getIntegerVT(RegSizeInBytes * 8);
  unsigned NumRegs = LastReg - FirstReg;

  if (NumRegs) {
    ArrayRef<MCPhysReg> ArgRegs = ABI.GetByValArgRegs();
    bool LeftoverBytes = (NumRegs * RegSizeInBytes > ByValSizeInBytes);
    unsigned I = 0;

    // Copy words to registers.
    for (; I < NumRegs - LeftoverBytes; ++I, OffsetInBytes += RegSizeInBytes) {
      SDValue LoadPtr = DAG.getNode(ISD::ADD, DL, PtrTy, Arg,
                                    DAG.getConstant(OffsetInBytes, DL, PtrTy));
      SDValue LoadVal = DAG.getLoad(RegTy, DL, Chain, LoadPtr,
                                    MachinePointerInfo(), Alignment);
      MemOpChains.push_back(LoadVal.getValue(1));
      unsigned ArgReg = ArgRegs[FirstReg + I];
      RegsToPass.push_back(std::make_pair(ArgReg, LoadVal));
    }

    // Return if the struct has been fully copied.
    if (ByValSizeInBytes == OffsetInBytes)
      return;

    // Copy the remainder of the byval argument with sub-word loads and shifts.
    if (LeftoverBytes) {
      SDValue Val;

      for (unsigned LoadSizeInBytes = RegSizeInBytes / 2, TotalBytesLoaded = 0;
           OffsetInBytes < ByValSizeInBytes; LoadSizeInBytes /= 2) {
        unsigned RemainingSizeInBytes = ByValSizeInBytes - OffsetInBytes;

        if (RemainingSizeInBytes < LoadSizeInBytes)
          continue;

        // Load subword.
        SDValue LoadPtr = DAG.getNode(ISD::ADD, DL, PtrTy, Arg,
                                      DAG.getConstant(OffsetInBytes, DL,
                                                      PtrTy));
        SDValue LoadVal = DAG.getExtLoad(
            ISD::ZEXTLOAD, DL, RegTy, Chain, LoadPtr, MachinePointerInfo(),
            MVT::getIntegerVT(LoadSizeInBytes * 8), Alignment);
        MemOpChains.push_back(LoadVal.getValue(1));

        // Shift the loaded value.
        unsigned Shamt;

        Shamt = TotalBytesLoaded * 8;

        SDValue Shift = DAG.getNode(ISD::SHL, DL, RegTy, LoadVal,
                                    DAG.getConstant(Shamt, DL, MVT::i32));

        if (Val.getNode())
          Val = DAG.getNode(ISD::OR, DL, RegTy, Val, Shift);
        else
          Val = Shift;

        OffsetInBytes += LoadSizeInBytes;
        TotalBytesLoaded += LoadSizeInBytes;
        Alignment = std::min(Alignment, Align(LoadSizeInBytes));
      }

      unsigned ArgReg = ArgRegs[FirstReg + I];
      RegsToPass.push_back(std::make_pair(ArgReg, Val));
      return;
    }
  }

  // Copy remainder of byval arg to it with memcpy.
  unsigned MemCpySize = ByValSizeInBytes - OffsetInBytes;
  SDValue Src = DAG.getNode(ISD::ADD, DL, PtrTy, Arg,
                            DAG.getConstant(OffsetInBytes, DL, PtrTy));
  SDValue Dst = DAG.getNode(ISD::ADD, DL, PtrTy, StackPtr,
                            DAG.getIntPtrConstant(VA.getLocMemOffset(), DL));
  Chain = DAG.getMemcpy(
      Chain, DL, Dst, Src, DAG.getConstant(MemCpySize, DL, PtrTy),
      Align(Alignment), /*isVolatile=*/false, /*AlwaysInline=*/false,
      /*isTailCall=*/false, MachinePointerInfo(), MachinePointerInfo());
  MemOpChains.push_back(Chain);
}

void LoongArchTargetLowering::writeVarArgRegs(std::vector<SDValue> &OutChains,
                                         SDValue Chain, const SDLoc &DL,
                                         SelectionDAG &DAG,
                                         CCState &State) const {
  ArrayRef<MCPhysReg> ArgRegs = ABI.GetVarArgRegs();
  unsigned Idx = State.getFirstUnallocated(ArgRegs);
  unsigned RegSizeInBytes = Subtarget.getGPRSizeInBytes();
  MVT RegTy = MVT::getIntegerVT(RegSizeInBytes * 8);
  const TargetRegisterClass *RC = getRegClassFor(RegTy);
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  LoongArchFunctionInfo *LoongArchFI = MF.getInfo<LoongArchFunctionInfo>();

  // Offset of the first variable argument from stack pointer.
  int VaArgOffset;

  if (ArgRegs.size() == Idx)
    VaArgOffset = alignTo(State.getNextStackOffset(), RegSizeInBytes);
  else {
    VaArgOffset =
        (int)ABI.GetCalleeAllocdArgSizeInBytes(State.getCallingConv()) -
        (int)(RegSizeInBytes * (ArgRegs.size() - Idx));
  }

  // Record the frame index of the first variable argument
  // which is a value necessary to VASTART.
  int FI = MFI.CreateFixedObject(RegSizeInBytes, VaArgOffset, true);
  LoongArchFI->setVarArgsFrameIndex(FI);

  // Copy the integer registers that have not been used for argument passing
  // to the argument register save area. For LP32, the save area is allocated
  // in the caller's stack frame, while for LPX32/LP64, it is allocated in the
  // callee's stack frame.
  for (unsigned I = Idx; I < ArgRegs.size();
       ++I, VaArgOffset += RegSizeInBytes) {
    unsigned Reg = addLiveIn(MF, ArgRegs[I], RC);
    SDValue ArgValue = DAG.getCopyFromReg(Chain, DL, Reg, RegTy);
    FI = MFI.CreateFixedObject(RegSizeInBytes, VaArgOffset, true);
    SDValue PtrOff = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
    SDValue Store =
        DAG.getStore(Chain, DL, ArgValue, PtrOff, MachinePointerInfo());
    cast<StoreSDNode>(Store.getNode())->getMemOperand()->setValue(
        (Value *)nullptr);
    OutChains.push_back(Store);
  }
}

void LoongArchTargetLowering::HandleByVal(CCState *State, unsigned &Size,
                                          Align Alignment) const {
  const TargetFrameLowering *TFL = Subtarget.getFrameLowering();

  assert(Size && "Byval argument's size shouldn't be 0.");

  Alignment = std::min(Alignment, TFL->getStackAlign());

  unsigned FirstReg = 0;
  unsigned NumRegs = 0;

  if (State->getCallingConv() != CallingConv::Fast) {
    unsigned RegSizeInBytes = Subtarget.getGPRSizeInBytes();
    ArrayRef<MCPhysReg> IntArgRegs = ABI.GetByValArgRegs();
    // FIXME: The LP32 case actually describes no shadow registers.
    const MCPhysReg *ShadowRegs =
        ABI.IsLP32() ? IntArgRegs.data() : LoongArch64DPRegs;

    // We used to check the size as well but we can't do that anymore since
    // CCState::HandleByVal() rounds up the size after calling this function.
    assert(
        Alignment >= Align(RegSizeInBytes) &&
        "Byval argument's alignment should be a multiple of RegSizeInBytes.");

    FirstReg = State->getFirstUnallocated(IntArgRegs);

    // If Alignment > RegSizeInBytes, the first arg register must be even.
    // FIXME: This condition happens to do the right thing but it's not the
    //        right way to test it. We want to check that the stack frame offset
    //        of the register is aligned.
    if ((Alignment > RegSizeInBytes) && (FirstReg % 2)) {
      State->AllocateReg(IntArgRegs[FirstReg], ShadowRegs[FirstReg]);
      ++FirstReg;
      //assert(true && "debug#######################################");
    }

    // Mark the registers allocated.
    //Size = alignTo(Size, RegSizeInBytes);
    //for (unsigned I = FirstReg; Size > 0 && (I < IntArgRegs.size());
    //     Size -= RegSizeInBytes, ++I, ++NumRegs)
    //  State->AllocateReg(IntArgRegs[I], ShadowRegs[I]);
  }

  State->addInRegsParamInfo(FirstReg, FirstReg + NumRegs);
}

MachineBasicBlock *LoongArchTargetLowering::emitPseudoSELECT(MachineInstr &MI,
                                                        MachineBasicBlock *BB,
                                                        bool isFPCmp,
                                                        unsigned Opc) const {
  const TargetInstrInfo *TII =
      Subtarget.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();

  // To "insert" a SELECT instruction, we actually have to insert the
  // diamond control-flow pattern.  The incoming instruction knows the
  // destination vreg to set, the condition code register to branch on, the
  // true/false values to select between, and a branch opcode to use.
  const BasicBlock *LLVM_BB = BB->getBasicBlock();
  MachineFunction::iterator It = ++BB->getIterator();

  //  thisMBB:
  //  ...
  //   TrueVal = ...
  //   setcc r1, r2, r3
  //   bNE   r1, r0, copy1MBB
  //   fallthrough --> copy0MBB
  MachineBasicBlock *thisMBB  = BB;
  MachineFunction *F = BB->getParent();
  MachineBasicBlock *copy0MBB = F->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *sinkMBB  = F->CreateMachineBasicBlock(LLVM_BB);
  F->insert(It, copy0MBB);
  F->insert(It, sinkMBB);

  // Transfer the remainder of BB and its successor edges to sinkMBB.
  sinkMBB->splice(sinkMBB->begin(), BB,
                  std::next(MachineBasicBlock::iterator(MI)), BB->end());
  sinkMBB->transferSuccessorsAndUpdatePHIs(BB);

  // Next, add the true and fallthrough blocks as its successors.
  BB->addSuccessor(copy0MBB);
  BB->addSuccessor(sinkMBB);

  if (isFPCmp) {
    // bc1[tf] cc, sinkMBB
    BuildMI(BB, DL, TII->get(Opc))
        .addReg(MI.getOperand(1).getReg())
        .addMBB(sinkMBB);
  } else {
    BuildMI(BB, DL, TII->get(Opc))
        .addReg(MI.getOperand(1).getReg())
        .addReg(LoongArch::ZERO)
        .addMBB(sinkMBB);
  }

  //  copy0MBB:
  //   %FalseValue = ...
  //   # fallthrough to sinkMBB
  BB = copy0MBB;

  // Update machine-CFG edges
  BB->addSuccessor(sinkMBB);

  //  sinkMBB:
  //   %Result = phi [ %TrueValue, thisMBB ], [ %FalseValue, copy0MBB ]
  //  ...
  BB = sinkMBB;

  BuildMI(*BB, BB->begin(), DL, TII->get(LoongArch::PHI), MI.getOperand(0).getReg())
      .addReg(MI.getOperand(2).getReg())
      .addMBB(thisMBB)
      .addReg(MI.getOperand(3).getReg())
      .addMBB(copy0MBB);

  MI.eraseFromParent(); // The pseudo instruction is gone now.

  return BB;
}

bool LoongArchTargetLowering::isFMAFasterThanFMulAndFAdd(
    const MachineFunction &MF, EVT VT) const {

  VT = VT.getScalarType();

  if (!VT.isSimple())
    return false;

  switch (VT.getSimpleVT().SimpleTy) {
  case MVT::f32:
  case MVT::f64:
    return true;
  default:
    break;
  }

  return false;
}
