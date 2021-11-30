//===-- LoongArchAsmParser.cpp - Parse LoongArch assembly to MCInst instructions ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/LoongArchFPABIInfo.h"
#include "MCTargetDesc/LoongArchABIInfo.h"
#include "MCTargetDesc/LoongArchBaseInfo.h"
#include "MCTargetDesc/LoongArchMCExpr.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "LoongArchTargetStreamer.h"
#include "TargetInfo/LoongArchTargetInfo.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Triple.h"
#include "llvm/ADT/Twine.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCAsmParserExtension.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/MC/MCValue.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <memory>
#include <string>
#include <utility>

using namespace llvm;

#define DEBUG_TYPE "loongarch-asm-parser"

namespace llvm {

class MCInstrInfo;

} // end namespace llvm

namespace {

class LoongArchAssemblerOptions {
public:
  LoongArchAssemblerOptions(const FeatureBitset &Features_) : Features(Features_) {}

  LoongArchAssemblerOptions(const LoongArchAssemblerOptions *Opts) {
    Features = Opts->getFeatures();
  }

  const FeatureBitset &getFeatures() const { return Features; }
  void setFeatures(const FeatureBitset &Features_) { Features = Features_; }

private:
  FeatureBitset Features;
};

} // end anonymous namespace

namespace {

class LoongArchAsmParser : public MCTargetAsmParser {
  LoongArchTargetStreamer &getTargetStreamer() {
    MCTargetStreamer &TS = *getParser().getStreamer().getTargetStreamer();
    return static_cast<LoongArchTargetStreamer &>(TS);
  }

  LoongArchABIInfo ABI;
  SmallVector<std::unique_ptr<LoongArchAssemblerOptions>, 2> AssemblerOptions;
  MCSymbol *CurrentFn; // Pointer to the function being parsed. It may be a
                       // nullptr, which indicates that no function is currently
                       // selected. This usually happens after an '.end'
                       // directive.
  bool IsPicEnabled;

  // Map of register aliases created via the .set directive.
  StringMap<AsmToken> RegisterSets;

#define GET_ASSEMBLER_HEADER
#include "LoongArchGenAsmMatcher.inc"

  unsigned checkTargetMatchPredicate(MCInst &Inst) override;

  bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               OperandVector &Operands, MCStreamer &Out,
                               uint64_t &ErrorInfo,
                               bool MatchingInlineAsm) override;

  /// Parse a register as used in CFI directives
  bool ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) override;
  OperandMatchResultTy tryParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                        SMLoc &EndLoc) override;

  bool mnemonicIsValid(StringRef Mnemonic);

  bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;

  bool ParseDirective(AsmToken DirectiveID) override;

  OperandMatchResultTy parseMemOperand(OperandVector &Operands);
  OperandMatchResultTy
  matchAnyRegisterNameWithoutDollar(OperandVector &Operands,
                                    StringRef Identifier, SMLoc S);
  OperandMatchResultTy matchAnyRegisterWithoutDollar(OperandVector &Operands,
                                                     const AsmToken &Token,
                                                     SMLoc S);
  OperandMatchResultTy matchAnyRegisterWithoutDollar(OperandVector &Operands,
                                                     SMLoc S);
  OperandMatchResultTy parseAnyRegister(OperandVector &Operands);
  OperandMatchResultTy parseJumpTarget(OperandVector &Operands);

  bool searchSymbolAlias(OperandVector &Operands);

  bool parseOperand(OperandVector &, StringRef Mnemonic);

  enum MacroExpanderResultTy {
    MER_NotAMacro,
    MER_Success,
    MER_Fail,
  };

  // Expands assembly pseudo instructions.
  MacroExpanderResultTy tryExpandInstruction(MCInst &Inst, SMLoc IDLoc,
                                             MCStreamer &Out,
                                             const MCSubtargetInfo *STI);

  bool loadImmediate(int64_t ImmValue, unsigned DstReg, unsigned SrcReg,
                     bool Is32BitImm, bool IsAddress, SMLoc IDLoc,
                     MCStreamer &Out, const MCSubtargetInfo *STI);

  bool expandLoadImm(MCInst &Inst, bool Is32BitImm, SMLoc IDLoc,
                     MCStreamer &Out, const MCSubtargetInfo *STI);

  bool expandLoadAddress(unsigned DstReg, const MCOperand &Offset,
                         bool IsLocal, SMLoc IDLoc, MCStreamer &Out,
                         const MCSubtargetInfo *STI);

  bool reportParseError(Twine ErrorMsg);

  bool parseMemOffset(const MCExpr *&Res);

  bool isEvaluated(const MCExpr *Expr);
  bool parseDirectiveSet();

  bool parseSetFpDirective();
  bool parseSetSoftFloatDirective();
  bool parseSetHardFloatDirective();

  bool parseSetAssignment();

  bool parseDirectiveModule();
  bool parseDirectiveModuleFP();
  bool parseFpABIValue(LoongArchFPABIInfo::FpABIKind &FpABI,
                       StringRef Directive);

  bool parseInternalDirectiveReallowModule();

  int matchCPURegisterName(StringRef Symbol);

  int matchFPURegisterName(StringRef Name);

  int matchFCFRRegisterName(StringRef Name);
  int matchFCSRRegisterName(StringRef Name);

  bool processInstruction(MCInst &Inst, SMLoc IDLoc, MCStreamer &Out,
                          const MCSubtargetInfo *STI);

  void setFeatureBits(uint64_t Feature, StringRef FeatureString) {
    if (!(getSTI().getFeatureBits()[Feature])) {
      MCSubtargetInfo &STI = copySTI();
      setAvailableFeatures(
          ComputeAvailableFeatures(STI.ToggleFeature(FeatureString)));
      AssemblerOptions.back()->setFeatures(STI.getFeatureBits());
    }
  }

  void clearFeatureBits(uint64_t Feature, StringRef FeatureString) {
    if (getSTI().getFeatureBits()[Feature]) {
      MCSubtargetInfo &STI = copySTI();
      setAvailableFeatures(
          ComputeAvailableFeatures(STI.ToggleFeature(FeatureString)));
      AssemblerOptions.back()->setFeatures(STI.getFeatureBits());
    }
  }

  void setModuleFeatureBits(uint64_t Feature, StringRef FeatureString) {
    setFeatureBits(Feature, FeatureString);
    AssemblerOptions.front()->setFeatures(getSTI().getFeatureBits());
  }

  void clearModuleFeatureBits(uint64_t Feature, StringRef FeatureString) {
    clearFeatureBits(Feature, FeatureString);
    AssemblerOptions.front()->setFeatures(getSTI().getFeatureBits());
  }

public:
  enum LoongArchMatchResultTy {
    Match_RequiresNoZeroRegister = FIRST_TARGET_MATCH_RESULT_TY,
    Match_RequiresNoRaRegister,
    Match_RequiresSameSrcAndDst,
    Match_RequiresRange0_31,
    Match_RequiresRange0_63,
    Match_MsbHigherThanLsb,
    Match_RequiresPosSizeUImm6,
#define GET_OPERAND_DIAGNOSTIC_TYPES
#include "LoongArchGenAsmMatcher.inc"
#undef GET_OPERAND_DIAGNOSTIC_TYPES
  };

  LoongArchAsmParser(const MCSubtargetInfo &sti, MCAsmParser &parser,
                     const MCInstrInfo &MII, const MCTargetOptions &Options)
    : MCTargetAsmParser(Options, sti, MII),
        ABI(LoongArchABIInfo::computeTargetABI(Triple(sti.getTargetTriple()),
                                               sti.getCPU(), Options)) {
    MCAsmParserExtension::Initialize(parser);

    parser.addAliasForDirective(".asciiz", ".asciz");
    parser.addAliasForDirective(".hword", ".2byte");
    parser.addAliasForDirective(".word", ".4byte");
    parser.addAliasForDirective(".dword", ".8byte");

    // Initialize the set of available features.
    setAvailableFeatures(ComputeAvailableFeatures(getSTI().getFeatureBits()));

    // Remember the initial assembler options. The user can not modify these.
    AssemblerOptions.push_back(
        std::make_unique<LoongArchAssemblerOptions>(getSTI().getFeatureBits()));

    // Create an assembler options environment for the user to modify.
    AssemblerOptions.push_back(
        std::make_unique<LoongArchAssemblerOptions>(getSTI().getFeatureBits()));

    getTargetStreamer().updateABIInfo(*this);

    CurrentFn = nullptr;

    IsPicEnabled = getContext().getObjectFileInfo()->isPositionIndependent();
  }

  bool is64Bit() const {
    return getSTI().getFeatureBits()[LoongArch::Feature64Bit];
  }

  bool isFP64bit() const {
    return getSTI().getFeatureBits()[LoongArch::FeatureFP64Bit];
  }

  const LoongArchABIInfo &getABI() const { return ABI; }
  bool isABI_LPX32() const { return ABI.IsLPX32(); }
  bool isABI_LP64() const { return ABI.IsLP64(); }
  bool isABI_LP32() const { return ABI.IsLP32(); }

  bool inPicMode() {
    return IsPicEnabled;
  }

  bool useSoftFloat() const {
    return getSTI().getFeatureBits()[LoongArch::FeatureSoftFloat];
  }

  const MCExpr *createTargetUnaryExpr(const MCExpr *E,
                                      AsmToken::TokenKind OperatorToken,
                                      MCContext &Ctx) override {
    switch(OperatorToken) {
    default:
      llvm_unreachable("Unknown token");
      return nullptr;
#if 0
    case AsmToken::PercentPlt:
      return LoongArchMCExpr::create(LoongArchMCExpr::MEK_PLT, E, Ctx);
#endif
    }
  }
};

/// LoongArchOperand - Instances of this class represent a parsed LoongArch machine
/// instruction.
class LoongArchOperand : public MCParsedAsmOperand {
public:
  /// Broad categories of register classes
  /// The exact class is finalized by the render method.
  enum RegKind {
    RegKind_GPR = 1,      /// GPR32 and GPR64 (depending on is64Bit())
    RegKind_FGR = 2,      /// FGR32, FGR64 (depending on isFP64bit())
    RegKind_FCFR = 4,     /// FCFR
    RegKind_FCSR = 8,     /// FCSR
    RegKind_Numeric = RegKind_GPR | RegKind_FGR | RegKind_FCFR | RegKind_FCSR
  };

private:
  enum KindTy {
    k_Immediate,     /// An immediate (possibly involving symbol references)
    k_Memory,        /// Base + Offset Memory Address
    k_RegisterIndex, /// A register index in one or more RegKind.
    k_Token,         /// A simple token
    k_RegList,       /// A physical register list
  } Kind;

public:
  LoongArchOperand(KindTy K, LoongArchAsmParser &Parser)
      : MCParsedAsmOperand(), Kind(K), AsmParser(Parser) {}

  ~LoongArchOperand() override {
    switch (Kind) {
    case k_Memory:
      delete Mem.Base;
      break;
    case k_RegList:
      delete RegList.List;
      break;
    case k_Immediate:
    case k_RegisterIndex:
    case k_Token:
      break;
    }
  }

private:
  /// For diagnostics, and checking the assembler temporary
  LoongArchAsmParser &AsmParser;

  struct Token {
    const char *Data;
    unsigned Length;
  };

  struct RegIdxOp {
    unsigned Index; /// Index into the register class
    RegKind Kind;   /// Bitfield of the kinds it could possibly be
    struct Token Tok; /// The input token this operand originated from.
    const MCRegisterInfo *RegInfo;
  };

  struct ImmOp {
    const MCExpr *Val;
  };

  struct MemOp {
    LoongArchOperand *Base;
    const MCExpr *Off;
  };

  struct RegListOp {
    SmallVector<unsigned, 10> *List;
  };

  union {
    struct Token Tok;
    struct RegIdxOp RegIdx;
    struct ImmOp Imm;
    struct MemOp Mem;
    struct RegListOp RegList;
  };

  SMLoc StartLoc, EndLoc;

  /// Internal constructor for register kinds
  static std::unique_ptr<LoongArchOperand> CreateReg(unsigned Index, StringRef Str,
                                                     RegKind RegKind,
                                                     const MCRegisterInfo *RegInfo,
                                                     SMLoc S, SMLoc E,
                                                     LoongArchAsmParser &Parser) {
    auto Op = std::make_unique<LoongArchOperand>(k_RegisterIndex, Parser);
    Op->RegIdx.Index = Index;
    Op->RegIdx.RegInfo = RegInfo;
    Op->RegIdx.Kind = RegKind;
    Op->RegIdx.Tok.Data = Str.data();
    Op->RegIdx.Tok.Length = Str.size();
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

public:
  /// Coerce the register to GPR32 and return the real register for the current
  /// target.
  unsigned getGPR32Reg() const {
    assert(isRegIdx() && (RegIdx.Kind & RegKind_GPR) && "Invalid access!");
    unsigned ClassID = LoongArch::GPR32RegClassID;
    return RegIdx.RegInfo->getRegClass(ClassID).getRegister(RegIdx.Index);
  }

  /// Coerce the register to GPR32 and return the real register for the current
  /// target.
  unsigned getGPRMM16Reg() const {
    assert(isRegIdx() && (RegIdx.Kind & RegKind_GPR) && "Invalid access!");
    unsigned ClassID = LoongArch::GPR32RegClassID;
    return RegIdx.RegInfo->getRegClass(ClassID).getRegister(RegIdx.Index);
  }

  /// Coerce the register to GPR64 and return the real register for the current
  /// target.
  unsigned getGPR64Reg() const {
    assert(isRegIdx() && (RegIdx.Kind & RegKind_GPR) && "Invalid access!");
    unsigned ClassID = LoongArch::GPR64RegClassID;
    return RegIdx.RegInfo->getRegClass(ClassID).getRegister(RegIdx.Index);
  }

private:
  /// Coerce the register to FGR64 and return the real register for the current
  /// target.
  unsigned getFGR64Reg() const {
    assert(isRegIdx() && (RegIdx.Kind & RegKind_FGR) && "Invalid access!");
    return RegIdx.RegInfo->getRegClass(LoongArch::FGR64RegClassID)
        .getRegister(RegIdx.Index);
  }

  /// Coerce the register to FGR32 and return the real register for the current
  /// target.
  unsigned getFGR32Reg() const {
    assert(isRegIdx() && (RegIdx.Kind & RegKind_FGR) && "Invalid access!");
    return RegIdx.RegInfo->getRegClass(LoongArch::FGR32RegClassID)
        .getRegister(RegIdx.Index);
  }

  /// Coerce the register to FCFR and return the real register for the current
  /// target.
  unsigned getFCFRReg() const {
    assert(isRegIdx() && (RegIdx.Kind & RegKind_FCFR) && "Invalid access!");
    return RegIdx.RegInfo->getRegClass(LoongArch::FCFRRegClassID)
        .getRegister(RegIdx.Index);
  }

  /// Coerce the register to CCR and return the real register for the
  /// current target.
  unsigned getFCSRReg() const {
    assert(isRegIdx() && (RegIdx.Kind & RegKind_FCSR) && "Invalid access!");
    unsigned ClassID = LoongArch::FCSRRegClassID;
    return RegIdx.RegInfo->getRegClass(ClassID).getRegister(RegIdx.Index);
  }

public:
  void addExpr(MCInst &Inst, const MCExpr *Expr) const {
    // Add as immediate when possible.  Null MCExpr = 0.
    if (!Expr)
      Inst.addOperand(MCOperand::createImm(0));
    else if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Expr))
      Inst.addOperand(MCOperand::createImm(CE->getValue()));
    else
      Inst.addOperand(MCOperand::createExpr(Expr));
  }

  void addRegOperands(MCInst &Inst, unsigned N) const {
    llvm_unreachable("Use a custom parser instead");
  }

  /// Render the operand to an MCInst as a GPR32
  /// Asserts if the wrong number of operands are requested, or the operand
  /// is not a k_RegisterIndex compatible with RegKind_GPR
  void addGPR32ZeroAsmRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getGPR32Reg()));
  }

  void addGPR32NonZeroAsmRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getGPR32Reg()));
  }

  void addGPR32AsmRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getGPR32Reg()));
  }

  void addGPRMM16AsmRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getGPRMM16Reg()));
  }

  void addGPRMM16AsmRegZeroOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getGPRMM16Reg()));
  }

  void addGPRMM16AsmRegMovePOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getGPRMM16Reg()));
  }

  void addGPRMM16AsmRegMovePPairFirstOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getGPRMM16Reg()));
  }

  void addGPRMM16AsmRegMovePPairSecondOperands(MCInst &Inst,
                                               unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getGPRMM16Reg()));
  }

  /// Render the operand to an MCInst as a GPR64
  /// Asserts if the wrong number of operands are requested, or the operand
  /// is not a k_RegisterIndex compatible with RegKind_GPR
  void addGPR64AsmRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getGPR64Reg()));
  }

  void addStrictlyFGR64AsmRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getFGR64Reg()));
  }

  void addFGR64AsmRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getFGR64Reg()));
  }

  void addFGR32AsmRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getFGR32Reg()));
  }

  void addStrictlyFGR32AsmRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getFGR32Reg()));
  }

  void addFCFRAsmRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getFCFRReg()));
  }

  void addFCSRAsmRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getFCSRReg()));
  }

  template <unsigned Bits, int Offset = 0, int AdjustOffset = 0>
  void addConstantUImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    uint64_t Imm = getConstantImm() - Offset;
    Imm &= (1ULL << Bits) - 1;
    Imm += Offset;
    Imm += AdjustOffset;
    Inst.addOperand(MCOperand::createImm(Imm));
  }

  template <unsigned Bits>
  void addSImmOperands(MCInst &Inst, unsigned N) const {
    if (isImm() && !isConstantImm()) {
      addExpr(Inst, getImm());
      return;
    }
    addConstantSImmOperands<Bits, 0, 0>(Inst, N);
  }

  template <unsigned Bits>
  void addUImmOperands(MCInst &Inst, unsigned N) const {
    if (isImm() && !isConstantImm()) {
      addExpr(Inst, getImm());
      return;
    }
    addConstantUImmOperands<Bits, 0, 0>(Inst, N);
  }

  template <unsigned Bits, int Offset = 0, int AdjustOffset = 0>
  void addConstantSImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    int64_t Imm = getConstantImm() - Offset;
    Imm = SignExtend64<Bits>(Imm);
    Imm += Offset;
    Imm += AdjustOffset;
    Inst.addOperand(MCOperand::createImm(Imm));
  }

  void addImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    const MCExpr *Expr = getImm();
    addExpr(Inst, Expr);
  }

  void addMemOperands(MCInst &Inst, unsigned N) const {
    assert(N == 2 && "Invalid number of operands!");

    Inst.addOperand(MCOperand::createReg(AsmParser.getABI().ArePtrs64bit()
                                             ? getMemBase()->getGPR64Reg()
                                             : getMemBase()->getGPR32Reg()));

    const MCExpr *Expr = getMemOff();
    addExpr(Inst, Expr);
  }

  void addRegListOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");

    for (auto RegNo : getRegList())
      Inst.addOperand(MCOperand::createReg(RegNo));
  }

  bool isReg() const override {
    // As a special case until we sort out the definition of div/divu, accept
    // $0/$zero here so that MCK_ZERO works correctly.
    return isGPRAsmReg() && RegIdx.Index == 0;
  }

  bool isRegIdx() const { return Kind == k_RegisterIndex; }
  bool isImm() const override { return Kind == k_Immediate; }

  bool isConstantImm() const {
    int64_t Res;
    return isImm() && getImm()->evaluateAsAbsolute(Res);
  }

  bool isConstantImmz() const {
    return isConstantImm() && getConstantImm() == 0;
  }

  template <unsigned Bits, int Offset = 0> bool isConstantUImm() const {
    return isConstantImm() && isUInt<Bits>(getConstantImm() - Offset);
  }

  template <unsigned Bits> bool isSImm() const {
    return isConstantImm() ? isInt<Bits>(getConstantImm()) : isImm();
  }

  template <unsigned Bits> bool isUImm() const {
    return isConstantImm() ? isUInt<Bits>(getConstantImm()) : isImm();
  }

  template <unsigned Bits> bool isAnyImm() const {
    return isConstantImm() ? (isInt<Bits>(getConstantImm()) ||
                              isUInt<Bits>(getConstantImm()))
                           : isImm();
  }

  template <unsigned Bits, int Offset = 0> bool isConstantSImm() const {
    return isConstantImm() && isInt<Bits>(getConstantImm() - Offset);
  }

  template <unsigned Bottom, unsigned Top> bool isConstantUImmRange() const {
    return isConstantImm() && getConstantImm() >= Bottom &&
           getConstantImm() <= Top;
  }

  bool isToken() const override {
    // Note: It's not possible to pretend that other operand kinds are tokens.
    // The matcher emitter checks tokens first.
    return Kind == k_Token;
  }

  bool isMem() const override { return Kind == k_Memory; }

  bool isConstantMemOff() const {
    return isMem() && isa<MCConstantExpr>(getMemOff());
  }

  // Allow relocation operators.
  // FIXME: This predicate and others need to look through binary expressions
  //        and determine whether a Value is a constant or not.
  template <unsigned Bits, unsigned ShiftAmount = 0>
  bool isMemWithSimmOffset() const {
    if (!isMem())
      return false;
    if (!getMemBase()->isGPRAsmReg())
      return false;
    if (isa<MCTargetExpr>(getMemOff()) ||
        (isConstantMemOff() &&
         isShiftedInt<Bits, ShiftAmount>(getConstantMemOff())))
      return true;
    MCValue Res;
    bool IsReloc = getMemOff()->evaluateAsRelocatable(Res, nullptr, nullptr);
    return IsReloc && isShiftedInt<Bits, ShiftAmount>(Res.getConstant());
  }

  bool isMemWithPtrSizeOffset() const {
    if (!isMem())
      return false;
    if (!getMemBase()->isGPRAsmReg())
      return false;
    const unsigned PtrBits = AsmParser.getABI().ArePtrs64bit() ? 64 : 32;
    if (isa<MCTargetExpr>(getMemOff()) ||
        (isConstantMemOff() && isIntN(PtrBits, getConstantMemOff())))
      return true;
    MCValue Res;
    bool IsReloc = getMemOff()->evaluateAsRelocatable(Res, nullptr, nullptr);
    return IsReloc && isIntN(PtrBits, Res.getConstant());
  }

  bool isMemWithGRPMM16Base() const {
    return isMem() && getMemBase()->isMM16AsmReg();
  }

  template <unsigned Bits> bool isMemWithUimmOffsetSP() const {
    return isMem() && isConstantMemOff() && isUInt<Bits>(getConstantMemOff())
      && getMemBase()->isRegIdx() && (getMemBase()->getGPR32Reg() == LoongArch::SP);
  }

  template <unsigned Bits> bool isMemWithUimmWordAlignedOffsetSP() const {
    return isMem() && isConstantMemOff() && isUInt<Bits>(getConstantMemOff())
      && (getConstantMemOff() % 4 == 0) && getMemBase()->isRegIdx()
      && (getMemBase()->getGPR32Reg() == LoongArch::SP);
  }

  template <unsigned Bits, unsigned ShiftLeftAmount>
  bool isScaledUImm() const {
    return isConstantImm() &&
           isShiftedUInt<Bits, ShiftLeftAmount>(getConstantImm());
  }

  template <unsigned Bits, unsigned ShiftLeftAmount>
  bool isScaledSImm() const {
    if (isConstantImm() &&
        isShiftedInt<Bits, ShiftLeftAmount>(getConstantImm()))
      return true;
    // Operand can also be a symbol or symbol plus
    // offset in case of relocations.
    if (Kind != k_Immediate)
      return false;
    MCValue Res;
    bool Success = getImm()->evaluateAsRelocatable(Res, nullptr, nullptr);
    return Success && isShiftedInt<Bits, ShiftLeftAmount>(Res.getConstant());
  }

  bool isRegList16() const {
    if (!isRegList())
      return false;

    int Size = RegList.List->size();
    if (Size < 2 || Size > 5)
      return false;

    unsigned R0 = RegList.List->front();
    unsigned R1 = RegList.List->back();
    if (!((R0 == LoongArch::S0 && R1 == LoongArch::RA) ||
          (R0 == LoongArch::S0_64 && R1 == LoongArch::RA_64)))
      return false;

    int PrevReg = *RegList.List->begin();
    for (int i = 1; i < Size - 1; i++) {
      int Reg = (*(RegList.List))[i];
      if ( Reg != PrevReg + 1)
        return false;
      PrevReg = Reg;
    }

    return true;
  }

  bool isInvNum() const { return Kind == k_Immediate; }

  bool isLSAImm() const {
    if (!isConstantImm())
      return false;
    int64_t Val = getConstantImm();
    return 1 <= Val && Val <= 4;
  }

  bool isRegList() const { return Kind == k_RegList; }

  StringRef getToken() const {
    assert(Kind == k_Token && "Invalid access!");
    return StringRef(Tok.Data, Tok.Length);
  }

  unsigned getReg() const override {
    // As a special case until we sort out the definition of div/divu, accept
    // $0/$zero here so that MCK_ZERO works correctly.
    if (Kind == k_RegisterIndex && RegIdx.Index == 0 &&
        RegIdx.Kind & RegKind_GPR)
      return getGPR32Reg(); // FIXME: GPR64 too

    llvm_unreachable("Invalid access!");
    return 0;
  }

  const MCExpr *getImm() const {
    assert((Kind == k_Immediate) && "Invalid access!");
    return Imm.Val;
  }

  int64_t getConstantImm() const {
    const MCExpr *Val = getImm();
    int64_t Value = 0;
    (void)Val->evaluateAsAbsolute(Value);
    return Value;
  }

  LoongArchOperand *getMemBase() const {
    assert((Kind == k_Memory) && "Invalid access!");
    return Mem.Base;
  }

  const MCExpr *getMemOff() const {
    assert((Kind == k_Memory) && "Invalid access!");
    return Mem.Off;
  }

  int64_t getConstantMemOff() const {
    return static_cast<const MCConstantExpr *>(getMemOff())->getValue();
  }

  const SmallVectorImpl<unsigned> &getRegList() const {
    assert((Kind == k_RegList) && "Invalid access!");
    return *(RegList.List);
  }

  static std::unique_ptr<LoongArchOperand> CreateToken(StringRef Str, SMLoc S,
                                                  LoongArchAsmParser &Parser) {
    auto Op = std::make_unique<LoongArchOperand>(k_Token, Parser);
    Op->Tok.Data = Str.data();
    Op->Tok.Length = Str.size();
    Op->StartLoc = S;
    Op->EndLoc = S;
    return Op;
  }

  /// Create a numeric register (e.g. $1). The exact register remains
  /// unresolved until an instruction successfully matches
  static std::unique_ptr<LoongArchOperand>
  createNumericReg(unsigned Index, StringRef Str, const MCRegisterInfo *RegInfo,
                   SMLoc S, SMLoc E, LoongArchAsmParser &Parser) {
    LLVM_DEBUG(dbgs() << "createNumericReg(" << Index << ", ...)\n");
    return CreateReg(Index, Str, RegKind_Numeric, RegInfo, S, E, Parser);
  }

  /// Create a register that is definitely a GPR.
  /// This is typically only used for named registers such as $gp.
  static std::unique_ptr<LoongArchOperand>
  createGPRReg(unsigned Index, StringRef Str, const MCRegisterInfo *RegInfo,
               SMLoc S, SMLoc E, LoongArchAsmParser &Parser) {
    return CreateReg(Index, Str, RegKind_GPR, RegInfo, S, E, Parser);
  }

  /// Create a register that is definitely a FGR.
  /// This is typically only used for named registers such as $f0.
  static std::unique_ptr<LoongArchOperand>
  createFGRReg(unsigned Index, StringRef Str, const MCRegisterInfo *RegInfo,
               SMLoc S, SMLoc E, LoongArchAsmParser &Parser) {
    return CreateReg(Index, Str, RegKind_FGR, RegInfo, S, E, Parser);
  }

  /// Create a register that is definitely an FCFR.
  /// This is typically only used for named registers such as $fcc0.
  static std::unique_ptr<LoongArchOperand>
  createFCFRReg(unsigned Index, StringRef Str, const MCRegisterInfo *RegInfo,
               SMLoc S, SMLoc E, LoongArchAsmParser &Parser) {
    return CreateReg(Index, Str, RegKind_FCFR, RegInfo, S, E, Parser);
  }

  /// Create a register that is definitely an FCSR.
  /// This is typically only used for named registers such as $fcsr0.
  static std::unique_ptr<LoongArchOperand>
  createFCSRReg(unsigned Index, StringRef Str, const MCRegisterInfo *RegInfo,
                SMLoc S, SMLoc E, LoongArchAsmParser &Parser) {
    return CreateReg(Index, Str, RegKind_FCSR, RegInfo, S, E, Parser);
  }

  static std::unique_ptr<LoongArchOperand>
  CreateImm(const MCExpr *Val, SMLoc S, SMLoc E, LoongArchAsmParser &Parser) {
    auto Op = std::make_unique<LoongArchOperand>(k_Immediate, Parser);
    Op->Imm.Val = Val;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<LoongArchOperand>
  CreateMem(std::unique_ptr<LoongArchOperand> Base, const MCExpr *Off, SMLoc S,
            SMLoc E, LoongArchAsmParser &Parser) {
    auto Op = std::make_unique<LoongArchOperand>(k_Memory, Parser);
    Op->Mem.Base = Base.release();
    Op->Mem.Off = Off;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<LoongArchOperand>
  CreateRegList(SmallVectorImpl<unsigned> &Regs, SMLoc StartLoc, SMLoc EndLoc,
                LoongArchAsmParser &Parser) {
    assert(Regs.size() > 0 && "Empty list not allowed");

    auto Op = std::make_unique<LoongArchOperand>(k_RegList, Parser);
    Op->RegList.List = new SmallVector<unsigned, 10>(Regs.begin(), Regs.end());
    Op->StartLoc = StartLoc;
    Op->EndLoc = EndLoc;
    return Op;
  }

 bool isGPRZeroAsmReg() const {
    return isRegIdx() && RegIdx.Kind & RegKind_GPR && RegIdx.Index == 0;
  }

 bool isGPRNonZeroAsmReg() const {
   return isRegIdx() && RegIdx.Kind & RegKind_GPR && RegIdx.Index > 0 &&
          RegIdx.Index <= 31;
  }

  bool isGPRAsmReg() const {
    return isRegIdx() && RegIdx.Kind & RegKind_GPR && RegIdx.Index <= 31;
  }

  bool isMM16AsmReg() const {
    if (!(isRegIdx() && RegIdx.Kind))
      return false;
    return ((RegIdx.Index >= 2 && RegIdx.Index <= 7)
            || RegIdx.Index == 16 || RegIdx.Index == 17);

  }
  bool isMM16AsmRegZero() const {
    if (!(isRegIdx() && RegIdx.Kind))
      return false;
    return (RegIdx.Index == 0 ||
            (RegIdx.Index >= 2 && RegIdx.Index <= 7) ||
            RegIdx.Index == 17);
  }

  bool isMM16AsmRegMoveP() const {
    if (!(isRegIdx() && RegIdx.Kind))
      return false;
    return (RegIdx.Index == 0 || (RegIdx.Index >= 2 && RegIdx.Index <= 3) ||
      (RegIdx.Index >= 16 && RegIdx.Index <= 20));
  }

  bool isMM16AsmRegMovePPairFirst() const {
    if (!(isRegIdx() && RegIdx.Kind))
      return false;
    return RegIdx.Index >= 4 && RegIdx.Index <= 6;
  }

  bool isMM16AsmRegMovePPairSecond() const {
    if (!(isRegIdx() && RegIdx.Kind))
      return false;
    return (RegIdx.Index == 21 || RegIdx.Index == 22 ||
      (RegIdx.Index >= 5 && RegIdx.Index <= 7));
  }

  bool isFGRAsmReg() const {
    return isRegIdx() && RegIdx.Kind & RegKind_FGR && RegIdx.Index <= 31;
  }

  bool isStrictlyFGRAsmReg() const {
    return isRegIdx() && RegIdx.Kind == RegKind_FGR && RegIdx.Index <= 31;
  }

  bool isFCSRAsmReg() const {
    return isRegIdx() && RegIdx.Kind & RegKind_FCSR && RegIdx.Index <= 3;
  }

  bool isFCFRAsmReg() const {
    if (!(isRegIdx() && RegIdx.Kind & RegKind_FCFR))
      return false;
    return RegIdx.Index <= 7;
  }

  /// getStartLoc - Get the location of the first token of this operand.
  SMLoc getStartLoc() const override { return StartLoc; }
  /// getEndLoc - Get the location of the last token of this operand.
  SMLoc getEndLoc() const override { return EndLoc; }

  void print(raw_ostream &OS) const override {
    switch (Kind) {
    case k_Immediate:
      OS << "Imm<";
      OS << *Imm.Val;
      OS << ">";
      break;
    case k_Memory:
      OS << "Mem<";
      Mem.Base->print(OS);
      OS << ", ";
      OS << *Mem.Off;
      OS << ">";
      break;
    case k_RegisterIndex:
      OS << "RegIdx<" << RegIdx.Index << ":" << RegIdx.Kind << ", "
         << StringRef(RegIdx.Tok.Data, RegIdx.Tok.Length) << ">";
      break;
    case k_Token:
      OS << getToken();
      break;
    case k_RegList:
      OS << "RegList< ";
      for (auto Reg : (*RegList.List))
        OS << Reg << " ";
      OS <<  ">";
      break;
    }
  }

  bool isValidForTie(const LoongArchOperand &Other) const {
    if (Kind != Other.Kind)
      return false;

    switch (Kind) {
    default:
      llvm_unreachable("Unexpected kind");
      return false;
    case k_RegisterIndex: {
      StringRef Token(RegIdx.Tok.Data, RegIdx.Tok.Length);
      StringRef OtherToken(Other.RegIdx.Tok.Data, Other.RegIdx.Tok.Length);
      return Token == OtherToken;
    }
    }
  }
}; // class LoongArchOperand

} // end anonymous namespace

namespace llvm {

extern const MCInstrDesc LoongArchInsts[];

} // end namespace llvm

static const MCInstrDesc &getInstDesc(unsigned Opcode) {
  return LoongArchInsts[Opcode];
}

static const MCSymbol *getSingleMCSymbol(const MCExpr *Expr) {
  if (const MCSymbolRefExpr *SRExpr = dyn_cast<MCSymbolRefExpr>(Expr)) {
    return &SRExpr->getSymbol();
  }

  if (const MCBinaryExpr *BExpr = dyn_cast<MCBinaryExpr>(Expr)) {
    const MCSymbol *LHSSym = getSingleMCSymbol(BExpr->getLHS());
    const MCSymbol *RHSSym = getSingleMCSymbol(BExpr->getRHS());

    if (LHSSym)
      return LHSSym;

    if (RHSSym)
      return RHSSym;

    return nullptr;
  }

  if (const MCUnaryExpr *UExpr = dyn_cast<MCUnaryExpr>(Expr))
    return getSingleMCSymbol(UExpr->getSubExpr());

  return nullptr;
}

static unsigned countMCSymbolRefExpr(const MCExpr *Expr) {
  if (isa<MCSymbolRefExpr>(Expr))
    return 1;

  if (const MCBinaryExpr *BExpr = dyn_cast<MCBinaryExpr>(Expr))
    return countMCSymbolRefExpr(BExpr->getLHS()) +
           countMCSymbolRefExpr(BExpr->getRHS());

  if (const MCUnaryExpr *UExpr = dyn_cast<MCUnaryExpr>(Expr))
    return countMCSymbolRefExpr(UExpr->getSubExpr());

  return 0;
}

bool LoongArchAsmParser::processInstruction(MCInst &Inst, SMLoc IDLoc,
                                            MCStreamer &Out,
                                            const MCSubtargetInfo *STI) {
  const MCInstrDesc &MCID = getInstDesc(Inst.getOpcode());

  Inst.setLoc(IDLoc);

  if (MCID.isBranch() || MCID.isCall()) {
    const unsigned Opcode = Inst.getOpcode();
    MCOperand Offset;

    switch (Opcode) {
    default:
      break;
    case LoongArch::BEQ:
    case LoongArch::BNE:
      assert(MCID.getNumOperands() == 3 && "unexpected number of operands");
      Offset = Inst.getOperand(2);
      if (!Offset.isImm())
        break; // We'll deal with this situation later on when applying fixups.
      if (!isIntN(17, Offset.getImm()))
        return Error(IDLoc, "branch target out of range");
      if (offsetToAlignment(Offset.getImm(),
                            Align(1LL << 2)))
        return Error(IDLoc, "branch to misaligned address");
      break;
    }
  }

  bool IsPCRelativeLoad = (MCID.TSFlags & LoongArchII::IsPCRelativeLoad) != 0;
  if ((MCID.mayLoad() || MCID.mayStore()) && !IsPCRelativeLoad) {
    // Check the offset of memory operand, if it is a symbol
    // reference or immediate we may have to expand instructions.
    for (unsigned i = 0; i < MCID.getNumOperands(); i++) {
      const MCOperandInfo &OpInfo = MCID.OpInfo[i];
      if ((OpInfo.OperandType == MCOI::OPERAND_MEMORY) ||
          (OpInfo.OperandType == MCOI::OPERAND_UNKNOWN)) {
        MCOperand &Op = Inst.getOperand(i);
        if (Op.isImm()) {
          int64_t MemOffset = Op.getImm();
          if (MemOffset < -32768 || MemOffset > 32767) {
            return getParser().hasPendingError();
          }
        } else if (Op.isExpr()) {
          const MCExpr *Expr = Op.getExpr();
          if (Expr->getKind() == MCExpr::SymbolRef) {
            const MCSymbolRefExpr *SR =
                static_cast<const MCSymbolRefExpr *>(Expr);
            if (SR->getKind() == MCSymbolRefExpr::VK_None) {
              return getParser().hasPendingError();
            }
          } else if (!isEvaluated(Expr)) {
            return getParser().hasPendingError();
          }
        }
      }
    } // for
  }   // if load/store

  MacroExpanderResultTy ExpandResult =
      tryExpandInstruction(Inst, IDLoc, Out, STI);
  switch (ExpandResult) {
  case MER_NotAMacro:
    Out.emitInstruction(Inst, *STI);
    break;
  case MER_Success:
    break;
  case MER_Fail:
    return true;
  }

  return false;
}

LoongArchAsmParser::MacroExpanderResultTy
LoongArchAsmParser::tryExpandInstruction(MCInst &Inst, SMLoc IDLoc, MCStreamer &Out,
                                    const MCSubtargetInfo *STI) {
  switch (Inst.getOpcode()) {
  default:
    return MER_NotAMacro;
  //li.w $r12, $imm32
  case LoongArch::LoadImm32:
    return expandLoadImm(Inst, true, IDLoc, Out, STI) ? MER_Fail : MER_Success;
  //li.d $r12, $imm64
  case LoongArch::LoadImm64:
    return expandLoadImm(Inst, false, IDLoc, Out, STI) ? MER_Fail : MER_Success;
  //la.local $r12, symbol (pcrel)
  case LoongArch::LoadAddrLocal:
    assert(Inst.getOperand(0).isReg() && "expected register operand kind");
    assert(Inst.getOperand(1).isExpr() && "expected immediate operand kind");

    return expandLoadAddress(Inst.getOperand(0).getReg(),
                                 Inst.getOperand(1), true, IDLoc,
                                 Out, STI)
               ? MER_Fail
               : MER_Success;
  //la.global $r12, symbol (got)
  case LoongArch::LoadAddrGlobal:
  case LoongArch::LoadAddrGlobal_Alias:
    assert(Inst.getOperand(0).isReg() && "expected register operand kind");
    assert(Inst.getOperand(1).isExpr() && "expected immediate operand kind");
    return expandLoadAddress(Inst.getOperand(0).getReg(),
                                 Inst.getOperand(1), false, IDLoc,
                                 Out, STI)
               ? MER_Fail
               : MER_Success;
  }
}

/// Can the value be represented by a unsigned N-bit value and a shift left?
template <unsigned N> static bool isShiftedUIntAtAnyPosition(uint64_t x) {
  unsigned BitNum = findFirstSet(x);

  return (x == x >> BitNum << BitNum) && isUInt<N>(x >> BitNum);
}

bool LoongArchAsmParser::loadImmediate(int64_t ImmValue, unsigned DstReg,
                                  unsigned SrcReg, bool Is32BitImm,
                                  bool IsAddress, SMLoc IDLoc, MCStreamer &Out,
                                  const MCSubtargetInfo *STI) {
  LoongArchTargetStreamer &TOut = getTargetStreamer();
  unsigned ZeroReg = LoongArch::ZERO_64;
  SrcReg = ZeroReg;

  if (!Is32BitImm && !is64Bit()) {
    Error(IDLoc, "instruction requires a 64-bit architecture");
    return true;
  }

  if (Is32BitImm) {
    if (isInt<32>(ImmValue) || isUInt<32>(ImmValue)) {
      // Sign extend up to 64-bit so that the predicates match the hardware
      // behaviour. In particular, isInt<12>(0xfffff000) and similar should be
      // true.
      ImmValue = SignExtend64<32>(ImmValue);
      if (((ImmValue>>12)&0xfffff) != 0) {
        TOut.emitRI(LoongArch::LU12I_W, DstReg, (ImmValue>>12)&0xfffff, IDLoc, STI);
        SrcReg = DstReg;
      }
      if ((ImmValue&0xfff) != 0)
        TOut.emitRRI(LoongArch::ORI, DstReg, SrcReg, ImmValue&0xfff, IDLoc, STI);
      return false;
    } else {
      Error(IDLoc, "instruction requires a 32-bit immediate");
      return true;
    }
  }

  if (((ImmValue>>12)&0xfffff) != 0) {
    TOut.emitRI(LoongArch::LU12I_W, DstReg, (ImmValue>>12)&0xfffff, IDLoc, STI);
    SrcReg = DstReg;
  }
  if ((ImmValue&0xfff) != 0) {
    TOut.emitRRI(LoongArch::ORI, DstReg, SrcReg, ImmValue&0xfff, IDLoc, STI);
    SrcReg = DstReg;
  }
  if (((ImmValue>>32)&0xfffff) != 0) {
    TOut.emitRI(LoongArch::LU32I_D, DstReg, (ImmValue>>32)&0xfffff, IDLoc, STI);
    SrcReg = DstReg;
  }
  if (((ImmValue>>52)&0xfff) != 0)
    TOut.emitRRI(LoongArch::LU52I_D, DstReg, SrcReg, (ImmValue>>52)&0xfff, IDLoc, STI);

  return false;
}

bool LoongArchAsmParser::expandLoadImm(MCInst &Inst, bool Is32BitImm, SMLoc IDLoc,
                                  MCStreamer &Out, const MCSubtargetInfo *STI) {
  const MCOperand &ImmOp = Inst.getOperand(1);
  assert(ImmOp.isImm() && "expected immediate operand kind");
  const MCOperand &DstRegOp = Inst.getOperand(0);
  assert(DstRegOp.isReg() && "expected register operand kind");

  if (loadImmediate(ImmOp.getImm(), DstRegOp.getReg(), LoongArch::NoRegister,
                    Is32BitImm, false, IDLoc, Out, STI))
    return true;

  return false;
}

bool LoongArchAsmParser::expandLoadAddress(unsigned DstReg,
                                      const MCOperand &Offset,
                                      bool IsLocal, SMLoc IDLoc,
                                      MCStreamer &Out,
                                      const MCSubtargetInfo *STI) {
  LoongArchTargetStreamer &TOut = getTargetStreamer();
  const MCExpr *SymExpr = Offset.getExpr();
  MCValue Res;


  if (!SymExpr->evaluateAsRelocatable(Res, nullptr, nullptr)) {
    Error(IDLoc, "expected relocatable expression");
    return true;
  }
  if (Res.getSymB() != nullptr) {
    Error(IDLoc, "expected relocatable expression with only one symbol");
    return true;
  }

  if (IsLocal) {
    // pcaddu12i $rd, %hi(sym)
    // addi.d $rd, $rd, %lo(sym)
    const LoongArchMCExpr *HiExpr =
      LoongArchMCExpr::create(LoongArchMCExpr::MEK_PCREL_HI, SymExpr, getContext());
    const LoongArchMCExpr *LoExpr =
      LoongArchMCExpr::create(LoongArchMCExpr::MEK_PCREL_LO, SymExpr, getContext());

    TOut.emitRX(LoongArch::PCADDU12I, DstReg,
        MCOperand::createExpr(HiExpr), IDLoc, STI);
    TOut.emitRRX(LoongArch::ADDI_D, DstReg, DstReg,
        MCOperand::createExpr(LoExpr), IDLoc, STI);
  } else {
    // registe symbol "_GLOBAL_OFFSET_TABLE_"
    StringRef SymName("_GLOBAL_OFFSET_TABLE_");
    MCAssembler &MCA = (static_cast<MCELFStreamer &>(getStreamer())).getAssembler();
    MCSymbol *Got = MCA.getContext().getOrCreateSymbol(SymName);
    MCA.registerSymbol(*Got);

    // pcaddu12i $rd, %got_hi(sym)
    // ld.d $rd, $rd, %got_lo(sym)
    const LoongArchMCExpr *GotHiExpr =
      LoongArchMCExpr::create(LoongArchMCExpr::MEK_GOT_HI, SymExpr, getContext());
    const LoongArchMCExpr *GotLoExpr =
      LoongArchMCExpr::create(LoongArchMCExpr::MEK_GOT_LO, SymExpr, getContext());
    const MCExpr *GotSym = MCSymbolRefExpr::create("_GLOBAL_OFFSET_TABLE_",
                                MCSymbolRefExpr::VK_None, getContext());

    TOut.emitRXX(LoongArch::PCADDU12I_rii, DstReg,
        MCOperand::createExpr(GotHiExpr),
        MCOperand::createExpr(GotSym), IDLoc, STI);
    TOut.emitRRXX(LoongArch::LD_D_rrii, DstReg, DstReg,
        MCOperand::createExpr(GotLoExpr),
        MCOperand::createExpr(GotSym), IDLoc, STI);
  }

  return false;
}

unsigned LoongArchAsmParser::checkTargetMatchPredicate(MCInst &Inst) {
  switch (Inst.getOpcode()) {
  case LoongArch::BSTRINS_W:
  case LoongArch::BSTRPICK_W: {
    assert(Inst.getOperand(2).isImm() && Inst.getOperand(3).isImm() &&
           "Operands must be immediates for bstrins.w/bstrpick.w!");
    const signed Msbw = Inst.getOperand(2).getImm();
    const signed Lsbw = Inst.getOperand(3).getImm();
    if (Msbw < Lsbw)
      return Match_MsbHigherThanLsb;
    if ((Lsbw < 0) || (Msbw > 31))
      return Match_RequiresRange0_31;
    return Match_Success;
  }
  case LoongArch::BSTRINS_D:
  case LoongArch::BSTRPICK_D: {
    assert(Inst.getOperand(2).isImm() && Inst.getOperand(3).isImm() &&
           "Operands must be immediates for bstrins.d/bstrpick.d!");
    const signed Msbd = Inst.getOperand(2).getImm();
    const signed Lsbd = Inst.getOperand(3).getImm();
    if (Msbd < Lsbd)
      return Match_MsbHigherThanLsb;
    if ((Lsbd < 0) || (Msbd > 63))
      return Match_RequiresRange0_63;
    return Match_Success;
  }
  case LoongArch::CSRXCHG32:
  case LoongArch::CSRXCHG:
    if (Inst.getOperand(2).getReg() == LoongArch::ZERO ||
        Inst.getOperand(2).getReg() == LoongArch::ZERO_64)
      return Match_RequiresNoZeroRegister;
    if (Inst.getOperand(2).getReg() == LoongArch::RA ||
        Inst.getOperand(2).getReg() == LoongArch::RA_64)
      return Match_RequiresNoRaRegister;
    return Match_Success;
  }

  return Match_Success;
}

static SMLoc RefineErrorLoc(const SMLoc Loc, const OperandVector &Operands,
                            uint64_t ErrorInfo) {
  if (ErrorInfo != ~0ULL && ErrorInfo < Operands.size()) {
    SMLoc ErrorLoc = Operands[ErrorInfo]->getStartLoc();
    if (ErrorLoc == SMLoc())
      return Loc;
    return ErrorLoc;
  }
  return Loc;
}

bool LoongArchAsmParser::MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                                                 OperandVector &Operands,
                                                 MCStreamer &Out,
                                                 uint64_t &ErrorInfo,
                                                 bool MatchingInlineAsm) {
  MCInst Inst;
  unsigned MatchResult =
      MatchInstructionImpl(Operands, Inst, ErrorInfo, MatchingInlineAsm);
  switch (MatchResult) {
  case Match_Success:
    if (processInstruction(Inst, IDLoc, Out, STI))
      return true;
    return false;
  case Match_MissingFeature:
    Error(IDLoc, "instruction requires a CPU feature not currently enabled");
    return true;
  case Match_InvalidOperand: {
    SMLoc ErrorLoc = IDLoc;
    if (ErrorInfo != ~0ULL) {
      if (ErrorInfo >= Operands.size())
        return Error(IDLoc, "too few operands for instruction");

      ErrorLoc = Operands[ErrorInfo]->getStartLoc();
      if (ErrorLoc == SMLoc())
        ErrorLoc = IDLoc;
    }

    return Error(ErrorLoc, "invalid operand for instruction");
  }
  case Match_MnemonicFail:
    return Error(IDLoc, "invalid instruction");
  case Match_RequiresNoZeroRegister:
    return Error(IDLoc, "invalid operand ($zero) for instruction");
  case Match_RequiresNoRaRegister:
    return Error(IDLoc, "invalid operand ($r1) for instruction");
  case Match_RequiresSameSrcAndDst:
    return Error(IDLoc, "source and destination must match");
  case Match_InvalidImm0_3:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "immediate must be an integer in range [0, 3].");
  case Match_InvalidImm0_7:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "immediate must be an integer in range [0, 7].");
  case Match_InvalidImm0_31:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "immediate must be an integer in range [0, 31].");
  case Match_InvalidImm0_63:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "immediate must be an integer in range [0, 63].");
  case Match_InvalidImm0_4095:
  case Match_UImm12_Relaxed:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "immediate must be an integer in range [0, 4095].");
  case Match_InvalidImm0_32767:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "immediate must be an integer in range [0, 32767].");
  case Match_UImm16_Relaxed:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 16-bit unsigned immediate");
  case Match_UImm20_0:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 20-bit unsigned immediate");
  case Match_UImm26_0:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 26-bit unsigned immediate");
  case Match_UImm32_Coerced:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 32-bit immediate");
  case Match_InvalidSImm2:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 2-bit signed immediate");
  case Match_InvalidSImm3:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 3-bit signed immediate");
  case Match_InvalidSImm5:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 5-bit signed immediate");
  case Match_InvalidSImm8:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 8-bit signed immediate");
  case Match_InvalidSImm12:
  case Match_SImm12_Relaxed:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 12-bit signed immediate");
  case Match_InvalidSImm14:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 14-bit signed immediate");
  case Match_InvalidSImm15:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 15-bit signed immediate");
  case Match_InvalidSImm16:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 16-bit signed immediate");
  case Match_InvalidSImm20:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 20-bit signed immediate");
  case Match_InvalidSImm21:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 21-bit signed immediate");
  case Match_InvalidSImm26:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 26-bit signed immediate");
  case Match_SImm32:
  case Match_SImm32_Relaxed:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected 32-bit signed immediate");
  case Match_MemSImm14:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected memory with 14-bit signed offset");
  case Match_MemSImmPtr:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected memory with 32-bit signed offset");
  case Match_UImm2_1:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected immediate in range 1 .. 4");
  case Match_MemSImm14Lsl2:
    return Error(RefineErrorLoc(IDLoc, Operands, ErrorInfo),
                 "expected memory with 16-bit signed offset and multiple of 4");
  case Match_RequiresRange0_31: {
    SMLoc ErrorStart = Operands[3]->getStartLoc();
    SMLoc ErrorEnd = Operands[4]->getEndLoc();
    return Error(ErrorStart, "from lsbw to msbw are not in the range 0 .. 31",
                 SMRange(ErrorStart, ErrorEnd));
    }
  case Match_RequiresPosSizeUImm6: {
    SMLoc ErrorStart = Operands[3]->getStartLoc();
    SMLoc ErrorEnd = Operands[4]->getEndLoc();
    return Error(ErrorStart, "size plus position are not in the range 1 .. 63",
                 SMRange(ErrorStart, ErrorEnd));
    }
  case Match_RequiresRange0_63: {
    SMLoc ErrorStart = Operands[3]->getStartLoc();
    SMLoc ErrorEnd = Operands[4]->getEndLoc();
    return Error(ErrorStart, "from lsbd to msbd are not in the range 0 .. 63",
                 SMRange(ErrorStart, ErrorEnd));
    }
  case Match_MsbHigherThanLsb: {
    SMLoc ErrorStart = Operands[3]->getStartLoc();
    SMLoc ErrorEnd = Operands[4]->getEndLoc();
    return Error(ErrorStart, "msb are not higher than lsb", SMRange(ErrorStart, ErrorEnd));
    }
  }

  llvm_unreachable("Implement any new match types added!");
}

/*
 * Note: The implementation of this function must be sync with the definition
 * of GPR32/GPR64 RegisterClass in LoongArchRegisterInfo.td
 */
int LoongArchAsmParser::matchCPURegisterName(StringRef Name) {
  int CC;

  CC = StringSwitch<unsigned>(Name)
           .Cases("zero", "r0", 0)
           .Cases("a0", "v0", "r4", 1)
           .Cases("a1", "v1", "r5", 2)
           .Cases("a2", "r6", 3)
           .Cases("a3", "r7", 4)
           .Cases("a4", "r8", 5)
           .Cases("a5", "r9", 6)
           .Cases("a6", "r10", 7)
           .Cases("a7", "r11", 8)
           .Cases("t0", "r12", 9)
           .Cases("t1", "r13", 10)
           .Cases("t2", "r14", 11)
           .Cases("t3", "r15", 12)
           .Cases("t4", "r16", 13)
           .Cases("t5", "r17", 14)
           .Cases("t6", "r18", 15)
           .Cases("t7", "r19", 16)
           .Cases("t8", "r20", 17)
           .Cases("s0", "r23", 18)
           .Cases("s1", "r24", 19)
           .Cases("s2", "r25", 20)
           .Cases("s3", "r26", 21)
           .Cases("s4", "r27", 22)
           .Cases("s5", "r28", 23)
           .Cases("s6", "r29", 24)
           .Cases("s7", "r30", 25)
           .Cases("s8", "r31", 26)
           .Cases("ra", "r1", 27)
           .Cases("tp", "r2", 28)
           .Cases("sp", "r3", 29)
           .Case("r21", 30)
           .Cases("fp", "r22", 31)
           .Default(-1);

  return CC;
}

int LoongArchAsmParser::matchFPURegisterName(StringRef Name) {
  if (Name[0] == 'f') {
    int CC;

    CC = StringSwitch<unsigned>(Name)
             .Cases("f0", "fa0", "fv0", 0)
             .Cases("f1", "fa1", "fv1", 1)
             .Cases("f2", "fa2", 2)
             .Cases("f3", "fa3", 3)
             .Cases("f4", "fa4", 4)
             .Cases("f5", "fa5", 5)
             .Cases("f6", "fa6", 6)
             .Cases("f7", "fa7", 7)
             .Cases("f8", "ft0", 8)
             .Cases("f9", "ft1", 9)
             .Cases("f10", "ft2", 10)
             .Cases("f11", "ft3", 11)
             .Cases("f12", "ft4", 12)
             .Cases("f13", "ft5", 13)
             .Cases("f14", "ft6", 14)
             .Cases("f15", "ft7", 15)
             .Cases("f16", "ft8", 16)
             .Cases("f17", "ft9", 17)
             .Cases("f18", "ft10", 18)
             .Cases("f19", "ft11", 19)
             .Cases("f20", "ft12", 20)
             .Cases("f21", "ft13", 21)
             .Cases("f22", "ft14", 22)
             .Cases("f23", "ft15", 23)
             .Cases("f24", "fs0", 24)
             .Cases("f25", "fs1", 25)
             .Cases("f26", "fs2", 26)
             .Cases("f27", "fs3", 27)
             .Cases("f28", "fs4", 28)
             .Cases("f29", "fs5", 29)
             .Cases("f30", "fs6", 30)
             .Cases("f31", "fs7", 31)
             .Default(-1);

    return CC;
  }
  return -1;
}

int LoongArchAsmParser::matchFCFRRegisterName(StringRef Name) {
  if (Name.startswith("fcc")) {
    StringRef NumString = Name.substr(3);
    unsigned IntVal;
    if (NumString.getAsInteger(10, IntVal))
      return -1;    // This is not an integer.
    if (IntVal > 7) // There are only 8 fcc registers.
      return -1;
    return IntVal;
  }
  return -1;
}

int LoongArchAsmParser::matchFCSRRegisterName(StringRef Name) {
  if (Name.startswith("fcsr")) {
    StringRef NumString = Name.substr(4);
    unsigned IntVal;
    if (NumString.getAsInteger(10, IntVal))
      return -1;    // This is not an integer.
    if (IntVal > 3) // There are only 4 fcsr registers.
      return -1;
    return IntVal;
  }
  return -1;
}

bool LoongArchAsmParser::parseOperand(OperandVector &Operands, StringRef Mnemonic) {
  MCAsmParser &Parser = getParser();
  LLVM_DEBUG(dbgs() << "parseOperand\n");

  // Check if the current operand has a custom associated parser, if so, try to
  // custom parse the operand, or fallback to the general approach.
  OperandMatchResultTy ResTy = MatchOperandParserImpl(Operands, Mnemonic);
  if (ResTy == MatchOperand_Success)
    return false;
  // If there wasn't a custom match, try the generic matcher below. Otherwise,
  // there was a match, but an error occurred, in which case, just return that
  // the operand parsing failed.
  if (ResTy == MatchOperand_ParseFail)
    return true;

  LLVM_DEBUG(dbgs() << ".. Generic Parser\n");

  switch (getLexer().getKind()) {
  case AsmToken::Dollar: {
    // Parse the register.
    SMLoc S = Parser.getTok().getLoc();

    // Almost all registers have been parsed by custom parsers. There is only
    // one exception to this. $zero (and it's alias $0) will reach this point
    // for div, divu, and similar instructions because it is not an operand
    // to the instruction definition but an explicit register. Special case
    // this situation for now.
    if (parseAnyRegister(Operands) != MatchOperand_NoMatch)
      return false;

    // Maybe it is a symbol reference.
    StringRef Identifier;
    if (Parser.parseIdentifier(Identifier))
      return true;

    SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
    MCSymbol *Sym = getContext().getOrCreateSymbol("$" + Identifier);
    // Otherwise create a symbol reference.
    const MCExpr *Res =
        MCSymbolRefExpr::create(Sym, MCSymbolRefExpr::VK_None, getContext());

    Operands.push_back(LoongArchOperand::CreateImm(Res, S, E, *this));
    return false;
  }
  default: {
    LLVM_DEBUG(dbgs() << ".. generic integer expression\n");

    const MCExpr *Expr;
    SMLoc S = Parser.getTok().getLoc(); // Start location of the operand.
    if (getParser().parseExpression(Expr))
      return true;

    SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);

    Operands.push_back(LoongArchOperand::CreateImm(Expr, S, E, *this));
    return false;
  }
  } // switch(getLexer().getKind())
  return true;
}

bool LoongArchAsmParser::isEvaluated(const MCExpr *Expr) {
  switch (Expr->getKind()) {
  case MCExpr::Constant:
    return true;
  case MCExpr::SymbolRef:
    return (cast<MCSymbolRefExpr>(Expr)->getKind() != MCSymbolRefExpr::VK_None);
  case MCExpr::Binary: {
    const MCBinaryExpr *BE = cast<MCBinaryExpr>(Expr);
    if (!isEvaluated(BE->getLHS()))
      return false;
    return isEvaluated(BE->getRHS());
  }
  case MCExpr::Unary:
    return isEvaluated(cast<MCUnaryExpr>(Expr)->getSubExpr());
  case MCExpr::Target:
    return true;
  }
  return false;
}

bool LoongArchAsmParser::ParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                  SMLoc &EndLoc) {
  return tryParseRegister(RegNo, StartLoc, EndLoc) != MatchOperand_Success;
}

OperandMatchResultTy LoongArchAsmParser::tryParseRegister(unsigned &RegNo,
                                                          SMLoc &StartLoc,
                                                          SMLoc &EndLoc) {
  SmallVector<std::unique_ptr<MCParsedAsmOperand>, 1> Operands;
  OperandMatchResultTy ResTy = parseAnyRegister(Operands);
  if (ResTy == MatchOperand_Success) {
    assert(Operands.size() == 1);
    LoongArchOperand &Operand = static_cast<LoongArchOperand &>(*Operands.front());
    StartLoc = Operand.getStartLoc();
    EndLoc = Operand.getEndLoc();

    // AFAIK, we only support numeric registers and named GPR's in CFI
    // directives.
    // Don't worry about eating tokens before failing. Using an unrecognised
    // register is a parse error.
    if (Operand.isGPRAsmReg()) {
      // Resolve to GPR32 or GPR64 appropriately.
      RegNo = is64Bit() ? Operand.getGPR64Reg() : Operand.getGPR32Reg();
    }

    return (RegNo == (unsigned)-1) ? MatchOperand_NoMatch
                                   : MatchOperand_Success;
  }

  assert(Operands.size() == 0);
  return (RegNo == (unsigned)-1) ? MatchOperand_NoMatch : MatchOperand_Success;
}

bool LoongArchAsmParser::parseMemOffset(const MCExpr *&Res) {
  return getParser().parseExpression(Res);
}

OperandMatchResultTy
LoongArchAsmParser::parseMemOperand(OperandVector &Operands) {
  MCAsmParser &Parser = getParser();
  LLVM_DEBUG(dbgs() << "parseMemOperand\n");
  const MCExpr *IdVal = nullptr;
  SMLoc S;
  OperandMatchResultTy Res = MatchOperand_NoMatch;
  // First operand is the base.
  S = Parser.getTok().getLoc();

  Res = parseAnyRegister(Operands);
  if (Res != MatchOperand_Success)
    return Res;

  if (Parser.getTok().isNot(AsmToken::Comma)) {
    Error(Parser.getTok().getLoc(), "',' expected");
    return MatchOperand_ParseFail;
  }

  Parser.Lex(); // Eat the ',' token.

  if (parseMemOffset(IdVal))
    return MatchOperand_ParseFail;

  SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);

  // Replace the register operand with the memory operand.
  std::unique_ptr<LoongArchOperand> op(
      static_cast<LoongArchOperand *>(Operands.back().release()));
  // Remove the register from the operands.
  // "op" will be managed by k_Memory.
  Operands.pop_back();

  // when symbol not defined, error report.
  if (dyn_cast<MCSymbolRefExpr>(IdVal)) {
    return MatchOperand_ParseFail;
  }

  // Add the memory operand.
  if (dyn_cast<MCBinaryExpr>(IdVal)) {
    int64_t Imm;
    if (IdVal->evaluateAsAbsolute(Imm))
      IdVal = MCConstantExpr::create(Imm, getContext());
    else
      return MatchOperand_ParseFail;
  }

  Operands.push_back(LoongArchOperand::CreateMem(std::move(op), IdVal, S, E, *this));
  return MatchOperand_Success;
}

bool LoongArchAsmParser::searchSymbolAlias(OperandVector &Operands) {
  MCAsmParser &Parser = getParser();
  MCSymbol *Sym = getContext().lookupSymbol(Parser.getTok().getIdentifier());
  if (!Sym)
    return false;

  SMLoc S = Parser.getTok().getLoc();
  if (Sym->isVariable()) {
    const MCExpr *Expr = Sym->getVariableValue();
    if (Expr->getKind() == MCExpr::SymbolRef) {
      const MCSymbolRefExpr *Ref = static_cast<const MCSymbolRefExpr *>(Expr);
      StringRef DefSymbol = Ref->getSymbol().getName();
      if (DefSymbol.startswith("$")) {
        OperandMatchResultTy ResTy =
            matchAnyRegisterNameWithoutDollar(Operands, DefSymbol.substr(1), S);
        if (ResTy == MatchOperand_Success) {
          Parser.Lex();
          return true;
        }
        if (ResTy == MatchOperand_ParseFail)
          llvm_unreachable("Should never ParseFail");
      }
    }
  } else if (Sym->isUnset()) {
    // If symbol is unset, it might be created in the `parseSetAssignment`
    // routine as an alias for a numeric register name.
    // Lookup in the aliases list.
    auto Entry = RegisterSets.find(Sym->getName());
    if (Entry != RegisterSets.end()) {
      OperandMatchResultTy ResTy =
          matchAnyRegisterWithoutDollar(Operands, Entry->getValue(), S);
      if (ResTy == MatchOperand_Success) {
        Parser.Lex();
        return true;
      }
    }
  }

  return false;
}

OperandMatchResultTy
LoongArchAsmParser::matchAnyRegisterNameWithoutDollar(OperandVector &Operands,
                                                      StringRef Identifier,
                                                      SMLoc S) {
  int Index = matchCPURegisterName(Identifier);
  if (Index != -1) {
    Operands.push_back(LoongArchOperand::createGPRReg(
        Index, Identifier, getContext().getRegisterInfo(), S,
        getLexer().getLoc(), *this));
    return MatchOperand_Success;
  }

  Index = matchFPURegisterName(Identifier);
  if (Index != -1) {
    Operands.push_back(LoongArchOperand::createFGRReg(
        Index, Identifier, getContext().getRegisterInfo(), S,
        getLexer().getLoc(), *this));
    return MatchOperand_Success;
  }

  Index = matchFCFRRegisterName(Identifier);
  if (Index != -1) {
    Operands.push_back(LoongArchOperand::createFCFRReg(
        Index, Identifier, getContext().getRegisterInfo(), S,
        getLexer().getLoc(), *this));
    return MatchOperand_Success;
  }

  Index = matchFCSRRegisterName(Identifier);
  if (Index != -1) {
    Operands.push_back(LoongArchOperand::createFCSRReg(
        Index, Identifier, getContext().getRegisterInfo(), S,
        getLexer().getLoc(), *this));
    return MatchOperand_Success;
  }

  return MatchOperand_NoMatch;
}

OperandMatchResultTy
LoongArchAsmParser::matchAnyRegisterWithoutDollar(OperandVector &Operands,
                                                  const AsmToken &Token, SMLoc S) {
  if (Token.is(AsmToken::Identifier)) {
    LLVM_DEBUG(dbgs() << ".. identifier\n");
    StringRef Identifier = Token.getIdentifier();
    OperandMatchResultTy ResTy =
        matchAnyRegisterNameWithoutDollar(Operands, Identifier, S);
    return ResTy;
  } else if (Token.is(AsmToken::Integer)) {
    LLVM_DEBUG(dbgs() << ".. integer\n");
    int64_t RegNum = Token.getIntVal();
    if (RegNum < 0 || RegNum > 31) {
      // Show the error, but treat invalid register
      // number as a normal one to continue parsing
      // and catch other possible errors.
      Error(getLexer().getLoc(), "invalid register number");
    }
    Operands.push_back(LoongArchOperand::createNumericReg(
        RegNum, Token.getString(), getContext().getRegisterInfo(), S,
        Token.getLoc(), *this));
    return MatchOperand_Success;
  }

  LLVM_DEBUG(dbgs() << Token.getKind() << "\n");

  return MatchOperand_NoMatch;
}

OperandMatchResultTy
LoongArchAsmParser::matchAnyRegisterWithoutDollar(OperandVector &Operands, SMLoc S) {
  auto Token = getLexer().peekTok(false);
  return matchAnyRegisterWithoutDollar(Operands, Token, S);
}

OperandMatchResultTy
LoongArchAsmParser::parseAnyRegister(OperandVector &Operands) {
  MCAsmParser &Parser = getParser();
  LLVM_DEBUG(dbgs() << "parseAnyRegister\n");

  auto Token = Parser.getTok();

  SMLoc S = Token.getLoc();

  if (Token.isNot(AsmToken::Dollar)) {
    LLVM_DEBUG(dbgs() << ".. !$ -> try sym aliasing\n");
    if (Token.is(AsmToken::Identifier)) {
      if (searchSymbolAlias(Operands))
        return MatchOperand_Success;
    }
    LLVM_DEBUG(dbgs() << ".. !symalias -> NoMatch\n");
    return MatchOperand_NoMatch;
  }
  LLVM_DEBUG(dbgs() << ".. $\n");

  OperandMatchResultTy ResTy = matchAnyRegisterWithoutDollar(Operands, S);
  if (ResTy == MatchOperand_Success) {
    Parser.Lex(); // $
    Parser.Lex(); // identifier
  }
  return ResTy;
}

OperandMatchResultTy
LoongArchAsmParser::parseJumpTarget(OperandVector &Operands) {
  MCAsmParser &Parser = getParser();
  LLVM_DEBUG(dbgs() << "parseJumpTarget\n");

  SMLoc S = getLexer().getLoc();

  // Registers are a valid target and have priority over symbols.
  OperandMatchResultTy ResTy = parseAnyRegister(Operands);
  if (ResTy != MatchOperand_NoMatch)
    return ResTy;

  // Integers and expressions are acceptable
  const MCExpr *Expr = nullptr;
  if (Parser.parseExpression(Expr)) {
    // We have no way of knowing if a symbol was consumed so we must ParseFail
    return MatchOperand_ParseFail;
  }
  Operands.push_back(
      LoongArchOperand::CreateImm(Expr, S, getLexer().getLoc(), *this));
  return MatchOperand_Success;
}

static std::string LoongArchMnemonicSpellCheck(StringRef S,
                                               const FeatureBitset &FBS,
                                               unsigned VariantID = 0);

bool LoongArchAsmParser::ParseInstruction(ParseInstructionInfo &Info,
                                          StringRef Name, SMLoc NameLoc,
                                          OperandVector &Operands) {
  MCAsmParser &Parser = getParser();
  LLVM_DEBUG(dbgs() << "ParseInstruction\n");

  // We have reached first instruction, module directive are now forbidden.
  getTargetStreamer().forbidModuleDirective();

  // Check if we have valid mnemonic
  if (!mnemonicIsValid(Name)) {
    FeatureBitset FBS = ComputeAvailableFeatures(getSTI().getFeatureBits());
    std::string Suggestion = LoongArchMnemonicSpellCheck(Name, FBS);
    return Error(NameLoc, "unknown instruction" + Suggestion);
  }

  // First operand in MCInst is instruction mnemonic.
  Operands.push_back(LoongArchOperand::CreateToken(Name, NameLoc, *this));

  // Read the remaining operands.
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    // Read the first operand.
    if (parseOperand(Operands, Name)) {
      SMLoc Loc = getLexer().getLoc();
      return Error(Loc, "unexpected token in argument list");
    }

    while (getLexer().is(AsmToken::Comma)) {
      Parser.Lex(); // Eat the comma.
      // Parse and remember the operand.
      if (parseOperand(Operands, Name)) {
        SMLoc Loc = getLexer().getLoc();
        return Error(Loc, "unexpected token in argument list");
      }
    }
  }
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    SMLoc Loc = getLexer().getLoc();
    return Error(Loc, "unexpected token in argument list");
  }
  Parser.Lex(); // Consume the EndOfStatement.
  return false;
}

// FIXME: Given that these have the same name, these should both be
// consistent on affecting the Parser.
bool LoongArchAsmParser::reportParseError(Twine ErrorMsg) {
  SMLoc Loc = getLexer().getLoc();
  return Error(Loc, ErrorMsg);
}

bool LoongArchAsmParser::parseSetFpDirective() {
  MCAsmParser &Parser = getParser();
  LoongArchFPABIInfo::FpABIKind FpAbiVal;
  // Line can be: .set fp=32
  //              .set fp=64
  Parser.Lex(); // Eat fp token
  AsmToken Tok = Parser.getTok();
  if (Tok.isNot(AsmToken::Equal)) {
    reportParseError("unexpected token, expected equals sign '='");
    return false;
  }
  Parser.Lex(); // Eat '=' token.
  Tok = Parser.getTok();

  if (!parseFpABIValue(FpAbiVal, ".set"))
    return false;

  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    reportParseError("unexpected token, expected end of statement");
    return false;
  }
  getTargetStreamer().emitDirectiveSetFp(FpAbiVal);
  Parser.Lex(); // Consume the EndOfStatement.
  return false;
}

bool LoongArchAsmParser::parseSetSoftFloatDirective() {
  MCAsmParser &Parser = getParser();
  Parser.Lex();
  if (getLexer().isNot(AsmToken::EndOfStatement))
    return reportParseError("unexpected token, expected end of statement");

  setFeatureBits(LoongArch::FeatureSoftFloat, "soft-float");
  getTargetStreamer().emitDirectiveSetSoftFloat();
  return false;
}

bool LoongArchAsmParser::parseSetHardFloatDirective() {
  MCAsmParser &Parser = getParser();
  Parser.Lex();
  if (getLexer().isNot(AsmToken::EndOfStatement))
    return reportParseError("unexpected token, expected end of statement");

  clearFeatureBits(LoongArch::FeatureSoftFloat, "soft-float");
  getTargetStreamer().emitDirectiveSetHardFloat();
  return false;
}

bool LoongArchAsmParser::parseSetAssignment() {
  StringRef Name;
  const MCExpr *Value;
  MCAsmParser &Parser = getParser();

  if (Parser.parseIdentifier(Name))
    return reportParseError("expected identifier after .set");

  if (getLexer().isNot(AsmToken::Comma))
    return reportParseError("unexpected token, expected comma");
  Lex(); // Eat comma

  if (!Parser.parseExpression(Value)) {
    // Parse assignment of an expression including
    // symbolic registers:
    //   .set  $tmp, $BB0-$BB1
    //   .set  r2, $f2
    MCSymbol *Sym = getContext().getOrCreateSymbol(Name);
    Sym->setVariableValue(Value);
  } else {
    return reportParseError("expected valid expression after comma");
  }

  return false;
}

bool LoongArchAsmParser::parseDirectiveSet() {
  const AsmToken &Tok = getParser().getTok();
  StringRef IdVal = Tok.getString();
  SMLoc Loc = Tok.getLoc();

  if (IdVal == "bopt") {
    Warning(Loc, "'bopt' feature is unsupported");
    getParser().Lex();
    return false;
  }
  if (IdVal == "nobopt") {
    // We're already running in nobopt mode, so nothing to do.
    getParser().Lex();
    return false;
  }
  if (IdVal == "fp")
    return parseSetFpDirective();
  if (IdVal == "softfloat")
    return parseSetSoftFloatDirective();
  if (IdVal == "hardfloat")
    return parseSetHardFloatDirective();

  // It is just an identifier, look for an assignment.
  return parseSetAssignment();
}

/// parseDirectiveModule
///  ::= .module fp=value
///  ::= .module softfloat
///  ::= .module hardfloat
bool LoongArchAsmParser::parseDirectiveModule() {
  MCAsmParser &Parser = getParser();
  MCAsmLexer &Lexer = getLexer();
  SMLoc L = Lexer.getLoc();

  if (!getTargetStreamer().isModuleDirectiveAllowed()) {
    // TODO : get a better message.
    reportParseError(".module directive must appear before any code");
    return false;
  }

  StringRef Option;
  if (Parser.parseIdentifier(Option)) {
    reportParseError("expected .module option identifier");
    return false;
  }

  if (Option == "fp") {
    return parseDirectiveModuleFP();
  } else if (Option == "softfloat") {
    setModuleFeatureBits(LoongArch::FeatureSoftFloat, "soft-float");

    // Synchronize the ABI Flags information with the FeatureBits information we
    // updated above.
    getTargetStreamer().updateABIInfo(*this);

    // If printing assembly, use the recently updated ABI Flags information.
    // If generating ELF, don't do anything.
    getTargetStreamer().emitDirectiveModuleSoftFloat();

    // If this is not the end of the statement, report an error.
    if (getLexer().isNot(AsmToken::EndOfStatement)) {
      reportParseError("unexpected token, expected end of statement");
      return false;
    }

    return false; // parseDirectiveModule has finished successfully.
  } else if (Option == "hardfloat") {
    clearModuleFeatureBits(LoongArch::FeatureSoftFloat, "soft-float");

    // Synchronize the ABI Flags information with the FeatureBits information we
    // updated above.
    getTargetStreamer().updateABIInfo(*this);

    // If printing assembly, use the recently updated ABI Flags information.
    // If generating ELF, don't do anything.
    getTargetStreamer().emitDirectiveModuleHardFloat();

    // If this is not the end of the statement, report an error.
    if (getLexer().isNot(AsmToken::EndOfStatement)) {
      reportParseError("unexpected token, expected end of statement");
      return false;
    }

    return false; // parseDirectiveModule has finished successfully.
  }  else {
    return Error(L, "'" + Twine(Option) + "' is not a valid .module option.");
  }
}

/// parseDirectiveModuleFP
///  ::= =32
///  ::= =64
bool LoongArchAsmParser::parseDirectiveModuleFP() {
  MCAsmParser &Parser = getParser();
  MCAsmLexer &Lexer = getLexer();

  if (Lexer.isNot(AsmToken::Equal)) {
    reportParseError("unexpected token, expected equals sign '='");
    return false;
  }
  Parser.Lex(); // Eat '=' token.

  LoongArchFPABIInfo::FpABIKind FpABI;
  if (!parseFpABIValue(FpABI, ".module"))
    return false;

  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    reportParseError("unexpected token, expected end of statement");
    return false;
  }

  // Synchronize the abi information with the FeatureBits information we
  // changed above.
  getTargetStreamer().updateABIInfo(*this);

  // If printing assembly, use the recently updated abiflags information.
  // If generating ELF, don't do anything.
  getTargetStreamer().emitDirectiveModuleFP();

  Parser.Lex(); // Consume the EndOfStatement.
  return false;
}

bool LoongArchAsmParser::parseFpABIValue(LoongArchFPABIInfo::FpABIKind &FpABI,
                                    StringRef Directive) {
  MCAsmParser &Parser = getParser();
  MCAsmLexer &Lexer = getLexer();
  bool ModuleLevelOptions = Directive == ".module";

  if (Lexer.is(AsmToken::Identifier)) {
    Parser.Lex();

    if (!isABI_LP32()) {
      reportParseError("'" + Directive + " fp=32' requires the LP32 ABI");
      return false;
    }

    return true;
  }

  if (Lexer.is(AsmToken::Integer)) {
    unsigned Value = Parser.getTok().getIntVal();
    Parser.Lex();

    if (Value != 32 && Value != 64) {
      reportParseError("unsupported value, expected '32' or '64'");
      return false;
    }

    if (Value == 32) {
      if (!isABI_LP32()) {
        reportParseError("'" + Directive + " fp=32' requires the LP32 ABI");
        return false;
      }

      FpABI = LoongArchFPABIInfo::FpABIKind::S32;
      if (ModuleLevelOptions) {
        clearModuleFeatureBits(LoongArch::FeatureFP64Bit, "fp64");
      } else {
        clearFeatureBits(LoongArch::FeatureFP64Bit, "fp64");
      }
    } else {
      FpABI = LoongArchFPABIInfo::FpABIKind::S64;
      if (ModuleLevelOptions) {
        setModuleFeatureBits(LoongArch::FeatureFP64Bit, "fp64");
      } else {
        setFeatureBits(LoongArch::FeatureFP64Bit, "fp64");
      }
    }

    return true;
  }

  return false;
}

bool LoongArchAsmParser::ParseDirective(AsmToken DirectiveID) {
  // This returns false if this function recognizes the directive
  // regardless of whether it is successfully handles or reports an
  // error. Otherwise it returns true to give the generic parser a
  // chance at recognizing it.

  MCAsmParser &Parser = getParser();
  StringRef IDVal = DirectiveID.getString();

  if (IDVal == ".end") {
      while (getLexer().isNot(AsmToken::Eof))
        Parser.Lex();
    return false;
  }

  if (IDVal == ".set") {
    parseDirectiveSet();
    return false;
  }

  if (IDVal == ".module") {
    parseDirectiveModule();
    return false;
  }
  if (IDVal == ".llvm_internal_loongarch_reallow_module_directive") {
    parseInternalDirectiveReallowModule();
    return false;
  }

  return true;
}

bool LoongArchAsmParser::parseInternalDirectiveReallowModule() {
  // If this is not the end of the statement, report an error.
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    reportParseError("unexpected token, expected end of statement");
    return false;
  }

  getTargetStreamer().reallowModuleDirective();

  getParser().Lex(); // Eat EndOfStatement token.
  return false;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeLoongArchAsmParser() {
  RegisterMCAsmParser<LoongArchAsmParser> X(getTheLoongArch32Target());
  RegisterMCAsmParser<LoongArchAsmParser> A(getTheLoongArch64Target());
}

#define GET_REGISTER_MATCHER
#define GET_MATCHER_IMPLEMENTATION
#define GET_MNEMONIC_SPELL_CHECKER
#include "LoongArchGenAsmMatcher.inc"

bool LoongArchAsmParser::mnemonicIsValid(StringRef Mnemonic) {
  // Find the appropriate table for this asm variant.
  const MatchEntry *Start, *End;
  Start = std::begin(MatchTable0);
  End = std::end(MatchTable0);

  // Search the table.
  auto MnemonicRange = std::equal_range(Start, End, Mnemonic, LessOpcode());
  return MnemonicRange.first != MnemonicRange.second;
}
