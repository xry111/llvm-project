//=== LoongArchAsmParser.cpp - Parse LoongArch asm to MCInst instructions -===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/LoongArchAsmBackend.h"
#include "MCTargetDesc/LoongArchBaseInfo.h"
#include "MCTargetDesc/LoongArchInstPrinter.h"
#include "MCTargetDesc/LoongArchMCExpr.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "MCTargetDesc/LoongArchMatInt.h"
#include "MCTargetDesc/LoongArchTargetStreamer.h"
#include "TargetInfo/LoongArchTargetInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstBuilder.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/TargetRegistry.h"

#include <limits>

using namespace llvm;

#define DEBUG_TYPE "loongarch-asm-parser"

namespace {
struct LoongArchOperand;

struct ParserOptionsSet {
  bool IsPicEnabled;
};

class LoongArchAsmParser : public MCTargetAsmParser {
  SmallVector<FeatureBitset, 4> FeatureBitStack;

  SmallVector<ParserOptionsSet, 4> ParserOptionsStack;
  ParserOptionsSet ParserOptions;

  SMLoc getLoc() const { return getParser().getTok().getLoc(); }
  bool isLA64() const { return getSTI().hasFeature(LoongArch::Feature64Bit); }

  LoongArchTargetStreamer &getTargetStreamer() {
    MCTargetStreamer &TS = *getParser().getStreamer().getTargetStreamer();
    return static_cast<LoongArchTargetStreamer &>(TS);
  }

  bool generateImmOutOfRangeError(OperandVector &Operands, uint64_t ErrorInfo,
                                  int64_t Lower, int64_t Upper, Twine Msg);

  bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               OperandVector &Operands, MCStreamer &Out,
                               uint64_t &ErrorInfo,
                               bool MatchingInlineAsm) override;

  bool ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) override;
  OperandMatchResultTy tryParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                        SMLoc &EndLoc) override;

  bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;

  bool ParseDirective(AsmToken DirectiveID) override;

  // Helper to actually emit an instruction to the MCStreamer. Also, when
  // possible, compression of the instruction is performed.
  void emitToStreamer(MCStreamer &S, const MCInst &Inst);

  // Helper to emit a combination of LUI, ADDI(W), and SLLI instructions that
  // synthesize the desired immedate value into the destination register.
  void emitLoadImm(MCRegister DestReg, int64_t Value, MCStreamer &Out);

  // Check instruction constraints.
  bool validateInstruction(MCInst &Inst, OperandVector &Operands);

  /// Helper for processing MC instructions that have been successfully matched
  /// by MatchAndEmitInstruction. Modifications to the emitted instructions,
  /// like the expansion of pseudo instructions (e.g., "li"), can be performed
  /// in this method.
  bool processInstruction(MCInst &Inst, SMLoc IDLoc, OperandVector &Operands,
                          MCStreamer &Out);

// Auto-generated instruction matching functions
#define GET_ASSEMBLER_HEADER
#include "LoongArchGenAsmMatcher.inc"

  OperandMatchResultTy parseImmediate(OperandVector &Operands);
  OperandMatchResultTy parseRegister(OperandVector &Operands);
  OperandMatchResultTy parseOperandWithModifier(OperandVector &Operands);
  OperandMatchResultTy parseBareSymbol(OperandVector &Operands);
  OperandMatchResultTy parseCallSymbol(OperandVector &Operands);

  bool parseOperand(OperandVector &Operands, StringRef Mnemonic);

  bool parseDirectiveOption();
  bool parseDirectiveAttribute();

  void setFeatureBits(uint64_t Feature, StringRef FeatureString) {
    if (!(getSTI().getFeatureBits()[Feature])) {
      MCSubtargetInfo &STI = copySTI();
      setAvailableFeatures(
          ComputeAvailableFeatures(STI.ToggleFeature(FeatureString)));
    }
  }

  bool getFeatureBits(uint64_t Feature) {
    return getSTI().getFeatureBits()[Feature];
  }

  void clearFeatureBits(uint64_t Feature, StringRef FeatureString) {
    if (getSTI().getFeatureBits()[Feature]) {
      MCSubtargetInfo &STI = copySTI();
      setAvailableFeatures(
          ComputeAvailableFeatures(STI.ToggleFeature(FeatureString)));
    }
  }

  void pushFeatureBits() {
    assert(FeatureBitStack.size() == ParserOptionsStack.size() &&
           "These two stacks must be kept synchronized");
    FeatureBitStack.push_back(getSTI().getFeatureBits());
    ParserOptionsStack.push_back(ParserOptions);
  }

  bool popFeatureBits() {
    assert(FeatureBitStack.size() == ParserOptionsStack.size() &&
           "These two stacks must be kept synchronized");
    if (FeatureBitStack.empty())
      return true;

    FeatureBitset FeatureBits = FeatureBitStack.pop_back_val();
    copySTI().setFeatureBits(FeatureBits);
    setAvailableFeatures(ComputeAvailableFeatures(FeatureBits));

    ParserOptions = ParserOptionsStack.pop_back_val();

    return false;
  }

public:
  enum LoongArchMatchResultTy {
    Match_Dummy = FIRST_TARGET_MATCH_RESULT_TY,
#define GET_OPERAND_DIAGNOSTIC_TYPES
#include "LoongArchGenAsmMatcher.inc"
#undef GET_OPERAND_DIAGNOSTIC_TYPES
  };

  static bool classifySymbolRef(const MCExpr *Expr,
                                LoongArchMCExpr::VariantKind &Kind);

  LoongArchAsmParser(const MCSubtargetInfo &STI, MCAsmParser &Parser,
                     const MCInstrInfo &MII, const MCTargetOptions &Options)
      : MCTargetAsmParser(Options, STI, MII) {
    Parser.addAliasForDirective(".half", ".2byte");
    Parser.addAliasForDirective(".hword", ".2byte");
    Parser.addAliasForDirective(".word", ".4byte");
    Parser.addAliasForDirective(".dword", ".8byte");
    setAvailableFeatures(ComputeAvailableFeatures(STI.getFeatureBits()));

    auto ABIName = StringRef(Options.ABIName);
    if (ABIName.endswith("f") &&
        !getSTI().getFeatureBits()[LoongArch::FeatureStdExtF]) {
      errs() << "Hard-float 'f' ABI can't be used for a target that "
                "doesn't support the F instruction set extension (ignoring "
                "target-abi)\n";
    } else if (ABIName.endswith("d") &&
               !getSTI().getFeatureBits()[LoongArch::FeatureStdExtD]) {
      errs() << "Hard-float 'd' ABI can't be used for a target that "
                "doesn't support the D instruction set extension (ignoring "
                "target-abi)\n";
    }

    const MCObjectFileInfo *MOFI = Parser.getContext().getObjectFileInfo();
    ParserOptions.IsPicEnabled = MOFI->isPositionIndependent();
  }
};

/// LoongArchOperand - Instances of this class represent a parsed machine
/// instruction
struct LoongArchOperand : public MCParsedAsmOperand {

  enum class KindTy {
    Token,
    Register,
    Immediate,
    SystemRegister,
    VType,
  } Kind;

  bool IsLA64;

  struct RegOp {
    MCRegister RegNum;
  };

  struct ImmOp {
    const MCExpr *Val;
  };

  struct SysRegOp {
    const char *Data;
    unsigned Length;
    unsigned Encoding;
    // FIXME: Add the Encoding parsed fields as needed for checks,
    // e.g.: read/write or user/supervisor/machine privileges.
  };

  struct VTypeOp {
    unsigned Val;
  };

  SMLoc StartLoc, EndLoc;
  union {
    StringRef Tok;
    RegOp Reg;
    ImmOp Imm;
    struct SysRegOp SysReg;
    struct VTypeOp VType;
  };

  LoongArchOperand(KindTy K) : MCParsedAsmOperand(), Kind(K) {}

public:
  LoongArchOperand(const LoongArchOperand &o) : MCParsedAsmOperand() {
    Kind = o.Kind;
    IsLA64 = o.IsLA64;
    StartLoc = o.StartLoc;
    EndLoc = o.EndLoc;
    switch (Kind) {
    case KindTy::Register:
      Reg = o.Reg;
      break;
    case KindTy::Immediate:
      Imm = o.Imm;
      break;
    case KindTy::Token:
      Tok = o.Tok;
      break;
    case KindTy::SystemRegister:
      SysReg = o.SysReg;
      break;
    case KindTy::VType:
      VType = o.VType;
      break;
    }
  }

  bool isToken() const override { return Kind == KindTy::Token; }
  bool isReg() const override { return Kind == KindTy::Register; }
  bool isImm() const override { return Kind == KindTy::Immediate; }
  bool isMem() const override { return false; }
  bool isSystemRegister() const { return Kind == KindTy::SystemRegister; }
  bool isVType() const { return Kind == KindTy::VType; }

  bool isGPR() const {
    return Kind == KindTy::Register &&
           LoongArchMCRegisterClasses[LoongArch::GPRRegClassID].contains(
               Reg.RegNum);
  }

  static bool evaluateConstantImm(const MCExpr *Expr, int64_t &Imm,
                                  LoongArchMCExpr::VariantKind &VK) {
    if (auto *RE = dyn_cast<LoongArchMCExpr>(Expr)) {
      VK = RE->getKind();
      return RE->evaluateAsConstant(Imm);
    }

    if (auto CE = dyn_cast<MCConstantExpr>(Expr)) {
      VK = LoongArchMCExpr::VK_LoongArch_None;
      Imm = CE->getValue();
      return true;
    }

    return false;
  }

  // True if operand is a symbol with no modifiers, or a constant with no
  // modifiers and isShiftedInt<N-1, 1>(Op).
  template <int N> bool isBareSimmNLsb00(bool acceptPlt = false) const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    bool IsValid;
    if (!IsConstantImm)
      IsValid = LoongArchAsmParser::classifySymbolRef(getImm(), VK);
    else
      IsValid = isShiftedInt<N - 2, 2>(Imm);
    return IsValid &&
           (VK == LoongArchMCExpr::VK_LoongArch_None ||
            (acceptPlt && VK == LoongArchMCExpr::VK_LoongArch_CALL_PLT));
  }

  // Predicate methods for AsmOperands defined in LoongArchInstrInfo.td

  bool isBareSymbol() const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    // Must be of 'immediate' type but not a constant.
    if (!isImm() || evaluateConstantImm(getImm(), Imm, VK))
      return false;
    return LoongArchAsmParser::classifySymbolRef(getImm(), VK) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isCallSymbol() const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    // Must be of 'immediate' type but not a constant.
    if (!isImm() || evaluateConstantImm(getImm(), Imm, VK))
      return false;
    return LoongArchAsmParser::classifySymbolRef(getImm(), VK) &&
           (VK == LoongArchMCExpr::VK_LoongArch_CALL ||
            VK == LoongArchMCExpr::VK_LoongArch_CALL_PLT);
  }

  bool isPseudoJumpSymbol() const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    // Must be of 'immediate' type but not a constant.
    if (!isImm() || evaluateConstantImm(getImm(), Imm, VK))
      return false;
    return LoongArchAsmParser::classifySymbolRef(getImm(), VK) &&
           VK == LoongArchMCExpr::VK_LoongArch_CALL;
  }

  bool isCSRSystemRegister() const { return isSystemRegister(); }

  /// Return true if the operand is a valid for the fence instruction e.g.
  /// ('iorw').
  bool isFenceArg() const {
    if (!isImm())
      return false;
    const MCExpr *Val = getImm();
    auto *SVal = dyn_cast<MCSymbolRefExpr>(Val);
    if (!SVal || SVal->getKind() != MCSymbolRefExpr::VK_None)
      return false;

    StringRef Str = SVal->getSymbol().getName();
    // Letters must be unique, taken from 'iorw', and in ascending order. This
    // holds as long as each individual character is one of 'iorw' and is
    // greater than the previous character.
    char Prev = '\0';
    for (char c : Str) {
      if (c != 'i' && c != 'o' && c != 'r' && c != 'w')
        return false;
      if (c <= Prev)
        return false;
      Prev = c;
    }
    return true;
  }

  /// Return true if the operand is a valid floating point rounding mode.
  bool isFRMArg() const {
    if (!isImm())
      return false;
    const MCExpr *Val = getImm();
    auto *SVal = dyn_cast<MCSymbolRefExpr>(Val);
    if (!SVal || SVal->getKind() != MCSymbolRefExpr::VK_None)
      return false;

    StringRef Str = SVal->getSymbol().getName();

    return LoongArchFPRndMode::stringToRoundingMode(Str) !=
           LoongArchFPRndMode::Invalid;
  }

  bool isImm32LI() const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    // Given only Imm, ensuring that the actually specified constant is either
    // a signed or unsigned 64-bit number is unfortunately impossible.
    return IsConstantImm && VK == LoongArchMCExpr::VK_LoongArch_None &&
           (isInt<32>(Imm) || isUInt<32>(Imm));
  }

  bool isImm64LI() const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isUImmLog2XLen() const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    if (!isImm())
      return false;
    if (!evaluateConstantImm(getImm(), Imm, VK) ||
        VK != LoongArchMCExpr::VK_LoongArch_None)
      return false;
    return (isLA64() && isUInt<6>(Imm)) || isUInt<5>(Imm);
  }

  bool isUImmLog2XLenNonZero() const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    if (!isImm())
      return false;
    if (!evaluateConstantImm(getImm(), Imm, VK) ||
        VK != LoongArchMCExpr::VK_LoongArch_None)
      return false;
    if (Imm == 0)
      return false;
    return (isLA64() && isUInt<6>(Imm)) || isUInt<5>(Imm);
  }

  bool isUImmLog2XLenHalf() const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    if (!isImm())
      return false;
    if (!evaluateConstantImm(getImm(), Imm, VK) ||
        VK != LoongArchMCExpr::VK_LoongArch_None)
      return false;
    return (isLA64() && isUInt<5>(Imm)) || isUInt<4>(Imm);
  }

  bool isUImm2() const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isUInt<2>(Imm) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isUImm2Alsl() const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && 1 <= Imm && Imm <= 4 &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isUImm5() const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isUInt<5>(Imm) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isSImm5() const {
    if (!isImm())
      return false;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    int64_t Imm;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isInt<5>(Imm) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isUImm6() const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isUInt<6>(Imm) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isSImm6() const {
    if (!isImm())
      return false;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    int64_t Imm;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isInt<6>(Imm) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isSImm6NonZero() const {
    if (!isImm())
      return false;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    int64_t Imm;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isInt<6>(Imm) && (Imm != 0) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isCLUIImm() const {
    if (!isImm())
      return false;
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && (Imm != 0) &&
           (isUInt<5>(Imm) || (Imm >= 0xfffe0 && Imm <= 0xfffff)) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isUImm7Lsb00() const {
    if (!isImm())
      return false;
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isShiftedUInt<5, 2>(Imm) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isUImm8Lsb00() const {
    if (!isImm())
      return false;
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isShiftedUInt<6, 2>(Imm) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isUImm12() const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isUInt<12>(Imm) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isSImm12() const {
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    int64_t Imm;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isInt<12>(Imm) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isSImm14() const {
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    int64_t Imm;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isInt<14>(Imm) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isUImm15() const {
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    int64_t Imm;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isUInt<15>(Imm) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isSImm16() const {
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    int64_t Imm;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isInt<16>(Imm) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isSImm20() const {
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    int64_t Imm;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isInt<20>(Imm) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isSImm16Lsb00() const {
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    if (!isImm())
      return false;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    if (!IsConstantImm)
      return false;
    bool IsValid = isShiftedInt<16 - 2, 2>(Imm);
    return IsValid && VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isSImm18Lsb00() const { return isBareSimmNLsb00<18>(); }
  bool isSImm23Lsb00() const { return isBareSimmNLsb00<23>(); }
  bool isSImm28Lsb00() const {
    // B and BL can be used to call functions, so accept %plt here.
    return isBareSimmNLsb00<28>(true);
  }

  bool isImmZero() const {
    if (!isImm())
      return false;
    int64_t Imm;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && (Imm == 0) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  bool isSImm5Plus1() const {
    if (!isImm())
      return false;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    int64_t Imm;
    bool IsConstantImm = evaluateConstantImm(getImm(), Imm, VK);
    return IsConstantImm && isInt<5>(Imm - 1) &&
           VK == LoongArchMCExpr::VK_LoongArch_None;
  }

  /// getStartLoc - Gets location of the first token of this operand
  SMLoc getStartLoc() const override { return StartLoc; }
  /// getEndLoc - Gets location of the last token of this operand
  SMLoc getEndLoc() const override { return EndLoc; }
  /// True if this operand is for an LA64 instruction
  bool isLA64() const { return IsLA64; }

  unsigned getReg() const override {
    assert(Kind == KindTy::Register && "Invalid type access!");
    return Reg.RegNum.id();
  }

  StringRef getSysReg() const {
    assert(Kind == KindTy::SystemRegister && "Invalid type access!");
    return StringRef(SysReg.Data, SysReg.Length);
  }

  const MCExpr *getImm() const {
    assert(Kind == KindTy::Immediate && "Invalid type access!");
    return Imm.Val;
  }

  StringRef getToken() const {
    assert(Kind == KindTy::Token && "Invalid type access!");
    return Tok;
  }

  unsigned getVType() const {
    assert(Kind == KindTy::VType && "Invalid type access!");
    return VType.Val;
  }

  void print(raw_ostream &OS) const override {
    auto RegName = [](unsigned Reg) {
      if (Reg)
        return LoongArchInstPrinter::getRegisterName(Reg);
      else
        return "noreg";
    };

    switch (Kind) {
    case KindTy::Immediate:
      OS << *getImm();
      break;
    case KindTy::Register:
      OS << "<register " << RegName(getReg()) << ">";
      break;
    case KindTy::Token:
      OS << "'" << getToken() << "'";
      break;
    case KindTy::SystemRegister:
      OS << "<sysreg: " << getSysReg() << '>';
      break;
    case KindTy::VType:
      OS << "<vtype: ";
      LoongArchVType::printVType(getVType(), OS);
      OS << '>';
      break;
    }
  }

  static std::unique_ptr<LoongArchOperand> createToken(StringRef Str, SMLoc S,
                                                       bool IsLA64) {
    auto Op = std::make_unique<LoongArchOperand>(KindTy::Token);
    Op->Tok = Str;
    Op->StartLoc = S;
    Op->EndLoc = S;
    Op->IsLA64 = IsLA64;
    return Op;
  }

  static std::unique_ptr<LoongArchOperand> createReg(unsigned RegNo, SMLoc S,
                                                     SMLoc E, bool IsLA64) {
    auto Op = std::make_unique<LoongArchOperand>(KindTy::Register);
    Op->Reg.RegNum = RegNo;
    Op->StartLoc = S;
    Op->EndLoc = E;
    Op->IsLA64 = IsLA64;
    return Op;
  }

  static std::unique_ptr<LoongArchOperand> createImm(const MCExpr *Val, SMLoc S,
                                                     SMLoc E, bool IsLA64) {
    auto Op = std::make_unique<LoongArchOperand>(KindTy::Immediate);
    Op->Imm.Val = Val;
    Op->StartLoc = S;
    Op->EndLoc = E;
    Op->IsLA64 = IsLA64;
    return Op;
  }

  static std::unique_ptr<LoongArchOperand>
  createSysReg(StringRef Str, SMLoc S, unsigned Encoding, bool IsLA64) {
    auto Op = std::make_unique<LoongArchOperand>(KindTy::SystemRegister);
    Op->SysReg.Data = Str.data();
    Op->SysReg.Length = Str.size();
    Op->SysReg.Encoding = Encoding;
    Op->StartLoc = S;
    Op->IsLA64 = IsLA64;
    return Op;
  }

  void addExpr(MCInst &Inst, const MCExpr *Expr) const {
    assert(Expr && "Expr shouldn't be null!");
    int64_t Imm = 0;
    LoongArchMCExpr::VariantKind VK = LoongArchMCExpr::VK_LoongArch_None;
    bool IsConstant = evaluateConstantImm(Expr, Imm, VK);

    if (IsConstant)
      Inst.addOperand(MCOperand::createImm(Imm));
    else
      Inst.addOperand(MCOperand::createExpr(Expr));
  }

  // Used by the TableGen Code
  void addRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getReg()));
  }

  void addImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }

  void addFenceArgOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    // isFenceArg has validated the operand, meaning this cast is safe
    auto SE = cast<MCSymbolRefExpr>(getImm());

    unsigned Imm = 0;
    for (char c : SE->getSymbol().getName()) {
      switch (c) {
      default:
        llvm_unreachable("FenceArg must contain only [iorw]");
      case 'i':
        Imm |= LoongArchFenceField::I;
        break;
      case 'o':
        Imm |= LoongArchFenceField::O;
        break;
      case 'r':
        Imm |= LoongArchFenceField::R;
        break;
      case 'w':
        Imm |= LoongArchFenceField::W;
        break;
      }
    }
    Inst.addOperand(MCOperand::createImm(Imm));
  }

  void addCSRSystemRegisterOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createImm(SysReg.Encoding));
  }

  // Returns the rounding mode represented by this LoongArchOperand. Should only
  // be called after checking isFRMArg.
  LoongArchFPRndMode::RoundingMode getRoundingMode() const {
    // isFRMArg has validated the operand, meaning this cast is safe.
    auto SE = cast<MCSymbolRefExpr>(getImm());
    LoongArchFPRndMode::RoundingMode FRM =
        LoongArchFPRndMode::stringToRoundingMode(SE->getSymbol().getName());
    assert(FRM != LoongArchFPRndMode::Invalid && "Invalid rounding mode");
    return FRM;
  }

  void addFRMArgOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createImm(getRoundingMode()));
  }
};
} // end anonymous namespace.

#define GET_REGISTER_MATCHER
#define GET_SUBTARGET_FEATURE_NAME
#define GET_MATCHER_IMPLEMENTATION
#define GET_MNEMONIC_SPELL_CHECKER
#include "LoongArchGenAsmMatcher.inc"

bool LoongArchAsmParser::generateImmOutOfRangeError(
    OperandVector &Operands, uint64_t ErrorInfo, int64_t Lower, int64_t Upper,
    Twine Msg = "immediate must be an integer in the range") {
  SMLoc ErrorLoc = ((LoongArchOperand &)*Operands[ErrorInfo]).getStartLoc();
  return Error(ErrorLoc, Msg + " [" + Twine(Lower) + ", " + Twine(Upper) + "]");
}

static std::string LoongArchMnemonicSpellCheck(StringRef S,
                                               const FeatureBitset &FBS,
                                               unsigned VariantID = 0);

bool LoongArchAsmParser::MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                                                 OperandVector &Operands,
                                                 MCStreamer &Out,
                                                 uint64_t &ErrorInfo,
                                                 bool MatchingInlineAsm) {
  MCInst Inst;
  FeatureBitset MissingFeatures;

  auto Result = MatchInstructionImpl(Operands, Inst, ErrorInfo, MissingFeatures,
                                     MatchingInlineAsm);
  switch (Result) {
  default:
    break;
  case Match_Success:
    if (validateInstruction(Inst, Operands))
      return true;
    return processInstruction(Inst, IDLoc, Operands, Out);
  case Match_MissingFeature: {
    assert(MissingFeatures.any() && "Unknown missing features!");
    bool FirstFeature = true;
    std::string Msg = "instruction requires the following:";
    for (unsigned i = 0, e = MissingFeatures.size(); i != e; ++i) {
      if (MissingFeatures[i]) {
        Msg += FirstFeature ? " " : ", ";
        Msg += getSubtargetFeatureName(i);
        FirstFeature = false;
      }
    }
    return Error(IDLoc, Msg);
  }
  case Match_MnemonicFail: {
    FeatureBitset FBS = ComputeAvailableFeatures(getSTI().getFeatureBits());
    std::string Suggestion = LoongArchMnemonicSpellCheck(
        ((LoongArchOperand &)*Operands[0]).getToken(), FBS);
    return Error(IDLoc, "unrecognized instruction mnemonic" + Suggestion);
  }
  case Match_InvalidOperand: {
    SMLoc ErrorLoc = IDLoc;
    if (ErrorInfo != ~0U) {
      if (ErrorInfo >= Operands.size())
        return Error(ErrorLoc, "too few operands for instruction");

      ErrorLoc = ((LoongArchOperand &)*Operands[ErrorInfo]).getStartLoc();
      if (ErrorLoc == SMLoc())
        ErrorLoc = IDLoc;
    }
    return Error(ErrorLoc, "invalid operand for instruction");
  }
  }

  // Handle the case when the error message is of specific type
  // other than the generic Match_InvalidOperand, and the
  // corresponding operand is missing.
  if (Result > FIRST_TARGET_MATCH_RESULT_TY) {
    SMLoc ErrorLoc = IDLoc;
    if (ErrorInfo != ~0U && ErrorInfo >= Operands.size())
      return Error(ErrorLoc, "too few operands for instruction");
  }

  switch (Result) {
  default:
    break;
  case Match_InvalidBareSymbol: {
    SMLoc ErrorLoc = ((LoongArchOperand &)*Operands[ErrorInfo]).getStartLoc();
    return Error(ErrorLoc, "operand must be a bare symbol name");
  }
  case Match_InvalidCallSymbol: {
    SMLoc ErrorLoc = ((LoongArchOperand &)*Operands[ErrorInfo]).getStartLoc();
    return Error(ErrorLoc, "operand must be a bare symbol name");
  }
  case Match_InvalidImm32LI:
    return generateImmOutOfRangeError(Operands, ErrorInfo,
                                      std::numeric_limits<int32_t>::min(),
                                      std::numeric_limits<uint32_t>::max());
  case Match_InvalidImm64LI: {
    SMLoc ErrorLoc = ((LoongArchOperand &)*Operands[ErrorInfo]).getStartLoc();
    return Error(ErrorLoc, "operand must be a constant 64-bit integer");
  }
  case Match_InvalidImmZero: {
    SMLoc ErrorLoc = ((LoongArchOperand &)*Operands[ErrorInfo]).getStartLoc();
    return Error(ErrorLoc, "immediate must be zero");
  }
  case Match_InvalidSImm12:
    return generateImmOutOfRangeError(Operands, ErrorInfo, -(1 << 11),
                                      (1 << 11) - 1);
  case Match_InvalidSImm14:
    return generateImmOutOfRangeError(Operands, ErrorInfo, -(1 << 13),
                                      (1 << 13) - 1);
  case Match_InvalidSImm16:
    return generateImmOutOfRangeError(Operands, ErrorInfo, -(1 << 15),
                                      (1 << 15) - 1);
  case Match_InvalidSImm16Lsb00:
    return generateImmOutOfRangeError(
        Operands, ErrorInfo, -(1 << 11), (1 << 11) - 4,
        "operand must be a multiple of 4 in the range");
  case Match_InvalidSImm18Lsb00:
    return generateImmOutOfRangeError(
        Operands, ErrorInfo, -(1 << 17), (1 << 17) - 4,
        "operand must be a bare symbol or a multiple of 4 in the range");
  case Match_InvalidSImm20:
    return generateImmOutOfRangeError(Operands, ErrorInfo, -(1 << 19),
                                      (1 << 19) - 1);
  case Match_InvalidSImm23Lsb00:
    return generateImmOutOfRangeError(
        Operands, ErrorInfo, -(1 << 23), (1 << 23) - 4,
        "operand must be a bare symbol or a multiple of 4 in the range");
  case Match_InvalidSImm28Lsb00:
    return generateImmOutOfRangeError(
        Operands, ErrorInfo, -(1 << 27), (1 << 27) - 4,
        "operand must be a bare symbol, a symbol with %plt modifier, "
        "or a multiple of 4 in the range");
  case Match_InvalidUImm12:
    return generateImmOutOfRangeError(Operands, ErrorInfo, 0, (1 << 12) - 1);
  case Match_InvalidUImm15:
    return generateImmOutOfRangeError(Operands, ErrorInfo, 0, (1 << 15) - 1);
  case Match_InvalidUImm2:
    return generateImmOutOfRangeError(Operands, ErrorInfo, 0, (1 << 2) - 1);
  case Match_InvalidUImm2Alsl:
    return generateImmOutOfRangeError(Operands, ErrorInfo, 1, (1 << 2));
  case Match_InvalidUImm5:
    return generateImmOutOfRangeError(Operands, ErrorInfo, 0, (1 << 5) - 1);
  case Match_InvalidUImm6:
    return generateImmOutOfRangeError(Operands, ErrorInfo, 0, (1 << 6) - 1);
  case Match_InvalidUImmLog2XLen:
    if (isLA64())
      return generateImmOutOfRangeError(Operands, ErrorInfo, 0, (1 << 6) - 1);
    return generateImmOutOfRangeError(Operands, ErrorInfo, 0, (1 << 5) - 1);
  }

  llvm_unreachable("Unknown match type detected!");
}

// Attempts to match Name as a register (either using the default name or
// alternative ABI names), setting RegNo to the matching register. Upon
// failure, returns true and sets RegNo to 0.
static bool matchRegisterNameHelper(MCRegister &RegNo, StringRef Name) {
  RegNo = MatchRegisterName(Name);
  // The 32- and 64-bit FPRs have the same asm name. Check that the initial
  // match always matches the 64-bit variant, and not the 32-bit one.
  assert(!(RegNo >= LoongArch::F0_F && RegNo <= LoongArch::F31_F));
  // The default FPR register class is based on the tablegen enum ordering.
  static_assert(LoongArch::F0_D < LoongArch::F0_F,
                "FPR matching must be updated");
  if (RegNo == LoongArch::NoRegister)
    RegNo = MatchRegisterAltName(Name);
  return RegNo == LoongArch::NoRegister;
}

bool LoongArchAsmParser::ParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                       SMLoc &EndLoc) {
  if (tryParseRegister(RegNo, StartLoc, EndLoc) != MatchOperand_Success)
    return Error(StartLoc, "invalid register name");
  return false;
}

OperandMatchResultTy LoongArchAsmParser::tryParseRegister(unsigned &RegNo,
                                                          SMLoc &StartLoc,
                                                          SMLoc &EndLoc) {
  const AsmToken &Tok = getParser().getTok();
  StartLoc = Tok.getLoc();
  EndLoc = Tok.getEndLoc();
  RegNo = 0;

  if (getLexer().is(AsmToken::Dollar))
    getParser().Lex();
  else
    return MatchOperand_NoMatch;

  StringRef Name = getLexer().getTok().getIdentifier();
  if (matchRegisterNameHelper((MCRegister &)RegNo, Name))
    return MatchOperand_NoMatch;

  getParser().Lex(); // Eat identifier token.
  return MatchOperand_Success;
}

OperandMatchResultTy
LoongArchAsmParser::parseRegister(OperandVector &Operands) {
  AsmToken Dollar;

  // In LoongArch assembly (at least GNU as dialect), all register operands
  // starts with '$'.
  if (getLexer().is(AsmToken::Dollar)) {
    Dollar = getParser().getTok();
    getParser().Lex();
  } else
    return MatchOperand_NoMatch;

  switch (getLexer().getKind()) {
  default:
    getLexer().UnLex(Dollar);
    return MatchOperand_NoMatch;
  case AsmToken::Identifier:
    StringRef Name = getLexer().getTok().getIdentifier();
    MCRegister RegNo;
    matchRegisterNameHelper(RegNo, Name);

    if (RegNo == LoongArch::NoRegister) {
      return MatchOperand_NoMatch;
    }
    SMLoc S = getLoc();
    SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);
    getLexer().Lex();
    Operands.push_back(LoongArchOperand::createReg(RegNo, S, E, isLA64()));
  }

  return MatchOperand_Success;
}

OperandMatchResultTy
LoongArchAsmParser::parseImmediate(OperandVector &Operands) {
  SMLoc S = getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);
  const MCExpr *Res;

  switch (getLexer().getKind()) {
  default:
    return MatchOperand_NoMatch;
  case AsmToken::LParen:
  case AsmToken::Dot:
  case AsmToken::Minus:
  case AsmToken::Plus:
  case AsmToken::Exclaim:
  case AsmToken::Tilde:
  case AsmToken::Integer:
  case AsmToken::String:
  case AsmToken::Identifier:
    if (getParser().parseExpression(Res))
      return MatchOperand_ParseFail;
    break;
  case AsmToken::Percent:
    return parseOperandWithModifier(Operands);
  }

  Operands.push_back(LoongArchOperand::createImm(Res, S, E, isLA64()));
  return MatchOperand_Success;
}

OperandMatchResultTy
LoongArchAsmParser::parseOperandWithModifier(OperandVector &Operands) {
  SMLoc S = getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);

  if (getLexer().getKind() != AsmToken::Percent) {
    Error(getLoc(), "expected '%' for operand modifier");
    return MatchOperand_ParseFail;
  }

  getParser().Lex(); // Eat '%'

  if (getLexer().getKind() != AsmToken::Identifier) {
    Error(getLoc(), "expected valid identifier for operand modifier");
    return MatchOperand_ParseFail;
  }
  StringRef Identifier = getParser().getTok().getIdentifier();
  LoongArchMCExpr::VariantKind VK =
      LoongArchMCExpr::getVariantKindForName(Identifier);
  if (VK == LoongArchMCExpr::VK_LoongArch_Invalid) {
    Error(getLoc(), "unrecognized operand modifier");
    return MatchOperand_ParseFail;
  }

  getParser().Lex(); // Eat the identifier
  if (getLexer().getKind() != AsmToken::LParen) {
    Error(getLoc(), "expected '('");
    return MatchOperand_ParseFail;
  }
  getParser().Lex(); // Eat '('

  const MCExpr *SubExpr;
  if (getParser().parseParenExpression(SubExpr, E)) {
    return MatchOperand_ParseFail;
  }

  const MCExpr *ModExpr = LoongArchMCExpr::create(SubExpr, VK, getContext());
  Operands.push_back(LoongArchOperand::createImm(ModExpr, S, E, isLA64()));
  return MatchOperand_Success;
}

OperandMatchResultTy
LoongArchAsmParser::parseBareSymbol(OperandVector &Operands) {
  SMLoc S = getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);
  const MCExpr *Res;

  if (getLexer().getKind() != AsmToken::Identifier)
    return MatchOperand_NoMatch;

  StringRef Identifier;
  AsmToken Tok = getLexer().getTok();

  if (getParser().parseIdentifier(Identifier))
    return MatchOperand_ParseFail;

  if (Identifier.consume_back("@plt")) {
    Error(getLoc(), "'@plt' operand not valid for instruction");
    return MatchOperand_ParseFail;
  }

  MCSymbol *Sym = getContext().getOrCreateSymbol(Identifier);

  if (Sym->isVariable()) {
    const MCExpr *V = Sym->getVariableValue(/*SetUsed=*/false);
    if (!isa<MCSymbolRefExpr>(V)) {
      getLexer().UnLex(Tok); // Put back if it's not a bare symbol.
      return MatchOperand_NoMatch;
    }
    Res = V;
  } else
    Res = MCSymbolRefExpr::create(Sym, MCSymbolRefExpr::VK_None, getContext());

  MCBinaryExpr::Opcode Opcode;
  switch (getLexer().getKind()) {
  default:
    Operands.push_back(LoongArchOperand::createImm(Res, S, E, isLA64()));
    return MatchOperand_Success;
  case AsmToken::Plus:
    Opcode = MCBinaryExpr::Add;
    break;
  case AsmToken::Minus:
    Opcode = MCBinaryExpr::Sub;
    break;
  }

  const MCExpr *Expr;
  if (getParser().parseExpression(Expr))
    return MatchOperand_ParseFail;
  Res = MCBinaryExpr::create(Opcode, Res, Expr, getContext());
  Operands.push_back(LoongArchOperand::createImm(Res, S, E, isLA64()));
  return MatchOperand_Success;
}

OperandMatchResultTy
LoongArchAsmParser::parseCallSymbol(OperandVector &Operands) {
  SMLoc S = getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);
  const MCExpr *Res;

  if (getLexer().getKind() != AsmToken::Identifier)
    return MatchOperand_NoMatch;

  // Avoid parsing the register in `call rd, foo` as a call symbol.
  if (getLexer().peekTok().getKind() != AsmToken::EndOfStatement)
    return MatchOperand_NoMatch;

  StringRef Identifier;
  if (getParser().parseIdentifier(Identifier))
    return MatchOperand_ParseFail;

  LoongArchMCExpr::VariantKind Kind = LoongArchMCExpr::VK_LoongArch_CALL;
  if (Identifier.consume_back("@plt"))
    Kind = LoongArchMCExpr::VK_LoongArch_CALL_PLT;

  MCSymbol *Sym = getContext().getOrCreateSymbol(Identifier);
  Res = MCSymbolRefExpr::create(Sym, MCSymbolRefExpr::VK_None, getContext());
  Res = LoongArchMCExpr::create(Res, Kind, getContext());
  Operands.push_back(LoongArchOperand::createImm(Res, S, E, isLA64()));
  return MatchOperand_Success;
}

/// Looks at a token type and creates the relevant operand from this
/// information, adding to Operands. If operand was parsed, returns false, else
/// true.
bool LoongArchAsmParser::parseOperand(OperandVector &Operands,
                                      StringRef Mnemonic) {
  // Check if the current operand has a custom associated parser, if so, try to
  // custom parse the operand, or fallback to the general approach.
  OperandMatchResultTy Result =
      MatchOperandParserImpl(Operands, Mnemonic, /*ParseForAllFeatures=*/true);
  if (Result == MatchOperand_Success)
    return false;
  if (Result == MatchOperand_ParseFail)
    return true;

  // Attempt to parse token as a register.
  if (parseRegister(Operands) == MatchOperand_Success)
    return false;

  // Attempt to parse token as an immediate
  if (parseImmediate(Operands) == MatchOperand_Success)
    return false;

  // Finally we have exhausted all options and must declare defeat.
  Error(getLoc(), "unknown operand");
  return true;
}

bool LoongArchAsmParser::ParseInstruction(ParseInstructionInfo &Info,
                                          StringRef Name, SMLoc NameLoc,
                                          OperandVector &Operands) {
  // First operand is token for instruction
  Operands.push_back(LoongArchOperand::createToken(Name, NameLoc, isLA64()));

  // If there are no more operands, then finish
  if (getLexer().is(AsmToken::EndOfStatement))
    return false;

  // Parse first operand
  if (parseOperand(Operands, Name))
    return true;

  // Parse until end of statement, consuming commas between operands
  unsigned OperandIdx = 1;
  while (getLexer().is(AsmToken::Comma)) {
    // Consume comma token
    getLexer().Lex();

    // Parse next operand
    if (parseOperand(Operands, Name))
      return true;

    ++OperandIdx;
  }

  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    SMLoc Loc = getLexer().getLoc();
    getParser().eatToEndOfStatement();
    return Error(Loc, "unexpected token");
  }

  getParser().Lex(); // Consume the EndOfStatement.
  return false;
}

bool LoongArchAsmParser::classifySymbolRef(const MCExpr *Expr,
                                           LoongArchMCExpr::VariantKind &Kind) {
  Kind = LoongArchMCExpr::VK_LoongArch_None;

  if (const LoongArchMCExpr *RE = dyn_cast<LoongArchMCExpr>(Expr)) {
    Kind = RE->getKind();
    Expr = RE->getSubExpr();
  }

  MCValue Res;
  MCFixup Fixup;
  if (Expr->evaluateAsRelocatable(Res, nullptr, &Fixup))
    return Res.getRefKind() == LoongArchMCExpr::VK_LoongArch_None;
  return false;
}

bool LoongArchAsmParser::ParseDirective(AsmToken DirectiveID) {
  // This returns false if this function recognizes the directive
  // regardless of whether it is successfully handles or reports an
  // error. Otherwise it returns true to give the generic parser a
  // chance at recognizing it.
  StringRef IDVal = DirectiveID.getString();

  if (IDVal == ".option")
    return parseDirectiveOption();
  else if (IDVal == ".attribute")
    return parseDirectiveAttribute();

  return true;
}

bool LoongArchAsmParser::parseDirectiveOption() {
  MCAsmParser &Parser = getParser();
  // Get the option token.
  AsmToken Tok = Parser.getTok();
  // At the moment only identifiers are supported.
  if (Tok.isNot(AsmToken::Identifier))
    return Error(Parser.getTok().getLoc(),
                 "unexpected token, expected identifier");

  Warning(Parser.getTok().getLoc(), "'.option' is not supported yet");
  Parser.eatToEndOfStatement();
  return false;
}

/// parseDirectiveAttribute
///  ::= .attribute expression ',' ( expression | "string" )
///  ::= .attribute identifier ',' ( expression | "string" )
bool LoongArchAsmParser::parseDirectiveAttribute() {
  MCAsmParser &Parser = getParser();
  SMLoc TagLoc;
  TagLoc = Parser.getTok().getLoc();
  Error(TagLoc, ".attribute is not supported yet");
  return false;
}

void LoongArchAsmParser::emitToStreamer(MCStreamer &S, const MCInst &Inst) {
  MCInst CInst;
  S.emitInstruction(Inst, getSTI());
}

void LoongArchAsmParser::emitLoadImm(MCRegister DestReg, int64_t Value,
                                     MCStreamer &Out) {
  LoongArchMatInt::InstSeq Seq;
  LoongArchMatInt::generateInstSeq(
      Value, getSTI().getFeatureBits()[LoongArch::Feature64Bit], Seq);

  MCRegister SrcReg = LoongArch::R0;
  for (LoongArchMatInt::Inst &Inst : Seq) {
    if (Inst.Opc == LoongArch::LU12I_W)
      emitToStreamer(Out,
                     MCInstBuilder(Inst.Opc).addReg(DestReg).addImm(Inst.Imm));
    else
      emitToStreamer(
          Out, MCInstBuilder(Inst.Opc).addReg(DestReg).addReg(SrcReg).addImm(
                   Inst.Imm));

    SrcReg = DestReg;
  }
}

bool LoongArchAsmParser::validateInstruction(MCInst &Inst,
                                             OperandVector &Operands) {
  return false;
}

bool LoongArchAsmParser::processInstruction(MCInst &Inst, SMLoc IDLoc,
                                            OperandVector &Operands,
                                            MCStreamer &Out) {
  Inst.setLoc(IDLoc);

  switch (Inst.getOpcode()) {
  default:
    break;
  case LoongArch::PseudoLI_W: {
    MCRegister Reg = Inst.getOperand(0).getReg();
    int64_t Imm = Inst.getOperand(1).getImm();
    Imm = SignExtend64<32>(Imm);
    emitLoadImm(Reg, Imm, Out);
    return false;
  }
  case LoongArch::PseudoLI_D: {
    MCRegister Reg = Inst.getOperand(0).getReg();
    int64_t Imm = Inst.getOperand(1).getImm();
    emitLoadImm(Reg, Imm, Out);
    return false;
  }
  }

  emitToStreamer(Out, Inst);
  return false;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeLoongArchAsmParser() {
  RegisterMCAsmParser<LoongArchAsmParser> X(getTheLoongArch32Target());
  RegisterMCAsmParser<LoongArchAsmParser> Y(getTheLoongArch64Target());
}
