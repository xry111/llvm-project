//===--- LoongArch.cpp - Implement LoongArch target feature support -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements LoongArch TargetInfo objects.
//
//===----------------------------------------------------------------------===//

#include "LoongArch.h"
#include "Targets.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/MacroBuilder.h"
#include "clang/Basic/TargetBuiltins.h"
#include "llvm/ADT/StringSwitch.h"

using namespace clang;
using namespace clang::targets;

const Builtin::Info LoongArchTargetInfo::BuiltinInfo[] = {
#define BUILTIN(ID, TYPE, ATTRS)                                               \
  {#ID, TYPE, ATTRS, nullptr, ALL_LANGUAGES, nullptr},
#define LIBBUILTIN(ID, TYPE, ATTRS, HEADER)                                    \
  {#ID, TYPE, ATTRS, HEADER, ALL_LANGUAGES, nullptr},
#include "clang/Basic/BuiltinsLoongArch.def"
};

bool LoongArchTargetInfo::processorSupportsGPR64() const {
  return llvm::StringSwitch<bool>(CPU)
      .Case("la464", true)
      .Default(false);
  return false;
}

static constexpr llvm::StringLiteral ValidCPUNames[] = {
    {"la464"}
};

bool LoongArchTargetInfo::isValidCPUName(StringRef Name) const {
  return llvm::find(ValidCPUNames, Name) != std::end(ValidCPUNames);
}

void LoongArchTargetInfo::fillValidCPUList(
    SmallVectorImpl<StringRef> &Values) const {
  Values.append(std::begin(ValidCPUNames), std::end(ValidCPUNames));
}

void LoongArchTargetInfo::getTargetDefines(const LangOptions &Opts,
                                      MacroBuilder &Builder) const {
  Builder.defineMacro("__loongarch__");

  if (ABI == "lp64d" || ABI == "lp64s" || ABI == "lp64f") {
    Builder.defineMacro("__loongarch_lp64");
    Builder.defineMacro("__loongarch64");
    Builder.defineMacro("_ABILP64", "3");
    Builder.defineMacro("_LOONGARCH_SIM", "_ABILP64");
  } else
    llvm_unreachable("Invalid ABI.");

  Builder.defineMacro("__REGISTER_PREFIX__", "");

  Builder.defineMacro("_LOONGARCH_SZPTR", Twine(getPointerWidth(0)));
  Builder.defineMacro("_LOONGARCH_SZINT", Twine(getIntWidth()));
  Builder.defineMacro("_LOONGARCH_SZLONG", Twine(getLongWidth()));

  Builder.defineMacro("_LOONGARCH_TUNE", "\"" + CPU + "\"");
  Builder.defineMacro("_LOONGARCH_TUNE_" + StringRef(CPU).upper());

  Builder.defineMacro("_LOONGARCH_ARCH", "\"" + getTriple().getArchName() + "\"");
  Builder.defineMacro("_LOONGARCH_ARCH_" + StringRef(getTriple().getArchName()).upper());

  Builder.defineMacro("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1");
  Builder.defineMacro("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2");
  Builder.defineMacro("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4");

  // 32-bit loongarch processors don't have the necessary ll.d/sc.d instructions
  // found in 64-bit processors.
  if (ABI == "lp64d" || ABI == "lp64s" || ABI == "lp64f")
    Builder.defineMacro("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8");

  // Bit-width of general purpose registers.
  Builder.defineMacro("__loongarch_grlen", Twine(getRegisterWidth()));

  // Bit-width of floating-point registers. The possible values for
  // this macro are 0, 32 and 64. 0 if there is no FPU.
  if (HasBasicD || HasBasicF)
    Builder.defineMacro("__loongarch_frlen", HasBasicD ? "64" : "32");
  else
    Builder.defineMacro("__loongarch_frlen", "0");

  // FIXME: Defined if floating-point/extended ABI type is single or double.
  if (ABI == "lp64d" || ABI == "lp64f")
    Builder.defineMacro("__loongarch_hard_float");

  // FIXME: Defined if floating-point/extended ABI type is double.
  if (ABI == "lp64d")
    Builder.defineMacro("__loongarch_double_float");

  // FIXME: Defined if floating-point/extended ABI type is single.
  if (ABI == "lp64f")
    Builder.defineMacro("__loongarch_single_float");

  // FIXME: Defined if floating-point/extended ABI type is soft.
  if (ABI == "lp64s")
    Builder.defineMacro("__loongarch_soft_float");
}

bool LoongArchTargetInfo::hasFeature(StringRef Feature) const {
  return llvm::StringSwitch<bool>(Feature)
      .Case("d", HasBasicD)
      .Case("f", HasBasicF)
      .Default(false);
}

ArrayRef<Builtin::Info> LoongArchTargetInfo::getTargetBuiltins() const {
  return llvm::makeArrayRef(BuiltinInfo, clang::LoongArch::LastTSBuiltin -
                                             Builtin::FirstTSBuiltin);
}

bool LoongArchTargetInfo::validateTarget(DiagnosticsEngine &Diags) const {
  // 64-bit ABI's require 64-bit CPU's.
  if (!processorSupportsGPR64() &&
      (ABI == "lp64d" || ABI == "lp64s" || ABI == "lp64f")) {
    Diags.Report(diag::err_target_unsupported_abi) << ABI << CPU;
    return false;
  }

  // FIXME: It's valid to use lp64d/lp64s/lp64f on a loongarch32 triple
  // but the backend can't handle this yet. It's better to fail here than on the
  // backend assertion.
  if (getTriple().isLoongArch32() &&
      (ABI == "lp64d" || ABI == "lp64s" || ABI == "lp64f")) {
    Diags.Report(diag::err_target_unsupported_abi_for_triple)
        << ABI << getTriple().str();
    return false;
  }

  return true;
}
