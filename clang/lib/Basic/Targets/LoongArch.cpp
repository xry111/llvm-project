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

  if (ABI == "lp32") {
    Builder.defineMacro("__loongarch32");
  } else {
    Builder.defineMacro("__loongarch64");
  }

  if (ABI == "lp32") {
    Builder.defineMacro("_ABILP32", "1");
  } else if (ABI == "lpx32") {
    Builder.defineMacro("_ABILPX32", "2");
  } else if (ABI == "lp64") {
    Builder.defineMacro("_ABILP64", "3");
    Builder.defineMacro("_LOONGARCH_SIM", "_ABILP64");
  } else
    llvm_unreachable("Invalid ABI.");

  Builder.defineMacro("__REGISTER_PREFIX__", "");

  switch (FloatABI) {
  case HardFloat:
    Builder.defineMacro("__loongarch_hard_float", Twine(1));
    break;
  case SoftFloat:
    Builder.defineMacro("__loongarch_soft_float", Twine(1));
    break;
  }

  if (IsSingleFloat)
    Builder.defineMacro("__loongarch_single_float", Twine(1));

  switch (FPMode) {
  case FP32:
    Builder.defineMacro("__loongarch_fpr", Twine(32));
    break;
  case FP64:
    Builder.defineMacro("__loongarch_fpr", Twine(64));
    break;
  }

  Builder.defineMacro("_LOONGARCH_SZPTR", Twine(getPointerWidth(0)));
  Builder.defineMacro("_LOONGARCH_SZINT", Twine(getIntWidth()));
  Builder.defineMacro("_LOONGARCH_SZLONG", Twine(getLongWidth()));

  Builder.defineMacro("_LOONGARCH_ARCH", "\"" + CPU + "\"");
  Builder.defineMacro("_LOONGARCH_ARCH_" + StringRef(CPU).upper());

  Builder.defineMacro("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1");
  Builder.defineMacro("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2");
  Builder.defineMacro("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4");

  // 32-bit loongarch processors don't have the necessary lld/scd instructions
  // found in 64-bit processors. In the case of lp32 on a 64-bit processor,
  // the instructions exist but using them violates the ABI since they
  // require 64-bit GPRs and LP32 only supports 32-bit GPRs.
  if (ABI == "lpx32" || ABI == "lp64")
    Builder.defineMacro("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8");
}

bool LoongArchTargetInfo::hasFeature(StringRef Feature) const {
  return llvm::StringSwitch<bool>(Feature)
      .Case("fp64", FPMode == FP64)
      .Default(false);
}

ArrayRef<Builtin::Info> LoongArchTargetInfo::getTargetBuiltins() const {
  return llvm::makeArrayRef(BuiltinInfo, clang::LoongArch::LastTSBuiltin -
                                             Builtin::FirstTSBuiltin);
}

bool LoongArchTargetInfo::validateTarget(DiagnosticsEngine &Diags) const {
  // FIXME: It's valid to use LP32 on a 64-bit CPU but the backend can't handle
  //        this yet. It's better to fail here than on the backend assertion.
  if (processorSupportsGPR64() && ABI == "lp32") {
    Diags.Report(diag::err_target_unsupported_abi) << ABI << CPU;
    return false;
  }

  // 64-bit ABI's require 64-bit CPU's.
  if (!processorSupportsGPR64() && (ABI == "lpx32" || ABI == "lp64")) {
    Diags.Report(diag::err_target_unsupported_abi) << ABI << CPU;
    return false;
  }

  // FIXME: It's valid to use lp32 on a loongarch64 triple but the backend
  //        can't handle this yet. It's better to fail here than on the
  //        backend assertion.
  if (getTriple().isLoongArch64() && ABI == "lp32") {
    Diags.Report(diag::err_target_unsupported_abi_for_triple)
        << ABI << getTriple().str();
    return false;
  }

  // FIXME: It's valid to use lpx32/lp64 on a loongarch32 triple but the backend
  //        can't handle this yet. It's better to fail here than on the
  //        backend assertion.
  if (getTriple().isLoongArch32() && (ABI == "lpx32" || ABI == "lp64")) {
    Diags.Report(diag::err_target_unsupported_abi_for_triple)
        << ABI << getTriple().str();
    return false;
  }

  // -mfp32 and lpx32/lp64 ABIs are incompatible
  if (FPMode != FP64 && !IsSingleFloat &&
      (ABI == "lpx32" || ABI == "lp64")) {
    Diags.Report(diag::err_opt_not_valid_with_opt) << "-mfp32" << ABI;
    return false;
  }

  if (FPMode != FP64 && (CPU == "la464")) {
    Diags.Report(diag::err_opt_not_valid_with_opt) << "-mfp32" << CPU;
    return false;
  }

  return true;
}
