//===--- LoongArch.cpp - Tools Implementations -----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "LoongArch.h"
#include "ToolChains/CommonArgs.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/DriverDiagnostic.h"
#include "clang/Driver/Options.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Option/ArgList.h"

using namespace clang::driver;
using namespace clang::driver::tools;
using namespace clang;
using namespace llvm::opt;

// Get CPU and ABI names. They are not independent
// so we have to calculate them together.
void loongarch::getLoongArchCPUAndABI(const ArgList &Args, const llvm::Triple &Triple,
                            StringRef &CPUName, StringRef &ABIName) {
  const char *DefLoongArch32CPU = "loongarch32";
  const char *DefLoongArch64CPU = "la464";

  if (Arg *A = Args.getLastArg(clang::driver::options::OPT_march_EQ,
                               options::OPT_mcpu_EQ))
    CPUName = A->getValue();

  if (Arg *A = Args.getLastArg(options::OPT_mabi_EQ))
    ABIName = A->getValue();

  // Setup default CPU and ABI names.
  if (CPUName.empty() && ABIName.empty()) {
    switch (Triple.getArch()) {
    default:
      llvm_unreachable("Unexpected triple arch name");
    case llvm::Triple::loongarch32:
      CPUName = DefLoongArch32CPU;
      break;
    case llvm::Triple::loongarch64:
      CPUName = DefLoongArch64CPU;
      break;
    }
  }

  if (ABIName.empty()) {
    ABIName = llvm::StringSwitch<const char *>(CPUName)
                  .Case("loongarch32", "ilp32d")
                  .Case("la464", "lp64d")
                  .Default(Triple.isLoongArch32() ? "ilp32d" : "lp64d");
  }

  if (CPUName.empty()) {
    // Deduce CPU name from ABI name.
    CPUName = llvm::StringSwitch<const char *>(ABIName)
                  .Cases("lp64d", "lp64f", "lp64s", DefLoongArch64CPU)
                  .Default("");
  }

  if (Arg *A = Args.getLastArg(options::OPT_msingle_float,
                               options::OPT_mdouble_float,
                               options::OPT_msoft_float)) {
    if (A->getOption().matches(options::OPT_msingle_float))
      ABIName = "lp64f";
    else if (A->getOption().matches(options::OPT_mdouble_float))
      ABIName = "lp64d";
    else
      ABIName = "lp64s";
  }

  // FIXME: Warn on inconsistent use of -march and -mabi.
}

std::string loongarch::getLoongArchABILibSuffix(const ArgList &Args,
                                      const llvm::Triple &Triple) {
  StringRef CPUName, ABIName;
  tools::loongarch::getLoongArchCPUAndABI(Args, Triple, CPUName, ABIName);
  return llvm::StringSwitch<std::string>(ABIName)
      .Cases("ilp32d", "ilp32f", "ilp32s", "32")
      .Cases("lp64d", "lp64f", "lp64s", "64");
}

void loongarch::getLoongArchTargetFeatures(const Driver &D, const llvm::Triple &Triple,
                                 const ArgList &Args,
                                 std::vector<StringRef> &Features) {
  StringRef CPUName;
  StringRef ABIName;
  StringRef FPUValue;
  getLoongArchCPUAndABI(Args, Triple, CPUName, ABIName);

  bool NonPIC = false;

  Arg *LastPICArg = Args.getLastArg(options::OPT_fPIC, options::OPT_fno_PIC,
                                    options::OPT_fpic, options::OPT_fno_pic,
                                    options::OPT_fPIE, options::OPT_fno_PIE,
                                    options::OPT_fpie, options::OPT_fno_pie);
  if (LastPICArg) {
    Option O = LastPICArg->getOption();
    NonPIC =
        (O.matches(options::OPT_fno_PIC) || O.matches(options::OPT_fno_pic) ||
         O.matches(options::OPT_fno_PIE) || O.matches(options::OPT_fno_pie));
  }

  if (NonPIC) {
    NonPIC = false;
  }

  if (Arg *A = Args.getLastArg(options::OPT_mfpu_EQ))
    FPUValue = A->getValue();

  if (Arg *A = Args.getLastArg(options::OPT_msingle_float,
                               options::OPT_mdouble_float,
                               options::OPT_msoft_float)) {
    if (A->getOption().matches(options::OPT_msingle_float))
      FPUValue = "32";
    else if (A->getOption().matches(options::OPT_mdouble_float))
      FPUValue = "64";
    else
      FPUValue = "none";
  }

  // Setup feature.
  if (FPUValue.empty())
    Features.push_back("+d");
  else {
    if (FPUValue == "64")
      Features.push_back("+d");
    else if (FPUValue == "32")
      Features.push_back("+f");
    else if (FPUValue == "none") {
      Features.push_back("-f");
      Features.push_back("-d");
    } else
      D.Diag(clang::diag::err_drv_invalid_loongarch_mfpu)
          << FPUValue;
  }

  // lp64f ABI and -mfpu=none are incompatible.
  if (hasLoongArchAbiArg(Args, "lp64f") && hasLoongArchFpuArg(Args, "none")) {
    D.Diag(clang::diag::err_opt_not_valid_with_opt) << "lp64f"
                                                    << "-mfpu=none";
  }

  // Also lp64d ABI is only compatible with -mfpu=64.
  if ((hasLoongArchAbiArg(Args, "lp64d") || ABIName == "lp64d") &&
      (hasLoongArchFpuArg(Args, "none") || hasLoongArchFpuArg(Args, "32"))) {
    D.Diag(clang::diag::err_opt_not_valid_without_opt) << "lp64d"
                                                       << "-mfpu=64";
  }
}

bool loongarch::hasLoongArchAbiArg(const ArgList &Args, const char *Value) {
  Arg *A = Args.getLastArg(options::OPT_mabi_EQ);
  return A && (A->getValue() == StringRef(Value));
}

bool loongarch::isUCLibc(const ArgList &Args) {
  Arg *A = Args.getLastArg(options::OPT_m_libc_Group);
  return A && A->getOption().matches(options::OPT_muclibc);
}

bool loongarch::hasLoongArchFpuArg(const ArgList &Args, const char *Value) {
  Arg *A = Args.getLastArg(options::OPT_mfpu_EQ);
  return A && (A->getValue() == StringRef(Value));
}
