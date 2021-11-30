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

  if (Arg *A = Args.getLastArg(options::OPT_mabi_EQ)) {
    ABIName = A->getValue();
    // Convert a GNU style LoongArch ABI name to the name
    // accepted by LLVM LoongArch backend.
    ABIName = llvm::StringSwitch<llvm::StringRef>(ABIName)
                  .Case("32", "lp32")
                  .Case("64", "lp64")
                  .Default(ABIName);
  }

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
                  .Case("loongarch32", "lp32")
                  .Case("la464", "lp64")
                  .Default("");
  }

  if (ABIName.empty()) {
    // Deduce ABI name from the target triple.
    ABIName = Triple.isLoongArch32() ? "lp32" : "lp64";
  }

  if (CPUName.empty()) {
    // Deduce CPU name from ABI name.
    CPUName = llvm::StringSwitch<const char *>(ABIName)
                  .Case("lp32", DefLoongArch32CPU)
                  .Cases("lpx32", "lp64", DefLoongArch64CPU)
                  .Default("");
  }

  // FIXME: Warn on inconsistent use of -march and -mabi.
}

std::string loongarch::getLoongArchABILibSuffix(const ArgList &Args,
                                      const llvm::Triple &Triple) {
  StringRef CPUName, ABIName;
  tools::loongarch::getLoongArchCPUAndABI(Args, Triple, CPUName, ABIName);
  return llvm::StringSwitch<std::string>(ABIName)
      .Case("lp32", "")
      .Case("lpx32", "32")
      .Case("lp64", "64");
}

// Convert ABI name to the GNU tools acceptable variant.
StringRef loongarch::getGnuCompatibleLoongArchABIName(StringRef ABI) {
  return llvm::StringSwitch<llvm::StringRef>(ABI)
      .Case("lp32", "32")
      .Case("lp64", "64")
      .Default(ABI);
}

// Select the LoongArch float ABI as determined by -msoft-float, -mhard-float,
// and -mfloat-abi=.
loongarch::FloatABI loongarch::getLoongArchFloatABI(const Driver &D, const ArgList &Args) {
  loongarch::FloatABI ABI = loongarch::FloatABI::Invalid;
  if (Arg *A =
          Args.getLastArg(options::OPT_msoft_float, options::OPT_mhard_float,
                          options::OPT_mfloat_abi_EQ)) {
    if (A->getOption().matches(options::OPT_msoft_float))
      ABI = loongarch::FloatABI::Soft;
    else if (A->getOption().matches(options::OPT_mhard_float))
      ABI = loongarch::FloatABI::Hard;
    else {
      ABI = llvm::StringSwitch<loongarch::FloatABI>(A->getValue())
                .Case("soft", loongarch::FloatABI::Soft)
                .Case("hard", loongarch::FloatABI::Hard)
                .Default(loongarch::FloatABI::Invalid);
      if (ABI == loongarch::FloatABI::Invalid && !StringRef(A->getValue()).empty()) {
        D.Diag(clang::diag::err_drv_invalid_mfloat_abi) << A->getAsString(Args);
        ABI = loongarch::FloatABI::Hard;
      }
    }
  }

  // If unspecified, choose the default based on the platform.
  if (ABI == loongarch::FloatABI::Invalid) {
    // Assume "hard", because it's a default value used by gcc.
    // When we start to recognize specific target LoongArch processors,
    // we will be able to select the default more correctly.
    ABI = loongarch::FloatABI::Hard;
  }

  assert(ABI != loongarch::FloatABI::Invalid && "must select an ABI");
  return ABI;
}

void loongarch::getLoongArchTargetFeatures(const Driver &D, const llvm::Triple &Triple,
                                 const ArgList &Args,
                                 std::vector<StringRef> &Features) {
  StringRef CPUName;
  StringRef ABIName;
  getLoongArchCPUAndABI(Args, Triple, CPUName, ABIName);
  ABIName = getGnuCompatibleLoongArchABIName(ABIName);

  // At final link time, LP32 and LPX32 with CPIC will have another section
  // added to the binary which contains the stub functions to perform
  // any fixups required for PIC code.

  bool IsLP64 = ABIName == "64";
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

  if (IsLP64 && NonPIC) {
    NonPIC = false;
  }

  loongarch::FloatABI FloatABI = loongarch::getLoongArchFloatABI(D, Args);
  if (FloatABI == loongarch::FloatABI::Soft) {
    // FIXME: Note, this is a hack. We need to pass the selected float
    // mode to the LoongArchTargetInfoBase to define appropriate macros there.
    // Now it is the only method.
    Features.push_back("+soft-float");
  }

  AddTargetFeature(Args, Features, options::OPT_msingle_float,
                   options::OPT_mdouble_float, "single-float");

  // Add the last -mfp32/-mfp64, if none are given and fp64 is default,
  // pass fp64.
  if (Arg *A = Args.getLastArg(options::OPT_mfp32,
                               options::OPT_mfp64)) {
    if (A->getOption().matches(options::OPT_mfp32))
      Features.push_back("-fp64");
    else
      Features.push_back("+fp64");
  } else if (loongarch::isFP64Default(Args)) {
    Features.push_back("+fp64");
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

bool loongarch::isFP64Default(const ArgList &Args) {
  return Args.getLastArg(options::OPT_msingle_float) ? false : true;
}
