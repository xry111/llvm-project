//===--- LoongArch.h - LoongArch-specific Tool Helpers ----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_ARCH_LOONGARCH_H
#define LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_ARCH_LOONGARCH_H

#include "clang/Driver/Driver.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Option/Option.h"
#include <string>
#include <vector>

namespace clang {
namespace driver {
namespace tools {

namespace loongarch {
enum class FloatABI {
  Invalid,
  Soft,
  Hard,
};

void getLoongArchCPUAndABI(const llvm::opt::ArgList &Args,
                      const llvm::Triple &Triple, StringRef &CPUName,
                      StringRef &ABIName);
void getLoongArchTargetFeatures(const Driver &D, const llvm::Triple &Triple,
                           const llvm::opt::ArgList &Args,
                           std::vector<StringRef> &Features);
StringRef getGnuCompatibleLoongArchABIName(StringRef ABI);
loongarch::FloatABI getLoongArchFloatABI(const Driver &D, const llvm::opt::ArgList &Args);
std::string getLoongArchABILibSuffix(const llvm::opt::ArgList &Args,
                                const llvm::Triple &Triple);
bool hasLoongArchAbiArg(const llvm::opt::ArgList &Args, const char *Value);
bool isUCLibc(const llvm::opt::ArgList &Args);
bool isFP64Default(const llvm::opt::ArgList &Args);

} // end namespace loongarch
} // end namespace target
} // end namespace driver
} // end namespace clang

#endif // LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_ARCH_LOONGARCH_H
