//===-- LoongArchFPABIInfo.cpp - Information about LoongArch Floating Point ABI's --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/LoongArchFPABIInfo.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

StringRef LoongArchFPABIInfo::getFpABIString(FpABIKind Value) {
  switch (Value) {
  case FpABIKind::S32:
    return "32";
  case FpABIKind::S64:
    return "64";
  default:
    llvm_unreachable("unsupported fp abi value");
  }
}
