//===-- LoongArchMCAsmInfo.cpp - LoongArch Asm Properties ---------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the LoongArchMCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "LoongArchMCAsmInfo.h"
#include "llvm/ADT/Triple.h"

using namespace llvm;

void LoongArchMCAsmInfo::anchor() { }

LoongArchMCAsmInfo::LoongArchMCAsmInfo(const Triple &TheTriple,
                                       const MCTargetOptions &Options) {

  if (TheTriple.isLoongArch64())
    CodePointerSize = CalleeSaveStackSlotSize = 8;

  AlignmentIsInBytes          = false;
  Data16bitsDirective         = "\t.harf\t";
  Data32bitsDirective         = "\t.word\t";
  Data64bitsDirective         = "\t.dword\t";
  CommentString               = "#";
  ZeroDirective               = "\t.space\t";
  SupportsDebugInformation = true;
  ExceptionsType = ExceptionHandling::DwarfCFI;
  DwarfRegNumForCFI = true;
  //HasLoongArchExpressions = true;
  UseIntegratedAssembler = true;
}
