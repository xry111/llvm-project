//===-- LoongArchTargetObjectFile.cpp - LoongArch Object Info -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "LoongArchTargetObjectFile.h"
#include "LoongArchTargetMachine.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionELF.h"

using namespace llvm;

void LoongArchELFTargetObjectFile::Initialize(MCContext &Ctx,
                                              const TargetMachine &TM) {
  TargetLoweringObjectFileELF::Initialize(Ctx, TM);
}

MCSection *LoongArchELFTargetObjectFile::SelectSectionForGlobal(
    const GlobalObject *GO, SectionKind Kind, const TargetMachine &TM) const {
  return TargetLoweringObjectFileELF::SelectSectionForGlobal(GO, Kind, TM);
}

void LoongArchELFTargetObjectFile::getModuleMetadata(Module &M) {}

MCSection *LoongArchELFTargetObjectFile::getSectionForConstant(
    const DataLayout &DL, SectionKind Kind, const Constant *C,
    Align &Alignment) const {
  return TargetLoweringObjectFileELF::getSectionForConstant(DL, Kind, C,
                                                            Alignment);
}
