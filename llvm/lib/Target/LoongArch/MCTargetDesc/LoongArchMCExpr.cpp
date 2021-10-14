//===-- LoongArchMCExpr.cpp - LoongArch specific MC expression classes ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the implementation of the assembly expression modifiers
// accepted by the LoongArch architecture (e.g. ":lo12:", ":gottprel_g1:", ...).
//
//===----------------------------------------------------------------------===//

#include "LoongArchMCExpr.h"
#include "LoongArchFixupKinds.h"
#include "MCTargetDesc/LoongArchAsmBackend.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCAsmLayout.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define DEBUG_TYPE "loongarchmcexpr"

const LoongArchMCExpr *
LoongArchMCExpr::create(const MCExpr *Expr, VariantKind Kind, MCContext &Ctx) {
  return new (Ctx) LoongArchMCExpr(Expr, Kind);
}

void LoongArchMCExpr::printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const {
  VariantKind Kind = getKind();
  bool HasVariant =
      ((Kind != VK_LoongArch_None) && (Kind != VK_LoongArch_CALL));

  if (HasVariant)
    OS << '%' << getVariantKindName(getKind()) << '(';
  Expr->print(OS, MAI);
  if (HasVariant)
    OS << ')';
}

bool LoongArchMCExpr::evaluateAsRelocatableImpl(MCValue &Res,
                                                const MCAsmLayout *Layout,
                                                const MCFixup *Fixup) const {
  if (!getSubExpr()->evaluateAsRelocatable(Res, Layout, Fixup))
    return false;

  // Some custom fixup types are not valid with symbol difference expressions
  if (Res.getSymA() && Res.getSymB()) {
    switch (getKind()) {
    default:
      return true;
    }
  }

  return true;
}

void LoongArchMCExpr::visitUsedExpr(MCStreamer &Streamer) const {
  Streamer.visitUsedExpr(*getSubExpr());
}

LoongArchMCExpr::VariantKind
LoongArchMCExpr::getVariantKindForName(StringRef name) {
  return StringSwitch<LoongArchMCExpr::VariantKind>(name)
      .Case("plt", VK_LoongArch_CALL_PLT)
      .Default(VK_LoongArch_Invalid);
}

StringRef LoongArchMCExpr::getVariantKindName(VariantKind Kind) {
  switch (Kind) {
  default:
    llvm_unreachable("Invalid ELF symbol kind");
  case VK_LoongArch_CALL_PLT:
    return "plt";
  }
}

void LoongArchMCExpr::fixELFSymbolsInTLSFixups(MCAssembler &Asm) const {
  // Nothing to do here.  We're adding STT_TLS in MCCodeEmitter.
}

bool LoongArchMCExpr::evaluateAsConstant(int64_t &Res) const {
  MCValue Value;

  if (Kind == VK_LoongArch_CALL || Kind == VK_LoongArch_CALL_PLT)
    return false;

  if (!getSubExpr()->evaluateAsRelocatable(Value, nullptr, nullptr))
    return false;

  if (!Value.isAbsolute())
    return false;

  Res = evaluateAsInt64(Value.getConstant());
  return true;
}

int64_t LoongArchMCExpr::evaluateAsInt64(int64_t Value) const {
  switch (Kind) {
  default:
    llvm_unreachable("Invalid kind");
  }
}
