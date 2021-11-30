//===-- LoongArchMCExpr.cpp - LoongArch specific MC expression classes --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "LoongArchMCExpr.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"
#include <cstdint>

using namespace llvm;

#define DEBUG_TYPE "loongarchmcexpr"

const LoongArchMCExpr *LoongArchMCExpr::create(LoongArchMCExpr::LoongArchExprKind Kind,
                                     const MCExpr *Expr, MCContext &Ctx) {
  return new (Ctx) LoongArchMCExpr(Kind, Expr);
}

void LoongArchMCExpr::printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const {
  int64_t AbsVal;
  if (Expr->evaluateAsAbsolute(AbsVal))
    OS << AbsVal;
  else
    Expr->print(OS, MAI, true);
}

bool
LoongArchMCExpr::evaluateAsRelocatableImpl(MCValue &Res,
                                      const MCAsmLayout *Layout,
                                      const MCFixup *Fixup) const {
  if (!getSubExpr()->evaluateAsRelocatable(Res, Layout, Fixup))
    return false;

  if (Res.getRefKind() != MCSymbolRefExpr::VK_None)
    return false;

  // evaluateAsAbsolute() and evaluateAsValue() require that we evaluate the
  // %hi/%lo/etc. here. Fixup is a null pointer when either of these is the
  // caller.
  if (Res.isAbsolute() && Fixup == nullptr) {
    int64_t AbsVal = Res.getConstant();
    switch (Kind) {
    default:
      break;
    case MEK_None:
    case MEK_Special:
      llvm_unreachable("MEK_None and MEK_Special are invalid");
    }
    Res = MCValue::get(AbsVal);
    return true;
  }

  // We want to defer it for relocatable expressions since the constant is
  // applied to the whole symbol value.
  //
  // The value of getKind() that is given to MCValue is only intended to aid
  // debugging when inspecting MCValue objects. It shouldn't be relied upon
  // for decision making.
  Res = MCValue::get(Res.getSymA(), Res.getSymB(), Res.getConstant(), getKind());

  return true;
}

void LoongArchMCExpr::visitUsedExpr(MCStreamer &Streamer) const {
  Streamer.visitUsedExpr(*getSubExpr());
}

static void fixELFSymbolsInTLSFixupsImpl(const MCExpr *Expr, MCAssembler &Asm) {
  switch (Expr->getKind()) {
  case MCExpr::Target:
    fixELFSymbolsInTLSFixupsImpl(cast<LoongArchMCExpr>(Expr)->getSubExpr(), Asm);
    break;
  case MCExpr::Constant:
    break;
  case MCExpr::Binary: {
    const MCBinaryExpr *BE = cast<MCBinaryExpr>(Expr);
    fixELFSymbolsInTLSFixupsImpl(BE->getLHS(), Asm);
    fixELFSymbolsInTLSFixupsImpl(BE->getRHS(), Asm);
    break;
  }
  case MCExpr::SymbolRef: {
    // We're known to be under a TLS fixup, so any symbol should be
    // modified. There should be only one.
    const MCSymbolRefExpr &SymRef = *cast<MCSymbolRefExpr>(Expr);
    cast<MCSymbolELF>(SymRef.getSymbol()).setType(ELF::STT_TLS);
    break;
  }
  case MCExpr::Unary:
    fixELFSymbolsInTLSFixupsImpl(cast<MCUnaryExpr>(Expr)->getSubExpr(), Asm);
    break;
  }
}

void LoongArchMCExpr::fixELFSymbolsInTLSFixups(MCAssembler &Asm) const {
  switch (getKind()) {
  default:
    break;
  case MEK_None:
  case MEK_Special:
    llvm_unreachable("MEK_None and MEK_Special are invalid");
    break;
  case MEK_CALL_HI:
  case MEK_CALL_LO:
  case MEK_GOT_HI:
  case MEK_GOT_LO:
  case MEK_GOT_RRHI:
  case MEK_GOT_RRLO:
  case MEK_GOT_RRHIGHER:
  case MEK_GOT_RRHIGHEST:
  case MEK_ABS_HI:
  case MEK_ABS_LO:
  case MEK_ABS_HIGHER:
  case MEK_ABS_HIGHEST:
  case MEK_PCREL_HI:
  case MEK_PCREL_LO:
  case MEK_PCREL_RRHI:
  case MEK_PCREL_RRHIGHER:
  case MEK_PCREL_RRHIGHEST:
  case MEK_PCREL_RRLO:
  case MEK_PLT:
    // If we do have nested target-specific expressions, they will be in
    // a consecutive chain.
    if (const LoongArchMCExpr *E = dyn_cast<const LoongArchMCExpr>(getSubExpr()))
      E->fixELFSymbolsInTLSFixups(Asm);
    break;
  case MEK_TLSGD_HI:
  case MEK_TLSGD_LO:
  case MEK_TLSGD_RRHI:
  case MEK_TLSGD_RRHIGHER:
  case MEK_TLSGD_RRHIGHEST:
  case MEK_TLSGD_RRLO:
  case MEK_TLSLE_HI:
  case MEK_TLSLE_HIGHER:
  case MEK_TLSLE_HIGHEST:
  case MEK_TLSLE_LO:
  case MEK_TLSIE_HI:
  case MEK_TLSIE_LO:
  case MEK_TLSIE_RRHI:
  case MEK_TLSIE_RRHIGHER:
  case MEK_TLSIE_RRHIGHEST:
  case MEK_TLSIE_RRLO:
    fixELFSymbolsInTLSFixupsImpl(getSubExpr(), Asm);
    break;
  }
}
