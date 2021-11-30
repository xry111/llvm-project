//===- LoongArchMCExpr.h - LoongArch specific MC expression classes -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHMCEXPR_H
#define LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHMCEXPR_H

#include "llvm/MC/MCAsmLayout.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCValue.h"

namespace llvm {

class LoongArchMCExpr : public MCTargetExpr {
public:
  enum LoongArchExprKind {
    MEK_None,
    MEK_CALL_HI,
    MEK_CALL_LO,
    MEK_GOT_HI,
    MEK_GOT_LO,
    MEK_GOT_RRHI,
    MEK_GOT_RRHIGHER,
    MEK_GOT_RRHIGHEST,
    MEK_GOT_RRLO,
    MEK_ABS_HI,
    MEK_ABS_HIGHER,
    MEK_ABS_HIGHEST,
    MEK_ABS_LO,
    MEK_PCREL_HI,
    MEK_PCREL_LO,
    MEK_PCREL_RRHI,
    MEK_PCREL_RRHIGHER,
    MEK_PCREL_RRHIGHEST,
    MEK_PCREL_RRLO,
    MEK_TLSLE_HI,
    MEK_TLSLE_HIGHER,
    MEK_TLSLE_HIGHEST,
    MEK_TLSLE_LO,
    MEK_TLSIE_HI,
    MEK_TLSIE_LO,
    MEK_TLSIE_RRHI,
    MEK_TLSIE_RRHIGHER,
    MEK_TLSIE_RRHIGHEST,
    MEK_TLSIE_RRLO,
    MEK_TLSGD_HI,
    MEK_TLSGD_LO,
    MEK_TLSGD_RRHI,
    MEK_TLSGD_RRHIGHER,
    MEK_TLSGD_RRHIGHEST,
    MEK_TLSGD_RRLO,
    MEK_PLT,
    MEK_Special,
  };

private:
  const LoongArchExprKind Kind;
  const MCExpr *Expr;

  explicit LoongArchMCExpr(LoongArchExprKind Kind, const MCExpr *Expr)
      : Kind(Kind), Expr(Expr) {}

public:
  static const LoongArchMCExpr *create(LoongArchExprKind Kind, const MCExpr *Expr,
                                  MCContext &Ctx);
  static const LoongArchMCExpr *createGpOff(LoongArchExprKind Kind, const MCExpr *Expr,
                                       MCContext &Ctx);

  /// Get the kind of this expression.
  LoongArchExprKind getKind() const { return Kind; }

  /// Get the child of this expression.
  const MCExpr *getSubExpr() const { return Expr; }

  void printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const override;
  bool evaluateAsRelocatableImpl(MCValue &Res, const MCAsmLayout *Layout,
                                 const MCFixup *Fixup) const override;
  void visitUsedExpr(MCStreamer &Streamer) const override;

  MCFragment *findAssociatedFragment() const override {
    return getSubExpr()->findAssociatedFragment();
  }

  void fixELFSymbolsInTLSFixups(MCAssembler &Asm) const override;

  static bool classof(const MCExpr *E) {
    return E->getKind() == MCExpr::Target;
  }
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHMCEXPR_H
