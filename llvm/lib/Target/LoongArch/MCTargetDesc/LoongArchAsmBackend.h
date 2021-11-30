//===-- LoongArchAsmBackend.h - LoongArch Asm Backend  ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the LoongArchAsmBackend class.
//
//===----------------------------------------------------------------------===//
//

#ifndef LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHASMBACKEND_H
#define LLVM_LIB_TARGET_LOONGARCH_MCTARGETDESC_LOONGARCHASMBACKEND_H

#include "MCTargetDesc/LoongArchFixupKinds.h"
#include "llvm/ADT/Triple.h"
#include "llvm/MC/MCAsmBackend.h"

namespace llvm {

class MCAssembler;
struct MCFixupKindInfo;
class MCObjectWriter;
class MCRegisterInfo;
class MCSymbolELF;
class Target;

class LoongArchAsmBackend : public MCAsmBackend {
  Triple TheTriple;
  bool IsLPX32;

public:
  LoongArchAsmBackend(const Target &T, const MCRegisterInfo &MRI, const Triple &TT,
                 StringRef CPU, bool LPX32)
      : MCAsmBackend(support::little),
        TheTriple(TT), IsLPX32(LPX32) {
    assert(TT.isLittleEndian());
  }

  std::unique_ptr<MCObjectTargetWriter>
  createObjectTargetWriter() const override;

  void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                  const MCValue &Target, MutableArrayRef<char> Data,
                  uint64_t Value, bool IsResolved,
                  const MCSubtargetInfo *STI) const override;

  Optional<MCFixupKind> getFixupKind(StringRef Name) const override;
  const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override;

  unsigned getNumFixupKinds() const override {
    return LoongArch::NumTargetFixupKinds;
  }

  /// @name Target Relaxation Interfaces
  /// @{

  /// MayNeedRelaxation - Check whether the given instruction may need
  /// relaxation.
  ///
  /// \param Inst - The instruction to test.
  bool mayNeedRelaxation(const MCInst &Inst,
                         const MCSubtargetInfo &STI) const override {
    return false;
  }

  /// fixupNeedsRelaxation - Target specific predicate for whether a given
  /// fixup requires the associated instruction to be relaxed.
   bool fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                             const MCRelaxableFragment *DF,
                             const MCAsmLayout &Layout) const override {
    // FIXME.
    llvm_unreachable("RelaxInstruction() unimplemented");
    return false;
  }

  /// @}

  bool writeNopData(raw_ostream &OS, uint64_t Count) const override;

  bool shouldForceRelocation(const MCAssembler &Asm, const MCFixup &Fixup,
                             const MCValue &Target) override;

}; // class LoongArchAsmBackend

} // namespace

#endif
