//===-- LoongArchTargetStreamer.h - LoongArch Target Streamer ------------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_LOONGARCHTARGETSTREAMER_H
#define LLVM_LIB_TARGET_LOONGARCH_LOONGARCHTARGETSTREAMER_H

#include "MCTargetDesc/LoongArchFPABIInfo.h"
#include "MCTargetDesc/LoongArchABIInfo.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCELFStreamer.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"

namespace llvm {

class formatted_raw_ostream;

struct LoongArchFPABIInfo;

class LoongArchTargetStreamer : public MCTargetStreamer {
public:
  LoongArchTargetStreamer(MCStreamer &S);

  virtual void setPic(bool Value) {}

  virtual void emitDirectiveOptionPic0();
  virtual void emitDirectiveOptionPic2();

  virtual void emitDirectiveSetArch(StringRef Arch);
  virtual void emitDirectiveSetLoongArch32();
  virtual void emitDirectiveSetloongarch64();
  virtual void emitDirectiveSetSoftFloat();
  virtual void emitDirectiveSetHardFloat();

  virtual void emitDirectiveModuleFP();
  virtual void emitDirectiveModuleSoftFloat();
  virtual void emitDirectiveModuleHardFloat();
  virtual void emitDirectiveSetFp(LoongArchFPABIInfo::FpABIKind Value);

  void emitR(unsigned Opcode, unsigned Reg0, SMLoc IDLoc,
             const MCSubtargetInfo *STI);
  void emitII(unsigned Opcode, int16_t Imm1, int16_t Imm2, SMLoc IDLoc,
              const MCSubtargetInfo *STI);
  void emitRX(unsigned Opcode, unsigned Reg0, MCOperand Op1, SMLoc IDLoc,
              const MCSubtargetInfo *STI);
  void emitRI(unsigned Opcode, unsigned Reg0, int32_t Imm, SMLoc IDLoc,
              const MCSubtargetInfo *STI);
  void emitRR(unsigned Opcode, unsigned Reg0, unsigned Reg1, SMLoc IDLoc,
              const MCSubtargetInfo *STI);
  void emitRXX(unsigned Opcode, unsigned Reg0, MCOperand Op1, MCOperand Op2,
               SMLoc IDLoc, const MCSubtargetInfo *STI);
  void emitRRX(unsigned Opcode, unsigned Reg0, unsigned Reg1, MCOperand Op2,
               SMLoc IDLoc, const MCSubtargetInfo *STI);
  void emitRRR(unsigned Opcode, unsigned Reg0, unsigned Reg1, unsigned Reg2,
               SMLoc IDLoc, const MCSubtargetInfo *STI);
  void emitRRI(unsigned Opcode, unsigned Reg0, unsigned Reg1, int16_t Imm,
               SMLoc IDLoc, const MCSubtargetInfo *STI);
  void emitRRXX(unsigned Opcode, unsigned Reg0, unsigned Reg1, MCOperand Op2,
                MCOperand Op3, SMLoc IDLoc, const MCSubtargetInfo *STI);
  void emitRRIII(unsigned Opcode, unsigned Reg0, unsigned Reg1, int16_t Imm0,
                 int16_t Imm1, int16_t Imm2, SMLoc IDLoc,
                 const MCSubtargetInfo *STI);
  void emitAdd(unsigned DstReg, unsigned SrcReg, unsigned TrgReg, bool Is64Bit,
                const MCSubtargetInfo *STI);
  void emitDSLL(unsigned DstReg, unsigned SrcReg, int16_t ShiftAmount,
                SMLoc IDLoc, const MCSubtargetInfo *STI);
  void emitNop(SMLoc IDLoc, const MCSubtargetInfo *STI);

  void forbidModuleDirective() { ModuleDirectiveAllowed = false; }
  void reallowModuleDirective() { ModuleDirectiveAllowed = true; }
  bool isModuleDirectiveAllowed() { return ModuleDirectiveAllowed; }

  template <class PredicateLibrary>
  void updateABIInfo(const PredicateLibrary &P) {
    ABI = P.getABI();
    FPABIInfo.setAllFromPredicates(P);
  }

  LoongArchFPABIInfo &getFPABIInfo() { return FPABIInfo; }
  const LoongArchABIInfo &getABI() const {
    assert(ABI.hasValue() && "ABI hasn't been set!");
    return *ABI;
  }

protected:
  llvm::Optional<LoongArchABIInfo> ABI;
  LoongArchFPABIInfo FPABIInfo;

  bool GPRInfoSet;

  bool FPRInfoSet;

  bool FrameInfoSet;
  int FrameOffset;
  unsigned FrameReg;
  unsigned ReturnReg;

private:
  bool ModuleDirectiveAllowed;
};

// This part is for ascii assembly output
class LoongArchTargetAsmStreamer : public LoongArchTargetStreamer {
  formatted_raw_ostream &OS;

public:
  LoongArchTargetAsmStreamer(MCStreamer &S, formatted_raw_ostream &OS);

  void emitDirectiveOptionPic0() override;
  void emitDirectiveOptionPic2() override;

  void emitDirectiveSetArch(StringRef Arch) override;
  void emitDirectiveSetLoongArch32() override;
  void emitDirectiveSetloongarch64() override;
  void emitDirectiveSetSoftFloat() override;
  void emitDirectiveSetHardFloat() override;

  // FP abiflags directives
  void emitDirectiveModuleFP() override;
  void emitDirectiveModuleSoftFloat() override;
  void emitDirectiveModuleHardFloat() override;
  void emitDirectiveSetFp(LoongArchFPABIInfo::FpABIKind Value) override;
};

// This part is for ELF object output
class LoongArchTargetELFStreamer : public LoongArchTargetStreamer {
  const MCSubtargetInfo &STI;
  bool Pic;

public:
  MCELFStreamer &getStreamer();
  LoongArchTargetELFStreamer(MCStreamer &S, const MCSubtargetInfo &STI);

  void setPic(bool Value) override { Pic = Value; }

  void emitLabel(MCSymbol *Symbol) override;
  void finish() override;

  void emitDirectiveOptionPic0() override;
  void emitDirectiveOptionPic2() override;
};
}
#endif
