//===-- LoongArchTargetStreamer.cpp - LoongArch Target Streamer Methods -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides LoongArch specific target streamer methods.
//
//===----------------------------------------------------------------------===//

#include "LoongArchABIInfo.h"
#include "LoongArchELFStreamer.h"
#include "LoongArchInstPrinter.h"
#include "LoongArchMCExpr.h"
#include "LoongArchMCTargetDesc.h"
#include "LoongArchTargetObjectFile.h"
#include "LoongArchTargetStreamer.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

namespace {
static cl::opt<bool> RoundSectionSizes(
    "loongarch-round-section-sizes", cl::init(false),
    cl::desc("Round section sizes up to the section alignment"), cl::Hidden);
} // end anonymous namespace

LoongArchTargetStreamer::LoongArchTargetStreamer(MCStreamer &S)
    : MCTargetStreamer(S), ModuleDirectiveAllowed(true) {
  GPRInfoSet = FPRInfoSet = FrameInfoSet = false;
}
void LoongArchTargetStreamer::emitDirectiveOptionPic0() {}
void LoongArchTargetStreamer::emitDirectiveOptionPic2() {}
void LoongArchTargetStreamer::emitDirectiveSetArch(StringRef Arch) {
  forbidModuleDirective();
}
void LoongArchTargetStreamer::emitDirectiveSetLoongArch32() { forbidModuleDirective(); }
void LoongArchTargetStreamer::emitDirectiveSetloongarch64() { forbidModuleDirective(); }
void LoongArchTargetStreamer::emitDirectiveSetSoftFloat() {
  forbidModuleDirective();
}
void LoongArchTargetStreamer::emitDirectiveSetHardFloat() {
  forbidModuleDirective();
}
void LoongArchTargetStreamer::emitDirectiveModuleFP() {}

void LoongArchTargetStreamer::emitDirectiveModuleSoftFloat() {}
void LoongArchTargetStreamer::emitDirectiveModuleHardFloat() {}
void LoongArchTargetStreamer::emitDirectiveSetFp(
    LoongArchFPABIInfo::FpABIKind Value) {
  forbidModuleDirective();
}

void LoongArchTargetStreamer::emitR(unsigned Opcode, unsigned Reg0, SMLoc IDLoc,
                               const MCSubtargetInfo *STI) {
  MCInst TmpInst;
  TmpInst.setOpcode(Opcode);
  TmpInst.addOperand(MCOperand::createReg(Reg0));
  TmpInst.setLoc(IDLoc);
  getStreamer().emitInstruction(TmpInst, *STI);
}

void LoongArchTargetStreamer::emitRXX(unsigned Opcode, unsigned Reg0, MCOperand Op1,
                                MCOperand Op2, SMLoc IDLoc, const MCSubtargetInfo *STI) {
  MCInst TmpInst;
  TmpInst.setOpcode(Opcode);
  TmpInst.addOperand(MCOperand::createReg(Reg0));
  TmpInst.addOperand(Op1);
  TmpInst.addOperand(Op2);
  TmpInst.setLoc(IDLoc);
  getStreamer().emitInstruction(TmpInst, *STI);
}

void LoongArchTargetStreamer::emitRRXX(unsigned Opcode, unsigned Reg0, unsigned Reg1,
                                 MCOperand Op2, MCOperand Op3, SMLoc IDLoc,
                                 const MCSubtargetInfo *STI) {
  MCInst TmpInst;
  TmpInst.setOpcode(Opcode);
  TmpInst.addOperand(MCOperand::createReg(Reg0));
  TmpInst.addOperand(MCOperand::createReg(Reg1));
  TmpInst.addOperand(Op2);
  TmpInst.addOperand(Op3);
  TmpInst.setLoc(IDLoc);
  getStreamer().emitInstruction(TmpInst, *STI);
}

void LoongArchTargetStreamer::emitRX(unsigned Opcode, unsigned Reg0, MCOperand Op1,
                                SMLoc IDLoc, const MCSubtargetInfo *STI) {
  MCInst TmpInst;
  TmpInst.setOpcode(Opcode);
  TmpInst.addOperand(MCOperand::createReg(Reg0));
  TmpInst.addOperand(Op1);
  TmpInst.setLoc(IDLoc);
  getStreamer().emitInstruction(TmpInst, *STI);
}

void LoongArchTargetStreamer::emitRI(unsigned Opcode, unsigned Reg0, int32_t Imm,
                                SMLoc IDLoc, const MCSubtargetInfo *STI) {
  emitRX(Opcode, Reg0, MCOperand::createImm(Imm), IDLoc, STI);
}

void LoongArchTargetStreamer::emitRR(unsigned Opcode, unsigned Reg0, unsigned Reg1,
                                SMLoc IDLoc, const MCSubtargetInfo *STI) {
  emitRX(Opcode, Reg0, MCOperand::createReg(Reg1), IDLoc, STI);
}

void LoongArchTargetStreamer::emitII(unsigned Opcode, int16_t Imm1, int16_t Imm2,
                                SMLoc IDLoc, const MCSubtargetInfo *STI) {
  MCInst TmpInst;
  TmpInst.setOpcode(Opcode);
  TmpInst.addOperand(MCOperand::createImm(Imm1));
  TmpInst.addOperand(MCOperand::createImm(Imm2));
  TmpInst.setLoc(IDLoc);
  getStreamer().emitInstruction(TmpInst, *STI);
}

void LoongArchTargetStreamer::emitRRX(unsigned Opcode, unsigned Reg0, unsigned Reg1,
                                 MCOperand Op2, SMLoc IDLoc,
                                 const MCSubtargetInfo *STI) {
  MCInst TmpInst;
  TmpInst.setOpcode(Opcode);
  TmpInst.addOperand(MCOperand::createReg(Reg0));
  TmpInst.addOperand(MCOperand::createReg(Reg1));
  TmpInst.addOperand(Op2);
  TmpInst.setLoc(IDLoc);
  getStreamer().emitInstruction(TmpInst, *STI);
}

void LoongArchTargetStreamer::emitRRR(unsigned Opcode, unsigned Reg0, unsigned Reg1,
                                 unsigned Reg2, SMLoc IDLoc,
                                 const MCSubtargetInfo *STI) {
  emitRRX(Opcode, Reg0, Reg1, MCOperand::createReg(Reg2), IDLoc, STI);
}

void LoongArchTargetStreamer::emitRRI(unsigned Opcode, unsigned Reg0, unsigned Reg1,
                                 int16_t Imm, SMLoc IDLoc,
                                 const MCSubtargetInfo *STI) {
  emitRRX(Opcode, Reg0, Reg1, MCOperand::createImm(Imm), IDLoc, STI);
}

void LoongArchTargetStreamer::emitRRIII(unsigned Opcode, unsigned Reg0,
                                   unsigned Reg1, int16_t Imm0, int16_t Imm1,
                                   int16_t Imm2, SMLoc IDLoc,
                                   const MCSubtargetInfo *STI) {
  MCInst TmpInst;
  TmpInst.setOpcode(Opcode);
  TmpInst.addOperand(MCOperand::createReg(Reg0));
  TmpInst.addOperand(MCOperand::createReg(Reg1));
  TmpInst.addOperand(MCOperand::createImm(Imm0));
  TmpInst.addOperand(MCOperand::createImm(Imm1));
  TmpInst.addOperand(MCOperand::createImm(Imm2));
  TmpInst.setLoc(IDLoc);
  getStreamer().emitInstruction(TmpInst, *STI);
}

void LoongArchTargetStreamer::emitAdd(unsigned DstReg, unsigned SrcReg,
                                  unsigned TrgReg, bool Is64Bit,
                                  const MCSubtargetInfo *STI) {
  emitRRR(Is64Bit ? LoongArch::ADD_D : LoongArch::ADD_W, DstReg, SrcReg, TrgReg, SMLoc(),
          STI);
}

void LoongArchTargetStreamer::emitDSLL(unsigned DstReg, unsigned SrcReg,
                                  int16_t ShiftAmount, SMLoc IDLoc,
                                  const MCSubtargetInfo *STI) {
  if (ShiftAmount >= 32) {
    emitRRI(LoongArch::SLLI_D, DstReg, SrcReg, ShiftAmount - 32, IDLoc, STI);
    return;
  }

  emitRRI(LoongArch::SLLI_D, DstReg, SrcReg, ShiftAmount, IDLoc, STI);
}

void LoongArchTargetStreamer::emitNop(SMLoc IDLoc, const MCSubtargetInfo *STI) {
  emitRRI(LoongArch::ANDI, LoongArch::ZERO, LoongArch::ZERO, 0, IDLoc, STI);
}

LoongArchTargetAsmStreamer::LoongArchTargetAsmStreamer(MCStreamer &S,
                                             formatted_raw_ostream &OS)
    : LoongArchTargetStreamer(S), OS(OS) {}

void LoongArchTargetAsmStreamer::emitDirectiveOptionPic0() {
  OS << "\t.option\tpic0\n";
}

void LoongArchTargetAsmStreamer::emitDirectiveOptionPic2() {
  OS << "\t.option\tpic2\n";
}

void LoongArchTargetAsmStreamer::emitDirectiveSetArch(StringRef Arch) {
  OS << "\t.set arch=" << Arch << "\n";
  LoongArchTargetStreamer::emitDirectiveSetArch(Arch);
}

void LoongArchTargetAsmStreamer::emitDirectiveSetLoongArch32() {
  //OS << "\t.set\tloongarch32\n";
  LoongArchTargetStreamer::emitDirectiveSetLoongArch32();
}

void LoongArchTargetAsmStreamer::emitDirectiveSetloongarch64() {
  //OS << "\t.set\tloongarch64\n";
  LoongArchTargetStreamer::emitDirectiveSetloongarch64();
}

void LoongArchTargetAsmStreamer::emitDirectiveSetSoftFloat() {
  OS << "\t.set\tsoftfloat\n";
  LoongArchTargetStreamer::emitDirectiveSetSoftFloat();
}

void LoongArchTargetAsmStreamer::emitDirectiveSetHardFloat() {
  OS << "\t.set\thardfloat\n";
  LoongArchTargetStreamer::emitDirectiveSetHardFloat();
}

void LoongArchTargetAsmStreamer::emitDirectiveModuleFP() {
  LoongArchFPABIInfo::FpABIKind FpABI = FPABIInfo.getFpABI();
  if (FpABI == LoongArchFPABIInfo::FpABIKind::SOFT)
    OS << "\t.module\tsoftfloat\n";
  else
    OS << "\t.module\tfp=" << FPABIInfo.getFpABIString(FpABI) << "\n";
}

void LoongArchTargetAsmStreamer::emitDirectiveSetFp(
    LoongArchFPABIInfo::FpABIKind Value) {
  LoongArchTargetStreamer::emitDirectiveSetFp(Value);

  OS << "\t.set\tfp=";
  OS << FPABIInfo.getFpABIString(Value) << "\n";
}

void LoongArchTargetAsmStreamer::emitDirectiveModuleSoftFloat() {
  OS << "\t.module\tsoftfloat\n";
}

void LoongArchTargetAsmStreamer::emitDirectiveModuleHardFloat() {
  OS << "\t.module\thardfloat\n";
}

// This part is for ELF object output.
LoongArchTargetELFStreamer::LoongArchTargetELFStreamer(MCStreamer &S,
                                             const MCSubtargetInfo &STI)
    : LoongArchTargetStreamer(S), STI(STI) {
  MCAssembler &MCA = getStreamer().getAssembler();

  // It's possible that MCObjectFileInfo isn't fully initialized at this point
  // due to an initialization order problem where LLVMTargetMachine creates the
  // target streamer before TargetLoweringObjectFile calls
  // InitializeMCObjectFileInfo. There doesn't seem to be a single place that
  // covers all cases so this statement covers most cases and direct object
  // emission must call setPic() once MCObjectFileInfo has been initialized. The
  // cases we don't handle here are covered by LoongArchAsmPrinter.
  Pic = MCA.getContext().getObjectFileInfo()->isPositionIndependent();

  // Set the header flags that we can in the constructor.
  // FIXME: This is a fairly terrible hack. We set the rest
  // of these in the destructor. The problem here is two-fold:
  //
  // a: Some of the eflags can be set/reset by directives.
  // b: There aren't any usage paths that initialize the ABI
  //    pointer until after we initialize either an assembler
  //    or the target machine.
  // We can fix this by making the target streamer construct
  // the ABI, but this is fraught with wide ranging dependency
  // issues as well.
  unsigned EFlags = MCA.getELFHeaderEFlags();

  // FIXME: Fix a dependency issue by instantiating the ABI object to some
  // default based off the triple. The triple doesn't describe the target
  // fully, but any external user of the API that uses the MCTargetStreamer
  // would otherwise crash on assertion failure.

  ABI = LoongArchABIInfo(
      STI.getTargetTriple().getArch() == Triple::ArchType::loongarch32
          ? LoongArchABIInfo::LP32()
          : LoongArchABIInfo::LP64());

  EFlags |= ELF::EF_LARCH_ABI;
  MCA.setELFHeaderEFlags(EFlags);
}

void LoongArchTargetELFStreamer::emitLabel(MCSymbol *S) {
  auto *Symbol = cast<MCSymbolELF>(S);
  getStreamer().getAssembler().registerSymbol(*Symbol);
  uint8_t Type = Symbol->getType();
  if (Type != ELF::STT_FUNC)
    return;

}

void LoongArchTargetELFStreamer::finish() {
  MCAssembler &MCA = getStreamer().getAssembler();
  const MCObjectFileInfo &OFI = *MCA.getContext().getObjectFileInfo();

  // .bss, .text and .data are always at least 16-byte aligned.
  MCSection &TextSection = *OFI.getTextSection();
  MCA.registerSection(TextSection);
  MCSection &DataSection = *OFI.getDataSection();
  MCA.registerSection(DataSection);
  MCSection &BSSSection = *OFI.getBSSSection();
  MCA.registerSection(BSSSection);

  TextSection.setAlignment(Align(std::max(16u, TextSection.getAlignment())));
  DataSection.setAlignment(Align(std::max(16u, DataSection.getAlignment())));
  BSSSection.setAlignment(Align(std::max(16u, BSSSection.getAlignment())));

  if (RoundSectionSizes) {
    // Make sections sizes a multiple of the alignment. This is useful for
    // verifying the output of IAS against the output of other assemblers but
    // it's not necessary to produce a correct object and increases section
    // size.
    MCStreamer &OS = getStreamer();
    for (MCSection &S : MCA) {
      MCSectionELF &Section = static_cast<MCSectionELF &>(S);

      unsigned Alignment = Section.getAlignment();
      if (Alignment) {
        OS.SwitchSection(&Section);
        if (Section.UseCodeAlign())
          OS.emitCodeAlignment(Alignment, Alignment);
        else
          OS.emitValueToAlignment(Alignment, 0, 1, Alignment);
      }
    }
  }

  // Update e_header flags. See the FIXME and comment above in
  // the constructor for a full rundown on this.
  unsigned EFlags = MCA.getELFHeaderEFlags();

  // ABI
  // LP64 does not require any ABI bits.
  if (getABI().IsLP32())
    EFlags |= ELF::EF_LARCH_ABI_LP32;
  else if (getABI().IsLPX32())
    EFlags |= ELF::EF_LARCH_ABI_XLP32;
  else
    EFlags |= ELF::EF_LARCH_ABI_LP64;

  MCA.setELFHeaderEFlags(EFlags);
}

MCELFStreamer &LoongArchTargetELFStreamer::getStreamer() {
  return static_cast<MCELFStreamer &>(Streamer);
}

void LoongArchTargetELFStreamer::emitDirectiveOptionPic0() {
  MCAssembler &MCA = getStreamer().getAssembler();
  unsigned Flags = MCA.getELFHeaderEFlags();
  // This option overrides other PIC options like -KPIC.
  Pic = false;
  ///XXX:Reloc no this flags
  //Flags &= ~ELF::EF_LOONGARCH_PIC;
  MCA.setELFHeaderEFlags(Flags);
}

void LoongArchTargetELFStreamer::emitDirectiveOptionPic2() {
  MCAssembler &MCA = getStreamer().getAssembler();
  unsigned Flags = MCA.getELFHeaderEFlags();
  Pic = true;
  // NOTE: We are following the GAS behaviour here which means the directive
  // 'pic2' also sets the CPIC bit in the ELF header. This is different from
  // what is stated in the SYSV ABI which consider the bits EF_LOONGARCH_PIC and
  // EF_LOONGARCH_CPIC to be mutually exclusive.
  ///XXX:Reloc no this flags
  //Flags |= ELF::EF_LOONGARCH_PIC | ELF::EF_LOONGARCH_CPIC;
  MCA.setELFHeaderEFlags(Flags);
}
