//===-- LoongArchTargetStreamer.cpp - LoongArch Target Streamer Methods ---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides LoongArch specific target streamer methods.
//
//===----------------------------------------------------------------------===//

#include "LoongArchTargetStreamer.h"
#include "LoongArchMCTargetDesc.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

LoongArchTargetStreamer::LoongArchTargetStreamer(MCStreamer &S)
    : MCTargetStreamer(S) {}

void LoongArchTargetStreamer::finish() { finishAttributeSection(); }

void LoongArchTargetStreamer::emitDirectiveOptionPush() {}
void LoongArchTargetStreamer::emitDirectiveOptionPop() {}
void LoongArchTargetStreamer::emitDirectiveOptionPIC() {}
void LoongArchTargetStreamer::emitDirectiveOptionNoPIC() {}
void LoongArchTargetStreamer::emitDirectiveOptionRVC() {}
void LoongArchTargetStreamer::emitDirectiveOptionNoRVC() {}
void LoongArchTargetStreamer::emitDirectiveOptionRelax() {}
void LoongArchTargetStreamer::emitDirectiveOptionNoRelax() {}
void LoongArchTargetStreamer::emitAttribute(unsigned Attribute,
                                            unsigned Value) {}
void LoongArchTargetStreamer::finishAttributeSection() {}
void LoongArchTargetStreamer::emitTextAttribute(unsigned Attribute,
                                                StringRef String) {}
void LoongArchTargetStreamer::emitIntTextAttribute(unsigned Attribute,
                                                   unsigned IntValue,
                                                   StringRef StringValue) {}

void LoongArchTargetStreamer::emitTargetAttributes(const MCSubtargetInfo &STI) {
}

// This part is for ascii assembly output
LoongArchTargetAsmStreamer::LoongArchTargetAsmStreamer(
    MCStreamer &S, formatted_raw_ostream &OS)
    : LoongArchTargetStreamer(S), OS(OS) {}

void LoongArchTargetAsmStreamer::emitDirectiveOptionPush() {
  OS << "\t.option\tpush\n";
}

void LoongArchTargetAsmStreamer::emitDirectiveOptionPop() {
  OS << "\t.option\tpop\n";
}

void LoongArchTargetAsmStreamer::emitDirectiveOptionPIC() {
  OS << "\t.option\tpic\n";
}

void LoongArchTargetAsmStreamer::emitDirectiveOptionNoPIC() {
  OS << "\t.option\tnopic\n";
}

void LoongArchTargetAsmStreamer::emitDirectiveOptionRVC() {
  OS << "\t.option\trvc\n";
}

void LoongArchTargetAsmStreamer::emitDirectiveOptionNoRVC() {
  OS << "\t.option\tnorvc\n";
}

void LoongArchTargetAsmStreamer::emitDirectiveOptionRelax() {
  OS << "\t.option\trelax\n";
}

void LoongArchTargetAsmStreamer::emitDirectiveOptionNoRelax() {
  OS << "\t.option\tnorelax\n";
}

void LoongArchTargetAsmStreamer::emitAttribute(unsigned Attribute,
                                               unsigned Value) {
  OS << "\t.attribute\t" << Attribute << ", " << Twine(Value) << "\n";
}

void LoongArchTargetAsmStreamer::emitTextAttribute(unsigned Attribute,
                                                   StringRef String) {
  OS << "\t.attribute\t" << Attribute << ", \"" << String << "\"\n";
}

void LoongArchTargetAsmStreamer::emitIntTextAttribute(unsigned Attribute,
                                                      unsigned IntValue,
                                                      StringRef StringValue) {}

void LoongArchTargetAsmStreamer::finishAttributeSection() {}
