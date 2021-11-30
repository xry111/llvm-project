//===- LoongArchAnalyzeImmediate.cpp - Analyze Immediates ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "LoongArchAnalyzeImmediate.h"
#include "LoongArch.h"
#include "llvm/Support/MathExtras.h"
#include <cassert>
#include <cstdint>
#include <iterator>

using namespace llvm;

LoongArchAnalyzeImmediate::Inst::Inst(unsigned O, unsigned I) : Opc(O), ImmOpnd(I) {}

// Add I to the instruction sequences.
void LoongArchAnalyzeImmediate::AddInstr(InstSeqLs &SeqLs, const Inst &I) {
  // Add an instruction seqeunce consisting of just I.
  if (SeqLs.empty()) {
    SeqLs.push_back(InstSeq(1, I));
    return;
  }

  for (InstSeqLs::iterator Iter = SeqLs.begin(); Iter != SeqLs.end(); ++Iter)
    Iter->push_back(I);
}

void LoongArchAnalyzeImmediate::GetInstSeqLsADDI(uint64_t Imm, unsigned RemSize,
                                                 InstSeqLs &SeqLs) {
  GetInstSeqLs((Imm + 0x800ULL) & 0xfffffffffffff000ULL, RemSize, SeqLs);
  AddInstr(SeqLs, Inst(ADDI, Imm & 0xfffULL));
}

void LoongArchAnalyzeImmediate::GetInstSeqLsORI(uint64_t Imm, unsigned RemSize,
                                                InstSeqLs &SeqLs) {
  GetInstSeqLs(Imm & 0xfffffffffffff000ULL, RemSize, SeqLs);
  AddInstr(SeqLs, Inst(ORI, Imm & 0xfffULL));
}

void LoongArchAnalyzeImmediate::GetInstSeqLsSLLI(uint64_t Imm, unsigned RemSize,
                                                 InstSeqLs &SeqLs) {
  unsigned Shamt = countTrailingZeros(Imm);
  GetInstSeqLs(Imm >> Shamt, RemSize - Shamt, SeqLs);
  AddInstr(SeqLs, Inst(SLLI, Shamt));
}

void LoongArchAnalyzeImmediate::GetInstSeqLs(uint64_t Imm, unsigned RemSize,
                                             InstSeqLs &SeqLs) {
  uint64_t MaskedImm = Imm & (0xffffffffffffffffULL >> (64 - Size));

  // Do nothing if Imm is 0.
  if (!MaskedImm)
    return;

  // A single ADDI will do if RemSize <= 12.
  if (RemSize <= 12) {
    AddInstr(SeqLs, Inst(ORI, MaskedImm));
    return;
  }

  // Shift if the lower 12-bit is cleared.
  if (!(Imm & 0xfff)) {
    GetInstSeqLsSLLI(Imm, RemSize, SeqLs);
    return;
  }

  GetInstSeqLsADDI(Imm, RemSize, SeqLs);

  // If bit 11 is cleared, it doesn't make a difference whether the last
  // instruction is an ADDI or ORI. In that case, do not call GetInstSeqLsORI.
  if (Imm & 0x800) {
    InstSeqLs SeqLsORI;
    GetInstSeqLsORI(Imm, RemSize, SeqLsORI);
    SeqLs.append(std::make_move_iterator(SeqLsORI.begin()),
                 std::make_move_iterator(SeqLsORI.end()));
  }
}

// Replace a ADDI & SLLI pair with a LU12I.
// e.g. the following two instructions
//  ADDI 0x0111
//  SLLI 18
// are replaced with
//  LU12I 0x4440
void LoongArchAnalyzeImmediate::ReplaceADDISLLIWithLU12I(InstSeq &Seq) {
  //TODO: implement
}

void LoongArchAnalyzeImmediate::GetShortestSeq(InstSeqLs &SeqLs, InstSeq &Insts) {
  InstSeqLs::iterator ShortestSeq = SeqLs.end();
  // The length of an instruction sequence is at most 11.
  unsigned ShortestLength = 12;

  for (InstSeqLs::iterator S = SeqLs.begin(); S != SeqLs.end(); ++S) {
    ReplaceADDISLLIWithLU12I(*S);
    assert(S->size() <= 11);

    if (S->size() < ShortestLength) {
      ShortestSeq = S;
      ShortestLength = S->size();
    }
  }

  Insts.clear();
  Insts.append(ShortestSeq->begin(), ShortestSeq->end());
}

const LoongArchAnalyzeImmediate::InstSeq
&LoongArchAnalyzeImmediate::Analyze(uint64_t Imm, unsigned Size,
                                    bool LastInstrIsADDI) {
  this->Size = Size;
  //TODO:loongarch32
  ADDI = LoongArch::ADDI_D;
  ORI = LoongArch::ORI;
  SLLI = LoongArch::SLLI_D;
  LU12I = LoongArch::LU12I_W;

  InstSeqLs SeqLs;

  // Get the list of instruction sequences.
  if (LastInstrIsADDI | !Imm)
    GetInstSeqLsADDI(Imm, Size, SeqLs);
  else
    GetInstSeqLs(Imm, Size, SeqLs);

  // Set Insts to the shortest instruction sequence.
  GetShortestSeq(SeqLs, Insts);

  return Insts;
}
