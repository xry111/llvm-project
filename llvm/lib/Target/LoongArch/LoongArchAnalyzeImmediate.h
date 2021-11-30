//===- LoongArchAnalyzeImmediate.h - Analyze Immediates -------------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_LOONGARCH_LOONGARCHANALYZEIMMEDIATE_H
#define LLVM_LIB_TARGET_LOONGARCH_LOONGARCHANALYZEIMMEDIATE_H

#include "llvm/ADT/SmallVector.h"
#include <cstdint>

namespace llvm {

  class LoongArchAnalyzeImmediate {
  public:
    struct Inst {
      unsigned Opc, ImmOpnd;

      Inst(unsigned Opc, unsigned ImmOpnd);
    };
    using InstSeq = SmallVector<Inst, 11>;

    /// Analyze - Get an instruction sequence to load immediate Imm. The last
    /// instruction in the sequence must be an ADDI_{W/D} if LastInstrIsADDI is
    /// true;
    const InstSeq &Analyze(uint64_t Imm, unsigned Size, bool LastInstrIsADDI);

  private:
    using InstSeqLs = SmallVector<InstSeq, 9>;

    /// AddInstr - Add I to all instruction sequences in SeqLs.
    void AddInstr(InstSeqLs &SeqLs, const Inst &I);

    /// GetInstSeqLsADDI - Get instruction sequences which end with an ADDI to
    /// load immediate Imm
    void GetInstSeqLsADDI(uint64_t Imm, unsigned RemSize, InstSeqLs &SeqLs);

    /// GetInstSeqLsORI - Get instrutcion sequences which end with an ORI to
    /// load immediate Imm
    void GetInstSeqLsORI(uint64_t Imm, unsigned RemSize, InstSeqLs &SeqLs);

    /// GetInstSeqLsSLLI - Get instruction sequences which end with a SLLI to
    /// load immediate Imm
    void GetInstSeqLsSLLI(uint64_t Imm, unsigned RemSize, InstSeqLs &SeqLs);

    /// GetInstSeqLs - Get instruction sequences to load immediate Imm.
    void GetInstSeqLs(uint64_t Imm, unsigned RemSize, InstSeqLs &SeqLs);

    /// ReplaceADDISLLIWithLU12I - Replace an ADDI & SLLI pair with a LU12I.
    void ReplaceADDISLLIWithLU12I(InstSeq &Seq);

    /// GetShortestSeq - Find the shortest instruction sequence in SeqLs and
    /// return it in Insts.
    void GetShortestSeq(InstSeqLs &SeqLs, InstSeq &Insts);

    unsigned Size;
    unsigned ADDI, ORI, SLLI, LU12I;
    InstSeq Insts;
  };

} // end namespace llvm

#endif // LLVM_LIB_TARGET_LOONGARCH_LOONGARCHANALYZEIMMEDIATE_H
