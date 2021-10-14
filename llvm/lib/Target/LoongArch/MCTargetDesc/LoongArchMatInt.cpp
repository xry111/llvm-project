//===- LoongArchMatInt.cpp - Immediate materialisation ---------*- C++ -*--===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "LoongArchMatInt.h"
#include "MCTargetDesc/LoongArchMCTargetDesc.h"
#include "llvm/ADT/APInt.h"
#include "llvm/Support/MathExtras.h"

namespace llvm {

namespace LoongArchMatInt {
void generateInstSeq(int64_t Val, bool IsLA64, InstSeq &Res) {
  if (isInt<12>(Val)) {
    Res.push_back(Inst(LoongArch::ADDI_W, Val));
    return;
  }

  if (isUInt<12>(Val)) {
    Res.push_back(Inst(LoongArch::ORI, Val));
    return;
  }

  if (isInt<32>(Val)) {
    int64_t Hi20 = SignExtend64<20>(Val >> 12);
    Res.push_back(Inst(LoongArch::LU12I_W, Hi20));

    int64_t Lo12 = Val & 0xFFF;
    if (Lo12)
      Res.push_back(Inst(LoongArch::ORI, Lo12));
    return;
  }

  assert(IsLA64 && "Can't emit >32-bit imm for non-LA64 target");

  if (isInt<52>(Val)) {
    int64_t Hi20 = SignExtend64<20>(Val >> 32);
    int64_t Lo32 = SignExtend64<32>(Val);
    generateInstSeq(Lo32, IsLA64, Res);
    Res.push_back(Inst(LoongArch::LU32I_D, Hi20));
    return;
  }

  int64_t Hi12 = SignExtend64<12>(Val >> 52);
  int64_t Lo52 = SignExtend64<52>(Val);
  generateInstSeq(Lo52, IsLA64, Res);
  Res.push_back(Inst(LoongArch::LU52I_D, Hi12));
}

int getIntMatCost(const APInt &Val, unsigned Size, bool IsLA64) {
  int PlatRegSize = IsLA64 ? 64 : 32;

  // Split the constant into platform register sized chunks, and calculate cost
  // of each chunk.
  int Cost = 0;
  for (unsigned ShiftVal = 0; ShiftVal < Size; ShiftVal += PlatRegSize) {
    APInt Chunk = Val.ashr(ShiftVal).sextOrTrunc(PlatRegSize);
    InstSeq MatSeq;
    generateInstSeq(Chunk.getSExtValue(), IsLA64, MatSeq);
    Cost += MatSeq.size();
  }
  return std::max(1, Cost);
}
} // namespace LoongArchMatInt
} // namespace llvm
