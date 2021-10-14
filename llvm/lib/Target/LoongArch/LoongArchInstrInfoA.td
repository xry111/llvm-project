//===-- LoongArchInstrInfoA.td - RISC-V 'A' instructions -------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file describes the RISC-V instructions from the standard 'A', Atomic
// Instructions extension.
//
//===----------------------------------------------------------------------===//

/// Atomic loads and stores

// Fences will be inserted for atomic load/stores according to the logic in
// LoongArchTargetLowering::{emitLeadingFence,emitTrailingFence}.

defm : LALdPat12<atomic_load_8,  LD_B>;
defm : LALdPat12<atomic_load_16, LD_H>;
defm : LALdPat12<atomic_load_32, LD_W>;

multiclass AtomicStPat<PatFrag StoreOp, LAInst Inst, RegisterClass StTy> {
  def : Pat<(StoreOp GPR:$rs1, StTy:$rs2), (Inst StTy:$rs2, GPR:$rs1, 0)>;
  def : Pat<(StoreOp AddrFI:$rs1, StTy:$rs2), (Inst StTy:$rs2, AddrFI:$rs1, 0)>;
  def : Pat<(StoreOp (add GPR:$rs1, simm12:$imm12), StTy:$rs2),
            (Inst StTy:$rs2, GPR:$rs1, simm12:$imm12)>;
  def : Pat<(StoreOp (add AddrFI:$rs1, simm12:$imm12), StTy:$rs2),
            (Inst StTy:$rs2, AddrFI:$rs1, simm12:$imm12)>;
  def : Pat<(StoreOp (IsOrAdd AddrFI:$rs1, simm12:$imm12), StTy:$rs2),
            (Inst StTy:$rs2, AddrFI:$rs1, simm12:$imm12)>;
}

defm : AtomicStPat<atomic_store_8,  ST_B, GPR>;
defm : AtomicStPat<atomic_store_16, ST_H, GPR>;
defm : AtomicStPat<atomic_store_32, ST_W, GPR>;

/// Pseudo AMOs

class PseudoAMO : Pseudo<(outs GPR:$res, GPR:$scratch),
                         (ins GPR:$addr, GPR:$incr, ixlenimm:$ordering), []> {
  let Constraints = "@earlyclobber $res,@earlyclobber $scratch";
  let mayLoad = 1;
  let mayStore = 1;
  let hasSideEffects = 0;
}

def PseudoAtomicLoadNand32 : PseudoAMO;
// Ordering constants must be kept in sync with the AtomicOrdering enum in
// AtomicOrdering.h.
def : Pat<(atomic_load_nand_32_monotonic GPR:$addr, GPR:$incr),
          (PseudoAtomicLoadNand32 GPR:$addr, GPR:$incr, 2)>;
def : Pat<(atomic_load_nand_32_acquire GPR:$addr, GPR:$incr),
          (PseudoAtomicLoadNand32 GPR:$addr, GPR:$incr, 4)>;
def : Pat<(atomic_load_nand_32_release GPR:$addr, GPR:$incr),
          (PseudoAtomicLoadNand32 GPR:$addr, GPR:$incr, 5)>;
def : Pat<(atomic_load_nand_32_acq_rel GPR:$addr, GPR:$incr),
          (PseudoAtomicLoadNand32 GPR:$addr, GPR:$incr, 6)>;
def : Pat<(atomic_load_nand_32_seq_cst GPR:$addr, GPR:$incr),
          (PseudoAtomicLoadNand32 GPR:$addr, GPR:$incr, 7)>;

class PseudoMaskedAMO
    : Pseudo<(outs GPR:$res, GPR:$scratch),
             (ins GPR:$addr, GPR:$incr, GPR:$mask, ixlenimm:$ordering), []> {
  let Constraints = "@earlyclobber $res,@earlyclobber $scratch";
  let mayLoad = 1;
  let mayStore = 1;
  let hasSideEffects = 0;
}

class PseudoMaskedAMOMinMax
    : Pseudo<(outs GPR:$res, GPR:$scratch1, GPR:$scratch2),
             (ins GPR:$addr, GPR:$incr, GPR:$mask, ixlenimm:$sextshamt,
              ixlenimm:$ordering), []> {
  let Constraints = "@earlyclobber $res,@earlyclobber $scratch1,"
                    "@earlyclobber $scratch2";
  let mayLoad = 1;
  let mayStore = 1;
  let hasSideEffects = 0;
}

class PseudoMaskedAMOUMinUMax
    : Pseudo<(outs GPR:$res, GPR:$scratch1, GPR:$scratch2),
             (ins GPR:$addr, GPR:$incr, GPR:$mask, ixlenimm:$ordering), []> {
  let Constraints = "@earlyclobber $res,@earlyclobber $scratch1,"
                    "@earlyclobber $scratch2";
  let mayLoad = 1;
  let mayStore = 1;
  let hasSideEffects = 0;
}

class PseudoMaskedAMOPat<Intrinsic intrin, Pseudo AMOInst>
    : Pat<(intrin GPR:$addr, GPR:$incr, GPR:$mask, timm:$ordering),
          (AMOInst GPR:$addr, GPR:$incr, GPR:$mask, timm:$ordering)>;

class PseudoMaskedAMOMinMaxPat<Intrinsic intrin, Pseudo AMOInst>
    : Pat<(intrin GPR:$addr, GPR:$incr, GPR:$mask, GPR:$shiftamt,
           timm:$ordering),
          (AMOInst GPR:$addr, GPR:$incr, GPR:$mask, GPR:$shiftamt,
           timm:$ordering)>;

def PseudoMaskedAtomicSwap32 : PseudoMaskedAMO;
def : PseudoMaskedAMOPat<int_loongarch_masked_atomicrmw_xchg_i32,
                         PseudoMaskedAtomicSwap32>;
def PseudoMaskedAtomicLoadAdd32 : PseudoMaskedAMO;
def : PseudoMaskedAMOPat<int_loongarch_masked_atomicrmw_add_i32,
                         PseudoMaskedAtomicLoadAdd32>;
def PseudoMaskedAtomicLoadSub32 : PseudoMaskedAMO;
def : PseudoMaskedAMOPat<int_loongarch_masked_atomicrmw_sub_i32,
                         PseudoMaskedAtomicLoadSub32>;
def PseudoMaskedAtomicLoadNand32 : PseudoMaskedAMO;
def : PseudoMaskedAMOPat<int_loongarch_masked_atomicrmw_nand_i32,
                         PseudoMaskedAtomicLoadNand32>;
def PseudoMaskedAtomicLoadMax32 : PseudoMaskedAMOMinMax;
def : PseudoMaskedAMOMinMaxPat<int_loongarch_masked_atomicrmw_max_i32,
                               PseudoMaskedAtomicLoadMax32>;
def PseudoMaskedAtomicLoadMin32 : PseudoMaskedAMOMinMax;
def : PseudoMaskedAMOMinMaxPat<int_loongarch_masked_atomicrmw_min_i32,
                               PseudoMaskedAtomicLoadMin32>;
def PseudoMaskedAtomicLoadUMax32 : PseudoMaskedAMOUMinUMax;
def : PseudoMaskedAMOPat<int_loongarch_masked_atomicrmw_umax_i32,
                         PseudoMaskedAtomicLoadUMax32>;
def PseudoMaskedAtomicLoadUMin32 : PseudoMaskedAMOUMinUMax;
def : PseudoMaskedAMOPat<int_loongarch_masked_atomicrmw_umin_i32,
                         PseudoMaskedAtomicLoadUMin32>;

/// Compare and exchange

class PseudoCmpXchg
    : Pseudo<(outs GPR:$res, GPR:$scratch),
             (ins GPR:$addr, GPR:$cmpval, GPR:$newval, ixlenimm:$ordering), []> {
  let Constraints = "@earlyclobber $res,@earlyclobber $scratch";
  let mayLoad = 1;
  let mayStore = 1;
  let hasSideEffects = 0;
}

// Ordering constants must be kept in sync with the AtomicOrdering enum in
// AtomicOrdering.h.
multiclass PseudoCmpXchgPat<string Op, Pseudo CmpXchgInst> {
  def : Pat<(!cast<PatFrag>(Op#"_monotonic") GPR:$addr, GPR:$cmp, GPR:$new),
            (CmpXchgInst GPR:$addr, GPR:$cmp, GPR:$new, 2)>;
  def : Pat<(!cast<PatFrag>(Op#"_acquire") GPR:$addr, GPR:$cmp, GPR:$new),
            (CmpXchgInst GPR:$addr, GPR:$cmp, GPR:$new, 4)>;
  def : Pat<(!cast<PatFrag>(Op#"_release") GPR:$addr, GPR:$cmp, GPR:$new),
            (CmpXchgInst GPR:$addr, GPR:$cmp, GPR:$new, 5)>;
  def : Pat<(!cast<PatFrag>(Op#"_acq_rel") GPR:$addr, GPR:$cmp, GPR:$new),
            (CmpXchgInst GPR:$addr, GPR:$cmp, GPR:$new, 6)>;
  def : Pat<(!cast<PatFrag>(Op#"_seq_cst") GPR:$addr, GPR:$cmp, GPR:$new),
            (CmpXchgInst GPR:$addr, GPR:$cmp, GPR:$new, 7)>;
}

def PseudoCmpXchg32 : PseudoCmpXchg;
defm : PseudoCmpXchgPat<"atomic_cmp_swap_32", PseudoCmpXchg32>;

def PseudoMaskedCmpXchg32
    : Pseudo<(outs GPR:$res, GPR:$scratch),
             (ins GPR:$addr, GPR:$cmpval, GPR:$newval, GPR:$mask,
              ixlenimm:$ordering), []> {
  let Constraints = "@earlyclobber $res,@earlyclobber $scratch";
  let mayLoad = 1;
  let mayStore = 1;
  let hasSideEffects = 0;
}

def : Pat<(int_loongarch_masked_cmpxchg_i32
            GPR:$addr, GPR:$cmpval, GPR:$newval, GPR:$mask, timm:$ordering),
          (PseudoMaskedCmpXchg32
            GPR:$addr, GPR:$cmpval, GPR:$newval, GPR:$mask, timm:$ordering)>;

let Predicates = [IsLA64] in {

/// 64-bit atomic loads and stores

// Fences will be inserted for atomic load/stores according to the logic in
// LoongArchTargetLowering::{emitLeadingFence,emitTrailingFence}.
defm : LdPat<atomic_load_64, LD_D>;
defm : AtomicStPat<atomic_store_64, ST_D, GPR>;

/// 64-bit pseudo AMOs

def PseudoAtomicLoadNand64 : PseudoAMO;
// Ordering constants must be kept in sync with the AtomicOrdering enum in
// AtomicOrdering.h.
def : Pat<(atomic_load_nand_64_monotonic GPR:$addr, GPR:$incr),
          (PseudoAtomicLoadNand64 GPR:$addr, GPR:$incr, 2)>;
def : Pat<(atomic_load_nand_64_acquire GPR:$addr, GPR:$incr),
          (PseudoAtomicLoadNand64 GPR:$addr, GPR:$incr, 4)>;
def : Pat<(atomic_load_nand_64_release GPR:$addr, GPR:$incr),
          (PseudoAtomicLoadNand64 GPR:$addr, GPR:$incr, 5)>;
def : Pat<(atomic_load_nand_64_acq_rel GPR:$addr, GPR:$incr),
          (PseudoAtomicLoadNand64 GPR:$addr, GPR:$incr, 6)>;
def : Pat<(atomic_load_nand_64_seq_cst GPR:$addr, GPR:$incr),
          (PseudoAtomicLoadNand64 GPR:$addr, GPR:$incr, 7)>;

def : PseudoMaskedAMOPat<int_loongarch_masked_atomicrmw_xchg_i64,
                         PseudoMaskedAtomicSwap32>;
def : PseudoMaskedAMOPat<int_loongarch_masked_atomicrmw_add_i64,
                         PseudoMaskedAtomicLoadAdd32>;
def : PseudoMaskedAMOPat<int_loongarch_masked_atomicrmw_sub_i64,
                         PseudoMaskedAtomicLoadSub32>;
def : PseudoMaskedAMOPat<int_loongarch_masked_atomicrmw_nand_i64,
                         PseudoMaskedAtomicLoadNand32>;
def : PseudoMaskedAMOMinMaxPat<int_loongarch_masked_atomicrmw_max_i64,
                               PseudoMaskedAtomicLoadMax32>;
def : PseudoMaskedAMOMinMaxPat<int_loongarch_masked_atomicrmw_min_i64,
                               PseudoMaskedAtomicLoadMin32>;
def : PseudoMaskedAMOPat<int_loongarch_masked_atomicrmw_umax_i64,
                         PseudoMaskedAtomicLoadUMax32>;
def : PseudoMaskedAMOPat<int_loongarch_masked_atomicrmw_umin_i64,
                         PseudoMaskedAtomicLoadUMin32>;

/// 64-bit compare and exchange

def PseudoCmpXchg64 : PseudoCmpXchg;
defm : PseudoCmpXchgPat<"atomic_cmp_swap_64", PseudoCmpXchg64>;

def : Pat<(int_loongarch_masked_cmpxchg_i64
            GPR:$addr, GPR:$cmpval, GPR:$newval, GPR:$mask, timm:$ordering),
          (PseudoMaskedCmpXchg32
            GPR:$addr, GPR:$cmpval, GPR:$newval, GPR:$mask, timm:$ordering)>;
} // Predicates = [IsLA64]

let hasSideEffects = 0, mayLoad = 1, mayStore = 1 in
class LA_AMO_rr<bits<7> funct7, string opcodestr>
    : LAInst3R<0b0011100001, funct7,
               (outs GPR:$rd), (ins GPR:$rj, GPR:$rk),
               opcodestr, "$rd, $rk, $rj"> {
  // Section 2.2.7.1 says: "if rd = rj, a hardware exception will be
  // raised; if rd = rk, the behavior will be undefined".
  let Constraints = "@earlyclobber $rd";
}

def AMSWAP_W    : LA_AMO_rr<0b1000000, "amswap.w">, Sched<[]>;
def AMSWAP_D    : LA_AMO_rr<0b1000001, "amswap.d">, Sched<[]>;
def AMADD_W     : LA_AMO_rr<0b1000010, "amadd.w">, Sched<[]>;
def AMADD_D     : LA_AMO_rr<0b1000011, "amadd.d">, Sched<[]>;
def AMAND_W     : LA_AMO_rr<0b1000100, "amand.w">, Sched<[]>;
def AMAND_D     : LA_AMO_rr<0b1000101, "amand.d">, Sched<[]>;
def AMOR_W      : LA_AMO_rr<0b1000110, "amor.w">, Sched<[]>;
def AMOR_D      : LA_AMO_rr<0b1000111, "amor.d">, Sched<[]>;
def AMXOR_W     : LA_AMO_rr<0b1001000, "amxor.w">, Sched<[]>;
def AMXOR_D     : LA_AMO_rr<0b1001001, "amxor.d">, Sched<[]>;
def AMMAX_W     : LA_AMO_rr<0b1001010, "ammax.w">, Sched<[]>;
def AMMAX_D     : LA_AMO_rr<0b1001011, "ammax.d">, Sched<[]>;
def AMMIN_W     : LA_AMO_rr<0b1001100, "ammin.w">, Sched<[]>;
def AMMIN_D     : LA_AMO_rr<0b1001101, "ammin.d">, Sched<[]>;
def AMMAX_WU    : LA_AMO_rr<0b1001110, "ammax.wu">, Sched<[]>;
def AMMAX_DU    : LA_AMO_rr<0b1001111, "ammax.du">, Sched<[]>;
def AMMIN_WU    : LA_AMO_rr<0b1010000, "ammin.wu">, Sched<[]>;
def AMMIN_DU    : LA_AMO_rr<0b1010001, "ammin.du">, Sched<[]>;
def AMSWAP_DB_W : LA_AMO_rr<0b1010010, "amswap_db.w">, Sched<[]>;
def AMSWAP_DB_D : LA_AMO_rr<0b1010011, "amswap_db.d">, Sched<[]>;
def AMADD_DB_W  : LA_AMO_rr<0b1010100, "amadd_db.w">, Sched<[]>;
def AMADD_DB_D  : LA_AMO_rr<0b1010101, "amadd_db.d">, Sched<[]>;
def AMAND_DB_W  : LA_AMO_rr<0b1010110, "amand_db.w">, Sched<[]>;
def AMAND_DB_D  : LA_AMO_rr<0b1010111, "amand_db.d">, Sched<[]>;
def AMOR_DB_W   : LA_AMO_rr<0b1011000, "amor_db.w">, Sched<[]>;
def AMOR_DB_D   : LA_AMO_rr<0b1011001, "amor_db.d">, Sched<[]>;
def AMXOR_DB_W  : LA_AMO_rr<0b1011010, "amxor_db.w">, Sched<[]>;
def AMXOR_DB_D  : LA_AMO_rr<0b1011011, "amxor_db.d">, Sched<[]>;
def AMMAX_DB_W  : LA_AMO_rr<0b1011100, "ammax_db.w">, Sched<[]>;
def AMMAX_DB_D  : LA_AMO_rr<0b1011101, "ammax_db.d">, Sched<[]>;
def AMMIN_DB_W  : LA_AMO_rr<0b1011110, "ammin_db.w">, Sched<[]>;
def AMMIN_DB_D  : LA_AMO_rr<0b1011111, "ammin_db.d">, Sched<[]>;
def AMMAX_DB_WU : LA_AMO_rr<0b1100000, "ammax_db.wu">, Sched<[]>;
def AMMAX_DB_DU : LA_AMO_rr<0b1100001, "ammax_db.du">, Sched<[]>;
def AMMIN_DB_WU : LA_AMO_rr<0b1100010, "ammin_db.wu">, Sched<[]>;
def AMMIN_DB_DU : LA_AMO_rr<0b1100011, "ammin_db.du">, Sched<[]>;

let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in
class LA_LL<bits<3> funct3, string opcodestr>
    : LAInst2RI14<funct3, (outs GPR:$rd), (ins GPR:$rj, simm14:$imm14),
                  opcodestr, "$rd, $rj, $imm14">;
let hasSideEffects = 0, mayLoad = 1, mayStore = 1,
    Constraints = "$rd = $rd_dst" in
class LA_SC<bits<3> funct3, string opcodestr>
    : LAInst2RI14<funct3, (outs GPR:$rd_dst),
                  (ins GPR:$rd, GPR:$rj, simm14:$imm14),
                  opcodestr, "$rd, $rj, $imm14">;

def LL_W : LA_LL<0b000, "ll.w">, Sched<[]>;
def SC_W : LA_SC<0b001, "sc.w">, Sched<[]>;

let Predicates = [IsLA64] in {
def LL_D : LA_LL<0b010, "ll.d">, Sched<[]>;
def SC_D : LA_SC<0b011, "sc.d">, Sched<[]>;
}

multiclass LA_AMOPat<string AtomicOp, string BaseInst, string Suffix> {
  def : LAPatGprGpr<!cast<PatFrag>(AtomicOp#"_monotonic"),
                    !cast<LAInst>(BaseInst#Suffix)>;
  def : LAPatGprGpr<!cast<PatFrag>(AtomicOp#"_acquire"),
                    !cast<LAInst>(BaseInst#"_DB"#Suffix)>;
  def : LAPatGprGpr<!cast<PatFrag>(AtomicOp#"_release"),
                    !cast<LAInst>(BaseInst#"_DB"#Suffix)>;
  def : LAPatGprGpr<!cast<PatFrag>(AtomicOp#"_acq_rel"),
                    !cast<LAInst>(BaseInst#"_DB"#Suffix)>;
  def : LAPatGprGpr<!cast<PatFrag>(AtomicOp#"_seq_cst"),
                    !cast<LAInst>(BaseInst#"_DB"#Suffix)>;
}

let Predicates = [IsLA64] in {
defm : LA_AMOPat<"atomic_swap_32",      "AMSWAP", "_W">;
defm : LA_AMOPat<"atomic_load_add_32",  "AMADD",  "_W">;
defm : LA_AMOPat<"atomic_load_and_32",  "AMAND",  "_W">;
defm : LA_AMOPat<"atomic_load_or_32",   "AMOR",   "_W">;
defm : LA_AMOPat<"atomic_load_xor_32",  "AMXOR",  "_W">;
defm : LA_AMOPat<"atomic_load_max_32",  "AMMAX",  "_W">;
defm : LA_AMOPat<"atomic_load_min_32",  "AMMIN",  "_W">;
defm : LA_AMOPat<"atomic_load_umax_32", "AMMAX",  "_WU">;
defm : LA_AMOPat<"atomic_load_umin_32", "AMMIN",  "_WU">;
defm : LA_AMOPat<"atomic_swap_64",      "AMSWAP", "_D">;
defm : LA_AMOPat<"atomic_load_add_64",  "AMADD",  "_D">;
defm : LA_AMOPat<"atomic_load_and_64",  "AMAND",  "_D">;
defm : LA_AMOPat<"atomic_load_or_64",   "AMOR",   "_D">;
defm : LA_AMOPat<"atomic_load_xor_64",  "AMXOR",  "_D">;
defm : LA_AMOPat<"atomic_load_max_64",  "AMMAX",  "_D">;
defm : LA_AMOPat<"atomic_load_min_64",  "AMMIN",  "_D">;
defm : LA_AMOPat<"atomic_load_umax_64", "AMMAX",  "_DU">;
defm : LA_AMOPat<"atomic_load_umin_64", "AMMIN",  "_DU">;

def : Pat<(atomic_load_sub_32_monotonic GPR:$addr, GPR:$incr),
          (AMADD_W GPR:$addr, (SUB_W R0, GPR:$incr))>;
def : Pat<(atomic_load_sub_32_acquire GPR:$addr, GPR:$incr),
          (AMADD_DB_W GPR:$addr, (SUB_W R0, GPR:$incr))>;
def : Pat<(atomic_load_sub_32_release GPR:$addr, GPR:$incr),
          (AMADD_DB_W GPR:$addr, (SUB_W R0, GPR:$incr))>;
def : Pat<(atomic_load_sub_32_acq_rel GPR:$addr, GPR:$incr),
          (AMADD_DB_W GPR:$addr, (SUB_W R0, GPR:$incr))>;
def : Pat<(atomic_load_sub_32_seq_cst GPR:$addr, GPR:$incr),
          (AMADD_DB_W GPR:$addr, (SUB_W R0, GPR:$incr))>;

def : Pat<(atomic_load_sub_64_monotonic GPR:$addr, GPR:$incr),
          (AMADD_D GPR:$addr, (SUB_D R0, GPR:$incr))>;
def : Pat<(atomic_load_sub_64_acquire GPR:$addr, GPR:$incr),
          (AMADD_DB_D GPR:$addr, (SUB_D R0, GPR:$incr))>;
def : Pat<(atomic_load_sub_64_release GPR:$addr, GPR:$incr),
          (AMADD_DB_D GPR:$addr, (SUB_D R0, GPR:$incr))>;
def : Pat<(atomic_load_sub_64_acq_rel GPR:$addr, GPR:$incr),
          (AMADD_DB_D GPR:$addr, (SUB_D R0, GPR:$incr))>;
def : Pat<(atomic_load_sub_64_seq_cst GPR:$addr, GPR:$incr),
          (AMADD_DB_D GPR:$addr, (SUB_D R0, GPR:$incr))>;
}
