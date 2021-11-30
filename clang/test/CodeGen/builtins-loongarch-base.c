// REQUIRES: loongarch-registered-target
// RUN: %clang_cc1 -triple loongarch64-linux-gnu -emit-llvm %s -o - | FileCheck %s

#include <larchintrin.h>

typedef char i8;
typedef unsigned char u8;
typedef short i16;
typedef unsigned short u16;
typedef int i32;
typedef unsigned int u32;

#if __LONG_MAX__ == __LONG_LONG_MAX__
typedef long int i64;
typedef unsigned long int u64;
#else
typedef long long i64;
typedef unsigned long long u64;
#endif

__drdtime_t drdtime;
__rdtime_t rdtime;

void cpucfg(){

  u32 u32_r, u32_a;
  // __cpucfg
  // rd, rj
  // unsigned int, unsigned int
  u32_r= __builtin_loongarch_cpucfg(u32_a); // CHECK: call i32 @llvm.loongarch.cpucfg

}

void csrrd(){

  u32 u32_r;
  // __csrrd
  // rd, csr_num
  // unsigned int, uimm14_32
  u32_r=__builtin_loongarch_csrrd(1); // CHECK: call i32 @llvm.loongarch.csrrd

}

void dcsrrd(){

  u64 u64_r;
  // __dcsrrd
  // rd, csr_num
  // unsigned long int, uimm14
  u64_r=__builtin_loongarch_dcsrrd(1); // CHECK: call i64 @llvm.loongarch.dcsrrd

}

void csrwr(){

  u32 u32_r, u32_a;
  // __csrwr
  // rd, csr_num
  // unsigned int, uimm14_32
  u32_r=__builtin_loongarch_csrwr(u32_a, 1); // CHECK: call i32 @llvm.loongarch.csrwr

}

void dcsrwr(){

  u64 u64_r, u64_a;
  // __dcsrwr
  // rd, csr_num
  // unsigned long int, uimm14
  u64_r=__builtin_loongarch_dcsrwr(u64_a, 1); // CHECK: call i64 @llvm.loongarch.dcsrwr

}

void csrxchg(){

  u32 u32_r, u32_a, u32_b;
  // __csrxchg
  // rd, rj, csr_num
  // unsigned int, unsigned int, uimm14_32
  u32_r=__builtin_loongarch_csrxchg(u32_a, u32_b, 1); // CHECK: call i32 @llvm.loongarch.csrxchg

}

void dcsrxchg(){

  u64 u64_r, u64_a, u64_b;
  // __dcsrxchg
  // rd, rj, csr_num
  // unsigned long int, unsigned long int, uimm14
  u64_r=__builtin_loongarch_dcsrxchg(u64_a, u64_b, 1); // CHECK: call i64 @llvm.loongarch.dcsrxchg

}

void iocsrrd_b(){

  u32 u32_a;
  u8 u8_r;
  // __iocsrrd_b
  // rd, rj
  // unsigned char, unsigned int
  u8_r=__builtin_loongarch_iocsrrd_b(u32_a); // CHECK: call i32 @llvm.loongarch.iocsrrd.b

}

void iocsrrd_h(){

  u32 u32_a;
  u16 u16_r;
  // __iocsrrd_h
  // rd, rj
  // unsigned short, unsigned int
  u16_r=__builtin_loongarch_iocsrrd_h(u32_a); // CHECK: call i32 @llvm.loongarch.iocsrrd.h

}

void iocsrrd_w(){

  u32 u32_r, u32_a;
  // __iocsrrd_w
  // rd, rj
  // unsigned int, unsigned int
  u32_r=__builtin_loongarch_iocsrrd_w(u32_a); // CHECK: call i32 @llvm.loongarch.iocsrrd.w

}

void iocsrrd_d(){

  u32 u32_a;
  u64 u64_r;
  // __iocsrrd_d
  // rd, rj
  // unsigned long int, unsigned int
  u64_r=__builtin_loongarch_iocsrrd_d(u32_a); // CHECK: call i64 @llvm.loongarch.iocsrrd.d

}

void iocsrwr_b(){

  u32 u32_a;
  u8 u8_a;
  // __iocsrwr_b
  // rd, rj
  // unsigned char, unsigned int
  __builtin_loongarch_iocsrwr_b(u8_a, u32_a); // CHECK: void @llvm.loongarch.iocsrwr.b

}

void iocsrwr_h(){

  u32 u32_a;
  u16 u16_a;
  // __iocsrwr_h
  // rd, rj
  // unsigned short, unsigned int
  __builtin_loongarch_iocsrwr_h(u16_a, u32_a); // CHECK: void @llvm.loongarch.iocsrwr.h

}

void iocsrwr_w(){

  u32 u32_a, u32_b;
  // __iocsrwr_w
  // rd, rj
  // unsigned int, unsigned int
  __builtin_loongarch_iocsrwr_w(u32_a, u32_b); // CHECK: void @llvm.loongarch.iocsrwr.w

}

void iocsrwr_d(){

  u32 u32_a;
  u64 u64_a;
  // __iocsrwr_d
  // rd, rj
  // unsigned long int, unsigned int
  __builtin_loongarch_iocsrwr_d(u64_a, u32_a); // CHECK: void @llvm.loongarch.iocsrwr.d

}

void cacop(){

  i32 i32_a;
  // __cacop
  // op, rj, si12
  // uimm5, unsigned int, simm12
  __builtin_loongarch_cacop(1, i32_a, 2); // CHECK: void @llvm.loongarch.cacop

}

void dcacop(){

  i64 i64_a;
  // __dcacop
  // op, rj, si12
  // uimm5, unsigned long int, simm12
  __builtin_loongarch_dcacop(1, i64_a, 2); // CHECK: void @llvm.loongarch.dcacop

}

void rdtime_d(){

  drdtime= __builtin_loongarch_rdtime_d(); // CHECK: call { i64, i64 } asm sideeffect "rdtime.d\09$0,$1\0A\09", "=&r,=&r"()

}

void rdtimeh_w(){

  rdtime= __builtin_loongarch_rdtimeh_w(); // CHECK: call { i32, i32 } asm sideeffect "rdtimeh.w\09$0,$1\0A\09", "=&r,=&r"()

}

void rdtimel_w(){

  rdtime= __builtin_loongarch_rdtimel_w(); // CHECK: call { i32, i32 } asm sideeffect "rdtimel.w\09$0,$1\0A\09", "=&r,=&r"()

}

void crc_w_b_w(){

  i32 i32_r, i32_a;
  i8 i8_a;
  // __crc_w_b_w
  // rd, rj, rk
  // int, char, int
  i32_r=__builtin_loongarch_crc_w_b_w(i8_a, i32_a); // CHECK: call i32 @llvm.loongarch.crc.w.b.w

}

void crc_w_h_w(){

  i32 i32_r, i32_a;
  i16 i16_a;
  // __crc_w_h_w
  // rd, rj, rk
  // int, short, int
  i32_r=__builtin_loongarch_crc_w_h_w(i16_a, i32_a); // CHECK: call i32 @llvm.loongarch.crc.w.h.w

}

void crc_w_w_w(){

  i32 i32_r, i32_a, i32_b;
  // __crc_w_w_w
  // rd, rj, rk
  // int, int, int
  i32_r=__builtin_loongarch_crc_w_w_w(i32_a, i32_b); // CHECK: call i32 @llvm.loongarch.crc.w.w.w

}

void crc_w_d_w(){

  i32 i32_r, i32_a;
  i64 i64_a;
  // __crc_w_d_w
  // rd, rj, rk
  // int, long int, int
  i32_r=__builtin_loongarch_crc_w_d_w(i64_a, i32_a); // CHECK: call i32 @llvm.loongarch.crc.w.d.w

}

void crcc_w_b_w(){

  i32 i32_r, i32_a;
  i8 i8_a;
  // __crcc_w_b_w
  // rd, rj, rk
  // int, char, int
  i32_r=__builtin_loongarch_crcc_w_b_w(i8_a, i32_a); // CHECK: call i32 @llvm.loongarch.crcc.w.b.w

}

void crcc_w_h_w(){

  i32 i32_r, i32_a;
  i16 i16_a;
  // __crcc_w_h_w
  // rd, rj, rk
  // int, short, int
  i32_r=__builtin_loongarch_crcc_w_h_w(i16_a, i32_a); // CHECK: call i32 @llvm.loongarch.crcc.w.h.w

}

void crcc_w_w_w(){

  i32 i32_r, i32_a, i32_b;
  // __crcc_w_w_w
  // rd, rj, rk
  // int, int, int
  i32_r=__builtin_loongarch_crcc_w_w_w(i32_a, i32_b); // CHECK: call i32 @llvm.loongarch.crcc.w.w.w

}

void crcc_w_d_w(){

  i32 i32_r, i32_a;
  i64 i64_a;
  // __crcc_w_d_w
  // rd, rj, rk
  // int, long int, int
  i32_r=__builtin_loongarch_crcc_w_d_w(i64_a, i32_a); // CHECK: call i32 @llvm.loongarch.crcc.w.d.w

}

void tlbclr(){

  // __tlbclr
  __builtin_loongarch_tlbclr(); // CHECK: call void @llvm.loongarch.tlbclr

}

void tlbflush(){

  // __tlbflush
  __builtin_loongarch_tlbflush(); // CHECK: call void @llvm.loongarch.tlbflush

}

void tlbfill(){

  // __tlbfill
  __builtin_loongarch_tlbfill(); // CHECK: call void @llvm.loongarch.tlbfill 

}

void tlbrd(){

  // __tlbrd
  __builtin_loongarch_tlbrd(); // CHECK: call void @llvm.loongarch.tlbrd

}

void tlbwr(){

  // __tlbwr
  __builtin_loongarch_tlbwr(); // CHECK: call void @llvm.loongarch.tlbwr

}

void tlbsrch(){

  // __tlbsrch
  __builtin_loongarch_tlbsrch(); // CHECK: call void @llvm.loongarch.tlbsrch

}

void syscall(){

  // __syscall
  // Code
  // uimm15
  __builtin_loongarch_syscall(1); // CHECK: call void @llvm.loongarch.syscall

}

void break_builtin(){

  // __break
  // Code
  // uimm15
  __builtin_loongarch_break(1); // CHECK: call void @llvm.loongarch.break

}

void asrtle_d(){

  i64 i64_a, i64_b;
  // __asrtle_d
  // rj, rk
  // long int, long int
  __builtin_loongarch_asrtle_d(i64_a, i64_b); // CHECK: call void @llvm.loongarch.asrtle.d

}

void asrtgt_d(){

  i64 i64_a, i64_b;
  // __asrtgt_d
  // rj, rk
  // long int, long int
  __builtin_loongarch_asrtgt_d(i64_a, i64_b); // CHECK: call void @llvm.loongarch.asrtgt.d

}

void dbar(){

  // __dbar
  // hint
  // uimm15
  __builtin_loongarch_dbar(0); // CHECK: call void @llvm.loongarch.dbar

}

void ibar(){

  // __ibar
  // hint
  // uimm15
  __builtin_loongarch_ibar(0); // CHECK: call void @llvm.loongarch.ibar

}

void movfcsr2gr(){

  u32 u32_r;
  // __movfcsr2gr
  u32_r=__movfcsr2gr(0); // CHECK: call i32 asm sideeffect "movfcsr2gr $0, $$fcsr0", "=&r"()

}


void movgr2fcsr() {

  u32 u32_a;
  // __movgr2fcsr
  __movgr2fcsr(0, u32_a); // CHECK: call void asm sideeffect "movgr2fcsr $$fcsr0, $0", "r"(i32 %0)

}
