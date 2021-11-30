; Test the base intrinsics.
; RUN: llc -march=loongarch64 -o - %s | FileCheck %s

define void @cpucfg() {
entry:
  %u32_r = alloca i32, align 4
  %u32_a = alloca i32, align 4
  %0 = load i32, i32* %u32_a, align 4
  %1 = call i32 @llvm.loongarch.cpucfg(i32 %0)
  store i32 %1, i32* %u32_r, align 4
  ret void
}

declare i32 @llvm.loongarch.cpucfg(i32)

; CHECK-LABEL: cpucfg:
; CHECK: ld.w	$r[[REG:[0-9]+]], $sp, 8
; CHECK: cpucfg	$r[[REG:[0-9]+]], $r[[REG:[0-9]+]]
; CHECK: st.w	$r[[REG:[0-9]+]], $sp, 12
; CHECK: jr	$ra
;

define void @csrrd() {
entry:
  %u32_r = alloca i32, align 4
  %0 = call i32 @llvm.loongarch.csrrd(i32 1)
  store i32 %0, i32* %u32_r, align 4
  ret void
}

declare i32 @llvm.loongarch.csrrd(i32)

; CHECK-LABEL: csrrd:
; CHECK: csrrd	$r[[REG:[0-9]+]], 1
; CHECK: st.w	$r[[REG:[0-9]+]], $sp, 12
; CHECK: jr	$ra
;

define void @dcsrrd() {
entry:
  %u64_r = alloca i64, align 8
  %0 = call i64 @llvm.loongarch.dcsrrd(i64 1)
  store i64 %0, i64* %u64_r, align 8
  ret void
}

declare i64 @llvm.loongarch.dcsrrd(i64)

; CHECK-LABEL: dcsrrd:
; CHECK: csrrd	$r[[REG:[0-9]+]], 1
; CHECK: st.d	$r[[REG:[0-9]+]], $sp, 8
; CHECK: jr	$ra
;

define void @csrwr() {
entry:
  %u32_r = alloca i32, align 4
  %u32_a = alloca i32, align 4
  %0 = load i32, i32* %u32_a, align 4
  %1 = call i32 @llvm.loongarch.csrwr(i32 %0, i32 1)
  store i32 %1, i32* %u32_r, align 4
  ret void
}

declare i32 @llvm.loongarch.csrwr(i32, i32)

; CHECK-LABEL: csrwr:
; CHECK: ld.w	$r[[REG:[0-9]+]], $sp, 8
; CHECK: csrwr	$r[[REG:[0-9]+]], 1
; CHECK: st.w	$r[[REG:[0-9]+]], $sp, 12
; CHECK: jr	$ra
;

define void @dcsrwr() {
entry:
  %u64_r = alloca i64, align 8
  %u64_a = alloca i64, align 8
  %0 = load i64, i64* %u64_a, align 8
  %1 = call i64 @llvm.loongarch.dcsrwr(i64 %0, i64 1)
  store i64 %1, i64* %u64_r, align 8
  ret void
}

declare i64 @llvm.loongarch.dcsrwr(i64, i64)

; CHECK-LABEL: dcsrwr:
; CHECK: ld.d	$r[[REG:[0-9]+]], $sp, 0
; CHECK: csrwr	$r[[REG:[0-9]+]], 1
; CHECK: st.d	$r[[REG:[0-9]+]], $sp, 8
; CHECK: jr	$ra
;

define void @csrxchg() {
entry:
  %u32_r = alloca i32, align 4
  %u32_a = alloca i32, align 4
  %u32_b = alloca i32, align 4
  %0 = load i32, i32* %u32_a, align 4
  %1 = load i32, i32* %u32_b, align 4
  %2 = call i32 @llvm.loongarch.csrxchg(i32 %0, i32 %1, i32 1)
  store i32 %2, i32* %u32_r, align 4
  ret void
}

declare i32 @llvm.loongarch.csrxchg(i32, i32, i32)

; CHECK-LABEL: csrxchg:
; CHECK: ld.w	$r[[REG1:[0-9]+]], $sp, 4
; CHECK: ld.w	$r[[REG2:[0-9]+]], $sp, 8
; CHECK: csrxchg	$r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]], 1
; CHECK: st.w	$r[[REG1:[0-9]+]], $sp, 12
; CHECK: jr	$ra
;

define void @dcsrxchg() {
entry:
  %u64_r = alloca i64, align 8
  %u64_a = alloca i64, align 8
  %u64_b = alloca i64, align 8
  %0 = load i64, i64* %u64_a, align 8
  %1 = load i64, i64* %u64_b, align 8
  %2 = call i64 @llvm.loongarch.dcsrxchg(i64 %0, i64 %1, i64 1)
  store i64 %2, i64* %u64_r, align 8
  ret void
}

declare i64 @llvm.loongarch.dcsrxchg(i64, i64, i64)

; CHECK-LABEL: dcsrxchg:
; CHECK: ld.d	$r[[REG1:[0-9]+]], $sp, 8
; CHECK: ld.d	$r[[REG2:[0-9]+]], $sp, 16
; CHECK: csrxchg	$r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]], 1
; CHECK: st.d	$r[[REG1:[0-9]+]], $sp, 24
; CHECK: jr	$ra
;

define void @iocsrrd_b() {
entry:
  %u32_a = alloca i32, align 4
  %u8_r = alloca i8, align 1
  %0 = load i32, i32* %u32_a, align 4
  %1 = call i32 @llvm.loongarch.iocsrrd.b(i32 %0)
  %conv = trunc i32 %1 to i8
  store i8 %conv, i8* %u8_r, align 1
  ret void
}

declare i32 @llvm.loongarch.iocsrrd.b(i32)

; CHECK-LABEL: iocsrrd_b:
; CHECK: ld.w	$r[[REG:[0-9]+]], $sp, 12
; CHECK: iocsrrd.b	$r[[REG:[0-9]+]], $r[[REG:[0-9]+]]
; CHECK: st.b	$r[[REG:[0-9]+]], $sp, 8
; CHECK: jr	$ra
;

define void @iocsrrd_h() {
entry:
  %u32_a = alloca i32, align 4
  %u16_r = alloca i16, align 2
  %0 = load i32, i32* %u32_a, align 4
  %1 = call i32 @llvm.loongarch.iocsrrd.h(i32 %0)
  %conv = trunc i32 %1 to i16
  store i16 %conv, i16* %u16_r, align 2
  ret void
}

declare i32 @llvm.loongarch.iocsrrd.h(i32)

; CHECK-LABEL: iocsrrd_h:
; CHECK: ld.w	$r[[REG:[0-9]+]], $sp, 12
; CHECK: iocsrrd.h	$r[[REG:[0-9]+]], $r[[REG:[0-9]+]]
; CHECK: st.h	$r[[REG:[0-9]+]], $sp, 8
; CHECK: jr	$ra
;

define void @iocsrrd_w() {
entry:
  %u32_r = alloca i32, align 4
  %u32_a = alloca i32, align 4
  %0 = load i32, i32* %u32_a, align 4
  %1 = call i32 @llvm.loongarch.iocsrrd.w(i32 %0)
  store i32 %1, i32* %u32_r, align 4
  ret void
}

declare i32 @llvm.loongarch.iocsrrd.w(i32)

; CHECK-LABEL: iocsrrd_w:
; CHECK: ld.w	$r[[REG:[0-9]+]], $sp, 8
; CHECK: iocsrrd.w	$r[[REG:[0-9]+]], $r[[REG:[0-9]+]]
; CHECK: st.w	$r[[REG:[0-9]+]], $sp, 12
; CHECK: jr	$ra
;

define void @iocsrrd_d() {
entry:
  %u32_a = alloca i32, align 4
  %u64_r = alloca i64, align 8
  %0 = load i32, i32* %u32_a, align 4
  %1 = call i64 @llvm.loongarch.iocsrrd.d(i32 %0)
  store i64 %1, i64* %u64_r, align 8
  ret void
}

declare i64 @llvm.loongarch.iocsrrd.d(i32)

; CHECK-LABEL: iocsrrd_d:
; CHECK: ld.w	$r[[REG:[0-9]+]], $sp, 12
; CHECK: iocsrrd.d	$r[[REG:[0-9]+]], $r[[REG:[0-9]+]]
; CHECK: st.d	$r[[REG:[0-9]+]], $sp, 0
; CHECK: jr	$ra
;

define void @iocsrwr_b() {
entry:
  %u32_a = alloca i32, align 4
  %u8_a = alloca i8, align 1
  %0 = load i8, i8* %u8_a, align 1
  %conv = zext i8 %0 to i32
  %1 = load i32, i32* %u32_a, align 4
  call void @llvm.loongarch.iocsrwr.b(i32 %conv, i32 %1)
  ret void
}

declare void @llvm.loongarch.iocsrwr.b(i32, i32)

; CHECK-LABEL: iocsrwr_b:
; CHECK: ld.w	$r[[REG1:[0-9]+]], $sp, 12
; CHECK: ld.bu	$r[[REG2:[0-9]+]], $sp, 8
; CHECK: iocsrwr.b	$r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]]
; CHECK: jr	$ra
;

define void @iocsrwr_h() {
entry:
  %u32_a = alloca i32, align 4
  %u16_a = alloca i16, align 2
  %0 = load i16, i16* %u16_a, align 2
  %conv = zext i16 %0 to i32
  %1 = load i32, i32* %u32_a, align 4
  call void @llvm.loongarch.iocsrwr.h(i32 %conv, i32 %1)
  ret void
}

declare void @llvm.loongarch.iocsrwr.h(i32, i32)

; CHECK-LABEL: iocsrwr_h:
; CHECK: ld.w	$r[[REG1:[0-9]+]], $sp, 12
; CHECK: ld.hu	$r[[REG2:[0-9]+]], $sp, 8
; CHECK: iocsrwr.h	$r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]]
; CHECK: jr	$ra
;

define void @iocsrwr_w() {
entry:
  %u32_a = alloca i32, align 4
  %u32_b = alloca i32, align 4
  %0 = load i32, i32* %u32_a, align 4
  %1 = load i32, i32* %u32_b, align 4
  call void @llvm.loongarch.iocsrwr.w(i32 %0, i32 %1)
  ret void
}

declare void @llvm.loongarch.iocsrwr.w(i32, i32)

; CHECK-LABEL: iocsrwr_w:
; CHECK: ld.w	$r[[REG1:[0-9]+]], $sp, 8
; CHECK: ld.w	$r[[REG2:[0-9]+]], $sp, 12
; CHECK: iocsrwr.w	$r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]]
; CHECK: jr	$ra
;

define void @iocsrwr_d() {
entry:
  %u32_a = alloca i32, align 4
  %u64_a = alloca i64, align 8
  %0 = load i64, i64* %u64_a, align 8
  %1 = load i32, i32* %u32_a, align 4
  call void @llvm.loongarch.iocsrwr.d(i64 %0, i32 %1)
  ret void
}

declare void @llvm.loongarch.iocsrwr.d(i64, i32)

; CHECK-LABEL: iocsrwr_d:
; CHECK: ld.w	$r[[REG1:[0-9]+]], $sp, 12
; CHECK: ld.d	$r[[REG2:[0-9]+]], $sp, 0
; CHECK: iocsrwr.d	$r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]]
; CHECK: jr	$ra
;

define void @cacop() {
entry:
  %i32_a = alloca i32, align 4
  %0 = load i32, i32* %i32_a, align 4
  call void @llvm.loongarch.cacop(i32 1, i32 %0, i32 2)
  ret void
}

declare void @llvm.loongarch.cacop(i32, i32, i32)

; CHECK-LABEL: cacop:
; CHECK: ld.w	$r[[REG:[0-9]+]], $sp, 12
; CHECK: cacop	1, $r[[REG:[0-9]+]], 2
; CHECK: jr	$ra
;

define void @dcacop() {
entry:
  %i64_a = alloca i64, align 8
  %0 = load i64, i64* %i64_a, align 8
  call void @llvm.loongarch.dcacop(i32 1, i64 %0, i64 2)
  ret void
}

declare void @llvm.loongarch.dcacop(i32, i64, i64)

; CHECK-LABEL: dcacop:
; CHECK: ld.d	$r[[REG:[0-9]+]], $sp, 8
; CHECK: cacop	1, $r[[REG:[0-9]+]], 2
; CHECK: jr	$ra
;

define void @rdtime_d() {
entry:
  %value = alloca i64, align 8
  %timeid = alloca i64, align 8
  %0 = call { i64, i64 } asm sideeffect "rdtime.d\09$0,$1\0A\09", "=&r,=&r"() nounwind
  %asmresult0 = extractvalue { i64, i64 } %0, 0
  %asmresult1 = extractvalue { i64, i64 } %0, 1
  store i64 %asmresult0, i64* %value, align 8
  store i64 %asmresult1, i64* %timeid, align 8
  ret void
}

; CHECK-LABEL: rdtime_d:
; CHECK: rdtime.d	$r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]]
; CHECK: st.d	$r[[REG2:[0-9]+]], $sp, 8
; CHECK: st.d	$r[[REG1:[0-9]+]], $sp, 0
; CHECK: jr	$ra
;

define void @rdtimeh_w() {
entry:
  %value = alloca i32, align 4
  %timeid = alloca i32, align 4
  %0 = call { i32, i32 } asm sideeffect "rdtimeh.w\09$0,$1\0A\09", "=&r,=&r"() nounwind
  %asmresult0 = extractvalue { i32, i32 } %0, 0
  %asmresult1 = extractvalue { i32, i32 } %0, 1
  store i32 %asmresult0, i32* %value, align 4
  store i32 %asmresult1, i32* %timeid, align 4
  ret void
}

; CHECK-LABEL: rdtimeh_w:
; CHECK: rdtimeh.w	$r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]]
; CHECK: st.w	$r[[REG2:[0-9]+]], $sp, 12
; CHECK: st.w	$r[[REG1:[0-9]+]], $sp, 8
; CHECK: jr	$ra
;

define void @rdtimel_w() {
entry:
  %value = alloca i32, align 4
  %timeid = alloca i32, align 4
  %0 = call { i32, i32 } asm sideeffect "rdtimel.w\09$0,$1\0A\09", "=&r,=&r"() nounwind
  %asmresult0 = extractvalue { i32, i32 } %0, 0
  %asmresult1 = extractvalue { i32, i32 } %0, 1
  store i32 %asmresult0, i32* %value, align 4
  store i32 %asmresult1, i32* %timeid, align 4
  ret void
}

; CHECK-LABEL: rdtimel_w:
; CHECK: rdtimel.w	$r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]]
; CHECK: st.w	$r[[REG2:[0-9]+]], $sp, 12
; CHECK: st.w	$r[[REG1:[0-9]+]], $sp, 8
; CHECK: jr	$ra
;

define void @crc_w_b_w() {
entry:
  %i32_r = alloca i32, align 4
  %i32_a = alloca i32, align 4
  %i8_a = alloca i8, align 1
  %0 = load i8, i8* %i8_a, align 1
  %conv = sext i8 %0 to i32
  %1 = load i32, i32* %i32_a, align 4
  %2 = call i32 @llvm.loongarch.crc.w.b.w(i32 %conv, i32 %1)
  store i32 %2, i32* %i32_r, align 4
  ret void
}

declare i32 @llvm.loongarch.crc.w.b.w(i32, i32)

; CHECK-LABEL: crc_w_b_w:
; CHECK: ld.w	$r[[REG1:[0-9]+]], $sp, 8
; CHECK: ld.b	$r[[REG2:[0-9]+]], $sp, 4
; CHECK: crc.w.b.w	$r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]]
; CHECK: jr	$ra
;

define void @crc_w_h_w() {
entry:
  %i32_r = alloca i32, align 4
  %i32_a = alloca i32, align 4
  %i16_a = alloca i16, align 2
  %0 = load i16, i16* %i16_a, align 2
  %conv = sext i16 %0 to i32
  %1 = load i32, i32* %i32_a, align 4
  %2 = call i32 @llvm.loongarch.crc.w.h.w(i32 %conv, i32 %1)
  store i32 %2, i32* %i32_r, align 4
  ret void
}

declare i32 @llvm.loongarch.crc.w.h.w(i32, i32)

; CHECK-LABEL: crc_w_h_w:
; CHECK: ld.w	$r[[REG1:[0-9]+]], $sp, 8
; CHECK: ld.h	$r[[REG2:[0-9]+]], $sp, 4
; CHECK: crc.w.h.w	$r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]]
; CHECK: jr	$ra
;

define void @crc_w_w_w() {
entry:
  %i32_r = alloca i32, align 4
  %i32_a = alloca i32, align 4
  %i32_b = alloca i32, align 4
  %0 = load i32, i32* %i32_a, align 4
  %1 = load i32, i32* %i32_b, align 4
  %2 = call i32 @llvm.loongarch.crc.w.w.w(i32 %0, i32 %1)
  store i32 %2, i32* %i32_r, align 4
  ret void
}

declare i32 @llvm.loongarch.crc.w.w.w(i32, i32)

; CHECK-LABEL: crc_w_w_w:
; CHECK: ld.w	$r[[REG1:[0-9]+]], $sp, 4
; CHECK: ld.w	$r[[REG2:[0-9]+]], $sp, 8
; CHECK: crc.w.w.w	$r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]]
; CHECK: jr	$ra
;

define void @crc_w_d_w() {
entry:
  %i32_r = alloca i32, align 4
  %i32_a = alloca i32, align 4
  %i64_a = alloca i64, align 8
  %0 = load i64, i64* %i64_a, align 8
  %1 = load i32, i32* %i32_a, align 4
  %2 = call i32 @llvm.loongarch.crc.w.d.w(i64 %0, i32 %1)
  store i32 %2, i32* %i32_r, align 4
  ret void
}

declare i32 @llvm.loongarch.crc.w.d.w(i64, i32)

; CHECK-LABEL: crc_w_d_w:
; CHECK: ld.w	$r[[REG1:[0-9]+]], $sp, 8
; CHECK: ld.d	$r[[REG2:[0-9]+]], $sp, 0
; CHECK: crc.w.d.w	$r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]]
; CHECK: jr	$ra
;

define void @crcc_w_b_w() {
entry:
  %i32_r = alloca i32, align 4
  %i32_a = alloca i32, align 4
  %i8_a = alloca i8, align 1
  %0 = load i8, i8* %i8_a, align 1
  %conv = sext i8 %0 to i32
  %1 = load i32, i32* %i32_a, align 4
  %2 = call i32 @llvm.loongarch.crcc.w.b.w(i32 %conv, i32 %1)
  store i32 %2, i32* %i32_r, align 4
  ret void
}

declare i32 @llvm.loongarch.crcc.w.b.w(i32, i32)

; CHECK-LABEL: crcc_w_b_w:
; CHECK: ld.w	$r[[REG1:[0-9]+]], $sp, 8
; CHECK: ld.b	$r[[REG2:[0-9]+]], $sp, 4
; CHECK: crcc.w.b.w	$r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]]
; CHECK: jr	$ra
;

define void @crcc_w_h_w() {
entry:
  %i32_r = alloca i32, align 4
  %i32_a = alloca i32, align 4
  %i16_a = alloca i16, align 2
  %0 = load i16, i16* %i16_a, align 2
  %conv = sext i16 %0 to i32
  %1 = load i32, i32* %i32_a, align 4
  %2 = call i32 @llvm.loongarch.crcc.w.h.w(i32 %conv, i32 %1)
  store i32 %2, i32* %i32_r, align 4
  ret void
}

declare i32 @llvm.loongarch.crcc.w.h.w(i32, i32)

; CHECK-LABEL: crcc_w_h_w:
; CHECK: ld.w	$r[[REG1:[0-9]+]], $sp, 8
; CHECK: ld.h	$r[[REG2:[0-9]+]], $sp, 4
; CHECK: crcc.w.h.w	$r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]]
; CHECK: jr	$ra
;

define void @crcc_w_w_w() {
entry:
  %i32_r = alloca i32, align 4
  %i32_a = alloca i32, align 4
  %i32_b = alloca i32, align 4
  %0 = load i32, i32* %i32_a, align 4
  %1 = load i32, i32* %i32_b, align 4
  %2 = call i32 @llvm.loongarch.crcc.w.w.w(i32 %0, i32 %1)
  store i32 %2, i32* %i32_r, align 4
  ret void
}

declare i32 @llvm.loongarch.crcc.w.w.w(i32, i32)

; CHECK-LABEL: crcc_w_w_w:
; CHECK: ld.w	$r[[REG1:[0-9]+]], $sp, 4
; CHECK: ld.w	$r[[REG2:[0-9]+]], $sp, 8
; CHECK: crcc.w.w.w	$r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]]
; CHECK: jr	$ra
;

define void @crcc_w_d_w() {
entry:
  %i32_r = alloca i32, align 4
  %i32_a = alloca i32, align 4
  %i64_a = alloca i64, align 8
  %0 = load i64, i64* %i64_a, align 8
  %1 = load i32, i32* %i32_a, align 4
  %2 = call i32 @llvm.loongarch.crcc.w.d.w(i64 %0, i32 %1)
  store i32 %2, i32* %i32_r, align 4
  ret void
}

declare i32 @llvm.loongarch.crcc.w.d.w(i64, i32)

; CHECK-LABEL: crcc_w_d_w:
; CHECK: ld.w	$r[[REG1:[0-9]+]], $sp, 8
; CHECK: ld.d	$r[[REG2:[0-9]+]], $sp, 0
; CHECK: crcc.w.d.w	$r[[REG1:[0-9]+]], $r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]]
; CHECK: jr	$ra
;

define void @tlbclr() {
entry:
  call void @llvm.loongarch.tlbclr()
  ret void
}

declare void @llvm.loongarch.tlbclr()

; CHECK-LABEL: tlbclr:
; CHECK: tlbclr
; CHECK: jr	$ra
;

define void @tlbflush() {
entry:
  call void @llvm.loongarch.tlbflush()
  ret void
}

declare void @llvm.loongarch.tlbflush()

; CHECK-LABEL: tlbflush:
; CHECK: tlbflush
; CHECK: jr	$ra
;

define void @tlbfill() {
entry:
  call void @llvm.loongarch.tlbfill()
  ret void
}

declare void @llvm.loongarch.tlbfill()

; CHECK-LABEL: tlbfill:
; CHECK: tlbfill
; CHECK: jr	$ra
;

define void @tlbrd() {
entry:
  call void @llvm.loongarch.tlbrd()
  ret void
}

declare void @llvm.loongarch.tlbrd()

; CHECK-LABEL: tlbrd:
; CHECK: tlbrd
; CHECK: jr	$ra
;

define void @tlbwr() {
entry:
  call void @llvm.loongarch.tlbwr()
  ret void
}

declare void @llvm.loongarch.tlbwr()

; CHECK-LABEL: tlbwr:
; CHECK: tlbwr
; CHECK: jr	$ra
;

define void @tlbsrch() {
entry:
  call void @llvm.loongarch.tlbsrch()
  ret void
}

declare void @llvm.loongarch.tlbsrch()

; CHECK-LABEL: tlbsrch:
; CHECK: tlbsrch
; CHECK: jr	$ra
;

define void @syscall() {
entry:
  call void @llvm.loongarch.syscall(i64 1)
  ret void
}

declare void @llvm.loongarch.syscall(i64)

; CHECK-LABEL: syscall:
; CHECK: syscall 1
; CHECK: jr	$ra
;

define void @break_builtin() {
entry:
  call void @llvm.loongarch.break(i64 1)
  ret void
}

declare void @llvm.loongarch.break(i64)

; CHECK-LABEL: break_builtin:
; CHECK: break 1
; CHECK: jr	$ra
;

define void @asrtle_d() {
entry:
  %i64_a = alloca i64, align 8
  %i64_b = alloca i64, align 8
  %0 = load i64, i64* %i64_a, align 8
  %1 = load i64, i64* %i64_b, align 8
  call void @llvm.loongarch.asrtle.d(i64 %0, i64 %1)
  ret void
}

declare void @llvm.loongarch.asrtle.d(i64, i64)

; CHECK-LABEL: asrtle_d:
; CHECK: ld.d	$r[[REG1:[0-9]+]], $sp, 0
; CHECK: ld.d	$r[[REG2:[0-9]+]], $sp, 8
; CHECK: asrtle.d	$r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]]
; CHECK: jr	$ra
;

define void @asrtgt_d() {
entry:
  %i64_a = alloca i64, align 8
  %i64_b = alloca i64, align 8
  %0 = load i64, i64* %i64_a, align 8
  %1 = load i64, i64* %i64_b, align 8
  call void @llvm.loongarch.asrtgt.d(i64 %0, i64 %1)
  ret void
}

declare void @llvm.loongarch.asrtgt.d(i64, i64)

; CHECK-LABEL: asrtgt_d:
; CHECK: ld.d	$r[[REG1:[0-9]+]], $sp, 0
; CHECK: ld.d	$r[[REG2:[0-9]+]], $sp, 8
; CHECK: asrtgt.d	$r[[REG2:[0-9]+]], $r[[REG1:[0-9]+]]
; CHECK: jr	$ra
;

define void @dbar() {
entry:
  call void @llvm.loongarch.dbar(i64 0)
  ret void
}

declare void @llvm.loongarch.dbar(i64)

; CHECK-LABEL: dbar:
; CHECK: dbar 0
; CHECK: jr	$ra
;

define void @ibar() {
entry:
  call void @llvm.loongarch.ibar(i64 0)
  ret void
}

declare void @llvm.loongarch.ibar(i64)

; CHECK-LABEL: ibar:
; CHECK: ibar 0
; CHECK: jr	$ra
;

define void @movfcsr2gr() {
entry:
  %u32_r = alloca i32, align 4
  %rd = alloca i32, align 4
  %0 = call i32 asm sideeffect "movfcsr2gr $0, $$fcsr0", "=&r"()
  store i32 %0, i32* %rd, align 4
  %1 = load i32, i32* %rd, align 4
  store i32 %1, i32* %u32_r, align 4
  ret void
}

; CHECK-LABEL: movfcsr2gr:
; CHECK: movfcsr2gr	$r[[REG:[0-9]+]], $fcsr[[REG:[0-9]+]]
; CHECK: st.w	$r[[REG:[0-9]+]], $sp, 8
; CHECK: st.w	$r[[REG:[0-9]+]], $sp, 12
; CHECK: jr	$ra
;

define void @movgr2fcsr() {
entry:
  %u32_a = alloca i32, align 4
  %0 = load i32, i32* %u32_a, align 4
  call void asm sideeffect "movgr2fcsr $$fcsr0, $0", "r"(i32 %0)
  ret void
}

; CHECK-LABEL: movgr2fcsr:
; CHECK: ld.w	$r[[REG:[0-9]+]], $sp, 12
; CHECK: movgr2fcsr	$fcsr[[REG:[0-9]+]], $r[[REG:[0-9]+]]
; CHECK: jr	$ra
;
