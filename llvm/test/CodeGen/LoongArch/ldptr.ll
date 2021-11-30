; Check whether ld.w/ld.d/ldptr.w/ldptr.d/ldx.w/ldx.d instructions are properly generated
; RUN: llc -march=loongarch64 -o - %s | FileCheck %s

define signext i32 @ld_w(i32* %p) {
; CHECK-LABEL: ld_w:
; CHECK: # %bb.0: # %entry
; CHECK-NEXT: ld.w  $r4, $r4, 2044
; CHECK-NEXT: jr $ra
entry:
  %addr = getelementptr inbounds i32, i32* %p, i64 511
  %val = load i32, i32* %addr, align 4
  ret i32 %val
}

define signext i32 @ldptr_w(i32* %p) {
; CHECK-LABEL: ldptr_w:
; CHECK: # %bb.0: # %entry
; CHECK-NEXT: ldptr.w  $r4, $r4, 2048
; CHECK-NEXT: jr $ra
entry:
  %addr = getelementptr inbounds i32, i32* %p, i64 512
  %val = load i32, i32* %addr, align 4
  ret i32 %val
}

define signext i32 @ldx_w(i32* %p) {
; CHECK-LABEL: ldx_w:
; CHECK: # %bb.0: # %entry
; CHECK-NEXT: lu12i.w $r[[REG:[0-9]+]], 8
; CHECK-NEXT: ldx.w  $r4, $r4, $r[[REG:[0-9]+]]
; CHECK-NEXT: jr $ra
entry:
  %addr = getelementptr inbounds i32, i32* %p, i64 8192
  %val = load i32, i32* %addr, align 4
  ret i32 %val
}

define i64 @ld_d(i64* %p) {
; CHECK-LABEL: ld_d:
; CHECK: # %bb.0: # %entry
; CHECK-NEXT: ld.d  $r4, $r4, 2040
; CHECK-NEXT: jr $ra
entry:
  %addr = getelementptr inbounds i64, i64* %p, i64 255
  %val = load i64, i64* %addr, align 8
  ret i64 %val
}

define i64 @ldptr_d(i64* %p) {
; CHECK-LABEL: ldptr_d:
; CHECK: # %bb.0: # %entry
; CHECK-NEXT: ldptr.d  $r4, $r4, 2048
; CHECK-NEXT: jr $ra
entry:
  %addr = getelementptr inbounds i64, i64* %p, i64 256
  %val = load i64, i64* %addr, align 8
  ret i64 %val
}

define i64 @ldx_d(i64* %p) {
; CHECK-LABEL: ldx_d:
; CHECK: # %bb.0: # %entry
; CHECK-NEXT: lu12i.w $r[[REG:[0-9]+]], 8
; CHECK-NEXT: ldx.d  $r4, $r4, $r[[REG:[0-9]+]]
; CHECK-NEXT: jr $ra
entry:
  %addr = getelementptr inbounds i64, i64* %p, i64 4096
  %val = load i64, i64* %addr, align 8
  ret i64 %val
}
