; Check whether st.w/st.d/stptr.w/stptr.d/stx.w/stx.d instructions are properly generated
; RUN: llc -march=loongarch64 -o - %s | FileCheck %s

define void @st_w(i32* %p, i32 signext %val) {
; CHECK: st.w  $r5, $r4, 2044
; CHECK: jr $ra
  %addr = getelementptr inbounds i32, i32* %p, i64 511
  store i32 %val, i32* %addr, align 4
  ret void
}

define void @stptr_w(i32* %p, i32 signext %val) {
; CHECK: stptr.w  $r5, $r4, 2048
; CHECK: jr $ra
  %addr = getelementptr inbounds i32, i32* %p, i64 512
  store i32 %val, i32* %addr, align 4
  ret void
}

define void @stx_w(i32* %p, i32 signext %val) {
; CHECK: lu12i.w $r[[REG:[0-9]+]], 8
; CHECK: stx.w  $r5, $r4, $r[[REG:[0-9]+]]
; CHECK: jr $ra
  %addr = getelementptr inbounds i32, i32* %p, i64 8192
  store i32 %val, i32* %addr, align 4
  ret void
}

define void @st_d(i64* %p, i64 %val) {
; CHECK: st.d  $r5, $r4, 2040
; CHECK: jr $ra
  %addr = getelementptr inbounds i64, i64* %p, i64 255
  store i64 %val, i64* %addr, align 8
  ret void
}

define void @stptr_d(i64* %p, i64 %val) {
; CHECK: stptr.d  $r5, $r4, 2048
; CHECK: jr $ra
  %addr = getelementptr inbounds i64, i64* %p, i64 256
  store i64 %val, i64* %addr, align 8
  ret void
}

define void @stx_d(i64* %p, i64 %val) {
; CHECK: lu12i.w $r[[REG:[0-9]+]], 8
; CHECK: stx.d  $r5, $r4, $r[[REG:[0-9]+]]
; CHECK: jr $ra
  %addr = getelementptr inbounds i64, i64* %p, i64 4096
  store i64 %val, i64* %addr, align 8
  ret void
}
