; RUN: llc -march=loongarch64 < %s | FileCheck %s

define i64 @get_r2() {
; CHECK-LABEL: get_r2:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    move $r4, $tp
; CHECK-NEXT:    jr $ra
entry:
  %0 = call i64 @llvm.read_register.i64(metadata !0)
  ret i64 %0
}

define i64 @get_r21() {
; CHECK-LABEL: get_r21:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    move $r4, $r21
; CHECK-NEXT:    jr $ra
entry:
  %0 = call i64 @llvm.read_register.i64(metadata !1)
  ret i64 %0
}

declare i64 @llvm.read_register.i64(metadata)

!llvm.named.register.$r2 = !{!0}
!llvm.named.register.$r21 = !{!1}

!0 = !{!"$r2"}
!1 = !{!"$r21"}
