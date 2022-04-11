; RUN: llc -march=loongarch64 -mattr=+d -o - %s | FileCheck %s

define float @fcopysign_s(float %a, float %b) {
; CHECK-LABEL: fcopysign_s:
; CHECK:    fcopysign.s $f0, $f0, $f1
  %ret = call float @llvm.copysign.f32(float %a, float %b)
  ret float %ret
}
declare float @llvm.copysign.f32(float %a, float %b)

define double @fcopysign_d(double %a, double %b) {
; CHECK-LABEL: fcopysign_d:
; CHECK:    fcopysign.d $f0, $f0, $f1
  %ret = call double @llvm.copysign.f64(double %a, double %b)
  ret double %ret
}
declare double @llvm.copysign.f64(double %a, double %b)
