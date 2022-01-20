; RUN: llc -march=loongarch64 -no-integrated-as -o - %s | FileCheck %s

define i64 @test(i64 %a) {
; CHECK:    add.d $r4, $r4, $r0
entry:
  %0 = tail call i64 asm sideeffect "add.d $0, $1, ${2:z}  \0A", "=r,r,Jr"(i64 %a, i64 0)
  ret i64 %0
}
