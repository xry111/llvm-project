; RUN: llc -march=loongarch64 -o - %s | FileCheck %s

define void @preld(i32* %p) {
entry:
  ; CHECK: preld 10, $r4, 23
  tail call void asm sideeffect "preld 10, $0, 23 \0A\09", "r"(i32* %p)
  ret void
}
