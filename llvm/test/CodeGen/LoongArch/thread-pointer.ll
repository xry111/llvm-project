; RUN: llc -march=loongarch64 < %s | FileCheck %s

declare i8* @llvm.thread.pointer() nounwind readnone

define i8* @thread_pointer() {
; CHECK: move    $r4, $tp
  %1 = tail call i8* @llvm.thread.pointer()
  ret i8* %1
}
