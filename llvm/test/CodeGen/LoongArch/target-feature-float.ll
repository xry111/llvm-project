; RUN: llc -march=loongarch64 -target-abi lp64d --relocation-model=pic < %s \
; RUN:   | FileCheck -check-prefix=ATTRN-DNF %s
; RUN: llc -march=loongarch64 -target-abi lp64d -mattr=+d < %s \
; RUN:   | FileCheck -check-prefix=ATTRD-F-DF-FD-NFD %s
; RUN: llc -march=loongarch64 -target-abi lp64d -mattr=+f < %s \
; RUN:   | FileCheck -check-prefix=ATTRD-F-DF-FD-NFD %s
; RUN: llc -march=loongarch64 -target-abi lp64d -mattr=+d,+f < %s \
; RUN:   | FileCheck -check-prefix=ATTRD-F-DF-FD-NFD %s
; RUN: llc -march=loongarch64 -target-abi lp64d -mattr=+f,+d < %s \
; RUN:   | FileCheck -check-prefix=ATTRD-F-DF-FD-NFD %s
; RUN: llc -march=loongarch64 -target-abi lp64d -mattr=+d,-f --relocation-model=pic < %s \
; RUN:   | FileCheck -check-prefix=ATTRN-DNF %s
; RUN: llc -march=loongarch64 -target-abi lp64d -mattr=-f,+d < %s \
; RUN:   | FileCheck -check-prefix=ATTRD-F-DF-FD-NFD %s

define float @test(float %a, float %b) {
; ATTRN-DNF-LABEL: test:
; ATTRN-DNF:       # %bb.0: # %entry
; ATTRN-DNF:    addi.d $sp, $sp, -16
; ATTRN-DNF:    st.d $ra, $sp, 8 # 8-byte Folded Spill
; ATTRN-DNF:    slli.w $r4, $r4, 0
; ATTRN-DNF:    slli.w $r5, $r5, 0
; ATTRN-DNF:    bl __addsf3
; ATTRN-DNF:    ld.d $ra, $sp, 8 # 8-byte Folded Reload
; ATTRN-DNF:    addi.d $sp, $sp, 16
; ATTRN-DNF:    jr $ra
;
; ATTRD-F-DF-FD-NFD-LABEL: test:
; ATTRD-F-DF-FD-NFD:       # %bb.0: # %entry
; ATTRD-F-DF-FD-NFD:    fadd.s $f0, $f0, $f1
; ATTRD-F-DF-FD-NFD:    jr $ra
entry:
  %add = fadd float %a, %b
  ret float %add
}
