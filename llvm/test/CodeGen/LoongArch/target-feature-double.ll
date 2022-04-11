; RUN: llc -march=loongarch64 -target-abi lp64d --relocation-model=pic < %s \
; RUN:   | FileCheck -check-prefix=ATTRN-F-DNF %s
; RUN: llc -march=loongarch64 -target-abi lp64d -mattr=+d < %s \
; RUN:   | FileCheck -check-prefix=ATTRD-DF-FD-NFD %s
; RUN: llc -march=loongarch64 -target-abi lp64d -mattr=+f --relocation-model=pic < %s \
; RUN:   | FileCheck -check-prefix=ATTRN-F-DNF %s
; RUN: llc -march=loongarch64 -target-abi lp64d -mattr=+d,+f < %s \
; RUN:   | FileCheck -check-prefix=ATTRD-DF-FD-NFD %s
; RUN: llc -march=loongarch64 -target-abi lp64d -mattr=+f,+d < %s \
; RUN:   | FileCheck -check-prefix=ATTRD-DF-FD-NFD %s
; RUN: llc -march=loongarch64 -target-abi lp64d -mattr=+d,-f --relocation-model=pic < %s \
; RUN:   | FileCheck -check-prefix=ATTRN-F-DNF %s
; RUN: llc -march=loongarch64 -target-abi lp64d -mattr=-f,+d < %s \
; RUN:   | FileCheck -check-prefix=ATTRD-DF-FD-NFD %s

define double @test(double %a, double %b) {
; ATTRN-F-DNF-LABEL: test:
; ATTRN-F-DNF:       # %bb.0: # %entry
; ATTRN-F-DNF:    addi.d $sp, $sp, -16
; ATTRN-F-DNF:    bl __adddf3
; ATTRN-F-DNF:    addi.d $sp, $sp, 16
; ATTRN-F-DNF:    jr $ra
;
; ATTRD-DF-FD-NFD-LABEL: test:
; ATTRD-DF-FD-NFD:       # %bb.0: # %entry
; ATTRD-DF-FD-NFD:    fadd.d $f0, $f0, $f1
; ATTRD-DF-FD-NFD:    jr $ra
entry:
  %add = fadd double %a, %b
  ret double %add
}
