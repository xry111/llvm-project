//===-- LoongArchCallingConv.td - Calling Conventions LoongArch ----*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This describes the calling conventions for the LoongArch architecture.
//
//===----------------------------------------------------------------------===//

// The RISC-V calling convention is handled with custom code in
// LoongArchISelLowering.cpp (CC_LoongArch).

def CSR_ILP32S_LP64S
    : CalleeSavedRegs<(add R1, R2, R21, (sequence "R%u", 22, 31))>;

def CSR_ILP32F_LP64F
    : CalleeSavedRegs<(add CSR_ILP32S_LP64S,
                       (sequence "F%u_F", 24, 31))>;

def CSR_ILP32D_LP64D
    : CalleeSavedRegs<(add CSR_ILP32F_LP64F,
                       (sequence "F%u_D", 24, 31))>;

// Needed for implementation of LoongArchRegisterInfo::getNoPreservedMask()
def CSR_NoRegs : CalleeSavedRegs<(add)>;

// Interrupt handler needs to save/restore all registers that are used,
// both Caller and Callee saved registers.
def CSR_Interrupt : CalleeSavedRegs<(add R1, R2, (sequence "R%u", 4, 31))>;

// Same as CSR_Interrupt, but including all 32-bit FP registers.
def CSR_XLEN_F32_Interrupt: CalleeSavedRegs<(add R1, R2,
    (sequence "R%u", 4, 31),
    (sequence "F%u_F", 0, 31))>;

// Same as CSR_Interrupt, but including all 64-bit FP registers.
def CSR_XLEN_F64_Interrupt: CalleeSavedRegs<(add R1, R2,
    (sequence "R%u", 4, 31),
    (sequence "F%u_D", 0, 31))>;
