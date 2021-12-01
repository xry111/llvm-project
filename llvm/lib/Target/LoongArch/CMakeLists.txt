add_llvm_component_group(LoongArch)

set(LLVM_TARGET_DEFINITIONS LoongArch.td)

tablegen(LLVM LoongArchGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM LoongArchGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM LoongArchGenCallingConv.inc -gen-callingconv)
tablegen(LLVM LoongArchGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM LoongArchGenDisassemblerTables.inc -gen-disassembler)
tablegen(LLVM LoongArchGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM LoongArchGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM LoongArchGenMCPseudoLowering.inc -gen-pseudo-lowering)
tablegen(LLVM LoongArchGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM LoongArchGenSubtargetInfo.inc -gen-subtarget)

add_public_tablegen_target(LoongArchCommonTableGen)

add_llvm_target(LoongArchCodeGen
  LoongArchAnalyzeImmediate.cpp
  LoongArchAsmPrinter.cpp
  LoongArchCCState.cpp
  LoongArchExpandPseudo.cpp
  LoongArchInstrInfo.cpp
  LoongArchISelDAGToDAG.cpp
  LoongArchISelLowering.cpp
  LoongArchFrameLowering.cpp
  LoongArchMCInstLower.cpp
  LoongArchMachineFunction.cpp
  LoongArchModuleISelDAGToDAG.cpp
  LoongArchRegisterInfo.cpp
  LoongArchSubtarget.cpp
  LoongArchTargetMachine.cpp
  LoongArchTargetObjectFile.cpp

  ADD_TO_COMPONENT
  LoongArch
  )

add_subdirectory(AsmParser)
add_subdirectory(Disassembler)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
