add_llvm_component_group(LoongArch)

set(LLVM_TARGET_DEFINITIONS LoongArch.td)

tablegen(LLVM LoongArchGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM LoongArchGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM LoongArchGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM LoongArchGenDisassemblerTables.inc -gen-disassembler)
tablegen(LLVM LoongArchGenGlobalISel.inc -gen-global-isel)
tablegen(LLVM LoongArchGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM LoongArchGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM LoongArchGenMCPseudoLowering.inc -gen-pseudo-lowering)
tablegen(LLVM LoongArchGenRegisterBank.inc -gen-register-bank)
tablegen(LLVM LoongArchGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM LoongArchGenSearchableTables.inc -gen-searchable-tables)
tablegen(LLVM LoongArchGenSubtargetInfo.inc -gen-subtarget)

add_public_tablegen_target(LoongArchCommonTableGen)

add_llvm_target(LoongArchCodeGen
  LoongArchAsmPrinter.cpp
  LoongArchCallLowering.cpp
  LoongArchExpandAtomicPseudoInsts.cpp
  LoongArchExpandPseudoInsts.cpp
  LoongArchFrameLowering.cpp
  LoongArchInstrInfo.cpp
  LoongArchInstructionSelector.cpp
  LoongArchISelDAGToDAG.cpp
  LoongArchISelLowering.cpp
  LoongArchLegalizerInfo.cpp
  LoongArchMCInstLower.cpp
  LoongArchRegisterBankInfo.cpp
  LoongArchRegisterInfo.cpp
  LoongArchSubtarget.cpp
  LoongArchTargetMachine.cpp
  LoongArchTargetObjectFile.cpp
  LoongArchTargetTransformInfo.cpp

  LINK_COMPONENTS
  Analysis
  AsmPrinter
  Core
  CodeGen
  MC
  LoongArchDesc
  LoongArchInfo
  SelectionDAG
  Support
  Target
  GlobalISel

  ADD_TO_COMPONENT
  LoongArch
  )

add_subdirectory(AsmParser)
add_subdirectory(Disassembler)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
