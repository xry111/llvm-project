add_llvm_component_library(LLVMLoongArchDisassembler
  LoongArchDisassembler.cpp

  ADD_TO_COMPONENT
  LoongArch

  LINK_COMPONENTS
  MCDisassembler
  )
