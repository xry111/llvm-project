add_llvm_component_library(LLVMLoongArchAsmParser
  LoongArchAsmParser.cpp

  LINK_COMPONENTS
  MC
  MCParser
  LoongArchDesc
  LoongArchInfo
  Support

  ADD_TO_COMPONENT
  LoongArch
  )
