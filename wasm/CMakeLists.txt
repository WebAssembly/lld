set(LLVM_TARGET_DEFINITIONS Options.td)
tablegen(LLVM Options.inc -gen-opt-parser-defs)
add_public_tablegen_target(WasmOptionsTableGen)

add_lld_library(lldwasm
  Driver.cpp
  Error.cpp
  InputFiles.cpp
  Strings.cpp
  SymbolTable.cpp
  Symbols.cpp
  Writer.cpp

  LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Analysis
  BinaryFormat
  BitReader
  BitWriter
  Codegen
  Core
  DebugInfoDWARF
  Demangle
  IPO
  Linker
  LTO
  Object
  Option
  Passes
  MC
  Support
  Target
  TransformUtils

  LINK_LIBS
  lldConfig
  lldCore
  ${PTHREAD_LIB}
  )
