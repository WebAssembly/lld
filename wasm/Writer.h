//===- Writer.h -------------------------------------------------*- C++ -*-===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLD_WASM_WRITER_H
#define LLD_WASM_WRITER_H

#include "llvm/BinaryFormat/Wasm.h"
#include "llvm/Object/Wasm.h"
#include "llvm/Support/raw_ostream.h"

using llvm::raw_fd_ostream;
using llvm::OwningArrayRef;
using namespace llvm::wasm;

namespace lld {
namespace wasm {

class SymbolTable;

// For patching purposes, we need to remember where each section starts, both
// for patching up the section size field, and for patching up references to
// locations within the section.
struct SectionBookkeeping {
  // Where the size of the section is written.
  uint64_t SizeOffset;
  // Where the contents of the section starts (after the header).
  uint64_t ContentsOffset;
};

void writeResult(SymbolTable *T);

} // namespace wasm
} // namespace lld

#endif
