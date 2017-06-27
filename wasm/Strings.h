//===- Strings.h ------------------------------------------------*- C++ -*-===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLD_WASM_STRINGS_H
#define LLD_WASM_STRINGS_H

namespace lld {
namespace wasm {

// Returns a demangled C++ symbol name. If Name is not a mangled
// name, it returns Optional::None.
llvm::Optional<std::string> demangle(StringRef Name);

std::string displayName(StringRef Name);

}
}

#endif
