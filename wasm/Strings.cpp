//===- Strings.cpp -------------------------------------------------------===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Config.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Demangle/Demangle.h"

using namespace llvm;

namespace lld {
namespace wasm {

// Returns the demangled C++ symbol name for Name.
Optional<std::string> demangle(StringRef Name) {
  // itaniumDemangle can be used to demangle strings other than symbol
  // names which do not necessarily start with "_Z". Name can be
  // either a C or C++ symbol. Don't call itaniumDemangle if the name
  // does not look like a C++ symbol name to avoid getting unexpected
  // result for a C symbol that happens to match a mangled type name.
  if (!Name.startswith("_Z"))
    return None;

  char *Buf = itaniumDemangle(Name.str().c_str(), nullptr, nullptr, nullptr);
  if (!Buf)
    return None;
  std::string S(Buf);
  free(Buf);
  return S;
}

std::string displayName(StringRef Name) {
  if (Config->Demangle)
    if (Optional<std::string> S = demangle(Name))
      return "`" + *S + "'";
  return Name;
}

} // namespace wasm
} // namespace lld
