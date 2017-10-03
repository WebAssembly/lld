//===- Error.h --------------------------------------------------*- C++ -*-===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLD_WASM_ERROR_H
#define LLD_WASM_ERROR_H

#include "lld/Common/LLVM.h"
#include "llvm/Support/Error.h"

namespace lld {
namespace wasm {

extern uint64_t ErrorCount;
extern llvm::raw_ostream *ErrorOS;
extern llvm::StringRef Argv0;

void log(const Twine &Msg);
void warn(const Twine &Msg);
void error(const Twine &Msg);
LLVM_ATTRIBUTE_NORETURN void fatal(const Twine &Msg);

// check() functions are convenient functions to strip errors
// from error-or-value objects.
template <class T> T check(ErrorOr<T> E) {
  if (auto EC = E.getError())
    fatal(EC.message());
  return std::move(*E);
}

template <class T> T check(Expected<T> E) {
  if (!E)
    fatal(llvm::toString(E.takeError()));
  return std::move(*E);
}

template <class T> T check(ErrorOr<T> E, const Twine &Prefix) {
  if (auto EC = E.getError())
    fatal(Prefix + ": " + EC.message());
  return std::move(*E);
}

template <class T> T check(Expected<T> E, const Twine &Prefix) {
  if (!E)
    fatal(Prefix + ": " + llvm::toString(E.takeError()));
  return std::move(*E);
}

} // namespace wasm
} // namespace lld

#endif
