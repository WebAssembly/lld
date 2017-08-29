//===- Threads.h ------------------------------------------------*- C++ -*-===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is essentially a fork of ELF/Threads.h
// TODO(sbc): Refactor into library to avoid duplication.
//
//===----------------------------------------------------------------------===//

#ifndef LLD_WASM_THREADS_H
#define LLD_WASM_THREADS_H

#include "Config.h"

#include "llvm/Support/Parallel.h"
#include <functional>

namespace lld {
namespace wasm {

template <typename R, class FuncTy> void parallelForEach(R &&Range, FuncTy Fn) {
  if (Config->Threads)
    for_each(llvm::parallel::par, std::begin(Range), std::end(Range), Fn);
  else
    for_each(llvm::parallel::seq, std::begin(Range), std::end(Range), Fn);
}

} // namespace wasm
} // namespace lld

#endif
