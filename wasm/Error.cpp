//===- Error.cpp ----------------------------------------------------------===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Error.h"

#include "Config.h"

#include "llvm/ADT/Twine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/raw_ostream.h"

#include <mutex>

#if !defined(_MSC_VER) && !defined(__MINGW32__)
#include <unistd.h>
#endif

using namespace llvm;

namespace lld {
namespace wasm {

uint64_t ErrorCount;
raw_ostream *ErrorOS;
StringRef Argv0;

// The functions defined in this file can be called from multiple threads,
// but outs() or errs() are not thread-safe. We protect them using a mutex.
static std::mutex Mu;

static void print(StringRef S, raw_ostream::Colors C) {
  *ErrorOS << Argv0 + ": ";
  if (Config->ColorDiagnostics) {
    ErrorOS->changeColor(C, true);
    *ErrorOS << S;
    ErrorOS->resetColor();
  } else {
    *ErrorOS << S;
  }
}

LLVM_ATTRIBUTE_NORETURN static void exitLld(int Val) {
  // Dealloc/destroy ManagedStatic variables before calling
  // _exit(). In a non-LTO build, this is a nop. In an LTO
  // build allows us to get the output of -time-passes.
  llvm_shutdown();

  outs().flush();
  errs().flush();
  _exit(Val);
}

void log(const Twine &Msg) {
  std::lock_guard<std::mutex> Lock(Mu);
  if (Config->Verbose)
    errs() << "lld: " << Msg << "\n";
}

void fatal(const Twine &Msg) {
  std::lock_guard<std::mutex> Lock(Mu);
  print("error: ", raw_ostream::RED);
  *ErrorOS << Msg << "\n";
  ErrorOS->flush();
  exitLld(1);
}

void error(const Twine &Msg) {
  std::lock_guard<std::mutex> Lock(Mu);

  if (Config->ErrorLimit == 0 || ErrorCount < Config->ErrorLimit) {
    print("error: ", raw_ostream::RED);
    *ErrorOS << Msg << "\n";
  } else if (ErrorCount == Config->ErrorLimit) {
    print("error: ", raw_ostream::RED);
    *ErrorOS << "too many errors emitted, stopping now"
             << " (use -error-limit=0 to see all errors)\n";
    exitLld(1);
  }

  ++ErrorCount;
}

void warn(const Twine &Msg) {
  std::lock_guard<std::mutex> Lock(Mu);
  print("warning: ", raw_ostream::MAGENTA);
  *ErrorOS << Msg << "\n";
}

} // namespace wasm
} // namespace lld
