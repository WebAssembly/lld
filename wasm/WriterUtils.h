//===- WriterUtils.h --------------------------------------------*- C++ -*-===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLD_WASM_WRITERUTILS_H
#define LLD_WASM_WRITERUTILS_H

#include "llvm/ADT/Twine.h"
#include "llvm/Object/Wasm.h"
#include "llvm/Support/raw_ostream.h"

using llvm::raw_ostream;
using namespace llvm;
using namespace llvm::wasm;

namespace lld {
namespace wasm {

struct OutputRelocation {
  WasmRelocation Reloc;
  uint32_t NewIndex;
  uint32_t Value;
};

void debugWrite(uint64_t offset, Twine msg);

void writeUleb128(raw_ostream &OS, uint32_t Number, const char *msg);

void writeSleb128(raw_ostream &OS, int32_t Number, const char *msg);

void writeBytes(raw_ostream &OS, const char *bytes, size_t count,
                const char *msg = nullptr);

void writeStr(raw_ostream &OS, const StringRef String,
              const char *msg = nullptr);

void writeU8(raw_ostream &OS, uint8_t byte, const char *msg);

void writeU32(raw_ostream &OS, uint32_t Number, const char *msg);

void writeValueType(raw_ostream &OS, int32_t Type, const char *msg);

void writeSig(raw_ostream &OS, const WasmSignature &Sig);

void writeInitExpr(raw_ostream &OS, const WasmInitExpr &InitExpr);

void writeLimits(raw_ostream &OS, const WasmLimits &Limits);

void writeGlobal(raw_ostream &OS, const WasmGlobal &Global);

void writeImport(raw_ostream &OS, const WasmImport &Import);

void writeExport(raw_ostream &OS, const WasmExport &Export);

void writeReloc(raw_ostream &OS, const OutputRelocation &Reloc);

} // namespace wasm
} // namespace lld

#endif // LLD_WASM_WRITERUTILS_H
