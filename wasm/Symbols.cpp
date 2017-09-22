//===- Symbols.cpp --------------------------------------------------------===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Symbols.h"

#include "Config.h"
#include "Error.h"
#include "InputFiles.h"
#include "Strings.h"

#define DEBUG_TYPE "lld"

using namespace llvm;

namespace lld {
namespace wasm {

uint32_t Symbol::getGlobalIndex() const {
  assert(!Sym->isFunction());
  return Sym->ElementIndex;
}

uint32_t Symbol::getFunctionIndex() const {
  assert(Sym->isFunction());
  return Sym->ElementIndex;
}

uint32_t Symbol::getFunctionTypeIndex() const {
  assert(Sym->isFunction());
  ObjectFile *Obj = cast<ObjectFile>(File);
  if (Obj->isImportedFunction(Sym->ElementIndex)) {
    const WasmImport &Import = Obj->getWasmObj()->imports()[Sym->ImportIndex];
    DEBUG(dbgs() << "getFunctionTypeIndex: import: " << Sym->ImportIndex
                 << " -> " << Import.SigIndex << "\n");
    return Import.SigIndex;
  }
  DEBUG(dbgs() << "getFunctionTypeIndex: non import: " << Sym->ElementIndex << "\n");
  uint32_t FuntionIndex = Sym->ElementIndex - Obj->NumFunctionImports();
  return Obj->getWasmObj()->functionTypes()[FuntionIndex];
}

uint32_t Symbol::getMemoryAddress() const {
  if (isUndefined())
    return 0;
  ObjectFile *Obj = cast<ObjectFile>(File);
  return Obj->getGlobalAddress(getGlobalIndex());
}

uint32_t Symbol::getOutputIndex() const {
  if (isUndefined() && isWeak())
    return 0;
  return OutputIndex.getValue();
}

void Symbol::update(Kind K, InputFile *F, const WasmSymbol *WasmSym) {
  SymbolKind = K;
  File = F;
  Sym = WasmSym;
}

bool Symbol::isWeak() const { return Sym && Sym->isWeak(); }

} // namespace wasm

// Returns a symbol name for an error message.
std::string toString(wasm::Symbol &Sym) {
  return wasm::displayName(Sym.getName());
}

std::string toString(wasm::Symbol::Kind &Kind) {
  switch (Kind) {
  case wasm::Symbol::DefinedFunctionKind:
    return "DefinedFunction";
  case wasm::Symbol::DefinedGlobalKind:
    return "DefinedGlobal";
  case wasm::Symbol::UndefinedFunctionKind:
    return "UndefinedFunction";
  case wasm::Symbol::UndefinedGlobalKind:
    return "UndefinedGlobal";
  case wasm::Symbol::LazyKind:
    return "LazyKind";
  }
}

} // namespace lld
