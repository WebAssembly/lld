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

StringRef Symbol::getName() const {
  return Name;
}

InputFile* Symbol::getFile() const {
  return File;
}

void Symbol::setArchiveSymbol(const Archive::Symbol &Sym) {
  ArchiveSymbol = Sym;
}

uint32_t Symbol::getGlobalIndex() const {
  const WasmExport &Export = getExport();
  assert(Export.Kind == llvm::wasm::WASM_EXTERNAL_GLOBAL);
  return Export.Index;
}

uint32_t Symbol::getFunctionIndex() const {
  const WasmExport &Export = getExport();
  assert(Export.Kind == llvm::wasm::WASM_EXTERNAL_FUNCTION);
  return Export.Index;
}

uint32_t Symbol::getFunctionTypeIndex() const {
  const WasmImport &Import = getImport();
  assert(Import.Kind == llvm::wasm::WASM_EXTERNAL_FUNCTION);
  return Import.SigIndex;
}

const WasmImport& Symbol::getImport() const {
  assert(isUndefined());
  assert(Sym != nullptr);
  assert(isa<ObjectFile>(File));
  ObjectFile* Obj = dyn_cast<ObjectFile>(File);
  assert(Sym->ElementIndex < Obj->getWasmObj()->imports().size());
  return Obj->getWasmObj()->imports()[Sym->ElementIndex];
}

const WasmExport& Symbol::getExport() const {
  DEBUG(dbgs() << "getExport: " << getName()
               << " ElementIndex: " << Sym->ElementIndex << "\n");
  assert(isDefined());
  assert(Sym != nullptr);
  assert(isa<ObjectFile>(File));
  ObjectFile* Obj = dyn_cast<ObjectFile>(File);
  assert(Sym->ElementIndex < Obj->getWasmObj()->exports().size());
  return Obj->getWasmObj()->exports()[Sym->ElementIndex];
}

uint32_t Symbol::getOutputIndex() const {
  if (isUndefined() && isWeak()) {
    return 0;
  }
  assert(OutputIndexSet);
  return OutputIndex;
}

void Symbol::setOutputIndex(uint32_t Index) {
  assert(!OutputIndexSet);
  DEBUG(dbgs() << "setOutputIndex: " << this << " " << Name << ": " << Index
               << " kind=" << toString(SymbolKind) << "\n");
  OutputIndex = Index;
  OutputIndexSet = true;
}

void Symbol::update(Kind K, InputFile *F, const WasmSymbol *WasmSym) {
  SymbolKind = K;
  File = F;
  Sym = WasmSym;
}

bool Symbol::isWeak() const {
  return Sym && Sym->isWeak();
}

} // namespace wasm

// Returns a symbol name for an error message.
std::string toString(wasm::Symbol &Sym) {
  if (wasm::Config->Demangle)
    if (Optional<std::string> S = wasm::demangle(Sym.getName()))
      return *S;
  return Sym.getName();
}

std::string toString(wasm::Symbol::Kind &Kind) {
  switch (Kind) {
  case wasm::Symbol::DefinedFunctionKind: return "DefinedFunction";
  case wasm::Symbol::DefinedGlobalKind: return "DefinedGlobal";
  case wasm::Symbol::UndefinedFunctionKind: return "UndefinedFunction";
  case wasm::Symbol::UndefinedGlobalKind: return "UndefinedGlobal";
  case wasm::Symbol::LazyKind: return "LazyKind";
  }
}

} // namespace lld
