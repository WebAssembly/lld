//===- Symbols.h ------------------------------------------------*- C++ -*-===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLD_WASM_SYMBOLS_H
#define LLD_WASM_SYMBOLS_H

#include "lld/Core/LLVM.h"
#include "llvm/Object/Archive.h"
#include "llvm/Object/Wasm.h"

using llvm::object::Archive;
using llvm::object::WasmSymbol;
using llvm::wasm::WasmImport;
using llvm::wasm::WasmExport;

namespace lld {
namespace wasm {

class InputFile;

class Symbol {
public:
  enum Kind {
    DefinedFunctionKind = 0,
    DefinedGlobalKind,

    LazyKind,
    UndefinedFunctionKind,
    UndefinedGlobalKind,

    LastDefinedKind = DefinedGlobalKind,
    InvalidKind,
  };

  Symbol(StringRef Name)
      : WrittenToSymtab(0), WrittenToNameSec(0), Name(Name), ArchiveSymbol(nullptr, 0, 0) {}

  Kind getKind() const { return SymbolKind; }

  bool isLazy() const { return SymbolKind == LazyKind; }
  bool isDefined() const { return SymbolKind <= LastDefinedKind; }
  bool isUndefined() const { return !isDefined(); }
  bool isFunction() const {
    return SymbolKind == DefinedFunctionKind ||
           SymbolKind == UndefinedFunctionKind;
  }
  bool isGlobal() const { return !isFunction(); }
  bool isWeak() const;

  // Returns the symbol name.
  StringRef getName() const;

  // Returns the file from which this symbol was created.
  InputFile *getFile() const;

  uint32_t getGlobalIndex() const;
  uint32_t getFunctionIndex() const;
  uint32_t getFunctionTypeIndex() const;
  uint32_t getOutputIndex() const;

  bool hasOutputIndex() { return OutputIndexSet; }

  void setOutputIndex(uint32_t Index);

  void update(Kind K, InputFile *F = nullptr, const WasmSymbol *Sym = nullptr);

  void setArchiveSymbol(const Archive::Symbol &Sym);
  const Archive::Symbol& getArchiveSymbol() { return ArchiveSymbol; }

  // This bit is used by Writer::writeNameSection() to prevent
  // symbols from being written to the symbol table more than once.
  unsigned WrittenToSymtab : 1;
  unsigned WrittenToNameSec : 1;

protected:
  const WasmImport &getImport() const;
  const WasmExport &getExport() const;

  StringRef Name;
  Archive::Symbol ArchiveSymbol;
  Kind SymbolKind = InvalidKind;
  InputFile* File = nullptr;
  const WasmSymbol* Sym = nullptr;
  uint32_t OutputIndex = 0;
  bool OutputIndexSet = false;
};

} // namespace wasm

std::string toString(wasm::Symbol &Sym);
std::string toString(wasm::Symbol::Kind &Kind);

inline llvm::raw_ostream &operator<<(raw_ostream &OS, wasm::Symbol &Sym) {
  OS << toString(Sym);
  return OS;
}

} // namespace lld


#endif
