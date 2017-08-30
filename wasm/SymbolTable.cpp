//===- SymbolTable.cpp ----------------------------------------------------===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "SymbolTable.h"

#include "Config.h"
#include "Error.h"
#include "Memory.h"
#include "Strings.h"
#include "llvm/ADT/SmallPtrSet.h"

#define DEBUG_TYPE "lld"

using namespace llvm;

namespace lld {
namespace wasm {

SymbolTable *Symtab;

void SymbolTable::addFile(InputFile *File) {
  log("Processing: " + toString(File));
  File->parse();

  if (auto *F = dyn_cast<ObjectFile>(File))
    ObjectFiles.push_back(F);
}

void SymbolTable::reportRemainingUndefines() {
  SmallPtrSet<Symbol *, 8> Undefs;
  for (auto &I : Symtab) {
    Symbol *Sym = I.second;
    if (Sym->isUndefined() && !Sym->isLazy() && !Sym->isWeak() &&
        Config->AllowUndefinedSymbols.count(Sym->getName()) == 0) {
      Undefs.insert(Sym);
    }
  }

  if (Undefs.empty())
    return;

  for (ObjectFile *File : ObjectFiles)
    for (Symbol *Sym : File->getSymbols())
      if (Undefs.count(Sym))
        error(toString(File) + ": undefined symbol: " + toString(*Sym));

  for (Symbol *Sym : Undefs)
    if (!Sym->getFile())
      error("undefined symbol: " + toString(*Sym));
}

Symbol *SymbolTable::find(StringRef Name) {
  auto It = Symtab.find(CachedHashStringRef(Name));
  if (It == Symtab.end())
    return nullptr;
  return It->second;
}

std::pair<Symbol *, bool> SymbolTable::insert(StringRef Name) {
  Symbol *&Sym = Symtab[CachedHashStringRef(Name)];
  if (Sym)
    return {Sym, false};
  Sym = make<Symbol>(Name);
  return {Sym, true};
}

void SymbolTable::reportDuplicate(Symbol *Existing, InputFile *NewFile) {
  error("duplicate symbol: " + toString(*Existing) + " in " +
        toString(Existing->getFile()) + " and in " +
        (NewFile ? toString(NewFile) : "(internal)"));
}

static void checkSymbolTypes(Symbol *Existing, InputFile *F,
                             const WasmSymbol *New) {
  if (Existing->isLazy())
    return;
  bool NewIsFunction = New->Type == WasmSymbol::SymbolType::FUNCTION_EXPORT ||
                       New->Type == WasmSymbol::SymbolType::FUNCTION_IMPORT;
  if (Existing->isFunction() != NewIsFunction) {
    error("symbol type mismatch: " + New->Name);
    StringRef Filename = "<builtin>";
    if (Existing->getFile())
      Filename = Existing->getFile()->getName();
    error(Filename + ": " + (Existing->isFunction() ? "Function" : "Global"));
    error(F->getName() + ": " + (NewIsFunction ? "Function" : "Global"));
  }
}

Symbol *SymbolTable::addDefinedGlobal(StringRef Name) {
  DEBUG(dbgs() << "addDefinedGlobal: " << Name << "\n");
  Symbol *S;
  bool WasInserted;
  std::tie(S, WasInserted) = insert(Name);
  if (WasInserted)
    S->update(Symbol::DefinedGlobalKind);
  else if (!S->isFunction())
    error("symbol type mismatch: " + Name);
  return S;
}

Symbol *SymbolTable::addDefined(InputFile *F, const WasmSymbol *Sym) {
  DEBUG(dbgs() << "addDefined: " << Sym->Name << "\n");
  Symbol *S;
  bool WasInserted;
  Symbol::Kind Kind = Symbol::DefinedFunctionKind;
  if (Sym->Type == WasmSymbol::SymbolType::GLOBAL_EXPORT)
    Kind = Symbol::DefinedGlobalKind;

  std::tie(S, WasInserted) = insert(Sym->Name);
  if (WasInserted) {
    S->update(Kind, F, Sym);
  } else if (!S->isDefined()) {
    // The existing symbol table entry in undefined. The new symbol replaces
    // it
    DEBUG(dbgs() << "resolving existing undefined symbol: " << Sym->Name
                 << "\n");
    checkSymbolTypes(S, F, Sym);
    S->update(Kind, F, Sym);
  } else {
    // The existing symbol is defined.
    if (Sym->isWeak()) {
      // the new symbol is weak we can ignore it
      DEBUG(dbgs() << "existing symbol takes precensence\n");
    } else if (S->isWeak()) {
      // the new symbol is not weak and the existing symbol is, so we replace
      // it
      DEBUG(dbgs() << "replacing existing weak symbol\n");
      S->update(Kind, F, Sym);
    } else {
      // niether symbol is week. They conflict.
      reportDuplicate(S, F);
    }
  }
  return S;
}

Symbol *SymbolTable::addUndefinedFunction(StringRef Name) {
  Symbol *S;
  bool WasInserted;
  std::tie(S, WasInserted) = insert(Name);
  if (WasInserted)
    S->update(Symbol::UndefinedFunctionKind);
  else if (!S->isFunction())
    error("symbol type mismatch: " + Name);
  return S;
}

Symbol *SymbolTable::addUndefined(InputFile *F, const WasmSymbol *Sym) {
  DEBUG(dbgs() << "addUndefined: " << displayName(Sym->Name) << "\n");
  Symbol *S;
  bool WasInserted;
  Symbol::Kind Kind = Symbol::UndefinedFunctionKind;
  if (Sym->Type == WasmSymbol::SymbolType::GLOBAL_IMPORT)
    Kind = Symbol::UndefinedGlobalKind;
  std::tie(S, WasInserted) = insert(Sym->Name);
  if (WasInserted) {
    S->update(Kind, F, Sym);
  } else if (S->isLazy()) {
    DEBUG(dbgs() << "resolved by existing lazy\n");
    auto *AF = cast<ArchiveFile>(S->getFile());
    AF->addMember(&S->getArchiveSymbol());
  } else if (S->isDefined()) {
    DEBUG(dbgs() << "resolved by existing\n");
    checkSymbolTypes(S, F, Sym);
  }
  return S;
}

void SymbolTable::addLazy(ArchiveFile *F, const Archive::Symbol *Sym) {
  DEBUG(dbgs() << "addLazy: " << displayName(Sym->getName()) << "\n");
  StringRef Name = Sym->getName();
  Symbol *S;
  bool WasInserted;
  std::tie(S, WasInserted) = insert(Name);
  if (WasInserted) {
    S->update(Symbol::LazyKind, F);
    S->setArchiveSymbol(*Sym);
  } else if (S->isUndefined() && !S->isLazy()) {
    // There is an existing undefined symbol.  The can load from the
    // archive.
    DEBUG(dbgs() << "replacing existing undefined\n");
    F->addMember(Sym);
  }
}

} // namespace wasm
} // namespace lld
