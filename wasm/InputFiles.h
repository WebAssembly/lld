//===- InputFiles.h ---------------------------------------------*- C++ -*-===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLD_WASM_INPUT_FILES_H
#define LLD_WASM_INPUT_FILES_H

#include "lld/Core/LLVM.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Object/Archive.h"
#include "llvm/Object/Wasm.h"
#include "llvm/Support/MemoryBuffer.h"

#include <vector>

using llvm::object::WasmObjectFile;
using llvm::object::WasmSection;
using llvm::object::WasmSymbol;
using llvm::object::Archive;

namespace lld {
namespace wasm {

class Symbol;

class InputFile {
public:
  enum Kind {
    ObjectKind,
    ArchiveKind,
  };

  virtual ~InputFile() {}

  // Returns the filename.
  StringRef getName() const { return MB.getBufferIdentifier(); }

  std::vector<Symbol *> &getSymbols() { return Symbols; }

  // Reads a file (the constructor doesn't do that).
  virtual void parse() = 0;

  Kind kind() const { return FileKind; }

  // An archive file name if this file is created from an archive.
  StringRef ParentName;

protected:
  InputFile(Kind K, MemoryBufferRef M) : MB(M), FileKind(K) {}
  MemoryBufferRef MB;

  // List of all symbols referenced or defined by this file.
  std::vector<Symbol *> Symbols;

private:
  const Kind FileKind;
};

// .a file (ar archive)
class ArchiveFile : public InputFile {
public:
  explicit ArchiveFile(MemoryBufferRef M) : InputFile(ArchiveKind, M) {}
  static bool classof(const InputFile *F) { return F->kind() == ArchiveKind; }

  void addMember(const Archive::Symbol *Sym);

  void parse() override;

private:
  std::unique_ptr<Archive> File;
  llvm::DenseSet<uint64_t> Seen;
};

// .o file (wasm object file)
class ObjectFile : public InputFile {
public:
  explicit ObjectFile(MemoryBufferRef M) : InputFile(ObjectKind, M) {}
  static bool classof(const InputFile *F) { return F->kind() == ObjectKind; }

  Symbol *createDefined(const WasmSymbol &Sym);
  Symbol *createUndefined(const WasmSymbol &Sym);

  void parse() override;

  // Returns the underlying wasm file.
  const WasmObjectFile *getWasmObj() { return WasmObj.get(); }

  void dumpInfo() const;

  uint32_t relocateTypeIndex(uint32_t original) const;
  uint32_t relocateFunctionIndex(uint32_t original) const;
  uint32_t relocateGlobalIndex(uint32_t original) const;
  uint32_t relocateTableIndex(uint32_t original) const;
  uint32_t relocateDataLocation(uint32_t original) const;

  int32_t getGlobalAddress(uint32_t index) const;

  // Returns true if the given function index is an imported function,
  // as opposed to the locally defined function.
  bool isImportedFunction(uint32_t index) const;
  // Returns true if the given global index is an imported global,
  // as opposed to the locally defined (exported) global.
  bool isImportedGlobal(uint32_t index) const;
  // Return true if the given imported (undefined) function has been resolved
  // in the output binary (i.e. defined by another object).
  bool isResolvedFunctionImport(uint32_t index) const;
  // Return true if the given imported (undefined) global has been resolved
  // in the output binary (i.e. defined by another object).
  bool isResolvedGlobalImport(uint32_t index) const;

  int32_t FunctionIndexOffset = 0;
  int32_t GlobalIndexOffset = 0;
  int32_t TableIndexOffset = 0;
  uint32_t DataOffset = 0;
  const WasmSection *CodeSection = nullptr;
  const WasmSection *DataSection = nullptr;

  llvm::DenseMap<uint32_t, uint32_t> TypeMap;
  std::vector<StringRef> FunctionImports;
  std::vector<StringRef> GlobalImports;

private:
  std::unique_ptr<WasmObjectFile> WasmObj;

  void initializeSymbols();
};

// Opens a given file.
llvm::Optional<MemoryBufferRef> readFile(StringRef Path);

} // namespace wasm

std::string toString(wasm::InputFile *File);
} // namespace lld

#endif
