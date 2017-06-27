//===- InputFiles.cpp -----------------------------------------------------===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "InputFiles.h"
#include "SymbolTable.h"
#include "Config.h"
#include "Driver.h"
#include "Error.h"
#include "Memory.h"
#include "Strings.h"
#include "llvm/Object/Binary.h"
#include "llvm/Object/Wasm.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "lld"

using namespace lld;
using namespace lld::wasm;

using namespace llvm;
using namespace llvm::object;
using namespace llvm::wasm;

namespace lld {
namespace wasm {

Optional<MemoryBufferRef> readFile(StringRef Path) {
  log("Loading: " + Path);

  auto MBOrErr = MemoryBuffer::getFile(Path);
  if (auto EC = MBOrErr.getError()) {
    error("cannot open " + Path + ": " + EC.message());
    return None;
  }
  std::unique_ptr<MemoryBuffer> &MB = *MBOrErr;
  MemoryBufferRef MBRef = MB->getMemBufferRef();
  make<std::unique_ptr<MemoryBuffer>>(std::move(MB)); // take MB ownership

  return MBRef;
}

void ObjectFile::dumpInfo() const {
  log("reloc info for: " + getName());
  log("        FunctionIndexOffset : " + Twine(FunctionIndexOffset));
  log("     FunctionImports.size() : " + Twine(FunctionImports.size()));
  log("           TableIndexOffset : " + Twine(TableIndexOffset));
  log("          GlobalIndexOffset : " + Twine(GlobalIndexOffset));
  log("                 DataOffset : " + Twine(DataOffset));
  log("       GlobalImports.size() : " + Twine(GlobalImports.size()));
}

bool ObjectFile::isImportedFunction(uint32_t index) const {
  return index < FunctionImports.size();
}

bool ObjectFile::isImportedGlobal(uint32_t index) const {
  return index < GlobalImports.size();
}

bool ObjectFile::isResolvedFunctionImport(uint32_t index) const {
  if (!isImportedFunction(index))
    return false;
  const WasmSymbol *WasmSym = FunctionImports[index];
  Symbol* Sym = Symtab->find(WasmSym->Name);
  return Sym->isDefined();
}

bool ObjectFile::isResolvedGlobalImport(uint32_t index) const {
  if (!isImportedGlobal(index))
    return false;
  const WasmSymbol *WasmSym = GlobalImports[index];
  Symbol* Sym = Symtab->find(WasmSym->Name);
  return Sym->isDefined();
}

int32_t ObjectFile::getGlobalAddress(uint32_t index) const {
  if (isImportedGlobal(index)) {
    return 0;
  } else {
    index -= GlobalImports.size();
    const WasmGlobal &Global = WasmObj->globals()[index];
    assert(Global.Type == WASM_TYPE_I32);
    return Global.InitExpr.Value.Int32 + DataOffset;
  }
}

uint32_t ObjectFile::relocateFunctionIndex(uint32_t original) const {
  if (isImportedFunction(original)) {
    const WasmSymbol *WasmSym = FunctionImports[original];
    Symbol* Sym = Symtab->find(WasmSym->Name);
    assert(Sym && "imported symbol not found in symbol table");
    return Sym->getOutputIndex();
  } else {
    return original + FunctionIndexOffset;
  }
}

uint32_t ObjectFile::relocateTypeIndex(uint32_t original) const {
  assert(TypeMap.count(original) > 0);
  return TypeMap.find(original)->second;
}

uint32_t ObjectFile::relocateTableIndex(uint32_t original) const {
  return original + TableIndexOffset;
}

uint32_t ObjectFile::relocateGlobalIndex(uint32_t original) const {
  if (isImportedGlobal(original)) {
    const WasmSymbol *WasmSym = GlobalImports[original];
    Symbol* Sym = Symtab->find(WasmSym->Name);
    assert(Sym && "imported symbol not found in symbol table");
    return Sym->getOutputIndex();
  } else {
    return original + GlobalIndexOffset;
  }
}

uint32_t ObjectFile::relocateCodeOffset(uint32_t original) const {
  return original + CodeSectionOffset;
}

void ObjectFile::parse() {
  // Parse a memory buffer as a wasm file.
  DEBUG(dbgs() << "Parsing object: " << toString(this) << "\n");
  std::unique_ptr<Binary> Bin = check(createBinary(MB), toString(this));

  if (auto *Obj = dyn_cast<WasmObjectFile>(Bin.get())) {
    Bin.release();
    WasmObj.reset(Obj);
  } else {
    fatal(toString(this) + " is not a wasm file");
  }

  for (const SectionRef &Sec: WasmObj->sections()) {
    const WasmSection &Section = WasmObj->getWasmSection(Sec);
    if (Section.Type == WASM_SEC_CODE)
      CodeSection = &Section;
    else if (Section.Type == WASM_SEC_DATA)
      DataSection = &Section;
  }

  initializeSymbols();
}

void ObjectFile::initializeSymbols() {
  uint32_t NumSymbols = WasmObj->getNumberOfSymbols();
  Symbols.reserve(NumSymbols);

  for (const SymbolRef &Sym: WasmObj->symbols()) {
    const WasmSymbol &WasmSym = WasmObj->getWasmSymbol(Sym.getRawDataRefImpl());
    switch (WasmSym.Type) {
      case WasmSymbol::SymbolType::FUNCTION_IMPORT:
        FunctionImports.emplace_back(&WasmSym);
        createUndefined(WasmSym);
        break;
      case WasmSymbol::SymbolType::GLOBAL_IMPORT:
        GlobalImports.emplace_back(&WasmSym);
        createUndefined(WasmSym);
        break;
      case WasmSymbol::SymbolType::FUNCTION_EXPORT:
      case WasmSymbol::SymbolType::GLOBAL_EXPORT:
        createDefined(WasmSym);
        break;
      case WasmSymbol::SymbolType::DEBUG_FUNCTION_NAME:
        // These are internal only, no need to create linker symbols for them
        break;
    }
  }
}

Symbol *ObjectFile::createUndefined(const WasmSymbol &Sym) {
  Symbol *S = Symtab->addUndefined(this, &Sym);
  if (S)
    Symbols.push_back(S);
  return S;
}

Symbol *ObjectFile::createDefined(const WasmSymbol &Sym) {
  Symbol *S = Symtab->addDefined(this, &Sym);
  if (S)
    Symbols.push_back(S);
  return S;
}

void ArchiveFile::parse() {
  // Parse a MemoryBufferRef as an archive file.
  DEBUG(dbgs() << "Parsing library: " << toString(this) << "\n");
  File = check(Archive::create(MB), toString(this));

  // Read the symbol table to construct Lazy objects.
  for (const Archive::Symbol &Sym : File->symbols())
    Symtab->addLazy(this, &Sym);
}

void ArchiveFile::addMember(const Archive::Symbol *Sym) {
  const Archive::Child &C =
      check(Sym->getMember(),
            "could not get the member for symbol " + Sym->getName());

  DEBUG(dbgs() << "loading lazy: " << displayName(Sym->getName()) << "\n");
  DEBUG(dbgs() << "from archive: " << toString(this) << "\n");
  //DEBUG(dbgs() << "loading symbol from object symbol: " << C.getName() << "\n");
  MemoryBufferRef MB =
      check(C.getMemoryBufferRef(),
            "could not get the buffer for the member defining symbol " +
                Sym->getName());
  Driver->addArchiveBuffer(MB, Sym->getName(), ParentName);
}

} // namespace wasm
} // namespace lld

// Returns the last element of a path, which is supposed to be a filename.
static StringRef getBasename(StringRef Path) {
  size_t Pos = Path.find_last_of("\\/");
  if (Pos == StringRef::npos)
    return Path;
  return Path.substr(Pos + 1);
}

// Returns a string in the format of "foo.obj" or "foo.obj(bar.lib)".
std::string lld::toString(wasm::InputFile *File) {
  if (!File)
    return "(internal)";

  if (File->ParentName.empty())
    return File->getName().lower();

  std::string Res =
      (getBasename(File->ParentName) + "(" + getBasename(File->getName()) + ")")
          .str();
  return StringRef(Res).lower();
}
