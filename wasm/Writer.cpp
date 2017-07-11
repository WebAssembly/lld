//===- Writer.cpp ---------------------------------------------------------===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Config.h"
#include "Error.h"
#include "Memory.h"
#include "SymbolTable.h"
#include "Writer.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/LEB128.h"
#include <cstdarg>

#define DEBUG_TYPE "lld"

using namespace llvm;
using namespace llvm::wasm;
using namespace lld;
using namespace lld::wasm;

namespace {

// Needed for WasmSignatureDenseMapInfo
bool operator==(const WasmSignature& LHS, const WasmSignature& RHS) {
  return LHS.ReturnType == RHS.ReturnType && LHS.ParamTypes == RHS.ParamTypes;
}

// Traits for using WasmSignature in a DenseMap.
struct WasmSignatureDenseMapInfo {
  static WasmSignature getEmptyKey() {
    WasmSignature Sig;
    Sig.ReturnType = 1;
    return Sig;
  }
  static WasmSignature getTombstoneKey() {
    WasmSignature Sig;
    Sig.ReturnType = 2;
    return Sig;
  }
  static unsigned getHashValue(const WasmSignature &Sig) {
    uintptr_t Value = 0;
    Value += DenseMapInfo<int32_t>::getHashValue(Sig.ReturnType);
    for (int32_t Param : Sig.ParamTypes)
      Value += DenseMapInfo<int32_t>::getHashValue(Param);
    return Value;
  }
  static bool isEqual(const WasmSignature &LHS,
                      const WasmSignature &RHS) {
    return LHS == RHS;
  }
};

// The writer writes a SymbolTable result to a file.
class Writer {
public:
  Writer(SymbolTable *T) : Symtab(T) {}
  void run();

private:
  void openFile();

  void assignSymbolIndexes();
  void calculateImports();
  void calculateOffsets();
  void calculateTypes();
  void layoutMemory();

  void writeHeader();
  void writeSections(raw_fd_ostream &OS);

  // Builtin sections
  void writeTypeSection(raw_fd_ostream &OS);
  void writeFunctionSection(raw_fd_ostream &OS);
  void writeTableSection(raw_fd_ostream &OS);
  void writeGlobalSection(raw_fd_ostream &OS);
  void writeExportSection(raw_fd_ostream &OS);
  void writeImportSection(raw_fd_ostream &OS);
  void writeMemorySection(raw_fd_ostream &OS);
  void writeElemSection(raw_fd_ostream &OS);
  void writeStartSection(raw_fd_ostream &OS);
  void writeCodeSection(raw_fd_ostream &OS);
  void writeDataSection(raw_fd_ostream &OS);

  // Custom sections
  void writeRelocSections(raw_fd_ostream &OS);
  void writeLinkingSection(raw_fd_ostream &OS);
  void writeNameSection(raw_fd_ostream &OS);

  void applyCodeRelocations(const ObjectFile &File,
                            OwningArrayRef<uint8_t> &data);

  SectionBookkeeping writeSectionHeader(uint32_t Type, raw_fd_ostream &OS);
  void endSection(SectionBookkeeping& Section, raw_fd_ostream &OS) const;

  uint32_t DataSize = 0;
  uint32_t DataAlignment = 1;
  uint32_t TotalFunctions = 0;
  uint32_t TotalGlobals = 0;
  uint32_t TotalMemoryPages = 0;
  uint32_t TotalTableLength = 0;
  uint32_t TotalElements = 0;
  uint32_t TotalDataSegments = 0;
  uint32_t TotalCodeRelocations = 0;
  uint32_t TotalDataRelocations = 0;
  uint32_t InitialTableOffset = 0;

  SymbolTable *Symtab;
  std::vector<const WasmSignature*> Types;
  DenseMap<WasmSignature, int32_t, WasmSignatureDenseMapInfo> TypeIndices;
  std::vector<Symbol*> FunctionImports;
  std::vector<Symbol*> GlobalImports;
  std::unique_ptr<raw_fd_ostream> OS;
};

// Return the padding size to write a 32-bit value into a 5-byte ULEB128.
unsigned PaddingFor5ByteULEB128(uint32_t X) {
  return X == 0 ? 4 : (4u - (31u - countLeadingZeros(X)) / 7u);
}

// Return the padding size to write a 32-bit value into a 5-byte SLEB128.
unsigned PaddingFor5ByteSLEB128(int32_t X) {
  return 5 - getSLEB128Size(X);
}

void debug_print(const char* fmt, ...) {
  if (Config->Verbose) {
    fprintf(stderr, "lld: ");
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
  }
}

const char* section_type_to_str(uint32_t SectionType) {
#define ECase(X)                 \
  case WASM_SEC_##X:             \
    return #X;
  switch (SectionType) {
    ECase(CUSTOM);
    ECase(TYPE);
    ECase(IMPORT);
    ECase(FUNCTION);
    ECase(TABLE);
    ECase(MEMORY);
    ECase(GLOBAL);
    ECase(EXPORT);
    ECase(START);
    ECase(ELEM);
    ECase(CODE);
    ECase(DATA);
  default:
    fatal("invalid section type");
    return nullptr;
  }
}

const char* value_type_to_str(int32_t Type) {
  switch (Type) {
  case WASM_TYPE_I32:
    return "i32";
  case WASM_TYPE_I64:
    return "i64";
  case WASM_TYPE_F32:
    return "f32";
  case WASM_TYPE_F64:
    return "f64";
  default:
    fatal("invalid value type: " + Twine(Type));
    return nullptr;
  }
}

void debug_write(raw_ostream& OS, const char* msg, const char* fmt=NULL, ...) {
  DEBUG(
    fprintf(stderr, "%08" PRIx64 ": %s", OS.tell(), msg);
    if (fmt) {
      fprintf(stderr, " [");
      va_list ap;
      va_start(ap, fmt);
      vfprintf(stderr, fmt, ap);
      va_end(ap);
      fprintf(stderr, "]");
    }
    fprintf(stderr, "\n");
  );
}

void write_u8(uint8_t byte, raw_ostream& OS, const char* msg) {
  OS << byte;
}

void write_u32(uint32_t Number, raw_ostream& OS, const char* msg) {
  debug_write(OS, msg, "%x", Number);
  support::endian::Writer<support::little>(OS).write(Number);
}

void write_uleb128(uint32_t Number, raw_ostream& OS, const char* msg) {
  if (msg)
    debug_write(OS, msg, "%x", Number);
  encodeULEB128(Number, OS);
}

void write_uleb128_padded(uint32_t Number, raw_ostream& OS, const char* msg) {
  if (msg)
    debug_write(OS, msg);
  unsigned Padding = PaddingFor5ByteULEB128(Number);
  encodeULEB128(Number, OS, Padding);
}

void write_sleb128(int32_t Number, raw_ostream& OS, const char* msg) {
  if (msg)
    debug_write(OS, msg, "%x", Number);
  encodeSLEB128(Number, OS);
}

void write_bytes(const char* bytes, uint32_t count, raw_ostream& OS, const char* msg) {
  if (msg)
    debug_write(OS, msg);
  OS.write(bytes, count);
}

void write_str(const StringRef String, raw_ostream& OS, const char* msg) {
  debug_write(OS, msg, "str[%d]: %.*s", String.size(), String.size(), String.data());
  write_uleb128(String.size(), OS, nullptr);
  write_bytes(String.data(), String.size(), OS, nullptr);
}

void write_value_type(int32_t Type, raw_ostream& OS, const char* msg) {
  debug_write(OS, msg, "type: %s", value_type_to_str(Type));
  write_sleb128(Type, OS, nullptr);
}

void write_sig(const WasmSignature &Sig, raw_ostream& OS) {
  write_sleb128(WASM_TYPE_FUNC, OS, "signature type");
  write_uleb128(Sig.ParamTypes.size(), OS, "param count");
  for (int32_t ParamType: Sig.ParamTypes) {
    write_value_type(ParamType, OS, "param type");
  }
  if (Sig.ReturnType == WASM_TYPE_NORESULT) {
    write_uleb128(0, OS, "result count");
  } else {
    write_uleb128(1, OS, "result count");
    write_value_type(Sig.ReturnType, OS, "result type");
  }
}

static void write_init_expr(const WasmInitExpr& InitExpr, raw_fd_ostream& OS) {
  write_u8(InitExpr.Opcode, OS, "opcode");
  switch (InitExpr.Opcode) {
  case WASM_OPCODE_I32_CONST:
    write_sleb128(InitExpr.Value.Int32, OS, "literal (i32)");
    break;
  case WASM_OPCODE_I64_CONST:
    write_sleb128(InitExpr.Value.Int64, OS, "literal (i64)");
    break;
  case WASM_OPCODE_GET_GLOBAL:
    write_uleb128(InitExpr.Value.Global, OS, "literal (global index)");
    break;
  default:
    fatal("unknown opcode in init expr: " + Twine(InitExpr.Opcode));
    break;
  }
  write_u8(WASM_OPCODE_END, OS, "opcode:end");
}

static void write_limits(const WasmLimits& Limits, raw_fd_ostream& OS) {
  write_uleb128(Limits.Flags, OS, "limits flags");
  write_uleb128(Limits.Initial, OS, "limits initial");
  if (Limits.Flags & WASM_LIMITS_FLAG_HAS_MAX)
    write_uleb128(Limits.Maximum, OS, "limits max");
}

static void write_global(const WasmGlobal& Global, raw_fd_ostream& OS) {
  write_value_type(Global.Type, OS, "global type");
  write_uleb128(Global.Mutable, OS, "global mutable");
  write_init_expr(Global.InitExpr, OS);
}

static void write_import(const WasmImport& Import, raw_fd_ostream& OS) {
  write_str(Import.Module, OS, "import module name");
  write_str(Import.Field, OS, "import field name");
  write_u8(Import.Kind, OS, "import kind");
  switch (Import.Kind) {
    case WASM_EXTERNAL_FUNCTION:
      write_uleb128(Import.SigIndex, OS, "import sig index");
      break;
    case WASM_EXTERNAL_GLOBAL:
      write_value_type(Import.Global.Type, OS, "import global type");
      write_uleb128(Import.Global.Mutable, OS, "import global mutable");
      break;
    case WASM_EXTERNAL_MEMORY:
      write_limits(Import.Memory, OS);
      break;
    default:
      fatal("unsupported import type: " + Twine(Import.Kind));
      break;
  }
}

static void write_export(const WasmExport& Export, raw_fd_ostream& OS) {
  write_str(Export.Name, OS, "export name");
  write_u8(Export.Kind, OS, "export kind");
  switch (Export.Kind) {
    case WASM_EXTERNAL_FUNCTION:
      write_uleb128(Export.Index, OS, "function index");
      break;
    case WASM_EXTERNAL_GLOBAL:
      write_sleb128(Export.Index, OS, "global index");
      break;
    case WASM_EXTERNAL_MEMORY:
      write_sleb128(Export.Index, OS, "memory index");
      break;
    default:
      fatal("unsupported export type: " + Twine(Export.Kind));
      break;
  }
}

SectionBookkeeping Writer::writeSectionHeader(uint32_t Type,
                                                  raw_fd_ostream &OS) {
  SectionBookkeeping Section;
  debug_write(OS, "section type", "%s", section_type_to_str(Type));
  write_uleb128(Type, OS, nullptr);
  Section.SizeOffset = OS.tell();
  write_uleb128_padded(0, OS, "section size");
  Section.ContentsOffset = OS.tell();
  return Section;
}

void Writer::endSection(SectionBookkeeping &Section,
                        raw_fd_ostream &OS) const {
  uint64_t End = OS.tell();
  uint64_t Size = End - Section.ContentsOffset;
  OS.seek(Section.SizeOffset);
  write_uleb128_padded(Size, OS, "fixup section size");
  OS.seek(End);
}

void Writer::writeImportSection(raw_fd_ostream& OS) {
  uint32_t TotalImports = FunctionImports.size() + GlobalImports.size();
  if (Config->ImportMemory)
    TotalImports++;

  if (TotalImports == 0)
    return;

  SectionBookkeeping Section = writeSectionHeader(WASM_SEC_IMPORT, OS);
  write_uleb128(TotalImports, OS, "import count");

  for (Symbol *Sym: FunctionImports) {
    WasmImport Import;
    Import.Module = "env";
    Import.Field = Sym->getName();
    Import.Kind = WASM_EXTERNAL_FUNCTION;
    assert(isa<ObjectFile>(Sym->getFile()));
    ObjectFile* Obj = dyn_cast<ObjectFile>(Sym->getFile());
    Import.SigIndex = Obj->relocateTypeIndex(Sym->getFunctionTypeIndex());
    write_import(Import, OS);
  }

  if (Config->ImportMemory) {
    WasmImport Import;
    Import.Module = "env";
    Import.Field = "memory";
    Import.Kind = WASM_EXTERNAL_MEMORY;
    Import.Memory.Flags = 0;
    Import.Memory.Initial = TotalMemoryPages;
    write_import(Import, OS);
  }

  for (Symbol *Sym: GlobalImports) {
    WasmImport Import;
    Import.Module = "env";
    Import.Field = Sym->getName();
    Import.Kind = WASM_EXTERNAL_GLOBAL;
    Import.Global.Mutable = false;
    assert(isa<ObjectFile>(Sym->getFile()));
    // TODO(sbc): Set type of this import
    //ObjectFile* Obj = dyn_cast<ObjectFile>(Sym->getFile());
    Import.Global.Type = WASM_TYPE_I32; //Sym->getGlobalType();
    write_import(Import, OS);
  }

  endSection(Section, OS);
}

void Writer::writeTypeSection(raw_fd_ostream& OS) {
  SectionBookkeeping Section = writeSectionHeader(WASM_SEC_TYPE, OS);
  write_uleb128(Types.size(), OS, "type count");
  for (const WasmSignature *Sig : Types) {
    write_sig(*Sig, OS);
  }
  endSection(Section, OS);
}

void Writer::writeFunctionSection(raw_fd_ostream& OS) {
  if (!TotalFunctions)
    return;
  SectionBookkeeping Section = writeSectionHeader(WASM_SEC_FUNCTION, OS);
  write_uleb128(TotalFunctions, OS, "function count");
  for (ObjectFile *File: Symtab->ObjectFiles) {
    for (uint32_t Sig: File->getWasmObj()->functionTypes()) {
      write_uleb128(File->relocateTypeIndex(Sig), OS, "sig index");
    }
  }
  endSection(Section, OS);
}

void Writer::writeMemorySection(raw_fd_ostream& OS) {
  if (Config->ImportMemory)
    return;

  SectionBookkeeping Section = writeSectionHeader(WASM_SEC_MEMORY, OS);

  write_uleb128(1, OS, "memory count");
  write_uleb128(0, OS, "memory limits flags");
  write_uleb128(TotalMemoryPages, OS, "initial pages");

  endSection(Section, OS);
}

void Writer::writeGlobalSection(raw_fd_ostream& OS) {
  SectionBookkeeping Section = writeSectionHeader(WASM_SEC_GLOBAL, OS);

  write_uleb128(TotalGlobals, OS, "global count");
  for (auto& Pair: Config->SyntheticGlobals) {
    WasmGlobal& Global = Pair.second;
    write_global(Global, OS);
  }

  if (Config->Relocatable) {
    for (ObjectFile *File: Symtab->ObjectFiles) {
      uint32_t GlobalIndex = File->GlobalImports.size();
      for (const WasmGlobal &Global: File->getWasmObj()->globals()) {
        WasmGlobal RelocatedGlobal(Global);
        if (Global.Type != WASM_TYPE_I32)
          fatal("unsupported global type: " + Twine(Global.Type));
        if (Global.InitExpr.Opcode != WASM_OPCODE_I32_CONST)
          fatal("unsupported global init opcode: " +
                Twine(Global.InitExpr.Opcode));
        RelocatedGlobal.InitExpr.Value.Int32 = File->getGlobalAddress(GlobalIndex);
        write_global(RelocatedGlobal, OS);
        GlobalIndex++;
      }
    }
  }
  endSection(Section, OS);
}

void Writer::writeTableSection(raw_fd_ostream& OS) {
  if (TotalTableLength == InitialTableOffset)
    return;
  SectionBookkeeping Section = writeSectionHeader(WASM_SEC_TABLE, OS);
  write_uleb128(1, OS, "table count");
  write_sleb128(WASM_TYPE_ANYFUNC, OS, "table type");
  write_uleb128(WASM_LIMITS_FLAG_HAS_MAX, OS, "table flags");
  write_uleb128(TotalTableLength, OS, "table initial size");
  write_uleb128(TotalTableLength, OS, "table max size");
  endSection(Section, OS);
}

void Writer::writeExportSection(raw_fd_ostream& OS) {
  // Memory is and main function are exported for executables.
  bool ExportMemory = !Config->Relocatable && !Config->ImportMemory;
  bool ExportMain = !Config->Relocatable;
  bool ExportOther = Config->Relocatable;

  uint32_t NumExports = 0;

  if (ExportMemory)
    NumExports += 1;

  if (ExportMain)
    NumExports += 1;

  if (ExportOther) {
    for (ObjectFile *File : Symtab->ObjectFiles)
      for (const Symbol *Sym : File->getSymbols())
        if (Sym->isDefined()) NumExports++;
  }

  if (!NumExports)
    return;

  SectionBookkeeping Section = writeSectionHeader(WASM_SEC_EXPORT, OS);
  write_uleb128(NumExports, OS, "export count");

  if (ExportMemory) {
    WasmExport MemoryExport;
    MemoryExport.Name = "memory";
    MemoryExport.Kind = WASM_EXTERNAL_MEMORY;
    MemoryExport.Index = 0;
    write_export(MemoryExport, OS);
  }

  if (ExportMain) {
    Symbol* Sym = Symtab->find(Config->Entry);
    if (!Sym->isFunction())
      fatal("entry point is not a function: " + Sym->getName());

    WasmExport MainExport;
    MainExport.Name = Config->ExportEntryAs;
    MainExport.Kind = WASM_EXTERNAL_FUNCTION;
    MainExport.Index = Sym->getOutputIndex();
    write_export(MainExport, OS);
  }

  if (ExportOther) {
    for (ObjectFile *File : Symtab->ObjectFiles) {
      for (const Symbol *Sym : File->getSymbols()) {
        if (Sym->isUndefined())
          continue;
        log("Export: " + Sym->getName());
        WasmExport Export;
        Export.Name = Sym->getName();
        Export.Index = Sym->getOutputIndex();
        if (Sym->isFunction())
          Export.Kind = WASM_EXTERNAL_FUNCTION;
        else
          Export.Kind = WASM_EXTERNAL_GLOBAL;
        write_export(Export, OS);
      }
    }

    // TODO(sbc): Export local symbols too, Even though they are not part
    // of the symbol table?
  }

  endSection(Section, OS);
}

void Writer::writeStartSection(raw_fd_ostream& OS) {
  /*
  if (Config->Entry.empty())
    return;

  Symbol* Sym = Symtab->find(Config->Entry);
  assert(isa<ObjectFile>(Sym->getFile()));
  ObjectFile* Obj = dyn_cast<ObjectFile>(Sym->getFile());

  SectionBookkeeping Section = writeSectionHeader(WASM_SEC_START, OS);
  write_uleb128(Obj->relocateFunctionIndex(Sym->getIndex()), OS, "start function index");
  endSection(Section, OS);
  */
}

void Writer::writeElemSection(raw_fd_ostream& OS) {
  if (!TotalElements)
    return;
  SectionBookkeeping Section = writeSectionHeader(WASM_SEC_ELEM, OS);
  write_uleb128(1, OS, "segment count");
  write_uleb128(0, OS, "table index");
  WasmInitExpr InitExpr;
  InitExpr.Opcode = WASM_OPCODE_I32_CONST;
  InitExpr.Value.Int32 = InitialTableOffset;
  write_init_expr(InitExpr, OS);
  write_uleb128(TotalElements, OS, "elem count");

  for (ObjectFile *File: Symtab->ObjectFiles) {
    for (const WasmElemSegment &Segment: File->getWasmObj()->elements()) {
      for (uint64_t FunctionIndex: Segment.Functions) {
        write_uleb128(File->relocateFunctionIndex(FunctionIndex), OS,
                      "function index");
      }
    }
  }
  endSection(Section, OS);
}

void Writer::applyCodeRelocations(const ObjectFile &File, OwningArrayRef<uint8_t> &Data) {
  DEBUG(dbgs() << "applyCodeRelocations: " << File.getName() << "\n");
  for (const WasmRelocation &Reloc: File.CodeSection->Relocations) {
    int64_t NewValue = 0;
    switch (Reloc.Type) {
    case R_WEBASSEMBLY_TYPE_INDEX_LEB:
      NewValue = File.relocateTypeIndex(Reloc.Index);
      break;
    case R_WEBASSEMBLY_FUNCTION_INDEX_LEB:
      NewValue = File.relocateFunctionIndex(Reloc.Index);
      break;
    case R_WEBASSEMBLY_TABLE_INDEX_I32:
    case R_WEBASSEMBLY_TABLE_INDEX_SLEB:
      NewValue = File.relocateTableIndex(Reloc.Index);
      break;
    case R_WEBASSEMBLY_GLOBAL_INDEX_LEB:
      NewValue = File.relocateGlobalIndex(Reloc.Index);
      break;
    case R_WEBASSEMBLY_GLOBAL_ADDR_LEB:
    case R_WEBASSEMBLY_GLOBAL_ADDR_SLEB:
    case R_WEBASSEMBLY_GLOBAL_ADDR_I32:
      NewValue = File.getGlobalAddress(Reloc.Index) + Reloc.Addend;
      break;
    default:
      fatal("unhandled relocation type: " + Twine(Reloc.Type));
      break;
    }

    DEBUG(dbgs() << "reloc: type=" << Reloc.Type << " index=" << Reloc.Index
                 << " offset=" << Reloc.Offset << " new=" << NewValue << "\n");

    uint8_t *Location = Data.data() + Reloc.Offset;

    switch (Reloc.Type) {
    case R_WEBASSEMBLY_TYPE_INDEX_LEB:
    case R_WEBASSEMBLY_FUNCTION_INDEX_LEB:
      assert(decodeULEB128(Location) == Reloc.Index);
    case R_WEBASSEMBLY_GLOBAL_ADDR_LEB:
    case R_WEBASSEMBLY_GLOBAL_INDEX_LEB: {
      unsigned Padding = PaddingFor5ByteULEB128(NewValue);
      assert(NewValue >= 0 && NewValue <= UINT32_MAX);
      encodeULEB128(NewValue, Location, Padding);
      break;
    }
    case R_WEBASSEMBLY_TABLE_INDEX_SLEB:
      assert(decodeSLEB128(Location) == Reloc.Index);
    case R_WEBASSEMBLY_GLOBAL_ADDR_SLEB: {
      unsigned Padding = PaddingFor5ByteSLEB128(NewValue);
      assert(NewValue >= INT32_MIN && NewValue <= INT32_MAX);
      encodeSLEB128(NewValue, Location, Padding);
      break;
    }
    case R_WEBASSEMBLY_TABLE_INDEX_I32:
    case R_WEBASSEMBLY_GLOBAL_ADDR_I32:
      llvm_unreachable("unimplemented");
      break;
    }
  }
}

void Writer::writeCodeSection(raw_fd_ostream& OS) {
  if (!TotalFunctions)
    return;
  SectionBookkeeping Section = writeSectionHeader(WASM_SEC_CODE, OS);
  write_uleb128(TotalFunctions, OS, "function count");
  uint32_t ContentsStart = OS.tell();
  for (ObjectFile *File: Symtab->ObjectFiles) {
    if (!File->CodeSection)
      continue;
    File->CodeSectionOffset = OS.tell() - ContentsStart;

    // Make copy of the section content so that we can apply relocations
    OwningArrayRef<uint8_t> Content(File->CodeSection->Content);
    applyCodeRelocations(*File, Content);

    // Payload doesn't include the intial function count
    unsigned PayloadOffset = 0;
    decodeULEB128(Content.data(), &PayloadOffset);

    const char* Payload = reinterpret_cast<const char *>(Content.data());
    write_bytes(Payload + PayloadOffset,
                Content.size() - PayloadOffset, OS, "section data");
  }
  endSection(Section, OS);
}

void Writer::writeDataSection(raw_fd_ostream& OS) {
  if (!TotalDataSegments)
    return;

  SectionBookkeeping Section = writeSectionHeader(WASM_SEC_DATA, OS);
  write_uleb128(TotalDataSegments, OS, "data segment count");

  for (ObjectFile *File: Symtab->ObjectFiles) {
    assert(File->getWasmObj()->dataSegments().size() <= 1);
    for (const WasmDataSegment &Segment: File->getWasmObj()->dataSegments()) {
      write_uleb128(Segment.Index, OS, "memory index");
      write_uleb128(WASM_OPCODE_I32_CONST, OS, "opcode:i32const");
      uint32_t NewOffset = Segment.Offset.Value.Int32 + File->DataOffset;
      write_sleb128(NewOffset, OS, "memory offset");
      write_uleb128(WASM_OPCODE_END, OS, "opcode:end");
      write_uleb128(Segment.Content.size(), OS, "segment size");
      write_bytes(reinterpret_cast<const char *>(Segment.Content.data()), Segment.Content.size(), OS, "memory data");
    }
  }

  endSection(Section, OS);
}

void Writer::writeRelocSections(raw_fd_ostream& OS) {
  if (!TotalCodeRelocations)
    return;

  SectionBookkeeping Section = writeSectionHeader(WASM_SEC_CUSTOM, OS);
  write_str("reloc.CODE", OS, "reloc section string name");
  write_uleb128(WASM_SEC_CODE, OS, "reloc section");
  write_uleb128(TotalCodeRelocations, OS, "reloc section");
  for (const ObjectFile *File: Symtab->ObjectFiles) {
    if (!File->CodeSection)
      continue;
    for (const WasmRelocation &Reloc: File->CodeSection->Relocations) {
      write_uleb128(Reloc.Type, OS, "reloc type");
      write_uleb128(File->relocateCodeOffset(Reloc.Offset), OS, "reloc offset");

      switch (Reloc.Type) {
      case R_WEBASSEMBLY_TYPE_INDEX_LEB:
        write_uleb128(File->relocateTypeIndex(Reloc.Index), OS, "reloc index");
        break;
      case R_WEBASSEMBLY_FUNCTION_INDEX_LEB:
        write_uleb128(File->relocateFunctionIndex(Reloc.Index), OS, "reloc index");
        break;
      case R_WEBASSEMBLY_TABLE_INDEX_I32:
      case R_WEBASSEMBLY_TABLE_INDEX_SLEB:
        write_uleb128(File->relocateTableIndex(Reloc.Index), OS, "reloc index");
        break;
      case R_WEBASSEMBLY_GLOBAL_ADDR_LEB:
      case R_WEBASSEMBLY_GLOBAL_ADDR_SLEB:
      case R_WEBASSEMBLY_GLOBAL_ADDR_I32:
      case R_WEBASSEMBLY_GLOBAL_INDEX_LEB:
        write_uleb128(File->relocateGlobalIndex(Reloc.Index), OS, "reloc index");
        break;
      default:
        llvm_unreachable("invalid reloc type");
        fatal("unhandled relocation type: " + Twine(Reloc.Type));
      }

      switch (Reloc.Type) {
      case R_WEBASSEMBLY_GLOBAL_ADDR_LEB:
      case R_WEBASSEMBLY_GLOBAL_ADDR_SLEB:
      case R_WEBASSEMBLY_GLOBAL_ADDR_I32:
        write_uleb128(Reloc.Addend, OS, "reloc addend");
        break;
      default:
        break;
      }
    }
  }

  endSection(Section, OS);
}

void Writer::writeLinkingSection(raw_fd_ostream& OS) {
  SectionBookkeeping Section = writeSectionHeader(WASM_SEC_CUSTOM, OS);
  write_str("linking", OS, "custom section name");

  SectionBookkeeping SubSection = writeSectionHeader(WASM_DATA_SIZE, OS);
  write_uleb128(DataSize, OS, "data size");
  endSection(SubSection, OS);

  SubSection = writeSectionHeader(WASM_DATA_ALIGNMENT, OS);
  write_uleb128(DataAlignment, OS, "data alignment");
  endSection(SubSection, OS);

  endSection(Section, OS);
}

void Writer::writeNameSection(raw_fd_ostream& OS) {
  size_t FunctionNameCount = 0;
  for (ObjectFile *File: Symtab->ObjectFiles) {
    const WasmObjectFile* WasmFile = File->getWasmObj();
    for (object::SymbolRef Sym : WasmFile->symbols()) {
      const WasmSymbol &WasmSym = WasmFile->getWasmSymbol(Sym);
      if (WasmSym.Type != WasmSymbol::SymbolType::DEBUG_FUNCTION_NAME)
        continue;
      if (File->isResolvedFunctionImport(Sym.getValue()))
        continue;
      Symbol* S = Symtab->find(WasmSym.Name);
      if (S) {
        assert(S);
        if (S->WrittenToSymtab)
          continue;
        S->WrittenToSymtab = true;
      }
      FunctionNameCount++;
    }
  }

  SectionBookkeeping Section = writeSectionHeader(WASM_SEC_CUSTOM, OS);
  write_str("name", OS, "name section string name");
  SectionBookkeeping SubSection = writeSectionHeader(WASM_NAMES_FUNCTION, OS);
  write_uleb128(FunctionNameCount, OS, "name count");

  // We have to iterate through the inputs twice so that all the imports
  // appear first before any of the local function names.
  for (bool ImportedNames: { true, false }) {
    for (ObjectFile *File: Symtab->ObjectFiles) {
      const WasmObjectFile* WasmFile = File->getWasmObj();
      for (object::SymbolRef Sym : WasmFile->symbols()) {
        if (File->isImportedFunction(Sym.getValue()) != ImportedNames)
          continue;

        const WasmSymbol &WasmSym = WasmFile->getWasmSymbol(Sym);
        if (WasmSym.Type != WasmSymbol::SymbolType::DEBUG_FUNCTION_NAME)
          continue;
        if (File->isResolvedFunctionImport(Sym.getValue()))
          continue;
        Symbol* S = Symtab->find(WasmSym.Name);
        if (S) {
          if (!S->WrittenToSymtab)
            continue;
          S->WrittenToSymtab = false;
        }
        write_uleb128(File->relocateFunctionIndex(Sym.getValue()), OS, "func index");
        Expected<StringRef> NameOrError = Sym.getName();
        if (!NameOrError)
          fatal("error getting symbol name");
        write_str(*NameOrError, OS, "symbol name");
      }
    }
  }
  endSection(SubSection, OS);
  endSection(Section, OS);
}

void Writer::writeSections(raw_fd_ostream& OS) {
  writeTypeSection(OS);
  writeImportSection(OS);
  writeFunctionSection(OS);
  writeTableSection(OS);
  writeMemorySection(OS);
  writeGlobalSection(OS);
  writeExportSection(OS);
  writeStartSection(OS);
  writeElemSection(OS);
  writeCodeSection(OS);
  writeDataSection(OS);

  // Optional, custom sections for relocations and debug names
  if (Config->EmitRelocs || Config->Relocatable)
    writeRelocSections(OS);
  if (Config->Relocatable)
    writeLinkingSection(OS);
  if (!Config->StripDebug && !Config->StripAll)
    writeNameSection(OS);
}

void Writer::layoutMemory() {
  uint32_t MemoryPtr = 0;
  if (!Config->Relocatable) {
    MemoryPtr += Config->GlobalBase;
    debug_print("mem: global base = %d\n", Config->GlobalBase);
  }

  // Static data from input object files comes first
  MemoryPtr = alignTo(MemoryPtr, DataAlignment);
  for (ObjectFile *File: Symtab->ObjectFiles) {
    const WasmObjectFile* WasmFile = File->getWasmObj();
    uint32_t Size = WasmFile->linkingData().DataSize;
    if (Size) {
      MemoryPtr = alignTo(MemoryPtr, WasmFile->linkingData().DataAlignment);
      debug_print("mem: [%s] offset=%#x size=%d\n",
                  File->getName().str().c_str(), File->DataOffset, Size);
      File->DataOffset = MemoryPtr;
      MemoryPtr += Size;
    } else {
      debug_print("mem: no data\n");
    }
  }

  DataSize = MemoryPtr;
  if (!Config->Relocatable)
    DataSize -= Config->GlobalBase;
  debug_print("mem: static data = %d\n", DataSize);

  // Stack comes last
  if (!Config->Relocatable) {
    debug_print("mem: stack base  = %d\n", MemoryPtr);
    MemoryPtr += Config->ZStackSize;
    Config->SyntheticGlobals[0].second.InitExpr.Value.Int32 = MemoryPtr;
    debug_print("mem: stack top   = %d\n", MemoryPtr);
  }

  uint32_t MemSize = alignTo(MemoryPtr, WasmPageSize);
  TotalMemoryPages = MemSize / WasmPageSize;
  debug_print("mem: total size  = %d\n", MemSize);
  debug_print("mem: total pages = %d\n", TotalMemoryPages);
}

void Writer::calculateOffsets() {
  TotalGlobals = Config->SyntheticGlobals.size();
  TotalTableLength = InitialTableOffset;

  for (ObjectFile *File: Symtab->ObjectFiles) {
    const WasmObjectFile* WasmFile = File->getWasmObj();

    DataAlignment =
        std::max(DataAlignment, WasmFile->linkingData().DataAlignment);

    // Function Index
    File->FunctionIndexOffset =
        FunctionImports.size() - File->FunctionImports.size() + TotalFunctions;
    TotalFunctions += WasmFile->functions().size();

    // Global Index
    if (Config->Relocatable) {
      File->GlobalIndexOffset =
          GlobalImports.size() - File->GlobalImports.size() + TotalGlobals;
      TotalGlobals += WasmFile->globals().size();
    }

    // Memory
    if (WasmFile->memories().size()) {
      if (WasmFile->memories().size() > 1) {
        fatal(File->getName() + ": contains more than one memory");
      }
    }

    // Table
    uint32_t TableCount = WasmFile->tables().size();
    if (TableCount) {
      if (TableCount > 1)
        fatal(File->getName() + ": contains more than one table");
      File->TableIndexOffset = TotalTableLength;
      TotalTableLength += WasmFile->tables()[0].Limits.Initial;
    }

    // Elem
    uint32_t SegmentCount = WasmFile->elements().size();
    if (SegmentCount) {
      if (SegmentCount > 1) {
        fatal(File->getName() + ": contains more than element segment");
      } else {
        const WasmElemSegment &Segment = WasmFile->elements()[0];
        if (Segment.TableIndex != 0)
          fatal(File->getName() + ": unsupported table index");
        else if (Segment.Offset.Value.Int32 != 0)
          fatal(File->getName() + ": unsupported segment offset");
        else
          TotalElements += Segment.Functions.size();
      }
    }

    // Data
    TotalDataSegments += WasmFile->dataSegments().size();

    if (File->CodeSection)
      TotalCodeRelocations += File->CodeSection->Relocations.size();
    if (File->DataSection)
      TotalDataRelocations += File->DataSection->Relocations.size();
  }
}

void Writer::calculateImports() {
  for (ObjectFile *File : Symtab->ObjectFiles) {
    for (Symbol *Sym : File->getSymbols()) {
      if (Sym->hasOutputIndex() || Sym->isDefined() || Sym->isWeak())
        continue;

      if (Sym->isFunction()) {
        Sym->setOutputIndex(FunctionImports.size());
        FunctionImports.push_back(Sym);
      } else {
        Sym->setOutputIndex(GlobalImports.size());
        GlobalImports.push_back(Sym);
      }
    }
  }
}

void Writer::calculateTypes() {
  for (ObjectFile *File : Symtab->ObjectFiles) {
    int Index = 0;
    for (const WasmSignature &Sig: File->getWasmObj()->types()) {
      auto Pair = TypeIndices.insert(std::make_pair(Sig, Types.size()));
      if (Pair.second)
        Types.push_back(&Sig);

      // Now we map the input files index to the index in the linked output
      File->TypeMap[Index++] = Pair.first->second;
    }
  }
}

void Writer::assignSymbolIndexes() {
  for (ObjectFile *File : Symtab->ObjectFiles) {
    for (Symbol *Sym : File->getSymbols()) {
      if (Sym->hasOutputIndex() || !Sym->isDefined())
        continue;

      if (Sym->getFile() && isa<ObjectFile>(Sym->getFile())) {
        ObjectFile* Obj = dyn_cast<ObjectFile>(Sym->getFile());
        if (Sym->isFunction())
          Sym->setOutputIndex(Obj->FunctionIndexOffset + Sym->getFunctionIndex());
        else
          Sym->setOutputIndex(Obj->GlobalIndexOffset + Sym->getGlobalIndex());
      }
    }
  }
}

void Writer::run() {
  if (!Config->Relocatable)
    InitialTableOffset = 1;

  log("-- calculateTypes");
  calculateTypes();
  log("-- calculateImports");
  calculateImports();
  log("-- calculateOffsets");
  calculateOffsets();
  log("-- assignSymbolIndexes");
  assignSymbolIndexes();
  log("-- layoutMemory");
  layoutMemory();

  if (Config->Verbose)
    for (ObjectFile *File: Symtab->ObjectFiles)
      File->dumpInfo();

  log("-- openFile");
  openFile();
  if (ErrorCount)
    return;

  writeHeader();
  log("-- writeHeader");
  if (ErrorCount)
    return;

  log("-- writeSections");
  writeSections(*OS);
  if (ErrorCount)
    return;
}

// Open a result file.
void Writer::openFile() {
  log("writing: " + Config->OutputFile);
  ::remove(Config->OutputFile.str().c_str());
  std::error_code EC;
  OS = llvm::make_unique<raw_fd_ostream>(
      StringRef(Config->OutputFile), EC, sys::fs::OpenFlags::F_None);
  if (EC)
    error("failed to open " + Config->OutputFile + ": " + EC.message());
}

void Writer::writeHeader() {
  write_bytes(WasmMagic, sizeof(WasmMagic), *OS, "wasm magic");
  write_u32(WasmVersion, *OS, "wasm version");
}

} // anonymous namespace

namespace lld {
namespace wasm {

void writeResult(SymbolTable *T) {
  Writer(T).run();
}

} // namespace wasm
} // namespace lld
