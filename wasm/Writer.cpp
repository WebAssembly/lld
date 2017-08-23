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

enum class RelocEncoding {
  Uleb128,
  Sleb128,
  I32,
};

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

  void applyRelocations(const ObjectFile &File, OwningArrayRef<uint8_t> &data,
                        const std::vector<WasmRelocation> &Relocs,
                        std::vector<WasmRelocation> &OutputRelocs,
                        uint32_t SectionOffset,
                        uint32_t OutputOffset);

  SectionBookkeeping writeSectionHeader(raw_fd_ostream &OS, uint32_t Type);
  void endSection(raw_fd_ostream &OS, SectionBookkeeping& Section) const;

  uint32_t DataSize = 0;
  uint32_t DataAlignment = 1;
  uint32_t TotalFunctions = 0;
  uint32_t TotalGlobals = 0;
  uint32_t TotalMemoryPages = 0;
  uint32_t TotalTableLength = 0;
  uint32_t TotalElements = 0;
  uint32_t TotalDataSegments = 0;
  uint32_t InitialTableOffset = 0;

  SymbolTable *Symtab;
  std::vector<const WasmSignature*> Types;
  DenseMap<WasmSignature, int32_t, WasmSignatureDenseMapInfo> TypeIndices;
  std::vector<Symbol*> FunctionImports;
  std::vector<Symbol*> GlobalImports;
  std::vector<WasmRelocation> CodeRelocations;
  std::vector<WasmRelocation> DataRelocations;
  std::unique_ptr<raw_fd_ostream> OS;
};

} // anonymous namespace

// Return the padding size to write a 32-bit value into a 5-byte ULEB128.
static unsigned paddingFor5ByteULEB128(uint32_t X) {
  return X == 0 ? 4 : (4u - (31u - countLeadingZeros(X)) / 7u);
}

// Return the padding size to write a 32-bit value into a 5-byte SLEB128.
static unsigned paddingFor5ByteSLEB128(int32_t X) {
  return 5 - getSLEB128Size(X);
}

static void debugPrint(const char* fmt, ...) {
  if (Config->Verbose) {
    fprintf(stderr, "lld: ");
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
  }
}

static const char* section_type_to_str(uint32_t SectionType) {
  switch (SectionType) {
  case WASM_SEC_CUSTOM:
    return "CUSTOM";
  case WASM_SEC_TYPE:
    return "TYPE";
  case WASM_SEC_IMPORT:
    return "IMPORT";
  case WASM_SEC_FUNCTION:
    return "FUNCTION";
  case WASM_SEC_TABLE:
    return "TABLE";
  case WASM_SEC_MEMORY:
    return "MEMORY";
  case WASM_SEC_GLOBAL:
    return "GLOBAL";
  case WASM_SEC_EXPORT:
    return "EXPORT";
  case WASM_SEC_START:
    return "START";
  case WASM_SEC_ELEM:
    return "ELEM";
  case WASM_SEC_CODE:
    return "CODE";
  case WASM_SEC_DATA:
    return "DATA";
  default:
    fatal("invalid section type");
    return nullptr;
  }
}

static const char* value_type_to_str(int32_t Type) {
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

static void debugWrite(raw_ostream &OS, const char *msg,
                        const char *fmt = NULL, ...) {
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

static void write_u8(raw_ostream& OS, uint8_t byte, const char* msg) {
  OS << byte;
}

static void write_u32(raw_ostream& OS, uint32_t Number, const char* msg) {
  debugWrite(OS, msg, "%x", Number);
  support::endian::Writer<support::little>(OS).write(Number);
}

static void write_uleb128(raw_ostream& OS, uint32_t Number, const char* msg) {
  if (msg)
    debugWrite(OS, msg, "%x", Number);
  encodeULEB128(Number, OS);
}

static void write_uleb128_padded(raw_ostream& OS, uint32_t Number,
                                 const char *msg) {
  if (msg)
    debugWrite(OS, msg);
  unsigned Padding = paddingFor5ByteULEB128(Number);
  encodeULEB128(Number, OS, Padding);
}

static void write_sleb128(raw_ostream& OS, int32_t Number, const char* msg) {
  if (msg)
    debugWrite(OS, msg, "%x", Number);
  encodeSLEB128(Number, OS);
}

static void write_bytes(raw_ostream &OS, const char *bytes, uint32_t count,
                        const char *msg) {
  if (msg)
    debugWrite(OS, msg);
  OS.write(bytes, count);
}

static void write_str(raw_ostream& OS, const StringRef String, const char* msg) {
  debugWrite(OS, msg, "str[%d]: %.*s", String.size(), String.size(), String.data());
  write_uleb128(OS, String.size(), nullptr);
  write_bytes(OS, String.data(), String.size(), nullptr);
}

static void write_value_type(raw_ostream& OS, int32_t Type, const char* msg) {
  debugWrite(OS, msg, "type: %s", value_type_to_str(Type));
  write_sleb128(OS, Type, nullptr);
}

static void write_sig(raw_ostream& OS, const WasmSignature &Sig) {
  write_sleb128(OS, WASM_TYPE_FUNC, "signature type");
  write_uleb128(OS, Sig.ParamTypes.size(), "param count");
  for (int32_t ParamType: Sig.ParamTypes) {
    write_value_type(OS, ParamType, "param type");
  }
  if (Sig.ReturnType == WASM_TYPE_NORESULT) {
    write_uleb128(OS, 0, "result count");
  } else {
    write_uleb128(OS, 1, "result count");
    write_value_type(OS, Sig.ReturnType, "result type");
  }
}

static void write_init_expr(raw_fd_ostream& OS, const WasmInitExpr& InitExpr) {
  write_u8(OS, InitExpr.Opcode, "opcode");
  switch (InitExpr.Opcode) {
  case WASM_OPCODE_I32_CONST:
    write_sleb128(OS, InitExpr.Value.Int32, "literal (i32)");
    break;
  case WASM_OPCODE_I64_CONST:
    write_sleb128(OS, InitExpr.Value.Int64, "literal (i64)");
    break;
  case WASM_OPCODE_GET_GLOBAL:
    write_uleb128(OS, InitExpr.Value.Global, "literal (global index)");
    break;
  default:
    fatal("unknown opcode in init expr: " + Twine(InitExpr.Opcode));
    break;
  }
  write_u8(OS, WASM_OPCODE_END, "opcode:end");
}

static void write_limits(raw_fd_ostream& OS, const WasmLimits& Limits) {
  write_uleb128(OS, Limits.Flags, "limits flags");
  write_uleb128(OS, Limits.Initial, "limits initial");
  if (Limits.Flags & WASM_LIMITS_FLAG_HAS_MAX)
    write_uleb128(OS, Limits.Maximum, "limits max");
}

static void write_global(raw_fd_ostream& OS, const WasmGlobal& Global) {
  write_value_type(OS, Global.Type, "global type");
  write_uleb128(OS, Global.Mutable, "global mutable");
  write_init_expr(OS, Global.InitExpr);
}

static void write_import(raw_fd_ostream& OS, const WasmImport& Import) {
  write_str(OS, Import.Module, "import module name");
  write_str(OS, Import.Field, "import field name");
  write_u8(OS, Import.Kind, "import kind");
  switch (Import.Kind) {
    case WASM_EXTERNAL_FUNCTION:
      write_uleb128(OS, Import.SigIndex, "import sig index");
      break;
    case WASM_EXTERNAL_GLOBAL:
      write_value_type(OS, Import.Global.Type, "import global type");
      write_uleb128(OS, Import.Global.Mutable, "import global mutable");
      break;
    case WASM_EXTERNAL_MEMORY:
      write_limits(OS, Import.Memory);
      break;
    default:
      fatal("unsupported import type: " + Twine(Import.Kind));
      break;
  }
}

static void write_export(raw_fd_ostream& OS, const WasmExport& Export) {
  write_str(OS, Export.Name, "export name");
  write_u8(OS, Export.Kind, "export kind");
  switch (Export.Kind) {
    case WASM_EXTERNAL_FUNCTION:
      write_uleb128(OS, Export.Index, "function index");
      break;
    case WASM_EXTERNAL_GLOBAL:
      write_uleb128(OS, Export.Index, "global index");
      break;
    case WASM_EXTERNAL_MEMORY:
      write_uleb128(OS, Export.Index, "memory index");
      break;
    default:
      fatal("unsupported export type: " + Twine(Export.Kind));
      break;
  }
}

static void write_reloc(raw_fd_ostream& OS, const WasmRelocation &Reloc) {
  write_uleb128(OS, Reloc.Type, "reloc type");
  write_uleb128(OS, Reloc.Offset, "reloc offset");
  write_uleb128(OS, Reloc.Index, "reloc index");

  switch (Reloc.Type) {
  case R_WEBASSEMBLY_GLOBAL_ADDR_LEB:
  case R_WEBASSEMBLY_GLOBAL_ADDR_SLEB:
  case R_WEBASSEMBLY_GLOBAL_ADDR_I32:
    write_uleb128(OS, Reloc.Addend, "reloc addend");
    break;
  default:
    break;
  }
}

SectionBookkeeping Writer::writeSectionHeader(raw_fd_ostream &OS,
                                              uint32_t Type) {
  SectionBookkeeping Section;
  debugWrite(OS, "section type", "%s", section_type_to_str(Type));
  write_uleb128(OS, Type, nullptr);
  Section.SizeOffset = OS.tell();
  write_uleb128_padded(OS, 0, "section size");
  Section.ContentsOffset = OS.tell();
  return Section;
}

void Writer::endSection(raw_fd_ostream &OS, SectionBookkeeping &Section) const {
  uint64_t End = OS.tell();
  uint64_t Size = End - Section.ContentsOffset;
  OS.seek(Section.SizeOffset);
  write_uleb128_padded(OS, Size, "fixup section size");
  OS.seek(End);
}

void Writer::writeImportSection(raw_fd_ostream& OS) {
  uint32_t TotalImports = FunctionImports.size() + GlobalImports.size();
  if (Config->ImportMemory)
    TotalImports++;

  if (TotalImports == 0)
    return;

  SectionBookkeeping Section = writeSectionHeader(OS, WASM_SEC_IMPORT);
  write_uleb128(OS, TotalImports, "import count");

  for (Symbol *Sym: FunctionImports) {
    WasmImport Import;
    Import.Module = "env";
    Import.Field = Sym->getName();
    Import.Kind = WASM_EXTERNAL_FUNCTION;
    assert(isa<ObjectFile>(Sym->getFile()));
    ObjectFile* Obj = dyn_cast<ObjectFile>(Sym->getFile());
    Import.SigIndex = Obj->relocateTypeIndex(Sym->getFunctionTypeIndex());
    write_import(OS, Import);
  }

  if (Config->ImportMemory) {
    WasmImport Import;
    Import.Module = "env";
    Import.Field = "memory";
    Import.Kind = WASM_EXTERNAL_MEMORY;
    Import.Memory.Flags = 0;
    Import.Memory.Initial = TotalMemoryPages;
    write_import(OS, Import);
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
    write_import(OS, Import);
  }

  endSection(OS, Section);
}

void Writer::writeTypeSection(raw_fd_ostream& OS) {
  SectionBookkeeping Section = writeSectionHeader(OS, WASM_SEC_TYPE);
  write_uleb128(OS, Types.size(), "type count");
  for (const WasmSignature *Sig : Types) {
    write_sig(OS, *Sig);
  }
  endSection(OS, Section);
}

void Writer::writeFunctionSection(raw_fd_ostream& OS) {
  if (!TotalFunctions)
    return;
  SectionBookkeeping Section = writeSectionHeader(OS, WASM_SEC_FUNCTION);
  write_uleb128(OS, TotalFunctions, "function count");
  for (ObjectFile *File: Symtab->ObjectFiles) {
    for (uint32_t Sig: File->getWasmObj()->functionTypes()) {
      write_uleb128(OS, File->relocateTypeIndex(Sig), "sig index");
    }
  }
  endSection(OS, Section);
}

void Writer::writeMemorySection(raw_fd_ostream& OS) {
  if (Config->ImportMemory)
    return;

  SectionBookkeeping Section = writeSectionHeader(OS, WASM_SEC_MEMORY);

  write_uleb128(OS, 1, "memory count");
  write_uleb128(OS, 0, "memory limits flags");
  write_uleb128(OS, TotalMemoryPages, "initial pages");

  endSection(OS, Section);
}

void Writer::writeGlobalSection(raw_fd_ostream& OS) {
  SectionBookkeeping Section = writeSectionHeader(OS, WASM_SEC_GLOBAL);

  write_uleb128(OS, TotalGlobals, "global count");
  for (auto& Pair: Config->SyntheticGlobals) {
    WasmGlobal& Global = Pair.second;
    write_global(OS, Global);
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
        write_global(OS, RelocatedGlobal);
        GlobalIndex++;
      }
    }
  }
  endSection(OS, Section);
}

void Writer::writeTableSection(raw_fd_ostream& OS) {
  SectionBookkeeping Section = writeSectionHeader(OS, WASM_SEC_TABLE);
  write_uleb128(OS, 1, "table count");
  write_sleb128(OS, WASM_TYPE_ANYFUNC, "table type");
  write_uleb128(OS, WASM_LIMITS_FLAG_HAS_MAX, "table flags");
  write_uleb128(OS, TotalTableLength, "table initial size");
  write_uleb128(OS, TotalTableLength, "table max size");
  endSection(OS, Section);
}

void Writer::writeExportSection(raw_fd_ostream& OS) {
  // Memory is and main function are exported for executables.
  bool ExportMemory = !Config->Relocatable && !Config->ImportMemory;
  bool ExportMain = !Config->Relocatable;
  bool ExportOther = true; //Config->Relocatable;

  if (Config->Entry == Config->ExportEntryAs && ExportOther)
    ExportMain = false;

  uint32_t NumExports = 0;

  if (ExportMemory)
    NumExports += 1;

  if (ExportMain)
    NumExports += 1;

  if (ExportOther) {
    for (ObjectFile *File : Symtab->ObjectFiles) {
      for (Symbol *Sym : File->getSymbols()) {
         if (!Sym->isFunction() || Sym->isUndefined() || Sym->WrittenToSymtab)
           continue;
         Sym->WrittenToSymtab = true;
         NumExports++;
      }
    }
  }

  if (!NumExports)
    return;

  SectionBookkeeping Section = writeSectionHeader(OS, WASM_SEC_EXPORT);
  write_uleb128(OS, NumExports, "export count");

  if (ExportMemory) {
    WasmExport MemoryExport;
    MemoryExport.Name = "memory";
    MemoryExport.Kind = WASM_EXTERNAL_MEMORY;
    MemoryExport.Index = 0;
    write_export(OS, MemoryExport);
  }

  if (ExportMain) {
    Symbol* Sym = Symtab->find(Config->Entry);
    if (!Sym->isFunction())
      fatal("entry point is not a function: " + Sym->getName());

    if (Config->Entry != Config->ExportEntryAs || !ExportOther) {
      Symbol* ExportAs = Symtab->find(Config->ExportEntryAs);
      if (ExportAs && ExportAs->isDefined()) {
        warn("can't export entry point");
        fatal("already an existing exported symbol: " + Config->ExportEntryAs);
      }
      WasmExport MainExport;
      MainExport.Name = Config->ExportEntryAs;
      MainExport.Kind = WASM_EXTERNAL_FUNCTION;
      MainExport.Index = Sym->getOutputIndex();
      write_export(OS, MainExport);
    }
  }

  if (ExportOther) {
    for (ObjectFile *File : Symtab->ObjectFiles) {
      for (Symbol *Sym : File->getSymbols()) {
        if (!Sym->isFunction() || Sym->isUndefined() || !Sym->WrittenToSymtab)
          continue;
        Sym->WrittenToSymtab = false;
        log("Export: " + Sym->getName());
        WasmExport Export;
        Export.Name = Sym->getName();
        Export.Index = Sym->getOutputIndex();
        if (Sym->isFunction())
          Export.Kind = WASM_EXTERNAL_FUNCTION;
        else
          Export.Kind = WASM_EXTERNAL_GLOBAL;
        write_export(OS, Export);
      }
    }

    // TODO(sbc): Export local symbols too, Even though they are not part
    // of the symbol table?
  }

  endSection(OS, Section);
}

void Writer::writeStartSection(raw_fd_ostream& OS) {}

void Writer::writeElemSection(raw_fd_ostream& OS) {
  if (!TotalElements)
    return;
  SectionBookkeeping Section = writeSectionHeader(OS, WASM_SEC_ELEM);
  write_uleb128(OS, 1, "segment count");
  write_uleb128(OS, 0, "table index");
  WasmInitExpr InitExpr;
  InitExpr.Opcode = WASM_OPCODE_I32_CONST;
  InitExpr.Value.Int32 = InitialTableOffset;
  write_init_expr(OS, InitExpr);
  write_uleb128(OS, TotalElements, "elem count");

  for (ObjectFile *File: Symtab->ObjectFiles) {
    for (const WasmElemSegment &Segment: File->getWasmObj()->elements()) {
      for (uint64_t FunctionIndex: Segment.Functions) {
        write_uleb128(OS, File->relocateFunctionIndex(FunctionIndex),
                      "function index");
      }
    }
  }
  endSection(OS, Section);
}

void Writer::applyRelocations(const ObjectFile &File,
                              OwningArrayRef<uint8_t> &Data,
                              const std::vector<WasmRelocation> &Relocs,
                              std::vector<WasmRelocation> &OutputRelocs,
                              uint32_t SectionOffset,
                              uint32_t OutputOffset) {
  log("applyRelocations: " + File.getName());
  DEBUG(dbgs() << "  SectionOffset : " << SectionOffset << "\n");
  DEBUG(dbgs() << "  datasize      : " << Data.size() << "\n");
  for (const WasmRelocation &Reloc: Relocs) {
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

    assert(Reloc.Offset - SectionOffset < Data.size());
    uint8_t *Location = Data.data() + Reloc.Offset - SectionOffset;

    RelocEncoding Encoding;
    switch (Reloc.Type) {
    case R_WEBASSEMBLY_TYPE_INDEX_LEB:
    case R_WEBASSEMBLY_FUNCTION_INDEX_LEB:
      assert(decodeULEB128(Location) == Reloc.Index);
    case R_WEBASSEMBLY_GLOBAL_ADDR_LEB:
    case R_WEBASSEMBLY_GLOBAL_INDEX_LEB:
      Encoding = RelocEncoding::Uleb128;
      break;
    case R_WEBASSEMBLY_TABLE_INDEX_SLEB:
      assert(decodeSLEB128(Location) == Reloc.Index);
    case R_WEBASSEMBLY_GLOBAL_ADDR_SLEB:
      Encoding = RelocEncoding::Sleb128;
      break;
    case R_WEBASSEMBLY_TABLE_INDEX_I32:
    case R_WEBASSEMBLY_GLOBAL_ADDR_I32:
      Encoding = RelocEncoding::I32;
      break;
    }

    // Encode the new value
    switch (Encoding) {
    case RelocEncoding::Uleb128: {
      unsigned Padding = paddingFor5ByteULEB128(NewValue);
      assert(NewValue >= 0 && NewValue <= UINT32_MAX);
      encodeULEB128(NewValue, Location, Padding);
      break;
    }
    case RelocEncoding::Sleb128: {
      unsigned Padding = paddingFor5ByteSLEB128(NewValue);
      assert(NewValue >= INT32_MIN && NewValue <= INT32_MAX);
      encodeSLEB128(NewValue, Location, Padding);
      break;
    }
    case RelocEncoding::I32:
      support::endian::write<int32_t, support::little, 1>(Location, NewValue);
      break;
    }

    WasmRelocation NewReloc = Reloc;
    NewReloc.Index = NewValue;
    NewReloc.Offset += OutputOffset;
    OutputRelocs.emplace_back(NewReloc);
  }
}

void Writer::writeCodeSection(raw_fd_ostream& OS) {
  if (!TotalFunctions)
    return;

  log("writeCodeSection");
  SectionBookkeeping Section = writeSectionHeader(OS, WASM_SEC_CODE);
  write_uleb128(OS, TotalFunctions, "function count");
  uint32_t ContentsStart = OS.tell();
  uint32_t TotalCodeRelocations = 0;

  for (ObjectFile *File: Symtab->ObjectFiles) {
    if (!File->CodeSection)
      continue;
    TotalCodeRelocations += File->CodeSection->Relocations.size();
    uint32_t CodeSectionOffset = OS.tell() - ContentsStart;

    // Make copy of the section content so that we can apply relocations
    OwningArrayRef<uint8_t> Content(File->CodeSection->Content);
    applyRelocations(*File, Content, File->CodeSection->Relocations,
                     CodeRelocations, 0, CodeSectionOffset);

    // Payload doesn't include the intial function count
    unsigned PayloadOffset = 0;
    decodeULEB128(Content.data(), &PayloadOffset);

    const char* Payload = reinterpret_cast<const char *>(Content.data());
    write_bytes(OS, Payload + PayloadOffset, Content.size() - PayloadOffset,
                "section data");
  }

  endSection(OS, Section);
  assert(CodeRelocations.size() == TotalCodeRelocations);
}

void Writer::writeDataSection(raw_fd_ostream& OS) {
  if (!TotalDataSegments)
    return;

  log("writeDataSection");
  SectionBookkeeping Section = writeSectionHeader(OS, WASM_SEC_DATA);
  write_uleb128(OS, TotalDataSegments, "data segment count");
  uint32_t ContentsStart = OS.tell();
  uint32_t TotalDataRelocations = 0;

  for (ObjectFile *File: Symtab->ObjectFiles) {
    if (!File->DataSection)
      continue;
    TotalDataRelocations += File->DataSection->Relocations.size();
    assert(File->getWasmObj()->dataSegments().size() <= 1);
    uint32_t DataSectionOffset = OS.tell() - ContentsStart;
    for (const object::WasmSegment &Segment: File->getWasmObj()->dataSegments()) {
      write_uleb128(OS, Segment.Data.MemoryIndex, "memory index");
      write_uleb128(OS, WASM_OPCODE_I32_CONST, "opcode:i32const");
      uint32_t NewOffset = Segment.Data.Offset.Value.Int32 + File->DataOffset;
      write_sleb128(OS, NewOffset, "memory offset");
      write_uleb128(OS, WASM_OPCODE_END, "opcode:end");
      write_uleb128(OS, Segment.Data.Content.size(), "segment size");

      OwningArrayRef<uint8_t> Content(Segment.Data.Content);
      applyRelocations(*File, Content, File->DataSection->Relocations,
                       DataRelocations, Segment.SectionOffset,
                       DataSectionOffset);

      const char* Payload = reinterpret_cast<const char *>(Content.data());
      write_bytes(OS, Payload, Content.size(), "segment data");
    }
  }

  endSection(OS, Section);
  assert(DataRelocations.size() == TotalDataRelocations);
}

void Writer::writeRelocSections(raw_fd_ostream& OS) {
  if (CodeRelocations.size()) {
    SectionBookkeeping Section = writeSectionHeader(OS, WASM_SEC_CUSTOM);
    write_str(OS, "reloc.CODE", "reloc section string name");
    write_uleb128(OS, WASM_SEC_CODE, "reloc section");
    write_uleb128(OS, CodeRelocations.size(), "reloc section");
    for (const WasmRelocation &Reloc: CodeRelocations)
      write_reloc(OS, Reloc);
    endSection(OS, Section);
  }

  if (DataRelocations.size()) {
    SectionBookkeeping Section = writeSectionHeader(OS, WASM_SEC_CUSTOM);
    write_str(OS, "reloc.DATA", "reloc section string name");
    write_uleb128(OS, WASM_SEC_DATA, "reloc section");
    write_uleb128(OS, DataRelocations.size(), "reloc section");
    for (const WasmRelocation &Reloc: DataRelocations)
      write_reloc(OS, Reloc);
    endSection(OS, Section);
  }
}

void Writer::writeLinkingSection(raw_fd_ostream& OS) {
  SectionBookkeeping Section = writeSectionHeader(OS, WASM_SEC_CUSTOM);
  write_str(OS, "linking", "custom section name");

  SectionBookkeeping SubSection = writeSectionHeader(OS, WASM_DATA_SIZE);
  write_uleb128(OS, DataSize, "data size");
  endSection(OS, SubSection);

  if (Config->Relocatable) {
    SubSection = writeSectionHeader(OS, WASM_DATA_ALIGNMENT);
    write_uleb128(OS, DataAlignment, "data alignment");
    endSection(OS, SubSection);
  }

  endSection(OS, Section);
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
        if (S->WrittenToNameSec)
          continue;
        S->WrittenToNameSec = true;
      }
      FunctionNameCount++;
    }
  }

  SectionBookkeeping Section = writeSectionHeader(OS, WASM_SEC_CUSTOM);
  write_str(OS, "name", "name section string name");
  SectionBookkeeping SubSection = writeSectionHeader(OS, WASM_NAMES_FUNCTION);
  write_uleb128(OS, FunctionNameCount, "name count");

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
          if (!S->WrittenToNameSec)
            continue;
          S->WrittenToNameSec = false;
        }
        Expected<StringRef> NameOrError = Sym.getName();
        if (!NameOrError)
          fatal("error getting symbol name");
        write_uleb128(OS, File->relocateFunctionIndex(Sym.getValue()), "func index");
        write_str(OS, *NameOrError, "symbol name");
      }
    }
  }
  endSection(OS, SubSection);
  endSection(OS, Section);
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
  writeLinkingSection(OS);
  if (!Config->StripDebug && !Config->StripAll)
    writeNameSection(OS);
}

void Writer::layoutMemory() {
  uint32_t MemoryPtr = 0;
  if (!Config->Relocatable) {
    MemoryPtr += Config->GlobalBase;
    debugPrint("mem: global base = %d\n", Config->GlobalBase);
  }

  // Static data from input object files comes first
  MemoryPtr = alignTo(MemoryPtr, DataAlignment);
  for (ObjectFile *File: Symtab->ObjectFiles) {
    const WasmObjectFile* WasmFile = File->getWasmObj();
    uint32_t Size = WasmFile->linkingData().DataSize;
    if (Size) {
      MemoryPtr = alignTo(MemoryPtr, WasmFile->linkingData().DataAlignment);
      debugPrint("mem: [%s] offset=%#x size=%d\n",
                  File->getName().str().c_str(), File->DataOffset, Size);
      File->DataOffset = MemoryPtr;
      MemoryPtr += Size;
    } else {
      debugPrint("mem: [%s] no data\n", File->getName().str().c_str());
    }
  }

  DataSize = MemoryPtr;
  if (!Config->Relocatable)
    DataSize -= Config->GlobalBase;
  debugPrint("mem: static data = %d\n", DataSize);

  // Stack comes last
  if (!Config->Relocatable) {
    debugPrint("mem: stack size  = %d\n", Config->ZStackSize);
    debugPrint("mem: stack base  = %d\n", MemoryPtr);
    MemoryPtr += Config->ZStackSize;
    Config->SyntheticGlobals[0].second.InitExpr.Value.Int32 = MemoryPtr;
    debugPrint("mem: stack top   = %d\n", MemoryPtr);
  }

  uint32_t MemSize = alignTo(MemoryPtr, WasmPageSize);
  TotalMemoryPages = MemSize / WasmPageSize;
  debugPrint("mem: total pages = %d\n", TotalMemoryPages);
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
    DEBUG(dbgs() << "assignSymbolIndexes: " << File->getName() << "\n");
    for (Symbol *Sym : File->getSymbols()) {
      if (Sym->hasOutputIndex() || !Sym->isDefined())
        continue;

      if (Sym->getFile() && isa<ObjectFile>(Sym->getFile())) {
        ObjectFile* Obj = dyn_cast<ObjectFile>(Sym->getFile());
        if (Sym->isFunction())
          Sym->setOutputIndex(
              Obj->relocateFunctionIndex(Sym->getFunctionIndex()));
        else
          Sym->setOutputIndex(Obj->relocateGlobalIndex(Sym->getGlobalIndex()));
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

  if (Config->Verbose) {
    log("TotalFunctions : " + Twine(TotalFunctions));
    log("TotalGlobals   : " + Twine(TotalGlobals));
    log("TotalImports   : " + Twine(FunctionImports.size() + GlobalImports.size()));
    log("FunctionImports: " + Twine(FunctionImports.size()));
    log("GlobalImports  : " + Twine(GlobalImports.size()));
    for (ObjectFile *File: Symtab->ObjectFiles)
      File->dumpInfo();
  }

  log("-- assignSymbolIndexes");
  assignSymbolIndexes();
  log("-- layoutMemory");
  layoutMemory();

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
  write_bytes(*OS, WasmMagic, sizeof(WasmMagic), "wasm magic");
  write_u32(*OS, WasmVersion, "wasm version");
}

namespace lld {
namespace wasm {

void writeResult(SymbolTable *T) {
  Writer(T).run();
}

} // namespace wasm
} // namespace lld
