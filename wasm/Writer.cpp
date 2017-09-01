//===- Writer.cpp ---------------------------------------------------------===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Writer.h"

#include "Config.h"
#include "Error.h"
#include "Memory.h"
#include "SymbolTable.h"
#include "Threads.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/FileOutputBuffer.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/LEB128.h"

#include <cstdarg>

#define DEBUG_TYPE "lld"

using namespace llvm;
using namespace llvm::wasm;
using namespace lld;
using namespace lld::wasm;

struct OutputRelocation : public WasmRelocation {
  uint32_t NewIndex;
  int64_t Value;
};

static void calcRelocations(const ObjectFile &File,
                            const std::vector<WasmRelocation> &Relocs,
                            std::vector<OutputRelocation> &OutputRelocs,
                            uint32_t OutputOffset, uint32_t Start=0, uint32_t End=0);

static void applyRelocations(uint8_t *Data,
                             const std::vector<OutputRelocation> &Relocs,
                             uint32_t Offset);

namespace {

static const int kStackAlignment = 16;

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

static void debugWrite(raw_ostream &OS, const char *msg,
                        const char *fmt = NULL, ...) {
  DEBUG(
    fprintf(stderr, "  | %08" PRIx64 ": %s", OS.tell(), msg);
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

static void write_uleb128(raw_ostream& OS, uint32_t Number, const char* msg) {
  if (msg)
    debugWrite(OS, msg, "%x", Number);
  encodeULEB128(Number, OS);
}

static void write_sleb128(raw_ostream& OS, int32_t Number, const char* msg) {
  if (msg)
    debugWrite(OS, msg, "%x", Number);
  encodeSLEB128(Number, OS);
}

static void write_bytes(raw_ostream &OS, const char *bytes, size_t count,
                        const char *msg = nullptr) {
  if (msg)
    debugWrite(OS, msg);
  OS.write(bytes, count);
}

static void write_data(raw_ostream& OS, const StringRef String, const char* msg = nullptr) {
  if (msg)
    debugWrite(OS, msg, "data[%d]", String.size());
  write_bytes(OS, String.data(), String.size());
}

static void write_str(raw_ostream& OS, const StringRef String, const char* msg = nullptr) {
  if (msg)
    debugWrite(OS, msg, "str[%d]: %.*s", String.size(), String.size(), String.data());
  write_uleb128(OS, String.size(), nullptr);
  write_bytes(OS, String.data(), String.size());
}

static const char* sectionTypeToString(uint32_t SectionType) {
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

class OutputSection;
std::string toString(OutputSection *Section);


class OutputSection {
public:
  OutputSection(uint32_t Type, std::string Name = "")
      : Type(Type), Name(Name), Offset(0) {}

  virtual ~OutputSection() = default;

  void setOffset(size_t NewOffset) {
    log("setOffset: " + toString(this) + " -> " + Twine(NewOffset));
    Offset = NewOffset;
  }

  void createHeader(size_t BodySize) {
    raw_string_ostream OS(Header);
    debugWrite(OS, "section type", "%s", sectionTypeToString(Type));
    write_uleb128(OS, Type, nullptr);
    write_uleb128(OS, BodySize, "section size");
    OS.flush();
    log("createHeader: " + toString(this) + " body=" + Twine(BodySize) +
        " total=" + Twine(getSize()));
  }

  virtual size_t getSize() const = 0;
  virtual void writeTo(uint8_t* Ptr) = 0;
  virtual void finalizeContents() = 0;

  std::string Header;
  uint32_t Type;
  std::string Name;
  std::vector<OutputRelocation> Relocations;

protected:
  size_t Offset;
};

class SyntheticSection : public OutputSection {
public:
  SyntheticSection(uint32_t Type, std::string Name = "")
      : OutputSection(Type, Name), BodyOutputStream(Body) {
    if (!Name.empty())
      write_str(BodyOutputStream, Name);
  }

  void writeTo(uint8_t* Ptr) override {
    assert(Offset);
    log("writing " + toString(this));
    Ptr += Offset;
    memcpy(Ptr, Header.data(), Header.size());
    Ptr += Header.size();
    memcpy(Ptr, Body.data(), Body.size());
  }

  size_t getSize() const override {
    return Header.size() + Body.size();
  }

  void finalizeContents() override {
    BodyOutputStream.flush();
    createHeader(Body.size());
  }

  raw_ostream& getStream() {
    return BodyOutputStream;
  }

  std::string Body;

protected:
  raw_string_ostream BodyOutputStream;
};

class SubSection : public SyntheticSection {
public:
  explicit SubSection(uint32_t Type) : SyntheticSection(Type) {}

  void writeToStream(raw_ostream& OS) {
    write_data(OS, Header);
    write_data(OS, Body);
  }
};

class CodeSection : public OutputSection {
public:
  explicit CodeSection(uint32_t TotalFunctions, std::vector<ObjectFile *> &Objs)
      : OutputSection(WASM_SEC_CODE), InputObjects(Objs), BodySize(0) {

    raw_string_ostream OS(CodeSectionHeader);
    write_uleb128(OS, TotalFunctions, "function count");
    OS.flush();
    BodySize = CodeSectionHeader.size();

    uint32_t NumRelocations = 0;
    for (ObjectFile *File: InputObjects) {
      if (!File->CodeSection)
        continue;

      NumRelocations += File->CodeSection->Relocations.size();
      const ArrayRef<uint8_t> Content = File->CodeSection->Content;
      unsigned HeaderSize = 0;
      decodeULEB128(Content.data(), &HeaderSize);

      calcRelocations(*File, File->CodeSection->Relocations,
                      Relocations, BodySize - CodeSectionHeader.size());

      size_t PayloadSize = Content.size() - HeaderSize;
      BodySize += PayloadSize;
    }
    assert(Relocations.size() == NumRelocations);
    log("NumRelocations: " + Twine(NumRelocations));
  }

  size_t getSize() const override {
    return Header.size() + BodySize;
  }

  void finalizeContents() override {
    createHeader(BodySize);
  }

  void writeTo(uint8_t* Ptr) override {
    log("writing " + toString(this));
    log(" size=" + Twine(getSize()));
    Ptr += Offset;

    // Write section header
    memcpy(Ptr, Header.data(), Header.size());
    Ptr += Header.size();

    uint8_t* ContentsStart = Ptr;

    // Write code section headers
    memcpy(Ptr, CodeSectionHeader.data(), CodeSectionHeader.size());
    Ptr += CodeSectionHeader.size();


    // Write code section body
    for (ObjectFile *File: InputObjects) {
      if (!File->CodeSection)
        continue;

      const ArrayRef<uint8_t>& Content(File->CodeSection->Content);

      // Payload doesn't include the initial header (function count)
      unsigned HeaderSize = 0;
      decodeULEB128(Content.data(), &HeaderSize);

      size_t PayloadSize = Content.size() - HeaderSize;
      memcpy(Ptr, Content.data() + HeaderSize, PayloadSize);

      Ptr += PayloadSize;
    }

    applyRelocations(ContentsStart, Relocations, 0);
  }

protected:
  std::vector<ObjectFile*>& InputObjects;
  std::string CodeSectionHeader;
  size_t BodySize;
};

struct OutputDataSegment {
  OutputDataSegment(std::string H, const object::WasmSegment* S) : Header(H), Segment(S) {}
  std::string Header;
  const object::WasmSegment* Segment;
};

class DataSection : public OutputSection {
public:
  explicit DataSection(uint32_t TotalDataSegments, std::vector<ObjectFile *> &Objs)
      : OutputSection(WASM_SEC_DATA), InputObjects(Objs), BodySize(0) {

    raw_string_ostream OS(DataSectionHeader);
    write_uleb128(OS, TotalDataSegments, "data segment count");
    OS.flush();
    BodySize = DataSectionHeader.size();

    uint32_t NumRelocations = 0;

    for (ObjectFile *File: Symtab->ObjectFiles) {
      if (!File->DataSection)
        continue;
      NumRelocations += File->DataSection->Relocations.size();
      assert(File->getWasmObj()->dataSegments().size() <= 1);
      //uint32_t DataSectionOffset = BodySize - DataSectionHeader.size();
      for (const object::WasmSegment &Segment: File->getWasmObj()->dataSegments()) {
        std::string SegmentHeader;
        raw_string_ostream OS(SegmentHeader);
        write_uleb128(OS, Segment.Data.MemoryIndex, "memory index");
        write_uleb128(OS, WASM_OPCODE_I32_CONST, "opcode:i32const");
        uint32_t NewOffset = Segment.Data.Offset.Value.Int32 + File->DataOffset;
        write_sleb128(OS, NewOffset, "memory offset");
        write_uleb128(OS, WASM_OPCODE_END, "opcode:end");
        write_uleb128(OS, Segment.Data.Content.size(), "segment size");
        OS.flush();
        BodySize += SegmentHeader.size();

        log("Data segment: size=" + Twine(Segment.Data.Content.size()));
        calcRelocations(*File, File->DataSection->Relocations, Relocations, BodySize - Segment.SectionOffset, Segment.SectionOffset, Segment.SectionOffset + Segment.Data.Content.size());
        BodySize += Segment.Data.Content.size();
        OutputSegments.emplace_back(SegmentHeader, &Segment);
      }
    }

    log("NumRelocations: " + Twine(NumRelocations));
    assert(Relocations.size() == NumRelocations);
  }

  size_t getSize() const override {
    return Header.size() + BodySize;
  }

  void finalizeContents() override {
    createHeader(BodySize);
  }

  void writeTo(uint8_t* Ptr) override {
    log("writing " + toString(this));
    log(" size=" + Twine(getSize()) + " body=" + Twine(BodySize));
    Ptr += Offset;

    // Write section header
    memcpy(Ptr, Header.data(), Header.size());
    Ptr += Header.size();

    uint8_t* ContentsStart = Ptr;

    // Write code section headers
    memcpy(Ptr, DataSectionHeader.data(), DataSectionHeader.size());
    Ptr += DataSectionHeader.size();

    for (OutputDataSegment& Segment: OutputSegments) {
      // Write segment header
      memcpy(Ptr, Segment.Header.data(), Segment.Header.size());
      Ptr += Segment.Header.size();

      // Data data payload
      const ArrayRef<uint8_t>& Content(Segment.Segment->Data.Content);
      memcpy(Ptr, Content.data(), Content.size());
      Ptr += Content.size();
    }

    applyRelocations(ContentsStart,
                     Relocations, 0);
                     //Segment.SectionOffset);
  }

protected:
  std::vector<ObjectFile*>& InputObjects;
  std::vector<OutputDataSegment> OutputSegments;
  std::string DataSectionHeader;
  size_t BodySize;
};

std::string toString(OutputSection *Section) {
  std::string rtn = "section: ";
  rtn += sectionTypeToString(Section->Type);
  if (!Section->Name.empty()) {
    rtn += "(";
    rtn += Section->Name;
    rtn += ")";
  }
  return rtn;
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
  void createHeader();
  void createSections();
  SyntheticSection *createSyntheticSection(uint32_t Type, std::string Name = "");

  // Builtin sections
  void createTypeSection();
  void createFunctionSection();
  void createTableSection();
  void createGlobalSection();
  void createExportSection();
  void createImportSection();
  void createMemorySection();
  void createElemSection();
  void createStartSection();
  void createCodeSection();
  void createDataSection();

  // Custom sections
  void createRelocSections();
  void createLinkingSection();
  void createNameSection();

  void writeHeader();
  void writeSections();
  void writeOutput(const uint8_t* Ptr, size_t Size, size_t Offset);

  uint64_t FileSize = 0;
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

  // Elements that are used to construct the final output
  std::string Header;
  std::vector<OutputSection *> OutputSections;

  std::unique_ptr<FileOutputBuffer> Buffer;
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

static void write_u8(raw_ostream& OS, uint8_t byte, const char* msg) {
  OS << byte;
}

static void write_u32(raw_ostream& OS, uint32_t Number, const char* msg) {
  debugWrite(OS, msg, "%x", Number);
  support::endian::Writer<support::little>(OS).write(Number);
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

static void write_init_expr(raw_ostream& OS, const WasmInitExpr& InitExpr) {
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

static void write_limits(raw_ostream& OS, const WasmLimits& Limits) {
  write_uleb128(OS, Limits.Flags, "limits flags");
  write_uleb128(OS, Limits.Initial, "limits initial");
  if (Limits.Flags & WASM_LIMITS_FLAG_HAS_MAX)
    write_uleb128(OS, Limits.Maximum, "limits max");
}

static void write_global(raw_ostream& OS, const WasmGlobal& Global) {
  write_value_type(OS, Global.Type, "global type");
  write_uleb128(OS, Global.Mutable, "global mutable");
  write_init_expr(OS, Global.InitExpr);
}

static void write_import(raw_ostream& OS, const WasmImport& Import) {
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

static void write_export(raw_ostream& OS, const WasmExport& Export) {
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

static void write_reloc(raw_ostream& OS, const OutputRelocation &Reloc) {
  write_uleb128(OS, Reloc.Type, "reloc type");
  write_uleb128(OS, Reloc.Offset, "reloc offset");
  write_uleb128(OS, Reloc.NewIndex, "reloc index");

  switch (Reloc.Type) {
  case R_WEBASSEMBLY_MEMORY_ADDR_LEB:
  case R_WEBASSEMBLY_MEMORY_ADDR_SLEB:
  case R_WEBASSEMBLY_MEMORY_ADDR_I32:
    write_uleb128(OS, Reloc.Addend, "reloc addend");
    break;
  default:
    break;
  }
}

void Writer::createImportSection() {
  uint32_t TotalImports = FunctionImports.size() + GlobalImports.size();
  if (Config->ImportMemory)
    TotalImports++;

  if (TotalImports == 0)
    return;

  SyntheticSection* Section = createSyntheticSection(WASM_SEC_IMPORT);
  raw_ostream& OS = Section->getStream();

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
}

void Writer::createTypeSection() {
  SyntheticSection* Section = createSyntheticSection(WASM_SEC_TYPE);
  raw_ostream& OS = Section->getStream();
  write_uleb128(OS, Types.size(), "type count");
  for (const WasmSignature *Sig : Types) {
    write_sig(OS, *Sig);
  }
}

void Writer::createFunctionSection() {
  if (!TotalFunctions)
    return;

  SyntheticSection* Section = createSyntheticSection(WASM_SEC_FUNCTION);
  raw_ostream& OS = Section->getStream();

  write_uleb128(OS, TotalFunctions, "function count");
  for (ObjectFile *File: Symtab->ObjectFiles) {
    for (uint32_t Sig: File->getWasmObj()->functionTypes()) {
      write_uleb128(OS, File->relocateTypeIndex(Sig), "sig index");
    }
  }
}

void Writer::createMemorySection() {
  if (Config->ImportMemory)
    return;

  SyntheticSection* Section = createSyntheticSection(WASM_SEC_MEMORY);
  raw_ostream& OS = Section->getStream();

  write_uleb128(OS, 1, "memory count");
  write_uleb128(OS, 0, "memory limits flags");
  write_uleb128(OS, TotalMemoryPages, "initial pages");
}

void Writer::createGlobalSection() {
  SyntheticSection* Section = createSyntheticSection(WASM_SEC_GLOBAL);
  raw_ostream& OS = Section->getStream();

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
}

void Writer::createTableSection() {
  SyntheticSection* Section = createSyntheticSection(WASM_SEC_TABLE);
  raw_ostream& OS = Section->getStream();

  write_uleb128(OS, 1, "table count");
  write_sleb128(OS, WASM_TYPE_ANYFUNC, "table type");
  write_uleb128(OS, WASM_LIMITS_FLAG_HAS_MAX, "table flags");
  write_uleb128(OS, TotalTableLength, "table initial size");
  write_uleb128(OS, TotalTableLength, "table max size");
}

void Writer::createExportSection() {
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

  SyntheticSection* Section = createSyntheticSection(WASM_SEC_EXPORT);
  raw_ostream& OS = Section->getStream();

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
}

void Writer::createStartSection() {}

void Writer::createElemSection() {
  if (!TotalElements)
    return;

  SyntheticSection* Section = createSyntheticSection(WASM_SEC_ELEM);
  raw_ostream& OS = Section->getStream();

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
}

static uint32_t calcNewIndex(const ObjectFile &File, const WasmRelocation &Reloc) {
  uint32_t NewIndex = 0;
  switch (Reloc.Type) {
  case R_WEBASSEMBLY_TYPE_INDEX_LEB:
    NewIndex = File.relocateTypeIndex(Reloc.Index);
    break;
  case R_WEBASSEMBLY_FUNCTION_INDEX_LEB:
    NewIndex = File.relocateFunctionIndex(Reloc.Index);
    break;
  case R_WEBASSEMBLY_TABLE_INDEX_I32:
  case R_WEBASSEMBLY_TABLE_INDEX_SLEB:
    NewIndex = File.relocateTableIndex(Reloc.Index);
    break;
  case R_WEBASSEMBLY_GLOBAL_INDEX_LEB:
  case R_WEBASSEMBLY_MEMORY_ADDR_LEB:
  case R_WEBASSEMBLY_MEMORY_ADDR_SLEB:
  case R_WEBASSEMBLY_MEMORY_ADDR_I32:
    NewIndex = File.getGlobalAddress(Reloc.Index);
    break;
  default:
    fatal("unhandled relocation type: " + Twine(Reloc.Type));
    break;
  }
  return NewIndex;
}

static void writeRelocValue(const OutputRelocation &Reloc, uint8_t *Location) {
  DEBUG(dbgs() << "write reloc: type=" << Reloc.Type << " index=" << Reloc.Index
               << " new=" << Reloc.NewIndex
               << " offset=" << Reloc.Offset << "\n");
  RelocEncoding Encoding;
  switch (Reloc.Type) {
  case R_WEBASSEMBLY_TYPE_INDEX_LEB:
  case R_WEBASSEMBLY_FUNCTION_INDEX_LEB:
    assert(decodeULEB128(Location) == Reloc.Index);
  case R_WEBASSEMBLY_MEMORY_ADDR_LEB:
  case R_WEBASSEMBLY_GLOBAL_INDEX_LEB:
    Encoding = RelocEncoding::Uleb128;
    break;
  case R_WEBASSEMBLY_TABLE_INDEX_SLEB:
    assert(decodeSLEB128(Location) == Reloc.Index);
  case R_WEBASSEMBLY_MEMORY_ADDR_SLEB:
    Encoding = RelocEncoding::Sleb128;
    break;
  case R_WEBASSEMBLY_TABLE_INDEX_I32:
  case R_WEBASSEMBLY_MEMORY_ADDR_I32:
    Encoding = RelocEncoding::I32;
    break;
  }

  // Encode the new value
  switch (Encoding) {
  case RelocEncoding::Uleb128: {
    unsigned Padding = paddingFor5ByteULEB128(Reloc.Value);
    encodeULEB128(Reloc.Value, Location, Padding);
    break;
  }
  case RelocEncoding::Sleb128: {
    unsigned Padding = paddingFor5ByteSLEB128(Reloc.Value);
    encodeSLEB128(Reloc.Value, Location, Padding);
    break;
  }
  case RelocEncoding::I32:
    support::endian::write32<support::little>(Location, Reloc.Value);
    break;
  }
}

static void calcRelocations(const ObjectFile &File,
                            const std::vector<WasmRelocation> &Relocs,
                            std::vector<OutputRelocation> &OutputRelocs,
                            uint32_t OutputOffset, uint32_t Start, uint32_t End) {
  log("calcRelocations: " + File.getName() + " offset=" + Twine(OutputOffset));
  for (const WasmRelocation &Reloc: Relocs) {
    int64_t NewIndex = calcNewIndex(File, Reloc);
    if (Start && End) {
      if (Reloc.Offset < Start && Reloc.Offset >= End)
        continue;
    }
    OutputRelocation NewReloc;
    NewReloc.Type = Reloc.Type;
    NewReloc.Index = Reloc.Index;
    NewReloc.Offset = Reloc.Offset + OutputOffset;
    NewReloc.Addend = Reloc.Addend;
    NewReloc.NewIndex = NewIndex;
    DEBUG(dbgs() << "reloc: type=" << Reloc.Type << " index=" << Reloc.Index
                 << " offset=" << Reloc.Offset << " new=" << NewIndex
                 << " newOffset=" << NewReloc.Offset << "\n");

    switch (Reloc.Type) {
    case R_WEBASSEMBLY_MEMORY_ADDR_SLEB:
    case R_WEBASSEMBLY_MEMORY_ADDR_I32:
    case R_WEBASSEMBLY_MEMORY_ADDR_LEB:
      NewReloc.Value = File.getGlobalAddress(Reloc.Index) + Reloc.Addend;
      break;
    default:
      NewReloc.Value = NewIndex;
    }

    OutputRelocs.emplace_back(NewReloc);
  }
}

static void applyRelocations(uint8_t *Data,
                             const std::vector<OutputRelocation> &Relocs,
                             uint32_t Offset) {
  log("applyRelocations: offset=" + Twine(Offset) + " count=" + Twine(Relocs.size()));
  for (const OutputRelocation &Reloc: Relocs) {
    uint8_t *Location = Data + Reloc.Offset - Offset;
    writeRelocValue(Reloc, Location);
  }
}

void Writer::createCodeSection() {
  if (!TotalFunctions)
    return;

  log("createCodeSection");

  auto Section = make<CodeSection>(TotalFunctions, Symtab->ObjectFiles);
  OutputSections.push_back(Section);
}

void Writer::createDataSection() {
  if (!TotalDataSegments)
    return;

  log("createDataSection");
  auto Section = make<DataSection>(TotalDataSegments, Symtab->ObjectFiles);
  OutputSections.push_back(Section);
}

void Writer::createRelocSections() {
  for (OutputSection* S: OutputSections) {
    if (S->Relocations.empty())
      continue;
    const char* name;
    if (S->Type == WASM_SEC_DATA) {
      name = "reloc.DATA";
    } else {
      assert(S->Type == WASM_SEC_CODE);
      name = "reloc.CODE";
    }
    SyntheticSection* Section = createSyntheticSection(WASM_SEC_CUSTOM, name);
    raw_ostream& OS = Section->getStream();
    write_uleb128(OS, S->Type, "reloc section");
    write_uleb128(OS, S->Relocations.size(), "reloc section");
    for (const OutputRelocation &Reloc: S->Relocations)
      write_reloc(OS, Reloc);
  }
}

void Writer::createLinkingSection() {
  SyntheticSection* Section = createSyntheticSection(WASM_SEC_CUSTOM, "linking");
  raw_ostream& OS = Section->getStream();

  SubSection DataSizeSubSection(WASM_DATA_SIZE);
  write_uleb128(DataSizeSubSection.getStream(), DataSize, "data size");
  DataSizeSubSection.finalizeContents();
  DataSizeSubSection.writeToStream(OS);

  if (Config->Relocatable) {
    SubSection DataAlignSubSection(WASM_DATA_ALIGNMENT);
    write_uleb128(DataAlignSubSection.getStream(), DataAlignment, "data align");
    DataAlignSubSection.finalizeContents();
    DataAlignSubSection.writeToStream(OS);
  }
}

void Writer::createNameSection() {
  SyntheticSection* Section = createSyntheticSection(WASM_SEC_CUSTOM, "name");

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

  SubSection FunctionSubsection(WASM_NAMES_FUNCTION);
  raw_ostream& OS = FunctionSubsection.getStream();
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

  FunctionSubsection.finalizeContents();
  FunctionSubsection.writeToStream(Section->getStream());
}

void Writer::writeOutput(const uint8_t* Ptr, size_t Size, size_t Offset) {
  memcpy(Buffer->getBufferStart() + Offset, Ptr, Size);
}

void Writer::writeHeader() {
  writeOutput((const uint8_t*)Header.data(), Header.size(), 0);
}

void Writer::writeSections() {
  uint8_t* Buf = Buffer->getBufferStart();
  parallelForEach(OutputSections, [Buf](OutputSection *S) {
    S->writeTo(Buf);
  });
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

  // Stack comes after static data
  if (!Config->Relocatable) {
    MemoryPtr = alignTo(MemoryPtr, kStackAlignment);
    if (Config->ZStackSize != alignTo(Config->ZStackSize, kStackAlignment))
      error("stack size must be " + Twine(kStackAlignment) + "-byte aligned");
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

SyntheticSection* Writer::createSyntheticSection(uint32_t Type, std::string Name) {
  auto Sec = make<SyntheticSection>(Type, Name);
  log("createSection: " + toString(Sec));
  OutputSections.push_back(Sec);
  return Sec;
}

void Writer::createSections() {
  // Known sections
  createTypeSection();
  createImportSection();
  createFunctionSection();
  createTableSection();
  createMemorySection();
  createGlobalSection();
  createExportSection();
  createStartSection();
  createElemSection();
  createCodeSection();
  createDataSection();

  // Custom sections
  if (Config->EmitRelocs || Config->Relocatable)
    createRelocSections();
  createLinkingSection();
  if (!Config->StripDebug && !Config->StripAll)
    createNameSection();

  for (OutputSection* S : OutputSections) {
    S->setOffset(FileSize);
    S->finalizeContents();
    FileSize += S->getSize();
  }
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
    log("TotalFunctions   : " + Twine(TotalFunctions));
    log("TotalGlobals     : " + Twine(TotalGlobals));
    log("TotalDataSegments: " + Twine(TotalDataSegments));
    log("TotalImports     : " +
        Twine(FunctionImports.size() + GlobalImports.size()));
    log("FunctionImports  : " + Twine(FunctionImports.size()));
    log("GlobalImports    : " + Twine(GlobalImports.size()));
    for (ObjectFile *File: Symtab->ObjectFiles)
      File->dumpInfo();
  }

  log("-- assignSymbolIndexes");
  assignSymbolIndexes();
  log("-- layoutMemory");
  layoutMemory();

  createHeader();
  log("-- createSections");
  createSections();

  log("-- openFile");
  openFile();
  if (ErrorCount)
    return;

  writeHeader();

  log("-- writeSections");
  writeSections();
  if (ErrorCount)
    return;

  if (auto EC = Buffer->commit())
    fatal("failed to write the output file: " + EC.message());
}

// Open a result file.
void Writer::openFile() {
  log("writing: " + Config->OutputFile);
  ::remove(Config->OutputFile.str().c_str());

  ErrorOr<std::unique_ptr<FileOutputBuffer>> BufferOrErr =
      FileOutputBuffer::create(Config->OutputFile, FileSize,
                               FileOutputBuffer::F_executable);

  if (auto EC = BufferOrErr.getError())
    error("failed to open " + Config->OutputFile + ": " + EC.message());
  else
    Buffer = std::move(*BufferOrErr);
}

void Writer::createHeader() {
  raw_string_ostream OS(Header);
  write_bytes(OS, WasmMagic, sizeof(WasmMagic), "wasm magic");
  write_u32(OS, WasmVersion, "wasm version");
  OS.flush();
  FileSize += Header.size();
}

namespace lld {
namespace wasm {

void writeResult(SymbolTable *T) {
  Writer(T).run();
}

} // namespace wasm
} // namespace lld
