//===- OutputSections.cpp -------------------------------------------------===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "OutputSections.h"

#include "Error.h"
#include "InputFiles.h"
#include "SymbolTable.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/LEB128.h"

#define DEBUG_TYPE "lld"

using namespace llvm;
using namespace lld::wasm;

enum class RelocEncoding {
  Uleb128,
  Sleb128,
  I32,
};

static const char *sectionTypeToString(uint32_t SectionType) {
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

std::string lld::toString(OutputSection *Section) {
  std::string rtn = sectionTypeToString(Section->Type);
  if (!Section->Name.empty())
    rtn += "(" + Section->Name + ")";
  return rtn;
};

// Relocations contain an index into the function, global or table index
// space of the input file.  This function takes a relocation and returns the
// relocated index (i.e. translates from the input index space to the output
// index space).
static uint32_t calcNewIndex(ObjectFile &File, const WasmRelocation &Reloc) {
  switch (Reloc.Type) {
  case R_WEBASSEMBLY_TYPE_INDEX_LEB:
    return File.relocateTypeIndex(Reloc.Index);
  case R_WEBASSEMBLY_FUNCTION_INDEX_LEB:
    return File.relocateFunctionIndex(Reloc.Index);
  case R_WEBASSEMBLY_TABLE_INDEX_I32:
  case R_WEBASSEMBLY_TABLE_INDEX_SLEB:
    return File.relocateTableIndex(Reloc.Index);
  case R_WEBASSEMBLY_GLOBAL_INDEX_LEB:
  case R_WEBASSEMBLY_MEMORY_ADDR_LEB:
  case R_WEBASSEMBLY_MEMORY_ADDR_SLEB:
  case R_WEBASSEMBLY_MEMORY_ADDR_I32:
    return File.relocateGlobalIndex(Reloc.Index);
  default:
    llvm_unreachable("unknown relocation type");
  }
}

// Take a vector of relocations from an input file and create output
// relocations based on them. Calculates the updated index and offset for
// each relocation as well as the value to write out in the final binary.
static void calcRelocations(ObjectFile &File, ArrayRef<WasmRelocation> Relocs,
                            std::vector<OutputRelocation> &OutputRelocs,
                            uint32_t OutputOffset, uint32_t Start = 0,
                            uint32_t End = 0) {
  log("calcRelocations: " + File.getName() + " offset=" + Twine(OutputOffset));
  for (const WasmRelocation &Reloc : Relocs) {
    int64_t NewIndex = calcNewIndex(File, Reloc);
    if (Start && End) {
      if (Reloc.Offset < Start || Reloc.Offset >= End)
        continue;
    }
    OutputRelocation NewReloc;
    NewReloc.Reloc = Reloc;
    NewReloc.Reloc.Offset += OutputOffset;
    NewReloc.NewIndex = NewIndex;
    DEBUG(dbgs() << "reloc: type=" << Reloc.Type << " index=" << Reloc.Index
                 << " offset=" << Reloc.Offset << " new=" << NewIndex
                 << " newOffset=" << NewReloc.Reloc.Offset << "\n");

    switch (Reloc.Type) {
    case R_WEBASSEMBLY_MEMORY_ADDR_SLEB:
    case R_WEBASSEMBLY_MEMORY_ADDR_I32:
    case R_WEBASSEMBLY_MEMORY_ADDR_LEB:
      NewReloc.Value = File.getRelocatedAddress(Reloc.Index) + Reloc.Addend;
      break;
    default:
      NewReloc.Value = NewIndex;
    }

    OutputRelocs.emplace_back(NewReloc);
  }
}

static void applyRelocation(uint8_t *Buf, const OutputRelocation &Reloc) {
  DEBUG(dbgs() << "write reloc: type=" << Reloc.Reloc.Type
               << " index=" << Reloc.Reloc.Index << " new=" << Reloc.NewIndex
               << " offset=" << Reloc.Reloc.Offset << "\n");
  switch (Reloc.Reloc.Type) {
  case R_WEBASSEMBLY_TYPE_INDEX_LEB:
  case R_WEBASSEMBLY_FUNCTION_INDEX_LEB:
    assert(decodeULEB128(Buf) == Reloc.Reloc.Index);
    LLVM_FALLTHROUGH;
  case R_WEBASSEMBLY_MEMORY_ADDR_LEB:
  case R_WEBASSEMBLY_GLOBAL_INDEX_LEB:
    encodeULEB128(Reloc.Value, Buf, 5);
    break;
  case R_WEBASSEMBLY_TABLE_INDEX_SLEB:
    assert(decodeSLEB128(Buf) == Reloc.Reloc.Index);
    LLVM_FALLTHROUGH;
  case R_WEBASSEMBLY_MEMORY_ADDR_SLEB:
    encodeSLEB128(Reloc.Value, Buf, 5);
    break;
  case R_WEBASSEMBLY_TABLE_INDEX_I32:
  case R_WEBASSEMBLY_MEMORY_ADDR_I32:
    support::endian::write32<support::little>(Buf, Reloc.Value);
    break;
  default:
    llvm_unreachable("unknown relocation type");
  }
}

static void applyRelocations(uint8_t *Buf,
                             const std::vector<OutputRelocation> &Relocs,
                             uint32_t Offset) {
  log("applyRelocations: offset=" + Twine(Offset) +
      " count=" + Twine(Relocs.size()));
  for (const OutputRelocation &Reloc : Relocs) {
    applyRelocation(Buf + Reloc.Reloc.Offset - Offset, Reloc);
  }
}

void OutputSection::createHeader(size_t BodySize) {
  raw_string_ostream OS(Header);
  debugWrite(OS.tell(),
             "section type [" + Twine(sectionTypeToString(Type)) + "]");
  writeUleb128(OS, Type, nullptr);
  writeUleb128(OS, BodySize, "section size");
  OS.flush();
  log("createHeader: " + toString(this) + " body=" + Twine(BodySize) +
      " total=" + Twine(getSize()));
}

CodeSection::CodeSection(uint32_t NumFunctions, std::vector<ObjectFile *> &Objs)
    : OutputSection(WASM_SEC_CODE), InputObjects(Objs) {
  raw_string_ostream OS(CodeSectionHeader);
  writeUleb128(OS, NumFunctions, "function count");
  OS.flush();
  BodySize = CodeSectionHeader.size();

  uint32_t NumRelocations = 0;
  for (ObjectFile *File : InputObjects) {
    if (!File->CodeSection)
      continue;

    NumRelocations += File->CodeSection->Relocations.size();
    ArrayRef<uint8_t> Content = File->CodeSection->Content;
    unsigned HeaderSize = 0;
    decodeULEB128(Content.data(), &HeaderSize);

    calcRelocations(*File, File->CodeSection->Relocations, Relocations,
                    BodySize - CodeSectionHeader.size());

    size_t PayloadSize = Content.size() - HeaderSize;
    BodySize += PayloadSize;
  }
  assert(Relocations.size() == NumRelocations);
  log("NumRelocations: " + Twine(NumRelocations));

  createHeader(BodySize);
}

void CodeSection::writeTo(uint8_t *Buf) {
  log("writing " + toString(this));
  log(" size=" + Twine(getSize()));
  Buf += Offset;

  // Write section header
  memcpy(Buf, Header.data(), Header.size());
  Buf += Header.size();

  uint8_t *ContentsStart = Buf;

  // Write code section headers
  memcpy(Buf, CodeSectionHeader.data(), CodeSectionHeader.size());
  Buf += CodeSectionHeader.size();

  // Write code section body
  for (ObjectFile *File : InputObjects) {
    if (!File->CodeSection)
      continue;

    ArrayRef<uint8_t> Content(File->CodeSection->Content);

    // Payload doesn't include the initial header (function count)
    unsigned HeaderSize = 0;
    decodeULEB128(Content.data(), &HeaderSize);

    size_t PayloadSize = Content.size() - HeaderSize;
    memcpy(Buf, Content.data() + HeaderSize, PayloadSize);

    Buf += PayloadSize;
  }

  applyRelocations(ContentsStart, Relocations, 0);
}

DataSection::DataSection(uint32_t NumDataSegments,
                         std::vector<ObjectFile *> &Objs)
    : OutputSection(WASM_SEC_DATA), InputObjects(Objs) {
  raw_string_ostream OS(DataSectionHeader);
  writeUleb128(OS, NumDataSegments, "data segment count");
  OS.flush();
  BodySize = DataSectionHeader.size();

  uint32_t NumRelocations = 0;

  for (ObjectFile *File : Symtab->ObjectFiles) {
    if (!File->DataSection)
      continue;
    NumRelocations += File->DataSection->Relocations.size();
    for (const object::WasmSegment &Segment :
         File->getWasmObj()->dataSegments()) {
      std::string SegmentHeader;
      raw_string_ostream OS(SegmentHeader);
      writeUleb128(OS, Segment.Data.MemoryIndex, "memory index");
      writeUleb128(OS, WASM_OPCODE_I32_CONST, "opcode:i32const");
      uint32_t NewOffset = Segment.Data.Offset.Value.Int32 + File->DataOffset;
      writeSleb128(OS, NewOffset, "memory offset");
      writeUleb128(OS, WASM_OPCODE_END, "opcode:end");
      writeUleb128(OS, Segment.Data.Content.size(), "segment size");
      OS.flush();
      BodySize += SegmentHeader.size();

      log("Data segment: size=" + Twine(Segment.Data.Content.size()));
      calcRelocations(*File, File->DataSection->Relocations, Relocations,
                      BodySize - Segment.SectionOffset, Segment.SectionOffset,
                      Segment.SectionOffset + Segment.Data.Content.size());
      BodySize += Segment.Data.Content.size();
      OutputSegments.emplace_back(SegmentHeader, &Segment);
    }
  }

  log("NumRelocations: " + Twine(NumRelocations));
  assert(Relocations.size() == NumRelocations);

  createHeader(BodySize);
}

void DataSection::writeTo(uint8_t *Buf) {
  log("writing " + toString(this) + " size=" + Twine(getSize()) +
      " body=" + Twine(BodySize));
  Buf += Offset;

  // Write section header
  memcpy(Buf, Header.data(), Header.size());
  Buf += Header.size();

  uint8_t *ContentsStart = Buf;

  // Write code section headers
  memcpy(Buf, DataSectionHeader.data(), DataSectionHeader.size());
  Buf += DataSectionHeader.size();

  for (OutputDataSegment &Segment : OutputSegments) {
    // Write segment header
    memcpy(Buf, Segment.Header.data(), Segment.Header.size());
    Buf += Segment.Header.size();

    // Data data payload
    ArrayRef<uint8_t> Content(Segment.Segment->Data.Content);
    memcpy(Buf, Content.data(), Content.size());
    Buf += Content.size();
  }

  applyRelocations(ContentsStart, Relocations, 0);
}
