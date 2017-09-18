//===- OutputSections.h -----------------------------------------*- C++ -*-===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLD_WASM_OUTPUT_SECTIONS_H
#define LLD_WASM_OUTPUT_SECTIONS_H

#include "WriterUtils.h"
#include "Error.h"

namespace lld {
namespace wasm {

class OutputSection;
class ObjectFile;
std::string toString(OutputSection *Section);

struct OutputDataSegment {
  OutputDataSegment(std::string H, const object::WasmSegment *S)
      : Header(H), Segment(S) {}
  std::string Header;
  const object::WasmSegment *Segment;
};

class OutputSection {
public:
  OutputSection(uint32_t Type, std::string Name = "")
      : Type(Type), Name(Name), Offset(0) {}

  virtual ~OutputSection() = default;

  void setOffset(size_t NewOffset) {
    log("setOffset: " + toString(this) + " -> " + Twine(NewOffset));
    Offset = NewOffset;
  }

  void createHeader(size_t BodySize);
  virtual size_t getSize() const = 0;
  virtual void writeTo(uint8_t *Buf) = 0;
  virtual void finalizeContents() {}

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
      writeStr(BodyOutputStream, Name);
  }

  void writeTo(uint8_t *Buf) override {
    assert(Offset);
    log("writing " + toString(this));
    Buf += Offset;
    memcpy(Buf, Header.data(), Header.size());
    Buf += Header.size();
    memcpy(Buf, Body.data(), Body.size());
  }

  size_t getSize() const override { return Header.size() + Body.size(); }

  void finalizeContents() override {
    BodyOutputStream.flush();
    createHeader(Body.size());
  }

  raw_ostream &getStream() { return BodyOutputStream; }

  std::string Body;

protected:
  raw_string_ostream BodyOutputStream;
};

// Some synthetic sections (e.g. "name" and "linking") have subsections.
// Just like the synthetic sections themselves these need to be created before
// they can be written out (since they are preceded by their length). This
// class is used to create subsections and then write them into the stream
// of the parent section.
class SubSection : public SyntheticSection {
public:
  explicit SubSection(uint32_t Type) : SyntheticSection(Type) {}

  void writeToStream(raw_ostream &OS) {
    writeData(OS, Header);
    writeData(OS, Body);
  }
};

class CodeSection : public OutputSection {
public:
  explicit CodeSection(uint32_t NumFunctions, std::vector<ObjectFile *> &Objs);
  size_t getSize() const override { return Header.size() + BodySize; }
  void writeTo(uint8_t *Buf) override;

protected:
  std::vector<ObjectFile *> &InputObjects;
  std::string CodeSectionHeader;
  size_t BodySize;
};

class DataSection : public OutputSection {
public:
  explicit DataSection(uint32_t NumDataSegments,
                       std::vector<ObjectFile *> &Objs);
  size_t getSize() const override { return Header.size() + BodySize; }
  void writeTo(uint8_t *Buf) override;

protected:
  std::vector<ObjectFile *> &InputObjects;
  std::vector<OutputDataSegment> OutputSegments;
  std::string DataSectionHeader;
  size_t BodySize;
};

} // namespace wasm
} // namespace lld

#endif // LLD_WASM_OUTPUT_SECTIONS_H
