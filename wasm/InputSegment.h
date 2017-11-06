//===- InputSegment.h -------------------------------------------*- C++ -*-===//
//
//                             The LLVM Linker
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLD_WASM_INPUT_SEGMENT_H
#define LLD_WASM_INPUT_SEGMENT_H

#include "lld/Common/ErrorHandler.h"
#include "llvm/Object/Wasm.h"

using llvm::object::WasmSegment;
using llvm::wasm::WasmRelocation;

namespace lld {
namespace wasm {

class ObjectFile;
class OutputSegment;

class InputSegment {
public:
  InputSegment(const WasmSegment *Seg, const ObjectFile *F)
      : Segment(Seg), File(F) {}

  // Translate an offset in the input segment to an offset in the output
  // segment.
  uint32_t translateVA(uint32_t Address) const;

  const OutputSegment *getOutputSegment() const {
    return OutputSeg;
  }

  uint32_t getOutputSegmentOffset() const {
    return OutputSegmentOffset;
  }

  uint32_t getInputSectionOffset() const {
    return Segment->SectionOffset;
  }

  void setOutputSegment(const OutputSegment* Segment, uint32_t Offset) {
    OutputSeg = Segment;
    OutputSegmentOffset = Offset;
  }

  uint32_t getSize() const { return Segment->Data.Content.size(); }
  uint32_t getAlignment() const { return Segment->Data.Alignment; }
  uint32_t startVA() const { return Segment->Data.Offset.Value.Int32; }
  uint32_t endVA() const { return startVA() + getSize(); }
  StringRef getName() const { return Segment->Data.Name; }

  const WasmSegment *Segment;
  const ObjectFile *File;
  std::vector<WasmRelocation> Relocations;

protected:
  const OutputSegment *OutputSeg = nullptr;
  uint32_t OutputSegmentOffset = 0;
};

} // namespace wasm
} // namespace lld

#endif // LLD_WASM_INPUT_SEGMENT_H
