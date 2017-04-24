; RUN: llc -mtriple wasm32-unknown-unknown-wasm -filetype=obj -o %t.o %s
; RUN: lld -flavor wasm -strip-debug %t.o -o %t.wasm
; RUN: obj2yaml %t.wasm | FileCheck %s

; Test that undefined weak externals (global_var) and (foo) don't cause
; link failures and resolve to zero.

@global_var = extern_weak global i32, align 4

declare extern_weak i32 @foo()

define hidden i8* @get_address_of_foo() #0 {
entry:
  ret i8* bitcast (i32 ()* @foo to i8*)
}

define hidden i32 @_start() #0 {
entry:
    %0 = load i32, i32* @global_var, align 4
    ret i32 %0
}

; CHECK: --- !WASM
; CHECK: FileHeader:
; CHECK:   Version:         0x00000001
; CHECK: Sections:
; CHECK:   - Type:            TYPE
; CHECK:     Signatures:
; CHECK:       - Index:           0
; CHECK:         ReturnType:      I32
; CHECK:         ParamTypes:
; CHECK:   - Type:            FUNCTION
; CHECK:     FunctionTypes:   [ 0, 0 ]
; CHECK:   - Type:            MEMORY
; CHECK:     Memories:
; CHECK:       - Initial:         0x00000002
; CHECK:   - Type:            GLOBAL
; CHECK:     Globals:
; CHECK:       - Type:            I32
; CHECK:         Mutable:         true
; CHECK:         InitExpr:
; CHECK:           Opcode:          I32_CONST
; CHECK:           Value:           66560
; CHECK:   - Type:            EXPORT
; CHECK:     Exports:
; CHECK:       - Name:            memory
; CHECK:         Kind:            MEMORY
; CHECK:         Index:           0
; CHECK:       - Name:            main
; CHECK:         Kind:            FUNCTION
; CHECK:         Index:           1
; CHECK:   - Type:            CODE
; CHECK:     Functions:
; CHECK:       - Locals:
; CHECK:         Body:            4180808080000B
; CHECK:       - Locals:
; CHECK:         Body:            4100280280808080000B
; CHECK: ...
