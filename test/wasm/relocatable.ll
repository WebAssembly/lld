; RUN: llc -filetype=obj %p/Inputs/hello.ll -o %t.hello.o
; RUN: llc -filetype=obj %s -o %t.o
; RUN: lld -flavor wasm -r -o %t.wasm %t.hello.o %t.o
; RUN: obj2yaml %t.wasm | FileCheck %s

; Wasm module generated from the following C code
;  int foo_import(void);
;   void local_func(void) { foo_import(); }

target datalayout = "e-m:e-p:32:32-i64:64-n32:64-S128"
target triple = "wasm32-unknown-unknown-wasm"

; Function Attrs: nounwind
define hidden void @local_func() local_unnamed_addr #0 {
entry:
  %call = tail call i32 @foo_import() #2
  ret void
}

declare i32 @foo_import() local_unnamed_addr #1

; CHECK: --- !WASM
; CHECK: FileHeader:      
; CHECK:   Version:         0x00000001
; CHECK: Sections:        
; CHECK:   - Type:            TYPE
; CHECK:     Signatures:      
; CHECK:       - Index:           0
; CHECK:         ReturnType:      NORESULT
; CHECK:         ParamTypes:      
; CHECK:       - Index:           1
; CHECK:         ReturnType:      NORESULT
; CHECK:         ParamTypes:      
; CHECK:           - I32
; CHECK:       - Index:           2
; CHECK:         ReturnType:      NORESULT
; CHECK:         ParamTypes:      
; CHECK:       - Index:           3
; CHECK:         ReturnType:      I32
; CHECK:         ParamTypes:      
; CHECK:   - Type:            IMPORT
; CHECK:     Imports:         
; CHECK:       - Module:          env
; CHECK:         Field:           puts
; CHECK:         Kind:            FUNCTION
; CHECK:         SigIndex:        1
; CHECK:       - Module:          env
; CHECK:         Field:           foo_import
; CHECK:         Kind:            FUNCTION
; CHECK:         SigIndex:        3
; CHECK:   - Type:            FUNCTION
; CHECK:     FunctionTypes:   [ 0, 2 ]
; CHECK:   - Type:            MEMORY
; CHECK:     Memories:        
; CHECK:       - Initial:         0x00000002
; CHECK:   - Type:            GLOBAL
; CHECK:     Globals:         
; CHECK:       - Type:            I32
; CHECK:         Mutable:         false
; CHECK:         InitExpr:        
; CHECK:           Opcode:          I32_CONST
; CHECK:           Value:           0
; CHECK:   - Type:            EXPORT
; CHECK:     Exports:         
; CHECK:       - Name:            hello
; CHECK:         Kind:            FUNCTION
; CHECK:         Index:           1
; CHECK:   - Type:            CODE
; CHECK:     Relocations:     
; CHECK:       - Type:            R_WEBASSEMBLY_GLOBAL_ADDR_SLEB
; CHECK:         Index:           0
; CHECK:         Offset:          0x00000004
; CHECK:       - Type:            R_WEBASSEMBLY_FUNCTION_INDEX_LEB
; CHECK:         Index:           0
; CHECK:         Offset:          0x0000000A
; CHECK:       - Type:            R_WEBASSEMBLY_FUNCTION_INDEX_LEB
; CHECK:         Index:           1
; CHECK:         Offset:          0x00000013
; CHECK:     Functions:       
; CHECK:       - Locals:          
; CHECK:       - Locals:          
; CHECK:   - Type:            DATA
; CHECK:     Segments:        
; CHECK:       - Index:           0
; CHECK:         Offset:          
; CHECK:           Opcode:          I32_CONST
; CHECK:           Value:           1024
; CHECK:         Content:         68656C6C6F0A00
; CHECK:   - Type:            CUSTOM
; CHECK:     Name:            name
; CHECK:     FunctionNames:   
; CHECK:       - Index:           0
; CHECK:         Name:            puts
; CHECK:       - Index:           1
; CHECK:         Name:            foo_import
; CHECK:       - Index:           2
; CHECK:         Name:            hello
; CHECK:       - Index:           3
; CHECK:         Name:            local_func

