; RUN: llc -filetype=obj %p/Inputs/ret32.ll -o %t.ret32.o
; RUN: llc -filetype=obj %s -o %t.o
; RUN: lld -flavor wasm -o %t.wasm %t.ret32.o %t.o
; RUN: obj2yaml %t.wasm | FileCheck %s

; Wasm module generated from the following C code:
; int foo(void) { return 1; }
; int (*indirect_func)(void) = &foo;
; void _start(void) { indirect_func(); }

target datalayout = "e-m:e-p:32:32-i64:64-n32:64-S128"
target triple = "wasm32-unknown-unknown-wasm"

@indirect_func = hidden local_unnamed_addr global i32 ()* @foo, align 4

; Function Attrs: norecurse nounwind readnone
define hidden i32 @foo() #0 {
entry:
  ret i32 1
}

; Function Attrs: nounwind
define hidden void @_start() local_unnamed_addr #1 {
entry:
  %0 = load i32 ()*, i32 ()** @indirect_func, align 4
  %call = tail call i32 %0() #2
  ret void
}

; CHECK: !WASM
; CHECK: FileHeader:      
; CHECK:   Version:         0x00000001
; CHECK: Sections:        
; CHECK:   - Type:            TYPE
; CHECK:     Signatures:      
; CHECK:       - Index:           0
; CHECK:         ReturnType:      I32
; CHECK:         ParamTypes:      
; CHECK:           - F32
; CHECK:       - Index:           1
; CHECK:         ReturnType:      I32
; CHECK:         ParamTypes:      
; CHECK:       - Index:           2
; CHECK:         ReturnType:      NORESULT
; CHECK:         ParamTypes:      
; CHECK:   - Type:            FUNCTION
; CHECK:     FunctionTypes:   [ 0, 1, 2 ]
; CHECK:   - Type:            TABLE
; CHECK:     Tables:          
; CHECK:       - ElemType:        ANYFUNC
; CHECK:         Limits:          
; CHECK:           Flags:           0x00000001
; CHECK:           Initial:         0x00000001
; CHECK:           Maximum:         0x00000001
; CHECK:   - Type:            MEMORY
; CHECK:     Memories:        
; CHECK:       - Initial:         0x00000002
; CHECK:   - Type:            GLOBAL
; CHECK:     Globals:         
; CHECK:       - Type:            I32
; CHECK:         Mutable:         true
; CHECK:         InitExpr:        
; CHECK:           Opcode:          I32_CONST
; CHECK:           Value:           66564
; CHECK:   - Type:            EXPORT
; CHECK:     Exports:         
; CHECK:       - Name:            memory
; CHECK:         Kind:            MEMORY
; CHECK:         Index:           0
; CHECK:       - Name:            main
; CHECK:         Kind:            FUNCTION
; CHECK:         Index:           2
; CHECK:   - Type:            ELEM
; CHECK:     Segments:        
; CHECK:       - Offset:          
; CHECK:           Opcode:          I32_CONST
; CHECK:           Value:           0
; CHECK:         Functions:       [ 0 ]
; CHECK:   - Type:            CODE
; CHECK:     Functions:       
; CHECK:       - Locals:          
; CHECK:       - Locals:          
; CHECK:       - Locals:          
; CHECK:   - Type:            DATA
; CHECK:     Segments:        
; CHECK:       - Index:           0
; CHECK:         Offset:          
; CHECK:           Opcode:          I32_CONST
; CHECK:           Value:           1024
; CHECK:         Content:         '00000000'
; CHECK:   - Type:            CUSTOM
; CHECK:     Name:            name
; CHECK:     FunctionNames:   
; CHECK:       - Index:           0
; CHECK:         Name:            ret32
; CHECK:       - Index:           1
; CHECK:         Name:            foo
; CHECK:       - Index:           2
; CHECK:         Name:            _start
