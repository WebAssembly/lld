; RUN: llc -filetype=obj %s -o %t.o
; RUN: not lld -flavor wasm -o %t.wasm %t.o 2>&1 | FileCheck %s

target datalayout = "e-m:e-p:32:32-i64:64-n32:64-S128"
target triple = "wasm32-unknown-unknown-wasm"

; Takes the address of the external foo() resulting in undefined external
@bar = hidden local_unnamed_addr global i8* bitcast (i32 ()* @foo to i8*), align 4

declare i32 @foo() #0

; CHECK: lld: error: {{.*}}.o: undefined symbol: foo
; CHECK: lld: error: link failed
