define i32 @foo() #0 {
entry:
  ret i32 0
}

@bar = weak alias i32 (), i32 ()* @foo
