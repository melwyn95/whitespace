type io = ReadChar | ReadNumber | OutputChar | OutputNumber

type stack_manipulation = 
  Push of int
| Dup
| Swap
| Drop
| Copy of int
| Slide of int

type arithmetic = Add | Sub | Mul | Div | Mod

type flow_control = 
  Mark of int
| End
| Call of int
| Jmp of int
| Jz of int
| Jneg of int
| EndProg

type head_access = Store | Retrive

type instruction = 
  IO of io 
| StackManipulaton of stack_manipulation
| Arithmetic of arithmetic
| FlowControl of flow_control
| HeapAccess of head_access

type t = instruction list