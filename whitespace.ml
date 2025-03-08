module Whitespace = struct

  type label = string

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
    Mark of label
  | End
  | Call of label
  | Jmp of label
  | Jz of label
  | Jneg of label
  | EndProg

  type heap_access = Store | Retrive

  type instruction = 
    IO of io 
  | StackManipulaton of stack_manipulation
  | Arithmetic of arithmetic
  | FlowControl of flow_control
  | HeapAccess of heap_access

  type t = instruction list

end

module Lexer = struct
  type t = Space | Tab | LineFeed
  type tokens = t list 

  let tokenize : string -> tokens =
    fun source ->
      let len = String.length source in
      let rec aux idx =
        if idx = len then []
        else
          match source.[idx] with
          | ' '  -> Space    :: aux (idx + 1)
          | '\t' -> Tab      :: aux (idx + 1)
          | '\n' -> LineFeed :: aux (idx + 1)
          | _    -> aux (idx + 1)
      in
      aux 0

  let to_char = function Space -> ' ' | Tab -> '\t' | LineFeed -> '\n'

end

module Parser = struct

  type t = Whitespace.t

  let rec power x = function 0 -> 1 | n -> x * power x (n - 1)

  let rec parse_number : Lexer.tokens -> int * Lexer.tokens =
    fun tokens ->
      let open Lexer in
      match tokens with
      | Space :: tokens -> parse_bits tokens
      | Tab :: tokens ->
        let n, tokens = parse_bits tokens in
        -n, tokens
      | _ -> failwith "ParsingError: [parse_number] Invalid sign"
    
  and parse_bits : Lexer.tokens -> int * Lexer.tokens =
    fun tokens ->
      let open Lexer in
      let rec aux tokens = 
        match tokens with
        | LineFeed :: tokens -> [], tokens
        | Space :: tokens ->
          let bs, tokens = aux tokens in
          0 :: bs, tokens
        | Tab :: tokens ->
          let bs, tokens = aux tokens in
          1 :: bs, tokens
        | [] -> failwith "ParseError: [parse_bits] Empty list"
      in
      let bs, tokens = aux tokens in
      let num = bs
        |> List.rev
        |> List.mapi (fun i b -> b * power 2 i)
        |> List.fold_left (+) 0 
      in
      num, tokens

  and parse_label : Lexer.tokens -> string * Lexer.tokens =
    fun tokens ->
      let open Lexer in
      let rec aux tokens = 
        match tokens with
        | LineFeed :: tokens -> Seq.empty, tokens
        | c :: tokens ->
          let s, tokens = aux tokens in
          Seq.cons (Lexer.to_char c) s, tokens
        | [] -> failwith "ParseError: [parse_label] Empty list"
      in
      let s, tokens = aux tokens in
      String.of_seq s, tokens

  and parse_io : Lexer.tokens -> Whitespace.io * Lexer.tokens =
    fun tokens ->
      let open Lexer in
      match tokens with
      | Tab :: Space :: tokens   -> ReadChar    , tokens
      | Tab :: Tab :: tokens     -> ReadNumber  , tokens
      | Space :: Space :: tokens -> OutputChar  , tokens
      | Space :: Tab :: tokens   -> OutputNumber, tokens
      | _ -> failwith "ParseError: [parse_io] Invalid case"

  and parse_stack_manipulation 
    : Lexer.tokens -> Whitespace.stack_manipulation * Lexer.tokens 
    = fun tokens ->
        let open Lexer in
        match tokens with
        | LineFeed :: Space :: tokens    -> Dup, tokens
        | LineFeed :: Tab :: tokens      -> Swap, tokens
        | LineFeed :: LineFeed :: tokens -> Drop, tokens
        | Tab :: Space :: tokens ->
          let n, tokens = parse_number tokens in
          Copy n, tokens
        | Tab :: LineFeed :: tokens ->
          let n, tokens = parse_number tokens in
          Slide n, tokens
        | Space :: tokens ->
          let num, tokens = parse_number tokens in
          Push num, tokens
        | _ -> failwith "ParseError: [parse_stack_manipulation] Invalid case"

  and parse_arithmetic 
    : Lexer.tokens -> Whitespace.arithmetic * Lexer.tokens 
    = fun tokens ->
        let open Lexer in
        match tokens with
        | Space :: Space :: tokens    -> Add, tokens
        | Space :: Tab :: tokens      -> Sub, tokens
        | Space :: LineFeed :: tokens -> Mul, tokens
        | Tab :: Space :: tokens      -> Div, tokens
        | Tab :: Tab :: tokens        -> Mod, tokens
        | _ -> failwith "ParseError: [parse_arithmetic] Invalid case"

  and parse_flow_control
    : Lexer.tokens -> Whitespace.flow_control * Lexer.tokens
    = fun tokens ->
        let open Lexer in
        match tokens with
        | Space :: Space :: tokens ->
          let label, tokens = parse_label tokens in
          Mark label, tokens
        | Space :: Tab :: tokens ->
          let label, tokens = parse_label tokens in
          Call label, tokens
        | Space :: LineFeed :: tokens ->
          let label, tokens = parse_label tokens in
          Jmp label, tokens
        | Tab :: Space :: tokens -> 
          let label, tokens = parse_label tokens in
          Jz label, tokens
        | Tab :: Tab :: tokens ->
          let label, tokens = parse_label tokens in
          Jneg label, tokens
        | Tab :: LineFeed :: tokens -> End, tokens
        | LineFeed :: LineFeed :: tokens -> EndProg, tokens
        | _ -> failwith "ParseError: [parse_flow_control] Invalid case"

  and parse_heap_access
    : Lexer.tokens -> Whitespace.heap_access * Lexer.tokens
    = fun tokens ->
        let open Lexer in
        match tokens with
        | Space :: tokens -> Store, tokens
        | Tab :: tokens   -> Retrive, tokens
        | _ -> failwith "ParseError: [parse_heap_access] Invalid case"

  and parse : Lexer.tokens -> Whitespace.t =
    fun tokens ->
      let open Lexer in
      match tokens with
      | []                        -> []
      | Tab :: LineFeed :: tokens -> 
        let io, tokens = parse_io tokens in
        IO io :: parse tokens
      | Tab :: Space :: tokens    -> 
        let ar, tokens = parse_arithmetic tokens in
        Arithmetic ar :: parse tokens
      | Tab :: Tab :: tokens      ->
        let ha, tokens = parse_heap_access tokens in
        HeapAccess ha :: parse tokens
      | Space :: tokens           ->
        let sm, tokens = parse_stack_manipulation tokens in
        StackManipulaton sm :: parse tokens
      | LineFeed :: tokens        ->
        let fc, tokens = parse_flow_control tokens in
        FlowControl fc :: parse tokens
      | _ -> failwith "ParseError: [parse] Invalid case"

end

module Interpreter = struct

  let stack = Stack.create ()

  let heap = Hashtbl.create 512

  let jmp_tbl = Hashtbl.create 512

  let call_stack = Stack.create ()

  let eval_arith op l r =
    let open Whitespace in
    match op with
    | Add -> l + r
    | Sub -> l - r
    | Mul -> l * r
    | Div -> l / r
    | Mod -> l mod r

  let rec record_jmps : Whitespace.t -> unit =
    fun instructions ->
    let open Whitespace in
    match instructions with
    | [] -> ()
    | FlowControl Mark label :: instructions ->
      let () = Hashtbl.add jmp_tbl label instructions in
      record_jmps instructions
    | _::instructions -> record_jmps instructions

  let rec eval : Whitespace.t -> unit =
    fun instructions ->
      let open Whitespace in
      match instructions with
      (* Flow Control *)
      | FlowControl EndProg :: _ | [] -> ()
      | FlowControl Mark label :: instructions ->
        (* let () = Hashtbl.add jmp_tbl label instructions in *)
        eval instructions
      | FlowControl Call label :: instructions ->
        let () = Stack.push instructions call_stack in
        let instructions = Hashtbl.find jmp_tbl label in
        eval instructions
      | FlowControl Jmp label :: instructions ->
        let instructions = Hashtbl.find jmp_tbl label in
        eval instructions
      | FlowControl Jz label :: instructions ->
        if Stack.pop stack = 0
          then let instructions = Hashtbl.find jmp_tbl label in
               eval instructions
        else eval instructions
      | FlowControl Jneg label :: instructions ->
        if Stack.pop stack < 0
          then let instructions = Hashtbl.find jmp_tbl label in
                eval instructions
        else eval instructions
      | FlowControl End :: instructions ->
        let instructions = Stack.pop call_stack in
        eval instructions
      (* IO *)
      | IO OutputChar :: instructions ->
        let top = Stack.pop stack in
        let ch = Char.unsafe_chr top in
        let () = Printf.printf "%c" ch in
        eval instructions
      | IO OutputNumber :: instructions ->
        let top = Stack.pop stack in
        let () = Printf.printf "%d" top in
        eval instructions
      | IO ReadChar :: instructions ->
        let addr = Stack.pop stack in
        let char = input_char stdin in
        let () = Hashtbl.replace heap addr (Char.code char) in
        eval instructions
      | IO ReadNumber :: instructions ->
        let addr = Stack.pop stack in
        let int = int_of_string (input_line stdin) in
        let () = Hashtbl.replace heap addr int in
        eval instructions
      (* Stack Manipulation *)
      | StackManipulaton Push n :: instructions ->
        let () = Stack.push n stack in
        eval instructions
      | StackManipulaton Dup :: instructions ->
        let top = Stack.top stack in
        let () = Stack.push top stack in
        eval instructions
      | StackManipulaton Swap :: instructions ->
        let fst = Stack.pop stack in
        let snd = Stack.pop stack in
        let () = Stack.push fst stack in
        let () = Stack.push snd stack in
        eval instructions
      | StackManipulaton Drop :: instructions ->
        let _ = Stack.pop stack in
        eval instructions
      (* | StackManipulaton Copy n :: instructions -> *)
      (* | StackManipulaton Slide n :: instructions -> *)
      (* Arithmetic *)
      | Arithmetic op :: instructions ->
        let r = Stack.pop stack in
        let l = Stack.pop stack in
        let () = Stack.push (eval_arith op l r) stack in
        eval instructions
      (* Heap Access *)
      | HeapAccess Store :: instructions ->
        let value = Stack.pop stack in
        let addr = Stack.pop stack in
        let () = Hashtbl.replace heap addr value in
        eval instructions
      | HeapAccess Retrive :: instructions ->
        let addr = Stack.pop stack in
        let value = Hashtbl.find heap addr in
        let () = Stack.push value stack in
        eval instructions
      | _ -> failwith "EvalError: [eval] Not implemented"

end

let hello_world = "   \t  \t   \n\t\n     \t\t  \t \t\n\t\n     \t\t \t\t  \n\t\n     \t\t \t\t  \n\t\n     \t\t \t\t\t\t\n\t\n     \t \t\t  \n\t\n     \t     \n\t\n     \t\t\t \t\t\t\n\t\n     \t\t \t\t\t\t\n\t\n     \t\t\t  \t \n\t\n     \t\t \t\t  \n\t\n     \t\t  \t  \n\t\n     \t    \t\n\t\n     \t    \t\n\t\n     \t \t \n\t\n  \n\n\n"
let count = "   \t\n\n   \t    \t\t\n \n \t\n \t   \t \t \n\t\n     \t\n\t    \n    \t \t\t\n\t  \t\n\t  \t   \t \t\n\n \n \t    \t\t\n\n   \t   \t \t\n \n\n\n\n\n"

let read_file file =
  let inc = open_in file in
  let rec read_lines s =
    try
      let line = input_line inc in
      read_lines (s ^line ^ "\n")
    with _ -> close_in inc ; s
  in
  read_lines ""

let ws prg = prg
  |> Lexer.tokenize
  |> Parser.parse
  |> fun ws -> Interpreter.record_jmps ws ; ws
  |> Interpreter.eval

let () = ws hello_world
let () = ws count
let () = ws (read_file "fact.ws")
let () = ws (read_file "fibonacci.ws")