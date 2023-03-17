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

  type head_access = Store | Retrive

  type instruction = 
    IO of io 
  | StackManipulaton of stack_manipulation
  | Arithmetic of arithmetic
  | FlowControl of flow_control
  | HeapAccess of head_access

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
    : Lexer.tokens -> Whitespace.head_access * Lexer.tokens
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

let hello_world = "   \t  \t   \n\t\n     \t\t  \t \t\n\t\n  \t\n     \t\t \t\t  \n\t\n     \t\t \t\t  \n\t\n     \t\t \t\t\t\t\n\t\n     \t \t\t  \n\t\n     \t     \n\t\n     \t\t\t \t\t\t\n\t\n     \t\t \t\t\t\t\n\t\n     \t\t\t  \t \n\t\n     \t\t \t\t  \n\t\n     \t\t  \t  \n\t\n     \t    \t\n\t\n     \t    \t\n\t\n  \n\n\n"

let ws = hello_world
  |> Lexer.tokenize
  |> Parser.parse