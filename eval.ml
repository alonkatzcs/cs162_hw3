open Ast

(* Helper function for parsing an expression. Useful for testing. *)
let parse (s: string) : Ast.expr =
  Parser.main Scanner.token (Lexing.from_string s)
(*******************************************************************|
|**********************   Interpreter   ****************************|
|*******************************************************************|
|*******************************************************************)

(* Exception indicating that evaluation is stuck *)
exception Stuck of string

(* Raises an exception indicating that evaluation got stuck. *)
let im_stuck msg = raise (Stuck msg)

(* Raises an exception for things that need to be implemented
 * in this assignment *)
let todo () = failwith "TODO"

(* Raises an exception for things to be in the next assignment *)
let hw4 () = failwith "Homework 4"

(* Helper function to check that an expression is a value, otherwise raises a
   Stuck exception. *)
let assert_value e =
  if is_value e then () else im_stuck (string_of_expr e ^ " is not a value")



(* Evaluates expression e *)
let rec eval (e : expr) : expr =
  try
    match e with
    (* Things you need to implement *)
    | NumLit n -> NumLit n
    | Binop (e1, op, e2) -> (match op, eval e1, eval e2 with
                            | Add , NumLit ex1, NumLit ex2 -> NumLit(ex1 + ex2)
                            | Sub , NumLit ex1, NumLit ex2 -> NumLit(ex1 - ex2)
                            | Mul , NumLit ex1, NumLit ex2 -> NumLit(ex1 * ex2)
                            | Gt  , NumLit ex1, NumLit ex2 -> if ex1 > ex2 then NumLit(1) else NumLit(0)
                            | Lt  , NumLit ex1, NumLit ex2 -> if ex1 < ex2 then NumLit(1) else NumLit(0)
                            | And , NumLit ex1, NumLit ex2 -> if ex1 <> 0 && ex2 <> 0 then NumLit(1) else NumLit(0)
                            | Or , NumLit ex1, NumLit ex2 -> if ex1 <> 0 || ex2 <> 0 then NumLit(1) else NumLit(0)
                            | Eq , NumLit ex1, NumLit ex2 -> if ex1 = ex2 then NumLit(1) else NumLit(0)
                            | _ -> im_stuck ("Binop")
                            )
    | IfThenElse (e1, e2, e3) -> (match eval e1 with
                                  | NumLit e ->  if (e <> 0) then (eval e2) else (eval e3)
                                  | _ -> im_stuck ("IfThenElse")
                                  ) 
    | ListNil -> ListNil 
    | ListCons (e1, e2) -> ListCons(eval e1, eval e2)
    | ListHead e -> match eval e with
                    | ListCons (e1, e2) -> e1
                    | _ -> im_stuck ("ListHead")
    | ListTail e -> match eval e with
                    | ListCons (e1, e2) -> e2
                    | _ -> im_stuck ("ListTail")
    | ListIsNil e -> match eval e with
                    | ListCons (e1, e2) -> NumLit 0
                    | ListNil -> NumLit 1
                    | _ -> im_stuck ("ListIsNil")
    (* Things you don't need to implement in this assignment *)
    | _ -> hw4 ()
  with
  | Stuck msg -> im_stuck (msg ^ "\nin expression " ^ string_of_expr e)



