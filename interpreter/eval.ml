(* How to deal with letbindings?*)

open Ast

type builtin = value list -> environment -> value

and procedure =
  | ProcBuiltin of builtin
  | ProcLambda of variable list * environment * expression list

and value =
  | ValDatum of datum
  | ValProcedure of procedure

and binding = value ref Environment.binding
and environment = value ref Environment.environment

exception InvalidVariable

let rec makelst (l: 'a list) (dat: datum) : 'a list =
  match dat with
  | Nil -> List.rev l
  | Cons( x, y) -> makelst (x::l) y 
  
(* Parses a datum into an expression.*)
let rec read_expression (input : datum) : expression =
  match input with
  | Atom (Identifier id) when Identifier.is_valid_variable id ->
     ExprVariable (Identifier.variable_of_identifier id)
  | Atom (Identifier id) -> 
    failwith "That's not a valid variable"
  | Atom (Boolean b) -> ExprSelfEvaluating (SEBoolean b) 
  | Atom (Integer i) -> ExprSelfEvaluating (SEInteger i) 
  | Cons (Atom (Identifier id)), cdr) -> 
    begin match id, cdr with
     |"quote", _ -> ExprQuote cdr
     |"if", Cons(exp1, Cons (exp2, Cons (exp3, Nil)))-> ExprIf (exp1, exp2, exp3)
     |"lambda", Cons(exp1, Cons(exp2, Nil)) -> ExprLambda (makelst exp1, makelst exp2) (* WATDO *)
     |"define", _ -> failwith "That's not a valid variable"
     |"set!", Cons(var, Cons(exp, Nil)) -> ExprAssignment (var, exp)
     |"let", Cons(letblst, Cons(explst, Nil)) -> ExprLet (makelst [] letblst, makelst [] explst)
     |"let*", Cons(letblst, Cons(explst, Nil)) -> ExprLetStar (makelst [] letblst, makelst [] explst) 
     |"letrec", Cons(letblst, Cons(explst, Nil)) -> ExprLetRec ( makelst [] letblst, makelst [] explst)
     |exp1, Cons(explst, Nil) -> ExprProcCall(exp1, makelst [] explst)                         
    end
  | Nil -> failwith "Unknown expression form"
  | _ -> failwith "Unknown expression form"

(* Parses a datum into a toplevel input. *)
let read_toplevel (input : datum) : toplevel =
  match Cons(e1, e2) with
  |"define", Cons(var, Cons(exp, Nil)) -> ToplevelDefinition (read_expression var, read_expression exp)
  |exp, Nil -> ToplevelExpression (read_expression exp)
  | _ -> failwith "Unknown toplevel form"

(* This function returns an initial environment with any built-in
   bound variables. *)
let rec initial_environment () : environment =
  failwith "You know!"

(* Evaluates an expression down to a value in a given environment. *)
(* You may want to add helper functions to make this function more
   readable, because it will get pretty long!  A good rule of thumb
   would be a helper function for each pattern in the match
   statement. *)
and eval (expression : expression) (env : environment) : value =
  match expression with
  | ExprSelfEvaluating _
  | ExprVariable _        ->
     failwith "'Oh I sure love to row my boat with my...oar."
  | ExprQuote _           ->
     failwith "Rowing!"
  | ExprLambda (_, _)
  | ExprProcCall _        ->
     failwith "Sing along with me as I row my boat!'"
  | ExprIf (_, _, _) ->
     failwith "But I love you!"
  | ExprAssignment (_, _) ->
     failwith "Say something funny, Rower!"
  | ExprLet (_, _)
  | ExprLetStar (_, _)
  | ExprLetRec (_, _)     ->
     failwith "Ahahaha!  That is classic Rower."

(* Evaluates a toplevel input down to a value and an output environment in a
   given environment. *)
let eval_toplevel (toplevel : toplevel) (env : environment) :
      value * environment =
  match toplevel with
  | ToplevelExpression expression -> (eval expression env, env)
  | ToplevelDefinition (_, _)     ->
     failwith "I couldn't have done it without the Rower!"

let rec string_of_value value =
  let rec string_of_datum datum =
    match datum with
    | Atom (Boolean b) -> if b then "#t" else "#f"
    | Atom (Integer n) -> string_of_int n
    | Atom (Identifier id) -> Identifier.string_of_identifier id
    | Nil -> "()"
    | Cons (car, cdr) -> string_of_cons car cdr

  and string_of_cons car cdr =
    let rec strings_of_cons cdr =
      match cdr with
      | Nil -> []
      | Cons (car, cdr) -> (string_of_datum car) :: (strings_of_cons cdr)
      | _ -> ["."; string_of_datum cdr;] in
    let string_list = (string_of_datum car) :: (strings_of_cons cdr) in
    "(" ^ (String.concat " " string_list) ^ ")" in
  
  match value with
  | ValDatum (datum) -> string_of_datum datum
  | ValProcedure (ProcBuiltin p) -> "#<builtin>"
  | ValProcedure (ProcLambda (_, _, _)) -> "#<lambda>"
