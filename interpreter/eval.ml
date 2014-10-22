(* How to deal with letbindings?*)

open Ast

type builtin = value list -> environment -> value

and procedure =
  | ProcBuiltin of builtin
  | ProcLambda of variable list * environment * expression list

and value =
  | ValDatum of datum
  | ValProcedure of procedure

and binding = value ref Environment.binding (* (Identifier.variable, value ref) *)
and environment = value ref Environment.environment

(* let rec makelst (l: 'a list) (dat: datum) : 'a list =
  match dat with
  | Nil -> List.rev l
  | Cons( x, y) -> makelst (x::l) y 
 *)
let rec read_expression (input : datum) : expression =
  match input with
  | Atom (Identifier id) when Identifier.is_valid_variable id ->
     ExprVariable (Identifier.variable_of_identifier id)
  | Atom (Identifier id) -> failwith "That's not a valid variable"
  | Atom (Boolean b) -> ExprSelfEvaluating (SEBoolean b) 
  | Atom (Integer i) -> ExprSelfEvaluating (SEInteger i)
  | Nil -> failwith "Unknown expression form"
  | _ -> failwith "Unknown expression form"

(* Parses a datum into a toplevel input. *)
let read_toplevel (input : datum) : toplevel =
  match input with
  (* ONLY WORKS FOR EXPRESSIONS FOR NOW *)
  | _ -> ToplevelExpression (read_expression input)

let eval_se (se : Ast.self_evaluating) : value =
  match se with
  | SEBoolean b -> ValDatum(Ast.Atom(Ast.Boolean b))
  | SEInteger i -> ValDatum(Ast.Atom(Ast.Integer i))

let eval_v (v : Ast.variable) (env: environment) : value =
  if Environment.is_bound env v
  then !(Environment.get_binding env v)
  else let var = Identifier.string_of_variable v in
    failwith (var^" is not bound in this environment.")

(* This function returns an initial environment with any built-in
   bound variables. *)
let rec initial_environment () : environment =
  let env = Environment.empty_environment in
  (* adding course -> 3110 *)
  Environment.add_binding env
  (Identifier.variable_of_identifier(Identifier.identifier_of_string "course"),
  ref (ValDatum(Ast.Atom(Ast.Integer 3110))))

(* Evaluates an expression down to a value in a given environment. *)
(* You may want to add helper functions to make this function more
   readable, because it will get pretty long!  A good rule of thumb
   would be a helper function for each pattern in the match
   statement. *)
and eval (expression : expression) (env : environment) : value =
  match expression with
  | ExprSelfEvaluating se -> eval_se se
  | ExprVariable v -> eval_v v env
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
