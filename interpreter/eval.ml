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

(* Parses a datum into an expression. *)
let rec read_expression (input : datum) : expression =
  match input with
  | Atom (Identifier id) when Identifier.is_valid_variable id ->
     ExprVariable (Identifier.variable_of_identifier id)
  | Atom (Identifier id) ->
     (* Above match case didn't succeed, so id is not a valid variable. 
        id must be one of the keywords, and I saw that it lined up with the
        expressions listed in ast.ml. Idk what to do with exprassignment, exprselfeval and exprproccall
        *)
     begin match id with
     | quote -> ExprQuote id
     | if -> ExprIf id
     | lambda -> ExprLambda id
     | define -> ExprAssignment (* Check with a TA *)
     | set! -> ExprSelfEvaluating (* Check with a TA *)
     | let -> ExprLet id
     | let* -> ExprLetStar id
     | letrec -> ExprLetRec id
     | _ -> ExprProcCall id (* Check with a TA *)
   end  
  | _ -> (* assuming that this is only reading expressions...*)
     failwith "That's not a valid expression!"

(* Parses a datum into a toplevel input. *)
(* I think this is asking us to use the read_expressions to figure out if its an expression or a definition. As for
the syntax, I'm trying to look at ast for the syntax of toplevelexpresion and topleveldefinition??????? *)
let read_toplevel (input : datum) : toplevel =
  match input with
  | Atom(Identifier id) -> ToplevelExpression (read_expression input) 
  | (define Atom(Identifier var) Atom(Identifier exp)) -> ToplevelDefinition (read_expression var, read_expression exp) (* WHAT DO SYNTAX *)
  | _ -> failwith "That's not a valid toplevel!" (* are there other forms of toplevel?*)

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
