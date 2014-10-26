(* How to deal with letbindings?
 *)

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

(* requires: list of Cons
returns: list of datum in a list of Cons *)
let listify (dat : datum) : 'a list =
  let rec helper (acc: 'a list) (dat: datum) : ' a list =
    match dat with
    | Nil -> acc
    | Cons(x, y) -> helper (acc@[x]) y
    | _ -> failwith "Not a list of Cons!" in
  helper [] dat

let rec read_expression (input : datum) : expression =
  match input with
  (* matches variables *)
  | Atom (Identifier id) ->
    if Identifier.is_valid_variable id then 
    ExprVariable (Identifier.variable_of_identifier id)
    else failwith "That's not a valid variable"
  (* matches bools, ints *)
  | Atom (Boolean b) -> ExprSelfEvaluating (SEBoolean b) 
  | Atom (Integer i) -> ExprSelfEvaluating (SEInteger i) 
  (* matches quote *)
  | Cons (Atom (Identifier id), Cons(dat, Nil)) 
    when id = Identifier.identifier_of_string "quote" ->  ExprQuote dat
  (* matches if *)
  | Cons (Atom (Identifier id), Cons(exp1, Cons (exp2, Cons (exp3, Nil)))) 
    when id = Identifier.identifier_of_string "if" -> 
      ExprIf (read_expression exp1, read_expression exp2, read_expression exp3)
  (* matches lambdas *)
  | Cons (Atom (Identifier id), Cons(varlst, explst))
    when id = Identifier.identifier_of_string "lambda" ->
      (* read in each variable datum *)
      let helper acc elm =
        let v = read_expression elm 
          in match v with
          | ExprVariable x -> acc@[x]
          | _ -> failwith "Invalid lambda variable"
        in ExprLambda (
        (List.fold_left (fun acc elm -> helper acc elm) [] (listify varlst)),
        (* read in each expression *)
        (List.fold_left (fun acc elm -> acc@[(read_expression elm)]) [] (listify explst)))
  (* not tested: matches define *)
  | Cons (Atom(Identifier id), _)
    when id = Identifier.identifier_of_string "define" ->
      failwith "Define not allowed as an expression, only at the toplevel"
  | Cons(Atom (Identifier id), Cons(Atom (Identifier var), Cons(exp, Nil)))
    when id = Identifier.identifier_of_string "set!" ->
      ExprAssignment ((Identifier.variable_of_identifier var), read_expression exp)
  (* not tested: matches assignment *)
  (* | Cons (Atom (Identifier id), ) *)
  (* matches Nil *)
  | Nil -> failwith "NILLLLLLLLL Unknown expression form"
  (* matches procedures *)
  | Cons (x, explst) ->
    (* read in each expression datum *)
    let lst = (List.fold_left (fun acc elm -> acc@[(read_expression elm)]) [] (listify explst)) in
    ExprProcCall((read_expression x), lst)

(* Parses a datum into a toplevel input. *)
let read_toplevel (input : datum) : toplevel =
  match input with
  | Cons(Atom (Identifier define), Cons(Atom (Identifier var), Cons(exp, Nil)))
    when define = Identifier.identifier_of_string "define" -> 
    ToplevelDefinition ((Identifier.variable_of_identifier var), read_expression exp)
  | _ -> ToplevelExpression (read_expression input)

(* requires: single cons-cell datum
returns: first element of a cons-cell datum *)
let car (cons : value list) (env : environment) : value =
  if List.length cons <> 1 then failwith "Invalid arguments to car."
  else match cons with
  | (ValDatum (Cons(x, _)))::t -> ValDatum x
  | _ -> failwith "Invalid arguments to car."

(* requires: single cons-cell datum
returns: second element of a cons-cell datum *)
let cdr (cons: value list) (env : environment) : value =
  if List.length cons <> 1 then failwith "Invalid arguments to carcdr."
  else match cons with
  | (ValDatum (Cons(_, x)))::t -> ValDatum x
  | _ -> failwith "Invalid arguments to cdr."

(* requires: exactly two datum in value list
returns: cons-cell of two datum *)
let cons (dats : value list) (env : environment) : value =
  match dats with
  | [ValDatum x; ValDatum y] -> ValDatum(Cons(x, y))
  | _ -> failwith "Invalid arguments to cons."

(* requires: value list, environment,
function taking two ints and returns and int and initial value for a fold
returns: result of folding with function on each value in list *)
let operate (lst : value list) (env: environment) (op: int -> int -> int) (init : int) : value =
  if List.length lst < 1 then failwith "Invalid arguments to arithmetic function"
  else let rec helper (acc : int) (elm: value) : int =
    match elm with
    | ValDatum(Atom(Integer i)) -> (op acc i)
    (* look up variable in environment *)
    | ValDatum(Atom(Identifier i)) when Identifier.is_valid_variable i ->
      (* if variable is bound *)
      if Environment.is_bound env (Identifier.variable_of_identifier i) then
        (* get value in environment and multiply by acc *)
        let v = !(Environment.get_binding env (Identifier.variable_of_identifier i)) in
          match v with
          | ValDatum(Atom(Integer i)) -> (op acc i)
          (* variable not bound to an integer *)
          | _ -> failwith "Invalid arguments to arithmetic function"
      else failwith "Variable is not bound in this environment."
    (* not an integer or variable *)
    | _ -> failwith "Invalid arguments to arithmetic function" in
    ValDatum(Atom(Integer (List.fold_left helper init lst)))

(* returns: sum of values in lst *)
let add (lst : value list) (env : environment) : value = operate lst env ( + ) 0

(* returns: product of values in lst *)
let mult (lst : value list) (env : environment) : value = operate lst env ( * ) 1

(* requires: two arguments that are not procedures
returns: whether its first argument is structurally equal
(in the OCaml sense of (=)) to its second argument *)
let equal (args: value list) (env : environment) : value =
  match args with
  | x::y::[] -> ValDatum(Atom(Boolean (x=y)))
  | _ -> failwith "Invalid arguments to equal?."

(* Returns: value of the self_evaluating expression *)
let eval_se (se : self_evaluating) : value =
  match se with
  | SEBoolean b -> ValDatum(Atom(Boolean b))
  | SEInteger i -> ValDatum(Atom(Integer i))

(* Requires: a valid variable of identifier
Returns: value of the variable in the environment *)
let eval_v (v : variable) (env: environment) : value =
  if Environment.is_bound env v
  then !(Environment.get_binding env v)
  else failwith "Variable is not bound in this environment."

(* This function returns an initial environment with any built-in
   bound variables. *)
let rec initial_environment () : environment =
  let env = Environment.empty_environment in
  (* binding course -> 3110 *)
  let env = Environment.add_binding env
    (Identifier.variable_of_identifier(Identifier.identifier_of_string "course"),
    ref (ValDatum(Atom(Integer 3110)))) in
  let env = Environment.add_binding env
    (Identifier.variable_of_identifier(Identifier.identifier_of_string "car"),
    ref (ValProcedure(ProcBuiltin car))) in
  let env = Environment.add_binding env
    (Identifier.variable_of_identifier(Identifier.identifier_of_string "cdr"),
    ref (ValProcedure(ProcBuiltin cdr))) in
  let env = Environment.add_binding env
    (Identifier.variable_of_identifier(Identifier.identifier_of_string "cons"),
    ref (ValProcedure(ProcBuiltin cons))) in
  let env = Environment.add_binding env
    (Identifier.variable_of_identifier(Identifier.identifier_of_string "+"),
    ref (ValProcedure(ProcBuiltin add))) in
  let env = Environment.add_binding env
    (Identifier.variable_of_identifier(Identifier.identifier_of_string "*"),
    ref (ValProcedure(ProcBuiltin mult))) in
  let env = Environment.add_binding env
    (Identifier.variable_of_identifier(Identifier.identifier_of_string "equal?"),
    ref (ValProcedure(ProcBuiltin equal))) in
  (* since eval takes in wrong type, we need our own evaluate for the builtin *)
  let evaluate (arg : value list) (env : environment) : value =
    match arg with
    | (ValDatum x)::[] -> eval (read_expression x) env
    | _ -> failwith "Invalid arguments to eval." in
  Environment.add_binding env
    (Identifier.variable_of_identifier(Identifier.identifier_of_string "eval"),
    ref (ValProcedure(ProcBuiltin evaluate)))

(* Evaluates an expression down to a value in a given environment. *)
(* You may want to add helper functions to make this function more
   readable, because it will get pretty long!  A good rule of thumb
   would be a helper function for each pattern in the match
   statement. *)
and eval (expression : expression) (env : environment) : value =
  match expression with
  | ExprSelfEvaluating se -> eval_se se
  | ExprVariable v -> eval_v v env
  | ExprQuote q -> ValDatum q
  | ExprLambda (varlst, explst) ->
      ValProcedure (ProcLambda (varlst, env, explst))
  | ExprProcCall (exp, inputs) ->
    begin match (eval exp env) with
    (* match if exp evaluates to type ProcLambda, a closure? *)
    | ValProcedure (ProcLambda (varlst, env2, explst)) ->
      (* check right num variables *)
      if List.length varlst <> List.length inputs
      then failwith "Invalid number of arguments to procedure."
      else
        let old_env = env2 in 
        let helper acc var input =
          Environment.add_binding acc (var, ref (eval input old_env)) in
        (* evaluate each input and bind to corresponding varlst, add to env *)
        let new_env = List.fold_left2 helper env2 varlst inputs in
        (* evaluate the procedure with new bindings *)
        List.fold_left (fun acc exp -> eval exp new_env) (ValDatum Nil) explst
    | _ -> failwith "Procedure does not evaluate to a closure."
    end
  | ExprIf (ExprSelfEvaluating (SEBoolean b), exp2, exp3) -> 
      if not b then eval exp3 env else eval exp2 env
  | ExprAssignment (var, exp) ->
      if (Environment.is_bound env var) then begin
        (Environment.get_binding env var) := (eval exp env); ValDatum(Nil) end
      else failwith ("Variable is not bounded in this environment")
  | ExprLet (_, _) -> failwith "d"
  | ExprLetStar (_, _) -> failwith "a"
  | ExprLetRec (_, _) -> failwith "Ahahaha! That is classic Rower."
  | _ -> failwith "Not a valid expression"

(* Evaluates a toplevel input down to a value and an output environment in a
   given environment. *)
let eval_toplevel (toplevel : toplevel) (env : environment) :
      value * environment =
  match toplevel with
  | ToplevelExpression expression -> (eval expression env, env)
  | ToplevelDefinition (var, exp) -> 
    if not (Environment.is_bound env var) then 
      let new_env = Environment.add_binding  env (var, ref (eval exp Environment.empty_environment)) 
    in 
      (ValDatum(Nil), new_env)
    else
      begin ((Environment.get_binding env var) := (eval exp Environment.empty_environment)); 
      (ValDatum(Nil), env) end

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
