open Array

(* requires: two ints n and k
returns: function type unit -> int
calling the function returns value of n incremented by k once
effects: increases ref value of function by k *)
let count_up_from n k =
	let x = ref (n-k)
	in (fun () -> x:=(!x + k); !x)

(* requires: f a pure function and an int n
returns: array of length n, each index i has the value f i *)
let tabulate f n = 
	(* build up a list of length n *)
	let rec make_index_list (i, acc) =
		if i < 0 then acc else make_index_list (i-1, (f i)::acc) 
	(* convert list to array *)
	in Array.of_list (make_index_list ((n-1), []))

(* Implementation of fold_left without recursion
requires: f a pure function, initial 'a accumulator, and 'b list to fold over
returns: value of 'a accumulator after applying f to acc and each element *)
let fold_left_imp f acc xs =
	(* turn acc into reference *)
	let acc = ref acc in
	(* turn xs into array *)
	let xs = Array.of_list xs in
	(* loop through the list *)
	for i = 0 to ((Array.length xs) - 1) do
		acc:=f !acc xs.(i)
	done; !acc (* return final value of acc *)

(* List.map zardoz (List.rev lst) != List.rev (List.map zardoz lst) *)
type t = int ref
type u = int
let lst : t list = [ref 1; ref 2; ref 3]
let zardoz (x: t) : u = x:= (!x)*(!x); !x