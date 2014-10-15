
(* requires: two ints n and k
returns: function type unit -> int
calling the function returns value of n incremented by k once*)
let count_up_from n k =
	let x = ref (n-k)
	in (fun () -> x:=(!x + k); !x)

(* requires: f a pure function and an int n
returns: array of length n, each index i has the value f i *)
let tabulate f n = 
	let rec make_index_list (i, acc) =
		if i < 0 then acc else make_index_list (i-1, (f i)::acc) 
	in make_index_list ((n-1), [])


(* requires: f a pure function, an initial accumulator,
	and list of elements to fold over
returns: accumulated result of folding over a list,
applying f to each element *)
let fold_left_imp f acc xs = failwith "TODO"

type t = unit  (* TODO: change unit to whatever you want *)
type u = unit  (* TODO: change unit to whatever you want *)
let lst : t list = failwith "TODO"
let zardoz (x:t) : u = failwith "TODO"

