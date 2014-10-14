let count_up_from n k =
	let x = ref (n-k)
	in (fun () -> x:=(!x + k); !x)

let tabulate f n = 
	let rec make_index_list (i, acc) =
		if i < 0 then acc else make_index_list (i-1, (f i)::acc) 
	in make_index_list ((n-1), [])

let fold_left_imp f acc xs = failwith "TODO"

type t = unit  (* TODO: change unit to whatever you want *)
type u = unit  (* TODO: change unit to whatever you want *)
let lst : t list = failwith "TODO"
let zardoz (x:t) : u = failwith "TODO"

