open Assertions
open Mutability


let c = count_up_from 3 2
TEST_UNIT "count_up_from_test1" = assert_true (c() = 3)
TEST_UNIT "count_up_from_test1" = assert_true (c() = 5)
TEST_UNIT "count_up_from_test1" = assert_true (c() = 7)

let my_fun (i: int) : int =
	i + 10
TEST_UNIT "tabulate_test1" = assert_true(tabulate my_fun 5 = [|10;11;12;13;14|])

TEST_UNIT "fold_left_imp_test1" = assert_true ((fold_left_imp (fun acc elm -> acc + elm) 0 [1;2;3]) = 6)
TEST_UNIT "zardoz_test" = assert_false (List.map zardoz (List.rev lst) = List.rev (List.map zardoz lst))

