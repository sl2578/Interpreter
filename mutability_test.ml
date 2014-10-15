open Assertions
open Mutability

TEST_UNIT "fold_left_imp_test1" = assert_true ((fold_left_imp (fun acc elm -> acc + elm) 0 [1;2;3]) = 6)
