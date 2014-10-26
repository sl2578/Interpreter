(* 1. testing NoResult
	 2. TakeIterator ListIterator *)

open Assertions
open Iterator

let list_iterator = ListIterator.create [1;2]
let empty_iterator = ListIterator.create ([] : int list)
TEST_UNIT "ListIterator_test1" = assert_true (ListIterator.has_next list_iterator = true)
TEST_UNIT "ListIterator_test2" = assert_true (ListIterator.next list_iterator = 1)
TEST_UNIT "ListIterator_test3" = assert_true (ListIterator.next list_iterator = 2)
TEST_UNIT "ListIterator_test4" = assert_raises (Some ListIterator.NoResult) ListIterator.next empty_iterator

let inorder_tree_iterator = InorderTreeIterator.create (Node(2, Node(1, Leaf, Leaf), Leaf))
TEST_UNIT "InorderTreeIterator_test1" = assert_true (InorderTreeIterator.has_next inorder_tree_iterator = true)
TEST_UNIT "InorderTreeIterator_test2" = assert_true (InorderTreeIterator.next inorder_tree_iterator = 1)
TEST_UNIT "InorderTreeIterator_test3" = assert_true (InorderTreeIterator.next inorder_tree_iterator = 2)
TEST_UNIT "InorderTreeIterator_test4" = assert_raises (Some InorderTreeIterator.NoResult) InorderTreeIterator.next inorder_tree_iterator

module TakeListIter = TakeIterator(ListIterator)
let iter1 = TakeListIter.create 3 (ListIterator.create [1;2])
let iter2 = TakeListIter.create 2 (ListIterator.create [1;2;3])
let iter3 = TakeListIter.create 0 (ListIterator.create [1;2;3])
let iter4 = TakeListIter.create 2 (ListIterator.create ([] : int list))

TEST_UNIT "TakeIterator_test1" = assert_true (TakeListIter.has_next iter1 = true)
TEST_UNIT "TakeIterator_test2" = assert_true (TakeListIter.has_next iter3 = false)
TEST_UNIT "TakeIterator_test3" = assert_true (TakeListIter.has_next iter4 = false)
TEST_UNIT "TakeIterator_test4" = assert_true (TakeListIter.next iter2 = 1)
TEST_UNIT "TakeIterator_test5" = assert_true (TakeListIter.next iter2 = 2)
TEST_UNIT "TakeIterator_test6" = assert_raises (Some TakeListIter.NoResult) TakeListIter.next iter2

let list_iter = ListIterator.create [1;2;3;4]
module UtilsIter = IteratorUtilsFn (ListIterator)
UtilsIter.advance 1 list_iter
TEST_UNIT "IteratorUtilsFn_test1" = assert_true (ListIterator.next list_iter = 2)
TEST_UNIT "IteratorUtilsFn_test2" = assert_true (UtilsIter.fold (fun x y -> x+y) 0 list_iter = 7)

module RangeIter = RangeIterator (ListIterator)
let range_iter = RangeIter.create 1 3 (ListIterator.create [1;2;3;4;5]) (* should yield [1;2;3] *)
TEST_UNIT "RangeIterator_test1" = assert_true (RangeIter.has_next range_iter = true)
TEST_UNIT "RangeIterator_test2" = assert_true (RangeIter.next range_iter = 1)
let res = RangeIter.next range_iter
let res = RangeIter.next range_iter
TEST_UNIT "RangeIterator_test3" = assert_true (RangeIter.has_next range_iter = false)
TEST_UNIT "RangeIterator_test4" = assert_raises (Some RangeIter.NoResult) RangeIter.next range_iter