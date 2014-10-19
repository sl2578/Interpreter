(* 1. testing NoResult
	 2. TakeIterator ListIterator *)

open Assertions
open Iterator

let list_iterator = ListIterator.create [1;2]
TEST_UNIT "ListIterator_test1" = assert_true (ListIterator.has_next list_iterator = true)
TEST_UNIT "ListIterator_test2" = assert_true (ListIterator.next list_iterator = 1)
TEST_UNIT "ListIterator_test3" = assert_true (ListIterator.next list_iterator = 2)
(* TEST_UNIT "ListIterator_test4" = assert_raises (Some NoResult) ListIterator.next list_iterator *)

let inorder_tree_iterator = InorderTreeIterator.create (Node(2, Node(1, Leaf, Leaf), Leaf))
TEST_UNIT "InorderTreeIterator_test1" = assert_true (InorderTreeIterator.has_next inorder_tree_iterator = true)
TEST_UNIT "InorderTreeIterator_test2" = assert_true (InorderTreeIterator.next inorder_tree_iterator = 1)
TEST_UNIT "InorderTreeIterator_test3" = assert_true (InorderTreeIterator.next inorder_tree_iterator = 2)
(* TEST_UNIT "InorderTreeIterator_test4" = assert_raises (Some NoResult) InorderTreeIterator.next inorder_tree_iterator *)

module TakeListIter : TAKE_ITERATOR = TakeIterator (Iterator.ListIterator)
let iter1 = TakeListIter.create 3 ListIterator.create [1;2]
let iter2 = TakeListIter.create 2 ListIterator.create [1;2;3]
let iter3 = TakeListIter.create 0 ListIterator.create [1;2;3]
let iter4 = TakeListIter.create 2 ListIterator.create []

TEST_UNIT "TakeIterator_test1" = assert_true (Iter.has_next iter1 = true)
TEST_UNIT "TakeIterator_test2" = assert_true (Iter.has_next iter3 = false)
TEST_UNIT "TakeIterator_test3" = assert_true (Iter.has_next iter4 = false)
TEST_UNIT "TakeIterator_test4" = assert_true (Iter.next iter2 = 1)
TEST_UNIT "TakeIterator_test5" = assert_true (Iter.next iter2 = 2)
TEST_UNIT "TakeIterator_test6" = assert_raises (Some NoResult) Iter.next iter2
