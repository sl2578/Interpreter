open Assertions
open Iterator

list_iterator = ListIterator.create [1;2]
TEST_UNIT "ListIterator_test1" = assert_true (ListIterator.has_next list_iterator = true)
TEST_UNIT "ListIterator_test2" = assert_true (ListIterator.next list_iterator = 1)
TEST_UNIT "ListIterator_test3" = assert_true (ListIterator.next list_iterator = 2)
(* TEST_UNIT "ListIterator_test4" = assert_raises (Some NoResult) ListIterator.next list_iterator *)

inorder_tree_iterator = InorderTreeIterator.create (Node(2, Node(1, Leaf, Leaf), Leaf))
TEST_UNIT "InorderTreeIterator_test1" = assert_true (InorderTreeIterator.has_next inorder_tree_iterator = true)
TEST_UNIT "InorderTreeIterator_test2" = assert_true (InorderTreeIterator.next inorder_tree_iterator = 1)
TEST_UNIT "InorderTreeIterator_test3" = assert_true (InorderTreeIterator.next inorder_tree_iterator = 2)
(* TEST_UNIT "InorderTreeIterator_test4" = assert_raises (Some NoResult) InorderTreeIterator.next inorder_tree_iterator *)

module Iter : TAKE_ITERATOR = TakeIterator ListIterator
iter1 = Iter.create 3 
TEST_UNIT "TakeIterator_test1" = assert_true (Iter.)