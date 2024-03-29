module type ITERATOR = sig
  type 'a t
  exception NoResult

  (* returns: true if there are more results to yield,
   *   otherwise returns false. *)
  val has_next: 'a t -> bool

  (* returns:  the next result, if there are more results
   *   to yield.  Otherwise raises NoResult.
   * effects:  modifies the state of the iterator to record
   *   the yield. *)
  val next: 'a t -> 'a
end


module type LIST_ITERATOR = sig
  include ITERATOR
  (* parameters:  a list l
   * returns:  an iterator that will yield the elements of l,the
   *   each exactly once, in the order that they appear in l,
   *   starting with the head.  *)
  val create: 'a list -> 'a t
end

module ListIterator : LIST_ITERATOR = struct
  type 'a t = 'a list ref
  exception NoResult

  let has_next (l: 'a t) : bool = List.length !l > 0

  let next (l: 'a t) : 'a =
    match !l with
    | [] -> raise NoResult
    | h::t -> l := t; h

  let create (l: 'a list) : 'a t = ref l
end


type 'a tree = Leaf | Node of ('a * 'a tree * 'a tree)

module type INORDER_TREE_ITERATOR = sig
  include ITERATOR
  (* parameters:  a tree t
   * returns:  an iterator that will yield the elements of t,
   *   each exactly once, in the order that would be produced
   *   by an in-order traversal of t. *)
  val create: 'a tree -> 'a t
end

module InorderTreeIterator : INORDER_TREE_ITERATOR = struct
  type 'a t = 'a list ref
  exception NoResult

  let has_next (l: 'a t) : bool = List.length !l > 0

  let next (l: 'a t) : 'a =
    match !l with
    | [] -> raise NoResult
    | h::t -> l := t; h

  let create (tr : 'a tree) : 'a t =
    let rec traverse (tr: 'a tree) : 'a list =
      match tr with
      | Leaf -> []
      | Node (v, l, r) -> (traverse l)@[v]@(traverse r) in
    ref (traverse tr)
end


module type TAKE_ITERATOR = functor (I: ITERATOR) -> sig
  include ITERATOR

  (* parameters:  an integer n and an iterator i
   * returns:  an iterator that behaves the same as i for
   *   exactly n calls to next, but afterwards
   *   raises NoResult. *)
  val create: int -> 'a I.t -> 'a t
end

module TakeIterator : TAKE_ITERATOR = functor (I : ITERATOR) -> struct
  (* tuple of iterator and int, where int is the n number
  of times iterator can still return a result *)
  type 'a t = ('a I.t * int) ref
  exception NoResult

  let has_next (i: 'a t) : bool = I.has_next (fst !i) && (snd !i) <> 0

  let next (i: 'a t) : 'a =
    match !i with
    | iter, 0 -> raise NoResult
    | iter, n -> let res = I.next iter in i:= (iter, n-1); res

  (* requires: n nonnegative
  returns: iterator that yields first n results of iterator*)
  let create (n: int) (iter: 'a I.t) : 'a t = ref (iter, n)
end


module IteratorUtilsFn (I : ITERATOR) = struct
  open I

  (* effects: causes i to yield n results, ignoring
   *   those results.  Raises NoResult if i does.  *)
  let advance (n: int) (iter: 'a I.t) : unit =
    for i=n downto 1 do
      I.next iter
    done

  (* returns: the final value of the accumulator after
   *   folding f over all the results returned by i,
   *   starting with acc as the initial accumulator.
   * effects: causes i to yield all its results. *)
  let rec fold (f : ('a -> 'b -> 'a)) (acc : 'a) (iter: 'b I.t) : 'a =
    if I.has_next iter then
      fold f (f acc (I.next iter)) iter
    else acc
end


module type RANGE_ITERATOR = functor (I : ITERATOR) -> sig
  include ITERATOR

  (* parameters: integers n and m and an iterator i
   * returns: an iterator that behaves the way I would
   *   on the nth through mth calls to next, and
   *   afterwards raises NoResult.
   *
   *   If n > m the resulting iterator should always raise NoResult.
   *)
  val create : int -> int -> 'a I.t -> 'a t
end

module RangeIterator : RANGE_ITERATOR = functor (I : ITERATOR) -> struct
  (* tuple of iterator and int, where int is the n number
  of times iterator can still return a result *)
  type 'a t = ('a I.t * int) ref
  exception NoResult

  let has_next (i: 'a t) : bool = I.has_next (fst !i) && (snd !i) <> 0

  let next (i: 'a t) : 'a =
    match !i with
    | iter, 0 -> raise NoResult
    | iter, n -> let res = I.next iter in i:= (iter, n-1); res

  module Utils = IteratorUtilsFn (I)
  (* requires: n and m not negative
  returns: iterator that yields the nth to (including) the mth element
  or NoResult if n > m *)
  let create (n: int) (m: int) (iter: 'a I.t) : 'a t =
    Utils.advance (n-1) iter;
    (* iter now at nth elm, with m-n more elements left to iter through *)
    ref (iter, (max 0 (m-n+1)))
end

