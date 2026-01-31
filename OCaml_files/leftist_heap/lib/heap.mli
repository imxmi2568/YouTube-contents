module type OrderedType = sig
  type t
  val compare: t -> t -> int
end

module type H = sig
	type value

	type heap

	exception EmptyHeap

	val empty: heap

	val merge: heap -> heap -> heap

	val insert: value -> heap -> heap

	val find_min: heap -> value

	val delete_min: heap -> heap
end

module Make: functor (Ord: OrderedType) -> H with type value = Ord.t