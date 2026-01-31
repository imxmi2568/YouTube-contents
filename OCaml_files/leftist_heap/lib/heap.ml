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

	val check_weight: 
end

module Make (Ord: OrderedType) : (H with type value = Ord.t) = struct
	type value = Ord.t

	type weight = int

	type heap =
	| Empty
	| Node of weight * value * heap * heap

	exception EmptyHeap

	let empty = Empty

	let weight = function
	| Empty -> 0
	| Node (w, _, _, _) -> w

	let rec merge h1 h2 = match h1, h2 with
	| Empty, h2 -> h2
	| h1, Empty -> h1
	| Node (_, v1, h11, h12), Node (_, v2, h21, h22) ->
		let is_left_smaller = Ord.compare v1 v2 <= 0 in
		let v = if is_left_smaller then v1 else v2 in
		let left' = if is_left_smaller then h11 else h21 in
		let right' = 
			if is_left_smaller then
				merge h12 h2
			else
				merge h1 h22
		in
		let w_left' = weight left' in
		let w_right' = weight right' in
		let (left, right) = 
			if w_left' >= w_right' then (left', right')
			else (right', left')
		in
		let w = w_left' + w_right' + 1 in
		Node (w, v, left, right)

	let insert v h = merge (Node (1, v, Empty, Empty)) h

	let find_min h = match h with
	| Empty -> raise EmptyHeap
	| Node (_, v, _, _) -> v

	let delete_min h = match h with
	| Empty -> raise EmptyHeap
	| Node (_, _, h1, h2) -> merge h1 h2
end