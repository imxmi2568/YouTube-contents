type weight = int

type value = int

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
	let is_left_smaller = v1 <= v2 in
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