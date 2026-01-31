type heap

exception EmptyHeap

val empty: heap

val merge: heap -> heap -> heap

val insert: int -> heap -> heap

val find_min: heap -> int

val delete_min: heap -> heap