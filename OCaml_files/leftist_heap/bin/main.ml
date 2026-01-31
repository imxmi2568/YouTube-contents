open Leftist_heap

let () = 
  let example = [5;2;8;1] in
  let h = List.fold_right Heap.insert example Heap.empty in
  let rec print_min h =
    let v = Heap.find_min h in
    print_int v;
    print_newline ();
    print_min (Heap.delete_min h)
  in
  try
    print_min h
  with
    Heap.EmptyHeap -> print_string ("end of the heap\n")


