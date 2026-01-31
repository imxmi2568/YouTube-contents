open Leftist_heap

module IntHeap = Heap.Make (struct
  type t = int
  let compare a b = compare a b
end)

module StrHeap = Heap.Make (struct
  type t = string
  let compare a b = compare a b
end)

let () = 
  let int_example = [5;2;8;1] in
  let h = List.fold_right IntHeap.insert int_example IntHeap.empty in
  let rec print_min_int h =
    let v = IntHeap.find_min h in
    print_int v;
    print_newline ();
    print_min_int (IntHeap.delete_min h)
  in
  begin try
    print_min_int h
  with
    IntHeap.EmptyHeap -> print_string ("end of the heap\n")
  end;
  let str_example = ["abc";"bad";"apple";"and"] in
  let h = List.fold_right StrHeap.insert str_example StrHeap.empty in
  let rec print_min_str h =
    let v = StrHeap.find_min h in
    print_string v;
    print_newline ();
    print_min_str (StrHeap.delete_min h)
  in
  begin try
    print_min_str h
  with
    StrHeap.EmptyHeap -> print_string ("end of the heap\n")
  end



