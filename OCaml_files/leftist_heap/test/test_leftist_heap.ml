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
  Random.self_init ();
  let rec make_rand_list n = 
    if n = 0 then []
    else Random.int 100 :: make_rand_list (n-1)
  in
  let i = ref 100 in
  let result = ref [] in
  while (!i > 0) do
    let l = make_rand_list 100 in
    let h = List.fold_right IntHeap.insert l IntHeap.empty in
    let rec lst_min_int h =
      let v = IntHeap.find_min h in
      result := v :: !result;
      let d = IntHeap.delete_min h in
      lst_min_int d
    in
    begin try
      lst_min_int h
    with
      IntHeap.EmptyHeap ->
        if List.sort compare l = List.rev !result then begin
          print_string (Format.asprintf "end of the test %d\n" !i);
          result := [];
          i := !i - 1
        end
        else begin
          print_string (Format.asprintf "result is not sorted\n");
          i := -1
        end
    end
  done
  
