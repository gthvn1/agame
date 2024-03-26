external myadd : int -> int -> int = "caml_add"

let () =
  let res : int = myadd 40 2 in
  let answer : string = string_of_int res in
  print_endline @@ "The answer is " ^ answer
