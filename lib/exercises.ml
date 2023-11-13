(* Tail of a List *)
let rec last (input: 'a list) = match input with
| [] -> None
| [ item ] -> Some item
| _first :: rest -> last rest

let%test _ =
  (last ["a" ; "b" ; "c" ; "d"]) = (Some "d")
let%test _ =
  (last []) = (None)

(* Last Two Elements of a List *)
let rec last_two (input: 'a list) = match input with
| [] -> None
| [ _one ] -> None
| [ last ; two ] -> Some ( last, two )
| _first :: rest -> last_two rest

let%test _ =
  (last_two ["a"; "b"; "c"; "d"]) = (Some ("c", "d"))
let%test _ =
  (last_two ["a"]) = (None)
