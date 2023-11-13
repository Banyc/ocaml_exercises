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

(* N'th Element of a List *)
let rec at ((list: 'a list), (index: int)) =
  if index < 0 then None
  else if index = 0 then match list with
    | first :: _rest -> Some first
    | _ -> None
  else match list with
    | _first :: rest -> at (rest, (index - 1))
    | _ -> None

let%test _ =
  (at (["a"; "b"; "c"; "d"; "e"], 2)) = (Some "c")
let%test _ =
  (at (["a"], 2)) = (None)
let%test _ =
  (at ([], 2)) = (None)

(* Length of a List *)
let length (list: 'a list) =
  let rec length_impl ((list: 'a list), (count: int)) = match list with
  | _first :: rest -> length_impl (rest, count + 1)
  | _ -> count
  in
  length_impl (list, 0)

let%test _ =
  (length ["a"; "b"; "c"]) = 3
let%test _ =
  (length []) = 0
