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
  (length ["a"]) = 1
let%test _ =
  (length []) = 0

(* Reverse a List *)
let rev (list: 'a list) =
  let rec rev_impl ((rest: 'a list), (rev: 'a list)) = match rest with
    | first :: rest -> rev_impl (rest, first :: rev)
    | _ -> rev
  in
  rev_impl (list, [])

let%test _ =
  (rev ["a"; "b"; "c"]) = ["c"; "b"; "a"]

(* Palindrome *)
let is_palindrome (list: 'a list) =
  let rev = (rev list) in
  let rec equal ((a: 'a list), (b: 'a list)) = match (a, b) with
  | ([], []) -> true
  | ([a], [b]) -> a = b
  | (a_h :: a_rest, b_h :: b_rest) -> if a_h = b_h then equal (a_rest, b_rest) else false
  | _ -> false
  in
  equal (list, rev)

let%test _ =
  is_palindrome ["x"; "a"; "m"; "a"; "x"]
let%test _ =
  not (is_palindrome ["a"; "b"])

(* Flatten a List ☡ *)
type 'a node =
  | One of 'a 
  | Many of 'a node list
let rec flatten (list: 'a node list) = match list with
  | [] -> []
  | One node :: rest ->  node :: (flatten rest)
  | Many list :: rest -> List.append (flatten list) (flatten rest)

let%test _ =
  (flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]]) = ["a"; "b"; "c"; "d"; "e"]

(* Eliminate Duplicates ☡ *)
let rec compress (list: 'a list) = match list with
| [] -> []
| [ one ] -> [ one ]
| first :: second :: rest -> if first = second then (compress (second :: rest)) else first :: (compress (second :: rest))

let%test _ =
  (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) = ["a"; "b"; "c"; "a"; "d"; "e"]

(* Pack Consecutive Duplicates ☡ *)
let rec pack (list: 'a list) =
  let rec collect ((rest: 'a list), (box: 'a list)) = match (rest, box) with
  | ((rest_h :: rest_t), (box_h :: box_t)) -> if rest_h = box_h then
    (collect (rest_t, (rest_h :: box_h :: box_t)))
  else
    ((rest_h :: rest_t), (box_h :: box_t))
  | ((rest_h :: rest_t), []) -> collect (rest_t, [rest_h])
  | _ -> (rest, box)
  in match list with
  | [] -> []
  | _ ->
    let (rest, box) = (collect (list, [])) in
    box :: (pack rest)

let%test _ =
  (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]) =
  [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]

let print_nested_list (list: 'a list list) =
  let rec print_outer_list (list: 'a list list) =
    let rec print_inner_list (list: 'a list) = match list with
    | first :: rest ->
      let _ = (Printf.printf "%s, " first) in
      let _ = (print_inner_list rest) in
      ()
    | _ -> ()
    in match list with
    | first :: rest ->
      let _ = (Printf.printf "[") in
      let _ = (print_inner_list first) in
      let _ = (Printf.printf "]") in
      let _ = (print_outer_list rest) in
      ()
    | _ -> ()
    in
  let _ = Printf.printf "[" in
  let _ = (print_outer_list list) in
  let _ = Printf.printf "]" in
  ()

let _ = Printf.printf "Expected: "
let _ = print_nested_list [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
let _ = Printf.printf "\n"
let _ = Printf.printf "Actual:   "
let _ = print_nested_list (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"])
let _ = Printf.printf "\n"

(* Run-Length Encoding *)
let rec encode (list: 'a list) =
  let rec count ((rest: 'a list), (counter: int), (target: 'a)) = match rest with
  | first :: rest -> if first = target then count (rest, counter + 1, target) else ((first :: rest), counter)
  | _ -> (rest, counter)
  in match list with
  | [] -> []
  | first :: _ ->
    let (rest, counter) = count (list, 0, first) in
    (counter, first) :: encode (rest)

let%test _ =
  (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) =
  [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]

(* Modified Run-Length Encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a
let rec rle_encode (list: 'a list) =
  let rec count ((rest: 'a list), (counter: int), (target: 'a)) = match rest with
  | first :: rest -> if first = target then count (rest, counter + 1, target) else ((first :: rest), counter)
  | _ -> (rest, counter)
  in match list with
  | [] -> []
  | first :: _ ->
    let (rest, counter) = count (list, 0, first) in
    if counter = 1 then One first :: rle_encode (rest) else Many (counter, first) :: rle_encode (rest)

let%test _ =
  (rle_encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) =
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]

(* Decode a Run-Length Encoded List ☡ *)
let rec decode (list: 'a rle list) = match list with
| [] -> []
| first :: rest ->
  match first with
  | One item -> item :: (decode rest)
  | Many (times, item) ->
    let rec repeat ((item: 'a), (times: int)) =
      if times = 0 then [] else item :: repeat (item, times - 1)
    in List.append (repeat (item, times)) (decode rest)

let%test _ =
  (decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]) =
  ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
