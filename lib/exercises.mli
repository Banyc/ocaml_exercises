val last : 'a list -> 'a option
val last_two : 'a list -> ('a * 'a) option
val at : ('a list * int) -> 'a option
val length : 'a list -> int
val rev : 'a list -> 'a list
val is_palindrome : 'a list -> bool
type 'a node =
  | One of 'a 
  | Many of 'a node list
val flatten : 'a node list -> 'a list
val compress : 'a list -> 'a list
val pack : 'a list -> 'a list list
val encode : 'a list -> (int * 'a) list
type 'a rle =
  | One of 'a
  | Many of int * 'a
val rle_encode : 'a list -> 'a rle list
val decode : 'a rle list -> 'a list
