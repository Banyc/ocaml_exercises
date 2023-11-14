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
