let argmax (arr : 'a list) ~(compare : 'a -> 'a -> int) ~(init : 'a) : int =
  let rec loop' (arr : 'a list) (max : 'a) (index : int) (i : int) =
    match arr with
    | [] -> index
    | hd :: tl ->
        if compare hd max = 1 then loop' tl hd i (i + 1)
        else loop' tl max index (i + 1)
  in
  loop' arr init 0 0

let float_argmax (arr : float list) : int =
  argmax arr ~compare:Float.compare ~init:Float.neg_infinity

(* Returns the squre of the float *)
let square (value : float) : float = Float.pow value 2.

(* Does floating point modulo *)
let modulo (dividend : float) (divisor : float) : float =
  dividend -. (divisor *. floor (dividend /. divisor))

(* Normalizes an angle to be between -PI and PI*)
let normalize_angle (ang : float) : float =
  let normalized = modulo (ang +. Float.pi) (2. *. Float.pi) 
  in
  Float.sub normalized Float.pi;;

(* Add These Tests And To .mli *)
let random_between (min : float) (max : float) : float =
  let diff = max -. min in
  Random.float diff +. min

let clip (min_value : 'a) (max_value : 'a) (value : 'a) : 'a =
  if value < min_value then min_value
  else if value > max_value then max_value
  else value
