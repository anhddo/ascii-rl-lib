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
