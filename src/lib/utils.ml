open Torch

type nn_model = {
  var_store : Var_store.t;
  forward : Tensor.t -> Tensor.t;
}

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

let softmax (arr : float array) : float array =
  let max_elem = Array.fold_left max neg_infinity arr in
  let exps = Array.map (fun x -> exp (x -. max_elem)) arr in
  let sum_exps = Array.fold_left ( +. ) 0.0 exps in
  Array.map (fun x -> x /. sum_exps) exps

(* Calculate the discounted cumulative reward *)
let calculate_returns (rewards : float list) (gamma : float) : float list =
  (* chronological order input and output*)
  let rec aux (acc : float) (returns : float list) = function
    | [] -> returns
    | r :: rs ->
        let g_t = r +. gamma *. acc in
        aux g_t (g_t :: returns) rs
  in
  aux 0.0 [] (List.rev rewards)

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
