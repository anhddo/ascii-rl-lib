[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]

(*
  We will assume that we are playing with an infinite deck, that is, there will be infinite replacement
*)

type t = int list (* Length is 3 | [current_sum, dealer_face_card, has_usable_ace ] *)

type action = int list (* Length is 1 | [hit (if positive), stand (if 0 or negative)] *)

(* observation : the new state of the simulation; the next step call should use this value *)
(* reward : 1 for a win, 0 for a tie, -1 for a loss *)
(* terminated : if a round has ended *)
(* truncated : idk *)
(* info : error handling, nothing for now *)
type response = {
  observation : t;
  reward : int;
  terminated : bool;
  truncated : bool;
  info : string;
}


(* Creates a new simulation *) (* TODO, CONSIDER IF EVEN NEEDED, probably will later as we increase code complexity *)
let create() : t = 
  [0; 0; 0];;




(* State: Length is 3 | [current_sum, dealer_face_card, has_usable_ace ] *)
(* Action: Length is 1 | [hit (if positive), stand (if 0 or negative)] *)
(* observation : the new state of the simulation; the next step call should use this value *)
(* reward : 1 for a win, 0 for a tie, -1 for a loss *)
(* terminated : if a round has ended *)
(* truncated : idk *)
(* info : error handling, nothing for now *)


module Make =
functor
  (C : Simulation.Config)
  ->
  struct
    include Simulation.T

    let env_type = Blackjack


    let clip (min_value : 'a) (max_value : 'a) (value : 'a) : 'a =
      if (value < min_value) then min_value
      else if (value > max_value) then max_value
      else value;;
    
    let draw_card () : float =
      let card_value = Float.of_int @@ (Random.int 13) + 1 
      in
      clip 1. 10. card_value;; (* Convert Js, Qs, Ks, to 10 *)

    (* Creates a new simulation *)
    let create () : t 
      = [0.; 0.; 0.];;


    (* Resets the simulation and returns the first response again *)
    let reset () : t * t =
      let first_card = draw_card() 
      in
      let second_card = draw_card() 
      in
      let has_usable_ace = (first_card = 1.) || (second_card = 1.)
      in
      let dealer_face_card = draw_card() 
      in
      let total_sum = 
        if has_usable_ace then 
          first_card +. second_card +. 10.
        else
          first_card +. second_card
      in  
      ([ 
        total_sum;
        dealer_face_card;
        if has_usable_ace 
          then 1.
        else 0.;
        ], []);;

    (* Applies the action to the environment, and returns the corresponding response *)
    let step (sim : t) (act : action) : response =
      let hit = 
        match act with
        | 0. :: [] -> false
        | 1. :: [] -> true 
        | _ -> failwith "invalid action for blackjack"
      in 
      let (curr_sum, dealer_face_card, has_usable_ace) = 
        match sim with 
        | curr_sum :: dealer_face_card :: has_usable_ace :: [] -> (
          curr_sum,
          dealer_face_card,
          has_usable_ace
        )
        | _ -> failwith "improper simulation state"
      in
      if hit then 
        let new_card = draw_card() 
        in
        let new_sum = curr_sum +. new_card
        in
        match (new_sum > 21.) with 
        | true -> failwith "BUST TODO"
        | false -> failwith "OK TODO"
      else
        failwith "STAND TODO" 

    let render (sim_state : t) : unit =
      failwith "TODO";;
    end