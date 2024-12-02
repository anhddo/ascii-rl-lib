[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]

(*
  We will assume that we are playing with an infinite deck, that is, there will be infinite replacement
*)

(* State: Length is 3 | [current_sum, dealer_face_card, has_usable_ace ] *)
(* Action: Length is 1 | [hit (if positive), stand (if 0 or negative)] *)
(* observation : the new state of the simulation; the next step call should use this value *)
(* reward : 1 for a win, 0 for a tie, -1 for a loss *)
(* terminated : if a round has ended by natural reasons of the simulation *)
(* truncated : if ended to reduce too long episodes *)
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
      let state = [ 
        total_sum;
        dealer_face_card;
        if has_usable_ace 
          then 1.
        else 0.;
        ]
      in
      (state, state);;

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
        if (new_sum > 21.) then 
          match has_usable_ace with 
          | 1. -> 
            let state = [
              new_sum -. 10.;
              dealer_face_card;
              0.
            ]
            in 
            {
              observation = state;
              reward = 0.;
              terminated = false;
              truncated = false;
              info = "busted, used ace to recover";
              internal_state = state;
            }
          | 0. -> let state = [
              new_sum;
              dealer_face_card;
              0.
            ]
            in
            {
              observation = state;
              reward = -1.;
              terminated = true;
              truncated = false;
              info = "busted";
              internal_state = state;
            }
          | _ -> failwith "improper value for has usable ace"
        else
          let state = [
              new_sum;
              dealer_face_card;
              has_usable_ace;
            ]
            in
            {
              observation = state;
              reward = 0.;
              terminated = true;
              truncated = false;
              info = "hit and okay";
              internal_state = state;
            }
      else
        let rec draw_for_dealer (curr_dealer_sum : float) (dealer_has_usable_ace : bool) : float =
          if (curr_dealer_sum > 21. && dealer_has_usable_ace) then (* busted, but has ace*)
            draw_for_dealer (curr_dealer_sum -. 10.) false
          else if (curr_dealer_sum > 21.) then (* busted *)
            curr_dealer_sum
          else if (curr_dealer_sum >= 17.) then (* safely above 17,*)
            curr_dealer_sum
          else (* not yet safe at 17, so has to draw again*)
            let new_card = draw_card() 
            in
            draw_for_dealer (new_card +. curr_dealer_sum) (dealer_has_usable_ace || new_card = 1.)
        in 
        let dealer_result = draw_for_dealer dealer_face_card (dealer_face_card = 1.)
        in
        match (dealer_result > 21.) with 
        | true -> let state = [ (* rebuild because we need to ensure proper formatting *)
            curr_sum;
            dealer_face_card;
            has_usable_ace;
          ]
          in 
          {
            observation = state;
            reward = 1.;
            terminated = true;
            truncated = false;
            info = "stand, dealer bust";
            internal_state = state;
          }
        | false -> 
          let get_reward (player_sum : float) (dealer_sum : float) : float= 
            if (player_sum = dealer_sum) then 0.
            else if (player_sum > dealer_sum) then 1.
            else -1. 
          in 
          let state = [ (* rebuild because we need to ensure proper formatting *)
            curr_sum;
            dealer_face_card;
            has_usable_ace;
          ]
          in 
          {
            observation = state;
            reward = get_reward curr_sum dealer_result;
            terminated = true;
            truncated = false;
            info = "stand, dealer played out";
            internal_state = state;
          };;

    let render (sim_state : t) : unit =
      failwith "TODO";;
    end