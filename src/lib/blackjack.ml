module Make =
functor
  (C : Simulation.Config)
  ->
  struct
    include Simulation.T

    let env_type = Blackjack

    let draw_card () : float =
      let card_value = Float.of_int @@ (Random.int 13 + 1) in
      Utils.clip 1. 10. card_value (* Convert Js, Qs, Ks, to 10 *)
    

    (* type t = float list Length is 3 | [ player's hand's sum, dealer's face card, does the player have a usable ace ] *)
    (* type action = float list Length is 1 | [ hit or stand ] *)

    (* Creates a new simulation *)
    let create () : t = [ 0.; 0.; 0. ]

    (* Resets the simulation and returns the first response again *)
    let reset () : t * t =
      let first_card = draw_card () in
      let second_card = draw_card () in
      let has_usable_ace = first_card = 1. || second_card = 1. in
      let dealer_face_card = draw_card () in
      let total_sum =
        if has_usable_ace then first_card +. second_card +. 10.
        else first_card +. second_card
      in
      let state =
        [ total_sum; dealer_face_card; (if has_usable_ace then 1. else 0.) ]
      in
      (state, state)

    (* Applies the action to the environment, and returns the corresponding response *)
    let step (sim : t) (act : action) : response =
      let hit =
        match act with
        | 0. :: [] -> false
        | 1. :: [] -> true
        | _ -> failwith "invalid action for blackjack"
      in
      let curr_sum, dealer_face_card, has_usable_ace =
        match sim with
        | [ curr_sum; dealer_face_card; has_usable_ace ] ->
            (curr_sum, dealer_face_card, has_usable_ace)
        | _ -> failwith "improper simulation state"
      in
      if hit then
        let new_card = draw_card () in
        let new_sum = curr_sum +. new_card in
        if new_sum > 21. then
          match has_usable_ace with
          | 1. ->
              let state = [ new_sum -. 10.; dealer_face_card; 0. ] in
              {
                observation = state;
                reward = 0.;
                terminated = false;
                truncated = false;
                info = "busted, used ace to recover";
                internal_state = state;
              }
          | 0. ->
              let state = [ new_sum; dealer_face_card; 0. ] in
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
          let state = [ new_sum; dealer_face_card; has_usable_ace ] in
          {
            observation = state;
            reward = 0.;
            terminated = false;
            (* Should be false here *)
            truncated = false;
            info = "hit and okay";
            internal_state = state;
          }
      else
        let rec draw_for_dealer (curr_dealer_sum : float)
            (dealer_has_usable_ace : bool) : float =
          if curr_dealer_sum > 21. && dealer_has_usable_ace then
            (* busted, but has ace*)
            draw_for_dealer (curr_dealer_sum -. 10.) false
          else if curr_dealer_sum > 21. then (* busted *)
            curr_dealer_sum
          else if curr_dealer_sum >= 17. then
            (* safely above 17,*)
            curr_dealer_sum
          else
            (* not yet safe at 17, so has to draw again*)
            let new_card = draw_card () in
            draw_for_dealer
              (new_card +. curr_dealer_sum)
              (dealer_has_usable_ace || new_card = 1.)
        in
        let dealer_result =
          draw_for_dealer dealer_face_card (dealer_face_card = 1.)
        in
        match dealer_result > 21. with
        | true ->
            let state =
              [
                (* rebuild because we need to ensure proper formatting *)
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
            let get_reward (player_sum : float) (dealer_sum : float) : float =
              if player_sum = dealer_sum then 0.
              else if player_sum > dealer_sum then 1.
              else -1.
            in
            let state =
              [
                (* rebuild because we need to ensure proper formatting *)
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
            }


    [@@@coverage off] (* Turning off coverage *)
    (* Helper function to convert card value to string *)
    let card_name (value : float) : string =
      match int_of_float value with
      | 1 -> "A"
      | 2 -> "2"
      | 3 -> "3"
      | 4 -> "4"
      | 5 -> "5"
      | 6 -> "6"
      | 7 -> "7"
      | 8 -> "8"
      | 9 -> "9"
      | 10 ->
          let face_cards = [| "10"; "J"; "Q"; "K" |] in
          let idx = Random.int 4 in
          face_cards.(idx)
      | _ -> "?"

    (* Suits for the cards *)
    let suits = [| "♠"; "♥"; "♦"; "♣" |]

    (* Helper function to create an ASCII representation of a card *)
    let ascii_card (card_value : string) : string =
      let suit = suits.(Random.int 4) in
      let top = "┌─────────┐\n" in
      let bottom = "└─────────┘\n" in
      let empty_line = "│         │\n" in
      let middle = Printf.sprintf "│  %2s %s   │\n" card_value suit in
      top ^ empty_line ^ middle ^ empty_line ^ bottom

    (* Helper function to display player's sum in a larger format *)
    let ascii_sum (sum : int) : string =
      let top = "╔══════════════════╗\n" in
      let bottom = "╚══════════════════╝\n" in
      let middle = Printf.sprintf "║ Player's Sum: %2d ║\n" sum in
      top ^ middle ^ bottom

    (* Render function to display the current state *)
    let render (sim_state : t) : unit =
      if C.render then
        match sim_state with
        | [ curr_sum; dealer_face_card; has_usable_ace ] ->
            Printf.printf "\027[2J\027[H";
            let dealer_card_str = card_name dealer_face_card in
            let dealer_card_ascii = ascii_card dealer_card_str in
            Printf.printf "Dealer's face-up card:\n%s\n" dealer_card_ascii;
            let player_sum_ascii = ascii_sum (int_of_float curr_sum) in
            Printf.printf "%s\n" player_sum_ascii;
            if has_usable_ace = 1. then
              Printf.printf "usable ace\n";
            (* else Printf.printf "Player does not have a usable ace.\n"; *)
            flush stdout
        | _ -> failwith "Invalid simulation state"

    (* Simulate function *)
    let simulate () =
      Random.self_init ();
      while true do
        let _, sim_state = reset () in
        let rec game_loop sim_state =
          render sim_state;
          let action = if Random.bool () then [ 1. ] else [ 0. ] in
          Unix.sleepf 0.5;
          let response = step sim_state action in
          if response.terminated then (
            render response.internal_state;
            Printf.printf "Game over. Reward: %f\n" response.reward;
            Unix.sleepf 0.3)
          else game_loop response.internal_state
        in
        game_loop sim_state
      done
      [@@@coverage on]
  end
