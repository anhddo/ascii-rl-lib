open Base_algorithm

module Make (Algo_config : Algo_config) (Env : Simulation.S) = struct
  include Algo_config
  module State_action_env = State_action.Make (Env)

  let state_bin = State_action_env.q_config.state_bin
  let action_bin = State_action_env.q_config.action_bin
  let obs_dim = State_action_env.q_config.obs_dim

  (*Get action dimension*)
  let action_dim =
    match action_bin with Discrete n -> n | Continuous x -> x.num_bins

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

  (*load model*)
  let load_q_table (filename : string) =
    let file_content = Core.In_channel.read_all filename in
    Sexplib.Conv.array_of_sexp
      (Sexplib.Conv.array_of_sexp Sexplib.Conv.float_of_sexp)
      (Sexplib.Sexp.of_string file_content)

  let q_table =
    let file_name = model_path in
    if Sys.file_exists file_name then load_q_table file_name
    else
      Core.Array.make_matrix
        ~dimx:(int_of_float @@ (float_of_int state_bin ** float_of_int obs_dim))
        ~dimy:action_dim 0.0

  (*save model using Sexp*)
  let save_model () =
    let sexp_str =
      Core.Sexp.to_string_hum
        (Core.Array.sexp_of_t
           (Core.Array.sexp_of_t Core.Float.sexp_of_t)
           q_table)
    in
    Core_unix.mkdir_p (Core.Filename.dirname Algo_config.model_path);
    Core.Out_channel.write_all Algo_config.model_path ~data:sexp_str

  (*train model*)
  let train () =
    let rec loop' (episode : int) (state : float list)
        (internal_state : float list) (reward_ep : float) =
      (* get action from q table*)
      let action =
        if Float.compare (Random.float 1.0) 0.1 = -1 then Random.int action_dim
        else
          let state_bin = State_action_env.convert_state_to_bin state in
          q_table.(state_bin) |> Array.to_list |> float_argmax
      in
      (*decide how pass action to simulation depending on the type of action Discrete or Continuous*)
      let passing_action_to_env =
        match action_bin with
        | Discrete _ -> [ float_of_int action ]
        | Continuous x -> [ State_action_env.bin_to_value action x ]
      in
      (* get next state, reward, is_done from the environment*)
      let response = Env.step internal_state passing_action_to_env in
      let next_state = response.observation in
      let reward = response.reward in
      let is_done = response.terminated in
      let truncated = response.truncated in
      let next_state_bin = State_action_env.convert_state_to_bin next_state in
      let state_bin = State_action_env.convert_state_to_bin state in
      (* update q table*)
      let _ =
        if is_done || truncated then q_table.(state_bin).(action) <- reward
        else
          let max_q =
            Core.List.max_elt
              (q_table.(next_state_bin) |> Array.to_list)
              ~compare:Float.compare
            |> Core.Option.value_exn
          in
          q_table.(state_bin).(action) <-
            ((1. -. learning_rate) *. (reward +. (gamma *. max_q)))
            +. (learning_rate *. q_table.(state_bin).(action))
      in
      let state = next_state in
      let reward_ep = reward_ep +. reward in
      if is_done || truncated then (
        if episode mod 100 = 0 then
          (* Printing out the total reward *)
          Printf.printf "episode %d reward_ep: %f\n" episode reward_ep;
        let state, internal_state = Env.reset () in
        loop' (episode - 1) state internal_state 0.0)
      else if episode > 0 then (
        Env.render response.internal_state;
        loop' episode state response.internal_state reward_ep)
      else ()
    in
    let state, internal_state = Env.reset () in
    loop' episode state internal_state 0.0
end
