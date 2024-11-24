[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]

module Make =
functor
  (Config : Simulation.Config)
  ->
  struct
    (* if not initialize then initialized*)
    if not (Py.is_initialized ()) then Py.initialize ()

    type t = float list (* Length is 2 | [location, ang_speed ] *)
    type action = float list

    type response = {
      observation : t;
      reward : float;
      terminated : bool;
      truncated : bool;
      info : string;
    }

    (*if not inialize then initialize*)

    let gym = Py.import "gymnasium"

    let init_environment (str : string) (render : bool) =
      if render then
        Py.Module.get_function_with_keywords gym "make"
          [| Py.String.of_string str |]
          [ ("render_mode", Py.String.of_string "human") ]
      else Py.Module.get_function gym "make" [| Py.String.of_string str |]

    let env = init_environment Config.name Config.render
    let create () : t = [ 0.; 0. ]

    let reset () : t =
      let reset_fn' = Py.Object.get_attr_string env "reset" in
      let result =
        Py.Callable.to_function (Core.Option.value_exn reset_fn') [||]
      in
      let result, _ = Py.Tuple.to_tuple2 result in
      (*convert to list of float*)
      let state = Py.List.to_list_map Py.Float.to_float result in
      state

    let step (state : t) (action : action) =
      let step_fn' = Py.Object.get_attr_string env "step" in
      let result =
        Py.Callable.to_function
          (Core.Option.value_exn step_fn')
          [| Py.Int.of_int @@ int_of_float (List.hd action) |]
      in
      let _state, _reward, _is_done, _truncated, _ =
        Py.Tuple.to_tuple5 result
      in
      {
        observation = Py.List.to_list_map Py.Float.to_float _state;
        reward = Py.Float.to_float _reward;
        terminated = Py.Bool.to_bool _is_done;
        truncated = false;
        info = "";
      }
    (* ( Py.List.to_list_map Py.Float.to_float _state,
       Py.Float.to_float _reward,
       Py.Bool.to_bool _is_done,
       false,
       "" ) *)

    let render (x : t) = [ 'a'; 'b' ]
  end
