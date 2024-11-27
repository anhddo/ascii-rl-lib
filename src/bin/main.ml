module Algo_config = struct
  let model_path = "pendulum.sexp"
end

module Pendulum_env = Pendulum.Make (struct
  let render = false
end)

module Pendulum_env_render = Pendulum.Make (struct
  let render = true
end)

module Qlearning_algo = Qlearning.Make (Algo_config) (Pendulum_env)

module Qlearning_algo_render =
  Qlearning.Make (Algo_config) (Pendulum_env_render)

let () =
  Qlearning_algo.train 30000;
  Qlearning_algo.save_q_table ();
  Qlearning_algo_render.train 4;
