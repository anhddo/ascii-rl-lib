module type S = sig
  type env_type =
    | Cartpole
    | Pendulum
    | MountainCar
    | Acrobot
    | LunarLander
    | LunarLanderContinuous
    | BipedalWalker
end
