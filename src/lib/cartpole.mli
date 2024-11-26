module Make (_ : Simulation.Config) : sig
  include Simulation.S

  val normalize_angle : float -> float
  val modulo : float -> float -> float
  val square : float -> float
  val random_between : float -> float -> float
  val clip : 'a -> 'a -> 'a -> 'a
  val simulate : t -> unit
end
