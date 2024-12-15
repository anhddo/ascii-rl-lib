module Make (_ : Simulation.Config) : sig
  include Simulation.S

  val random_between : float -> float -> float
  val square : float -> float
  val simulate : t -> unit

end