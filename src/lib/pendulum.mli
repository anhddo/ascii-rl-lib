
(* Pendulum is a Simulation *)
include Simulation.T

(* Expose Helper Functions for Testing *)

val normalize_angle : float -> float

val modulo : float -> float -> float 

val square : float -> float

val random_between : float -> float -> float 

val clip : 'a -> 'a -> 'a