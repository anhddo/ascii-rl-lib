# Implementation Plan

We are separating our project into a few sections, each with different features and each will be developed in parallel.
 - The RL-Learning Algorithm Library 
    - 2+, ideally 4,  RL-Learning Algorithmns Implemented
 - The Simulation Engines
    - 2+, ideally 4,  Simulation Engines Implemented, Based on Gymnasium Games
    - ASCII or Web-Based GUI


### Minimal Completion Timeline 

This timeline is the bare minimum that we will complete. We hope to be ahead of this schedule to allow for the implementation of more RL-Learning Algorithms and more Simulation Engines.

11/13 - All .mli Files
 - Finish the .mli files for all of our basic files, including the types that act as the interface for communication between the algorithms and the simulations. This way everyone can understand the structure of our project, and how their code and files fit into the schema.

11/17 - First Simulation Engine
 - Complete a simulation engine that follows the interface that we had determined earlier. This allows for the RL-Learning Algorithms to test using our own simulation. This has the added benefit that we can transfer away from using PyML and an older version of OCaml. We had to use these to test our RL-Learning Algorithm, since there is no built in method.

11/20 - Q-Learning Algorithm
 - Finished Q-Learning algorithm, which is a very simply RL-Learning Algorithm. The completion of this allows us to begin to begin developing to connect our simulations with our algorithms

11/22 - Second Simulation Engine
 - Complete a second simulation engine, the specific game is yet to be determined.

11/27 - Successfully Run a Simulation with Our Own Algorithm
 - Finish a program that can glue together our simulation code and our RL-Learning algorithm code

12/4 - Project Checkpoint Needs (Testing)
 - Complete everything that is necessary for the the checkpoint. Based on what we have above, we only need to add testing, as we will have reached 50% completion, have enough algorithmic complexity, and well hopefully have developed our code with good structure and overall quality.

12/8 - Second RL-Learning Algorithm
 - Complete a second algorithm

12/12 - GUI For Both Simulation Engine
 - Complete a GUI that can display the training process for any RL-Learniing algorithm. Likely, this will be a ASCII terminal based GUI. 




