# Overview
  The purpose of this project is to create a Reinforcement Learning library that enables users to train and test various RL algorithms within simulated environments. Our core goal is to implement key algorithms like Q-learning, Policy Gradient, and Actor-Critic from scratch, compatible with our control simulations to validate accuracy. Users will interact via a command-line interface, specifying parameters such as the algorithm, environment, and training configurations. The project is divided into two main components: the RL algorithm library and simulation environment integration, resulting in a comprehensive RL toolkit.
# Mock use of application
User will interact via command line.  There are two use cases for our application: **training** a model, and **testing** a model.  The user will be able to specify the algorithm, the simulation/game, and the output file to write to.  The user will also be able to specify the number of iterations to run, the learning rate, and the regularization parameter.  The following command is our demo for testing the library `pyml` (be sure to activate the python environment with `gymansium` installed):
`_build/default/src/bin/main.exe -episode 1000 -algo qlearning`  
For testing a learned model, the user will be able to specify the model file to read from, the simulation/game, and the output file to write to. User will also be able to specify the number of iterations to run. (we haven't implemented this yet) 
`_build/default/src/bin/main.exe  -episode 1000 -model qlearning_model.json -record record.gif`  
This command will open the simulation and run the model for 1000 iterations, and save the result animiation output to record.gif.  
The overall interaction protocal of our project is described as follows, 
There is an interactive of agent and environment:
  - At each time step, the agent receives the state of the environment, and based on the state, the agent will take an action.
  - The environment will return the reward and the next state.
  - The agent will update its policy based on the reward and the next state.
There are some algorithm that we want to implement: qlearning, policy gradient, actor-critic. Depend on the project progress, we may implement more algorithms, but these three are the main focus of our project.


3. We aim to use no other library than what we have in class at the end of the project. We will test our control algorithm on a python simulation (gymansium) to verify the correctness of our implementation, which we will use `pyml`. After we finish implement the simulation, we will test those algorithms on the simulation and exclude the `pyml` library from our final project.


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

# Comment on module type
4. See all .mli, excluding animation.mli and base_algorithm.mli because that likely will not find its way into the final submission.

  
6. See IMG_0235.JPEG for a flowchart of how information will flow 


