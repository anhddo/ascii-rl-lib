1. The purpose of this project is to create a Reinforcement Learning library that enables users to train and test various RL algorithms within simulated environments. Our core goal is to implement key algorithms like Q-learning, Policy Gradient, and Actor-Critic from scratch, compatible with our control simulations to validate accuracy. Users will interact via a command-line interface, specifying parameters such as the algorithm, environment, and training configurations. The project is divided into two main components: the RL algorithm library and simulation environment integration, resulting in a comprehensive RL toolkit.
  
2. User will interact via command line.  There are two use cases for our application: **training** a model, and **testing** a model.  The user will be able to specify the algorithm, the simulation/game, and the output file to write to.  The user will also be able to specify the number of iterations to run, the learning rate, and the regularization parameter.

`dune exec src/bin/main.exe --episode 1000 --learning_rate 0.01 --regularization 0.01 --algo qlearning --output qlearning_model.json`  
For testing a learned model, the user will be able to specify the model file to read from, the simulation/game, and the output file to write to.  The user will also be able to specify the number of iterations to run.
`dune exec src/bin/main.exe --episode 1000 --model qlearning_model.json --record record.gif`  
This command will open the simulation and run the model for 1000 iterations, and save the result animiation output to record.gif.  
The overall interaction protocal of our project is described as follows, 
There is an interactive of agent and environment:
  - At each time step, the agent receives the state of the environment, and based on the state, the agent will take an action.
  - The environment will return the reward and the next state.
  - The agent will update its policy based on the reward and the next state.
There are some algorithm that we want to implement: qlearning, policy gradient, actor-critic. Depend on the project progress, we may implement more algorithms, but these three are the main focus of our project.


3. We aim to use no other library than what we have in class at the end of the project. We will test our control algorithm on a python simulation (gymansium) to verify the correctness of our implementation, which we will use `pyml`. After we finish implement the simulation, we will test those algorithms on the simulation and exclude the `pyml` library from our final project.

4. See all .mli, excluding animation.mli and base_algorithm.mli because that likely will not find its way into the final submission.

5. See implementationplan.md
  
6. See IMG_0235.JPEG for a flowchart of how information will flow 
