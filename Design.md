1. An overview of the purpose of the project.
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

4. Commented module type declarations (.mli files) which will provide you with an initial specification to code to.
- You can change this later and don’t need every single detail filled out, but it should include as many details as you can think of, and you should try to cover the entire project.
- Include an initial pass at key types and functions needed and a brief comment if the meaning of a function is not clear.
5. An implementation plan: a list of the order in which you will implement features and by what date you hope to have them completed.
6. You may also include any other information that will make it easier to understand your project. 