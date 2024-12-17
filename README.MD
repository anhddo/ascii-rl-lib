# Group Members
 Jishuo Yang, Yang Shen, Spencer Ye, Anh Do 
# Introduction
Reinforcement Learning has emerged as a powerful paradigm for solving complex decision-making problems across various domains. Our team is implementing RL algorithms and integrating them with simulation environments. The purpose of this project is to create a Reinforcement Learning library that enables users to train and test various RL algorithms within simulated environments. We implemented key algorithms like Q-learning, Policy Gradient, and Policy Gradient with Neural Network. We integrated with multiple simulation environments, including CartPole, Pendulum, and Blackjack, to verify correctness and robustness. Users will interact via a command-line interface, specifying parameters such as the algorithm, environment, and training configurations. The project is divided into two main components: the RL algorithm library and simulation environment integration, resulting in a comprehensive RL toolkit.
 
# Implementation
## Interaction protocol.
In our implementation, the reinforcement learning algorithms and simulations interact through a well-defined interface. This interface allows the algorithms to interact with the simulations in a consistent manner, regardless of the specific simulation environment. The interface consists of the following key components:
- **State**: Represents the current state of the simulation environment. It is a record type that encapsulates all relevant information about the environment at a given time step.
- **Action**: Represents the action taken by the reinforcement learning algorithm in response to the current state. It is a variant type that can be either discrete or continuous, depending on the simulation environment.
- **Reward**: Represents the reward received by the reinforcement learning algorithm for taking a specific action in a given state. It is a scalar value that quantifies the quality of the action taken.
- **Step**: Represents the transition from one state to another based on the action taken by the reinforcement learning algorithm. It is a function that updates the state of the simulation environment and returns the reward for the action taken.
- **Episode**: Represents a sequence of steps that occur from the initial state to a terminal state. It is a function that executes multiple steps until a terminal condition is met, such as reaching a maximum number of steps or a specific goal.

![Picture Showing Simulations](https://images.spiceworks.com/wp-content/uploads/2022/09/29100907/Reinforcement-Learning-Model.png)
## Architecture

## Libraries

[Yang Shen] Mentioned all the dependent libraries. Decribe ocaml-torch.

# Simulations

## Overview
The purpose of our simulations is to provide environments, also called _situations_, where reinforcement learning (RL) algorithms can train and evaluate their performance. These simulations serve as controlled, reproducible benchmarks to test the capabilities of various RL methods.

These simulations take a discrete action from the reinforcement learning algorithm, called the _actor_, update its state, while also returning a reward for the actor's actions as it relates to the state.

>Our simulations do not store their own state, as that is not good functional programming. Rather they take in a record as a parameter that represents the state, and returns a similar record


We currently have implemented three classic simulations for actors to train on: Cartpole, Pendulum, and Blackjack. Descriptions of each are below.

--- 
## Key Features

##### Simplicity and Reproducibility
- The environments are intentionally simple to ensure that results can be easily interpreted and replicated across different experiments. This simplicity allows users to focus on algorithmic performance without distractions caused by overly complex environments
	
##### Command-Line Rendering
- Simulations can be rendered through the command line. We chose this approach because:
	- **Minimalism**: High-fidelity graphics are unnecessary for understanding the agents' interactions in these environments
	- **Lightweight**: By avoiding graphical libraries, we minimize external dependencies, making the library easier to install and more portable
	- **Debugging-Friendly**: Command-line outputs facilitate quick debugging and analysis without requiring a graphical interface
	
##### Modular Design
- The simulations are designed with modularity in mind. Users can:
	- Swap out environments with minimal code changes
	- Extend or create new simulations tailored to their specific needs
	
--- 
### Cartpole

- **Description**:  
     In this classic control problem, the agent's task is to balance a pole attached to a moving cart. The pole starts in an upright position, and the agent must apply forces to the cart to prevent the pole from falling over
- **Objective**:  
     Keep the pole balanced for as many time steps as possible without letting it fall or moving the cart out of bounds
- **Action Space**:
     **Discrete**: Move the cart left or right
- **Observation Space**:
	- Cart position
	- Cart velocity
	- Pole angle
	- Pole angular velocity


### Pendulum

- **Description**:  
     The pendulum starts in a downward position, and the agent must apply torque to swing it into an upright position and keep it balanced. The action space and dynamics are continuous, requiring fine-grained control.
- **Objective**:  
     Minimize the energy required to swing up and balance the pendulum upright while keeping it stable.
- **Action Space**:
     **Continuous**: A single scalar representing torque applied to the pendulum.
- **Observation Space**:
    - Pendulum's Angular position (cosine and sine representation for periodicity).
    - Pendulum's angular velocity


### Blackjack

- **Description**:  
     A simulation of the popular card game where the agent learns to make decisions (e.g., hit or stand) based on the state of the game. The environment introduces stochasticity and partial observability, as the agent cannot see all cards in play.
- **Objective**:  
     Maximize cumulative rewards by learning an optimal strategy for playing hands against a dealer.
- **Action Space**:
    - **Discrete**:
        - Hit (draw another card).
        - Stand (keep the current hand and end the turn).
- **Observation Space**:
    - Current hand value
    - Dealer’s visible card
    - Presence of a usable ace (value of 1 or 11)

# Algorithms
## Q-Learning
## Policy Gradient
## Policy Gradient with Neural Network



# How to run

## Environment

[Yang Shen] How to build environment using opam. How to install all libraries, including torch.



## Build and Run
make sure you have created a data folder at the root dictory by:
```
mkdir data
```
To build the project from the root directory:
```
dune clean; dune build
```
Explaination for basic parameters:
- algo: Specify the algorithm (e.g., qlearning, vpg, vpgnn).
- simulation: Specify the simulation environment (e.g., pendulum, cartpole, blackjack).
- episode: The number of training episodes to run.
- model-path: The file path for saving/loading the model
- render: Whether to render the simulation during training.

To train a Model, For example, to train a q-learning algorithm with the pendulum simulation for 300000 episodes, you can run the following command:  
```
dune exec -- ./src/bin/main.exe --episode 300000 --model-path data/pendulum.sexp
```
It would take a minute or two to complete 300000 episodes, we can obtain a relative good policy. to view the results of the trained q-learning algorithm with the pendulum simulation, you can add the `--render` argument:  
```
dune exec -- ./src/bin/main.exe --episode 300000 --model-path data/pendulum.sexp --render
```

## Demonstration

### Training

[qlearning_training.gif] [vpg_training.gif] [vpgnn_training.gif]

(in the same simulation, like cartpole)



### Simulation

[cartpole-qlearning.gif] [cartpole-vpg.gif] [cartpole-vpgnn.gif]

[pendulum-qlearning.gif] [pendulum-vpg.gif] [pendulum-vpgnn.gif]

[blackjack-qlearning.gif] [blackjack-vpg.gif]

# Conclusion and Future Work
Our project demonstrates the implementation of key RL algorithms—Q-Learning, Vanilla Policy Gradient, and Policy Gradient with Neural Networks—within OCaml codebase. By integrating these algorithms with diverse simulation environments, we have shown that our approach is both versatile and extensible. The use of functors and well-defined interfaces makes it straightforward to incorporate new algorithms or simulation engines. Our toolkit serves as a practical platform for experimenting with RL ideas.

For future work:
- Additional Algorithms: Expand the algorithm library to include Actor-Critic methods, PPO, DQN variations, or advanced algorithms like Soft Actor-Critic.
- More Environments: Integrating more simulations (e.g., Lunar Lander, Atari games, or custom robotics domains).
- Enhanced Visualization and Monitoring: Improve the graphical interfaces or adding web-based dashboards for monitoring training progress and analyzing policies.
- Performance Optimization: Explore optimization techniques, parallelization to handle more complex tasks and speed up training.
