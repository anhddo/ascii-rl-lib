# Group Members
 Jishuo Yang, Yang Shen, Spencer Ye, Anh Do 
# Introduction
Reinforcement Learning has emerged as a powerful paradigm for solving complex decision-making problems across various domains. Our team is implementing RL algorithms and integrating them with simulation environments. The purpose of this project is to create a Reinforcement Learning library that enables users to train and test various RL algorithms within simulated environments. We implemented key algorithms like Q-learning, Policy Gradient, and Policy Gradient with Neural Network. We integrated with multiple simulation environments, including CartPole, Pendulum, and Blackjack, to verify correctness and robustness. Users will interact via a command-line interface, specifying parameters such as the algorithm, environment, and training configurations. The project is divided into two main components: the RL algorithm library and simulation environment integration, resulting in a comprehensive RL toolkit.
 
# Implementation
## Interaction protocol
## Architecture

## Libraries

[Yang Shen] Mentioned all the dependent libraries. Decribe ocaml-torch.

# Simulation

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
