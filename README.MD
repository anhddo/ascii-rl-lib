### What is working so far
- We have implemented two different simulation enviroments: Pendulum, Blackjack
- We have implemented a reinforcement learning algorithms: QLearning
- We have an interaction between a Q-Learning Algorithm and the Pendulum Enviroment
- We have code coverage through bisect for our simulation enviroments, and the tests run properly through `dune test`
- We have modularized our code so that all enviroments, algorithms, and their respective configurations are all created through functors

### What is actively not working
- We want to have our main command line comment be able to take in arguments and select the appropriate reinforcement learning algorithm and simulation to pair together and render, but currently it cannot. Instead it is hard coded to always run the Pendulum enviroment with the Q-Learning Algorithm
- There are two reinforcement learning algorithms that are not yet integrated with our own simulations: Policy Gradient, Policy Gradient with Neural Network

### What we hope to add before the final submission
- We hope to implement 1+ more enviroments (tentatively Cartpole, then Lunar Lander, hence why there are boilerplate code for them)
- We hope to implement 1+ more reinforcement learning algorithms


#### How To Run

This is a basic way of running our demo of the Q-Learning Algorithm and the Pendulum Enviroment
dune exec -- ./src/bin/main.exe --episode 200000 --model-path data/pendulum.sexp --render

If you run this command without the --render, you can train the algorithm and get better results.


