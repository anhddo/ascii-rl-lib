### what is working so far
- we have implemented two different simulation enviroments: pendulum, blackjack
- we have implemented two different reinforcement learning algorithms: qlearning, policy gradient
- we have an interaction between a q-learning algorithm and the pendulum enviroment
- we have code coverage through bisect for our simulation enviroments, and the tests run properly through `dune test`
- we have modularized our code so that all enviroments, algorithms, and their respective configurations are all created through functors

### what is actively not working
- we want to have our main command line comment be able to take in arguments and select the appropriate reinforcement learning algorithm and simulation to pair together and render, but currently it cannot. instead it is hard coded to always run the pendulum enviroment with the q-learning algorithm
- there is one reinforcement learning algorithm that is not yet integrated with our own simulations: policy gradient with neural network
- we will add the following arguments to the command line: `--algo`, `--simulation` after we integrate the other reinforcement learning algorithms and simulations in to our library.

### what we hope to add before the final submission
- we hope to implement 1+ more enviroments (tentatively cartpole, then lunar lander, hence why there are boilerplate code for them)
- we hope to implement 1+ more reinforcement learning algorithms


#### how to run

to start training the q-learning algorithm with the pendulum simulation for 300000 episodes, you can run the following command:  
```dune exec -- ./src/bin/main.exe --episode 300000 --model-path data/pendulum.sexp ```

it would take a minute or two to complete 300000 episodes, we can obtain a relative good policy. to view the results of the trained q-learning algorithm with the pendulum simulation, you can add the `--render` argument:  
```dune exec -- ./src/bin/main.exe --episode 3 --model-path data/pendulum.sexp --render```

