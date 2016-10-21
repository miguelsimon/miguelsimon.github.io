---
layout: post
comments: true
title: "Solving a simple decision problem 2: Q-learning"
meta: "A tutorial walkthrough on solving a simple reinforcement learning problem using tabular Q-learning in python."
date: 2016-10-20
tags: [reinforcement_learning]
---

We're going to attempt to solve the problem we examined in the [first post](/blog/2016/10/15/solving-a-simple-decision-problem-1#problem_statement), but this time we're going to pretend we don't know the full specification of the problem: we don't know the transition probabilities and we don't know the reward function.

Our data now comes from interacting with the environment, choosing actions, and observing the rewards we get, not from a full specification of the system.

We need to use our interaction with the environment to refine our policy: this is [reinforcement learning](https://webdocs.cs.ualberta.ca/~sutton/book/ebook/node28.html).

[Code is on gihub](https://github.com/miguelsimon/miguelsimon.github.io/tree/master/code/simple_decision_problem/q_learning.py).

## Problem statement

We're deep underground in a secret facility, conducting research for our [thesis advisor](https://en.wikipedia.org/wiki/Donald_Ewen_Cameron#MKULTRA_Subproject_68).

We're observing a test subject in a room paved with black and white colored tiles in a checkerboard pattern. A buzzer sounds to mark a time step. At each time step, we have a microphone we can use to [persuade](http://www.syndicatewiki.com/wiki/Persuadertron) the subject to move to a white tile, a black tile, or move to a random tile. 

The subject will whimper when he feels uncomfortable, and smile when he feels ok: we're feeling pretty ambivalent about our research lately, and we really, *really* want to make the subject smile as much as possible. How can we turn our observations and interactions with the subject into a policy that chooses orders to maximize the subjects' smiling time?

We don't know *why* the subject might be comfortable or uncomfortable, or if the tile colors or actions have a bearing on his comfort level or not. What *do* we formally know?

### State space $$ S $$
The subject can be on a white tile or on a black tile

$$
S = \{w, b\}
$$

We can express this in python code as

```python
white = 0
black = 1

state_names = ['white', 'black']
num_states = len(state_names)
```

### Action space $$ A $$
We can order our subject to go to a white tile, go to a black tile, or go to a random tile:

$$
A = \{gw, gb, gr\}
$$

In code,

```python
go_white = 0
go_black = 1
go_random = 2

action_names = ['go_white', 'go_black', 'go_random']
num_actions = len(action_names)
```

### Environment

At a given time step $$ t $$, we find ourselves in a state $$ s_t $$. We can choose an action $$ a_t $$ and apply it: observing the environment at time step $$ t + 1 $$ tells us

* The *next* state $$ s_{t+1} $$
* The reward $$ r_{t+1} $$

$$ Env $$ *is not a function*; different invocations of $$ Env $$ with the same parameters may give different results, because transitions can be stochastic.

$$

s_{t+1}, r_{t+1} = Env(s_t, a_t) 

$$

*We* aren't supposed to know the transition probabilities and the reward function, but the *system* knows them: let's use them to implement the environment in python, so we can apply the P and R matrices from the first post. We'll provide a function ```make_environment(P, R)``` that takes an MDP specification and returns an environment that can be invoked:

```python
def sample_environment(P, R, s, a):
    """Returns a new state and a reward when given a state and an action"""
    num_states = P.shape[1]
    p = P[a, s]
    s_prime = numpy.random.choice(num_states, p = p) #sample next state
    r = R[a, s, s_prime]
    return s_prime, r
    
def make_environment(P, R):
    def env(s, a):
        return sample_environment(P, R, s, a)
    return env

Env = make_environment(value_iteration.P, value_iteration.R)
```

## What is a stochastic policy?

In the previous post we limited ourselves to deterministic policies, that is, policies that always choose the same action given the same state.

We're going to need more general *stochastic* policies now. Given a state $$ s $$, a stochastic policy $$ \pi $$ chooses an action $$ a $$ from a distribution that depends on the state $$ s $$.

Previously we represented a deterministic policy as a simple mapping from states to actions:

$$

\pi: S \rightarrow A

$$

We represent a stochastic policy as a mapping from a state $$ s $$ and an action $$ a $$ to the probability of taking action $$ a $$ given state $$ s $$

$$

\pi: S \times A \rightarrow \mathbb{R}

$$

$$

\pi(s, a) = P(a \mid s)

$$

To choose an action given a stochastic policy $$ \pi $$ and a state $$ s $$, we just sample an action from the distribution $$ P(a \mid s) $$ defined by the policy.

Let's see a few examples of stochastic policies. We can write them as a table of probabilities, with states as rows and actions as columns; the probabilities on a row must sum to 1.

First, let's write the deterministic policy of always choosing a random tile in this new notation:

$$

\pi(s) = \begin{array}{c|rrr}
 & gw & gb & gr \\
\hline
w & 0 & 0 & 1\\
b & 0 & 0 & 1\\
\end{array}

$$

Another, more stochastic policy would be to equiprobably choose between the go_black and go_random moves on a white tile, and always choose the go_white move on a black tile:

$$

\pi(s) = \begin{array}{c|rrr}
 & gw & gb & gr \\
\hline
w & 0 & 0.5 & 0.5\\
b & 1 & 0 & 0 \\
\end{array}

$$

## Q-learning

### The action-value function $$ Q(s, a) $$

The Q in Q-learning stands for the [action-value function](https://webdocs.cs.ualberta.ca/~sutton/book/ebook/node34.html); the action-value function for a given policy $$ Q^{\pi}(s, a) $$ gives the expected return of starting in state s, choosing action a, and following policy $$ \pi $$ from then on:

$$

Q^{\pi}(s_0, a_0) = \mathbb{E}_\pi \left[ R(s_0, a_0, s_1) + \sum^{\infty}_{t=1} {\gamma^t R(s_t, a_t, s_{t+1})} \right]

$$

An optimal action-value function $$ Q^* $$ exists, for the same reasons an optimal value function exists.

[Q-learning](https://webdocs.cs.ualberta.ca/~sutton/book/ebook/node65.html) is a way of approximating the optimal value function, $$ Q^* $$, given realizations of the Markov process generated using a *behaviour policy*. Interestingly, *any* policy that puts nonzero probability on all state-action pairs will eventually lead to convergence (rates of convergence will differ though). So the behaviour policy has to be stochastic.

The policy we actually choose to follow once we believe our estimate of $$ Q^* $$ probably will be different from our behaviour policy, and is called the *estimation policy*. The estimation policy can be deterministic.

### Learning the optimal $$ Q^*(s, a) $$

<a name="q-learning-update"></a> The Q-learning update looks like this (from [wikipedia](https://en.wikipedia.org/wiki/Q-learning)):

$$

Q_{t+1}(s_t, a_t) \leftarrow \underbrace{Q(s_{t},a_{t})}_{\rm old~value} + \underbrace{\alpha}_{\rm learning~rate} \cdot \left( \overbrace{\underbrace{r_{t+1}}_{\rm reward} + \underbrace{\gamma}_{\rm discount~factor} \cdot \underbrace{\max_{a}Q(s_{t+1}, a)}_{\rm estimate~of~optimal~future~value}}^{\rm learned~value} - \underbrace{Q(s_{t},a_{t})}_{\rm old~value} \right)

$$

In python code, destructively updating Q:

```python
def Q_learning_update(Q, alpha, gamma, s, a, r, s_prime):
    max_q_estimate = Q[s_prime].max()
    Q[s, a] = Q[s, a] + alpha * (r + gamma * max_q_estimate - Q[s, a])
```

### $$ \epsilon $$-greedy policy construction from current estimate $$ Q(s, a) $$

We need to choose a behaviour policy: we'll construct an [$$ \epsilon $$-greedy](https://webdocs.cs.ualberta.ca/~sutton/book/ebook/node16.html) policy using our current action-value estimate $$ Q $$ and $$ \epsilon $$; this guarantees a nonzero probability of visiting every state action pair $$ (s, a) $$.

A greedy policy just maximizes our estimated action-value function $$ \pi(s) := \arg \max_a Q(s, a) $$ and is combined with a policy of randomly selecting an action to yield the $$ \epsilon $$-greedy policy:

$$

\pi(s) = \begin{cases}
\arg \max_a Q(s, a) & \text{with probability } 1 - \epsilon, \\
\text{random a} & \text{with probability } \epsilon \end{cases}

$$

In code, 

```python
def epsilon_greedy_policy(Q, epsilon, s):
    if numpy.random.random() < epsilon:
        return numpy.random.choice(num_states)
    else:
        return Q[s].argmax()
```

### One learning step

All our ducks are in a row:

* We have an estimate Q(s, a) of the action-value function
* We have Env(s, a),  procedure we can use to query the environment
* We have a way of constructing our behaviour policy
* We have a way of [updating our action-value estimate](#action-value-update)

One learning iteration looks like:

- We find ourselves in state $$ s_t $$
- choose an action $$ a_t $$ using our $$ \epsilon $$-greedy behaviour policy
- find our reward and next state by querying the environment: $$ s_{t+1}, r_{t+1} = Env(s_t, a_t) $$
- update our estimate of the action-value Q

```python
def one_learning_iter(Q, Env, alpha, gamma, epsilon, s):
    a = epsilon_greedy_policy(Q, epsilon, s)
    s_prime, r = Env(s, a)
    Q_learning_update(Q, alpha, gamma, s, a, r, s_prime)
    return a, r, s_prime
```

And learning for num_iters iterations can be implemented like this:

```python
def learn(Q, Env, alpha, gamma, epsilon, num_iters):
    #choose initial state
    num_states = Q.shape[0]
    s = numpy.random.choice(num_states)
    rewards = []
    states = [s]
    actions = []
    
    for i in range(num_iters):
        a, r, s_prime = one_learning_iter(Q, Env, alpha, gamma, epsilon, s)
        actions.append(a)
        rewards.append(r)
        states.append(s_prime)
        
        s = s_prime
    return rewards, states, actions
```

Let's apply it to our problem:

```python
if __name__ == '__main__':
    alpha = 0.1
    gamma = 0.8
    epsilon = 0.1
    s = black
    Q = numpy.zeros((num_states, num_actions))
    
    learn(Q, Env, alpha, gamma, epsilon, 10000)

    
    policy = get_policy(Q)
    value_iteration.print_policy_function(state_names, action_names, policy)
    print
    print_action_value_function(state_names, action_names, Q)
```

The results are encouraging: the optimal policy is found, and the action-value estimates for the optimal actions roughly coincide with the value function found by value iteration in the previous post:

```
               s |       policy(s) |
------------------------------------
           white |        go_black |
------------------------------------
           black |        go_white |
------------------------------------


                 |        go_white |        go_black |       go_random |
------------------------------------------------------------------------
           white |   2.69229095237 |   4.42739200648 |   3.33323536995 |
------------------------------------------------------------------------
           black |   4.47769861276 |   2.72897511745 |   3.67471266008 |
------------------------------------------------------------------------
```

So Q-learning manages to keep the test subject smiling constantly once we stop using the stochastic behaviour policy and switch to using our deterministic estimation policy.







