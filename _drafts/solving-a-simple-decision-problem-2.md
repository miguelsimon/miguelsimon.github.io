---
layout: post
comments: true
title: "Solving a simple decision problem part 2"
meta: "A tutorial walkthrough on solving a simple reinforcement learning problem using tabular Q-learning in python."
date: 2016-10-15
---

## Solving a simple decision problem with Q-learning

We're going to attempt to solve the problem we examined in the [first post](/blog/2016/10/15/solving-a-simple-decision-problem-1), but this time we're going to pretend we don't know the full specification of the problem: we don't know the transition probabilities and we don't know the reward function.

Our data now comes from interacting with the environment, choosing actions, and observing the rewards we get, not from a full specification of the system.

We need to use our interaction with the environment to refine our policy: this is [reinforcement learning](https://webdocs.cs.ualberta.ca/~sutton/book/ebook/node28.html).

[Code is on gihub](https://github.com/miguelsimon/miguelsimon.github.io/tree/master/code).

## Problem statement

We're deep underground in a secret facility, conducting research for our [thesis advisor](https://en.wikipedia.org/wiki/Donald_Ewen_Cameron#MKULTRA_Subproject_68).

We're observing a test subject in a room paved with black and white colored tiles in a checkerboard pattern. A buzzer sounds to mark a time step. At each time step, we have a microphone we can use to [order](http://www.syndicatewiki.com/wiki/Persuadertron) the subject to move to a white tile, a black tile, or move to a random tile. 

The subject will whimper when he feels uncomfortable, and smile when he feels ok: we're feeling pretty ambivalent about our research lately, and we really want to make the subject smile as much as possible. How can we turn our observations and interactions with the subject into a policy that chooses orders to maximize the subjects' smiling time?

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

The *next* state $$ s_{t+1} $$
The reward $$ r_{t+1} $$

$$ Env $$ *is not a function*; different invocations of $$ Env $$ with the same parameters may give different results, because transitions can be stochastic.

$$

s_{t+1}, r_{t+1} = Env(s_t, a_t) 

$$

*We* aren't supposed to know the transition probabilities and the reward function, but the *system* knows them: let's use them to implement the environment in python, so we can apply the P and R matrices from the first post. We'll provide a function ```make_environment``` that returns an environment that can be invoked:

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

Let's see a few examples of stochastic policies:




