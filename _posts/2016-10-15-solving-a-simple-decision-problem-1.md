---
layout: post
comments: true
title: "Solving a simple decision problem 1: value iteration"
meta: "A tutorial walkthrough on using value iteration to solve a simple Markov decision process in python."
date: 2016-10-15
tags: [reinforcement_learning]
---

This is the first in a series of posts solving a toy decision problem using different techniques, as I'm in the process of learning about them. **There may be conceptual and implementation mistakes**, so take everything you read here with a sack of salt, and please leave a comment if you spot a mistake.

I'm using the excellent Sutton and Barto book, [Reinforcement Learning: An Introduction](https://webdocs.cs.ualberta.ca/~sutton/book/ebook/the-book.html).

You'll need [python](https://www.python.org/) and [numpy](http://www.numpy.org/) to run the code.

[Code is on github](https://github.com/miguelsimon/miguelsimon.github.io/tree/master/code/simple_decision_problem/value_iteration.py).

## <a name="problem_statement"></a> Problem statement

It's the 60s; we're an unwilling participant in a [questionably ethical](https://en.wikipedia.org/wiki/Project_MKUltra) research program.

We wake to find ourselves barefoot in a room paved with blue and red colored tiles in a checkerboard pattern: the blue tiles are very cold, and the red tiles are very hot.

A buzzer sounds to mark a time step. At each time step, we can decide to move to a hot tile, or to move to a cold tile, or close our eyes and move to a random tile.

We want to choose a way of moving that maximizes our comfort. If we stay on the same tile type for too long, our feet become either too cold or too hot. [For some reason](https://en.wikipedia.org/wiki/Project_MKUltra#Drugs) we find our normal thought processes don't seem to work correctly: we are unable to find a sensible movement policy.

In our drugged, terrified state, we decide that applying the full machinery of Markov decision processes to find a solution is a good idea. Inexplicably, we have access to a modern computer equipped with python and numpy and connected to the not-yet existent internet as we're hopping around.

### Markov decision process recap

[Here is a good explanation of what a Markov decision process is](https://webdocs.cs.ualberta.ca/~sutton/book/ebook/node33.html) by people who actually know what they're talking about. The aim of this post is to *solve* a Markov decision problem, not to explain MDPs in general.

To formulate our problem as a finite, discounted-reward MDP we need to decompose it in the following way:

* A *state space* $$ S $$ of possible states we can find ourselves in.
* An *action space* $$ A $$ of possible actions we can take in each state
* *Transition probabilities* $$ P(s' \mid a, s) $$ that tell us the probability of going to the next step $$ s' $$ if we take an action $$ a $$ in state $$ s $$.
* A *reward function* $$ R(a, s, s') $$ that tells us the payoff of moving from state $$ s $$ to state $$ s' $$ using action $$ a $$.
* A *discount factor* $$ \gamma \lt 1$$ that tells us how future rewards are discounted: at time step t, the reward is multiplied by $$ \gamma ^ t $$.

We can use this formulation to find an *optimal policy*, that is, an optimal way of choosing actions given states so as to maximize our long term reward.

### State space $$ S $$
We can either be on a hot tile $$ h $$ or on a cold tile $$ c $$, hence we have 2 possible states:

$$
S = \{h, c\}
$$

We can express this in python code as

```python
cold = 0
hot = 1

state_names = ['cold', 'hot']
num_states = len(state_names)
```

### Action space $$ A $$
When we move from our current tile, we can either go to a hot tile, $$ gh $$, go to a cold tile, $$ gc $$, or close our eyes and go to a random tile, $$ gr $$, so our set of actions is:

$$
A = \{gh, gc, gr\}
$$

In code,

```python
go_cold = 0
go_hot = 1
go_random = 2

action_names = ['go_cold', 'go_hot', 'go_random']
num_actions = len(action_names)
```

### Reward function $$ R(a, s, s') $$

$$ R(a, s, s') $$ means "the reward we get if we apply action $$ a $$ at state $$ s $$ and end up in state $$ s' $$".

If we *stay* on a hot tile or *stay* on a cold tile we become uncomfortable; it is only when we move from a hot tile to a cold tile, or from a cold tile to a hot tile, that we feel ok. In this case the reward is independent of the action taken: it just depends on the states $$ s $$ and $$ s' $$. Let's define our reward function $$ R(a, s, s') $$ as a table. We use . as a wildcard to mean any action: this saves us from writing out the full (redundant) table:

$$

\begin{array}{c| r | r| r}
a & s & s' & R(a, s, s') \\ \hline
\cdot & h & h & -1 \\ \hline
\cdot & h & c & 1 \\ \hline
\cdot & c & h & 1 \\ \hline
\cdot & c & c & -1 \\ \hline
\end{array}

$$

The reward function is represented as a tensor of shape (num_actions, num_states, num_states)

```python
R = numpy.zeros((num_actions, num_states, num_states))

R[go_random] = numpy.array([
    [-1, 1],
    [1, -1]])
    
R[go_hot] = R[go_random]
R[go_cold] = R[go_random]
```

### Transition probabilities $$ P(a, s, s') $$

$$ P(a, s, s') $$ means "the probability that choosing action $$ a $$ in state $$ s $$ leaves us in state $$ s' $$", and can also be written, using notation familiar from probability theory, as $$ P(s' \mid a, s) $$.

We need to specify the probability of the next state given the current state and the action. When we make a random move $$ gr $$ our probability of ending up on a hot or cold tile is equal. When we make a move to a hot tile, $$ gh $$, or to a cold tile, $$ gc $$, our nervous system doesn't *always* do what we tell it, but it does so 95% of the time.

$$

\begin{array}{c| r | r| r}
a & s & s' & P(a, s, s') \\ \hline
gr & h & h & 0.5 \\ \hline
gr & h & c & 0.5 \\ \hline
gr & c & h & 0.5 \\ \hline
gr & c & c & 0.5 \\ \hline \\ \hline

gh & h & h & 0.95 \\ \hline
gh & h & c & 0.05 \\ \hline
gh & c & h & 0.95 \\ \hline
gh & c & c & 0.05 \\ \hline \\ \hline

gc & h & h & 0.05 \\ \hline
gc & h & c & 0.95 \\ \hline
gc & c & h & 0.05 \\ \hline
gc & c & c & 0.95 \\ \hline
\end{array}

$$

The transition probability function is represented as a tensor of shape (num_actions, num_states, num_states); as before, these dimensions correspond to the action, state at time t, and state at time t + 1

```python
P = numpy.zeros((num_actions, num_states, num_states))

P[go_random] = [
    [0.5, 0.5],
    [0.5, 0.5]]
    
P[go_hot] = [
    [0.05, 0.95],
    [0.05, 0.95]]
    
P[go_cold] = [
    [0.95, 0.05],
    [0.95, 0.05]]
```

### Discount factor $$ \gamma $$
Finally we're going to specify the discount factor. We're rather arbitrarly going to set it at 0.8:

$$

\gamma = 0.8

$$

```python
gamma = 0.8
```

In a flash of insight, we visualize a nasty-looking [transition graph](https://webdocs.cs.ualberta.ca/~sutton/book/ebook/node33.html#fig:st-graph) for our predicament:

![transition_graph](/resources/simple_decision_problem/transition_graph.gv.svg){:class="img-responsive"}

## What is a policy?

We are looking for a *policy* $$ \pi $$. A policy is a way of choosing an action given our current state.

If we have a a *deterministic* policy, given a state $$ s $$, we always choose the same action $$ a $$. The mapping from states to actions is our policy specification; it can be implemented as a simple lookup table. A deterministic policy can be seen as a mapping from states to actions $$ \pi: S \rightarrow A $$

If we have a *stochastic* policy, given a state $$ s $$, we choose the action $$ a $$ from a probability distribution that depends on $$ s $$, $$ Pr(a \mid s) $$. Notice that deterministic policies are just a special case of stochastic policies.

We're going to focus on deterministic policies in this post, so for us a policy is just a mapping $$ \pi: S \rightarrow A $$

Let's see a few examples of deterministic policies:

One not very good policy might be to always go to the same tile type we're currently on, so we are either always hot or always cold:

$$

\pi(s) = \begin{array}{c|rrr}
 s & \pi(s) \\
\hline
h & gh \\
c & gc \\
\end{array}

$$

Another, slightly better policy might be to always choose the next tile randomly:

$$

\pi(s) = \begin{array}{c|rrr}
 s & \pi(s) \\
\hline
h & gr \\
c & gr \\
\end{array}

$$


## What is a good policy?

Let's turn to the Sutton and Barto book for an explanation of how to [rigorously define the quantity our policy is supposed to maximize](https://webdocs.cs.ualberta.ca/~sutton/book/ebook/node30.html).

The quantity of interest is the  **infinite-horizon discounted reward**:

$$

\underbrace{\sum^{\infty}_{t=0}}_{\rm infinite-horizon} {\underbrace{\gamma^t}_{\rm discount} \cdot \underbrace{R(a_t, s_t, s_{t+1})}_{\rm reward}}

$$

Because out process is a [Markov process](https://webdocs.cs.ualberta.ca/~sutton/book/ebook/node32.html), if our actions $$ a $$ are chosen via a policy $$ \pi $$, the state $$ s' $$ only depends on the state $$ s $$ and the state sequence forms a Markov chain $$ s_0, s_1, ... $$ We're seeking a policy that maximizes the infinite horizon discounted reward over many realizations of this Markov chain:

$$ 

\mathbb{E}_\pi \left[ \sum^{\infty}_{t=0} {\gamma^t R(\pi(s_t), s_t, s_{t+1})} \right]

$$

### Value function

A very useful concept is that of the [value function](https://webdocs.cs.ualberta.ca/~sutton/book/ebook/node34.html). Given a policy, a value function assigns a value to every state which is the expected reward achieved if that policy is followed from that state onwards. So if we follow a policy $$ \pi $$, the value function for a state $$ x $$ is

$$

V^\pi(x) = \mathbb{E}_\pi \left[ R(\pi(x), x, s_1) + \sum^{\infty}_{t=1} {\gamma^t R(\pi(s_t), s_t, s_{t+1})} \right]

$$

We can write a [Monte Carlo approximation](https://en.wikipedia.org/wiki/Monte_Carlo_method) to estimate this, both as an exercise and as a sanity check for later work:

```python
def simulate_one(R, P, gamma, policy, s, epsilon = 10e-5):
    """Simulate a markov chain started at state s, and return the cumulative discounted reward."""
    num_states = R.shape[1]
    states = numpy.arange(num_states)
    accum = 0 # cumulative discounted reward
    count = 0
    gamma_factor = gamma ** count
    while gamma_factor > epsilon: #stop the simulation when changes to cumulative reward are tiny enough
        gamma_factor = gamma ** count
        action = policy[s]
        p = P[action, s]
        s_prime = numpy.random.choice(states, p = p) #sample the next state given the transition probabilities
        accum += gamma_factor * R[action, s, s_prime]
        count += 1
        s = s_prime #update the state for the next simulation step
    return accum
    
def monte_carlo_value_estimate(R, P, gamma, policy, num_samples = 1000):
    """Estimate the value function for a given policy."""   
    num_states = R.shape[1]
    value = numpy.zeros(num_states)
    for i in xrange(num_samples):
        for s in xrange(num_states):
            value[s] += simulate_one(R, P, gamma, policy, s)
    return value / num_samples
```

We also add some code to format policy statistics:

```python
def print_policy_statistics(state_names, action_names, R, P, gamma, policy):    
    print 'policy function:'
    print_policy_function(state_names, action_names, policy)
    print 'estimated value function:'
    v = monte_carlo_value_estimate(R, P, gamma, policy)
    print_value_function(state_names, v)
    print
```

Let's check out our monte carlo estimate for the value function of the bad policy:

$$

\pi(s) = \begin{array}{c|rrr}
 s & \pi(s) \\
\hline
h & gh \\
c & gc \\
\end{array}

$$

```python
bad_policy = numpy.zeros(num_states, dtype = int)
bad_policy[cold] = go_cold
bad_policy[hot] = go_hot

print 'bad policy'
print_policy_statistics(state_names, action_names, R, P, gamma, bad_policy)
print
```

returns:

```
bad policy
policy function:

               s |       policy(s) |
------------------------------------
            cold |         go_cold |
------------------------------------
             hot |          go_hot |
------------------------------------
estimated value function:

               s |            V(s) |
------------------------------------
            cold |  -4.48901598535 |
------------------------------------
             hot |  -4.47581611133 |
------------------------------------
```

So, not very good.

Let's check out our monte carlo estimate for the value function of the random policy:

$$

\pi(s) = \begin{array}{c|rrr}
 s & \pi(s) \\
\hline
h & gr \\
c & gr \\
\end{array}

$$

```python
random_policy = numpy.zeros(num_states, dtype = int)
random_policy[cold] = go_random
random_policy[hot] = go_random

print 'random policy'
print_policy_statistics(state_names, action_names, R, P, gamma, random_policy)
print
```

prints out:

```
random policy
policy function:

               s |       policy(s) |
------------------------------------
            cold |       go_random |
------------------------------------
             hot |       go_random |
------------------------------------
estimated value function:

               s |            V(s) |
------------------------------------
            cold |-0.0547534204783 |
------------------------------------
             hot |-0.00431196610621 |
------------------------------------
```

Much better, but we can still find a better policy.

## Finding the optimal policy with fixed point iteration

[Finite MDPs such as this one have well-defined solutions in terms of their associated optimal value functions](https://webdocs.cs.ualberta.ca/~sutton/book/ebook/node34.html#eq:Bellman-Vpi).

[There are several ways to compute optimal value functions via fixed point iterations](https://en.wikipedia.org/wiki/Markov_decision_process#Value_iteration); once we have an optimal value function, we can recover an underlying optimal policy and the problem is solved.

We're going to apply Bellman's value iteration algorithm, which says that the optimal value function is a fixed point of this iteration:

$$

V_{i+1}(s) := \max_a \left\{ \sum_{s'} P(s' | a, s) \left( R(a, s, s') + \gamma V_i(s') \right) \right\}

$$

We can straightforwardly translate the value iteration to python (we'd vectorize it for real use though):

```python
def one_value_iteration(R, P, gamma, s, v):
    num_states = R.shape[1]
    num_actions = R.shape[0]
    v_s_next = numpy.zeros(num_actions)
    for a in xrange(num_actions):
        for s_prime in xrange(num_states):
            v_s_next[a] += P[a, s, s_prime] * (R[a, s, s_prime] + gamma * v[s_prime])
    return v_s_next.max()
            
def value_iteration(R, P, gamma, max_iter):
    num_states = R.shape[1]
    v = numpy.zeros(num_states)
    for i in xrange(max_iter):
        v_next = v.copy()
        for s in xrange(num_states):
            v_next[s] = one_value_iteration(R, P, gamma, s, v)
        
        v = v_next
        #if converged(v, v_next):
            #break
    return v_next
```

And we can [calculate the policy corresponding to a given value function](https://en.wikipedia.org/wiki/Markov_decision_process#Algorithms) with

$$

\pi(s) := \arg \max_a \left\{ \sum_{s'} P(a, s,s') \left( R(a, s,s') + \gamma V(s') \right) \right\}

$$


```python
def _calculate_policy(R, P, gamma, s, v):
    num_states = R.shape[1]
    num_actions = R.shape[0]
    action_scores = numpy.zeros(num_actions)
    for a in xrange(num_actions):
        for s_prime in xrange(num_states):
            action_scores[a] += P[a, s, s_prime] * (R[a, s, s_prime] + gamma * v[s_prime])
    return action_scores.argmax()
    
def calculate_policy(R, P, gamma, v):
    num_states = R.shape[1]
    policy = numpy.zeros(num_states, dtype=int)
    for s in xrange(num_states):
        policy[s] = _calculate_policy(R, P, gamma, s, v)
    return policy
```

We now apply this to our fully-specified MDP so we can find an optimal value, its corresponding policy, and use our monte carlo estimate of the value function given a policy to double-check our work:

```python
value = value_iteration(R, P, gamma, max_iter = 500)
policy = calculate_policy(R, P, gamma, value)

print 'optimal value:'
print_value_function(state_names, value)
print
print 'optimal policy:'
print_policy_function(state_names, action_names, policy)
print
estimated_value = monte_carlo_value_estimate(R, P, gamma, policy)
print 'value estimate for optimal policy:'
print_value_function(state_names, estimated_value)
```

The results are encouraging:

```
optimal value:

               s |            V(s) |
------------------------------------
            cold |             4.5 |
------------------------------------
             hot |             4.5 |
------------------------------------

optimal policy:

               s |       policy(s) |
------------------------------------
            cold |          go_hot |
------------------------------------
             hot |         go_cold |
------------------------------------

value estimate for optimal policy:

               s |            V(s) |
------------------------------------
            cold |   4.53990837168 |
------------------------------------
             hot |    4.5019358837 |
------------------------------------
```

Our program chose the policy of hopping from cold tiles to hot tiles, and from hot tiles to cold tiles:

$$

\pi(s) = \begin{array}{c|rrr}
 s & \pi(s) \\
\hline
h & gc \\
c & gh \\
\end{array}

$$

and our monte carlo estimate of the value of that function roughly coincides with the optimal value found by the value iteration algorithm, so that's a relief.

We just invested a lot of notation and some code to find a policy we already knew was optimal. Yay?

Next time, we'll make the problem harder: can we find a policy when we *don't* know the transition probabilities and the reward function?

