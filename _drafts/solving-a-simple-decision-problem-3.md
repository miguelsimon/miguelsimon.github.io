---
layout: post
comments: true
title: "Solving a simple decision problem 3: Policy gradient"
meta: "A tutorial walkthrough on solving a simple reinforcement learning problem using policy gradient methods."
date: 2016-11-14
tags: [reinforcement_learning]
---

We're going to solve the problem as formulated in the [second post](/blog/2016/10/20/solving-a-simple-decision-problem-2): we don't know the transition probabilities or the reward function, instead we rely on interactions with the environment to learn the optimal policy.

We used Q-learning in the last post: we estimated an optimal action-value function $$ Q(s, a) $$, and constructed an optimal policy based on our estimated $$ Q(s, a) $$.

Policy gradient methods take a different approach: instead of estimating an action-value function, policy gradient methods parameterize the space of policies and optimize the policy parameters by gradient descent. The idea is to formulate a cost function in terms of the policy parameters, and then optimize those policy parameters.

### Parameterized policies

We're going to focus on stochastic policies, [though policy gradient methods can be applied to deterministic policies as well](http://jmlr.org/proceedings/papers/v32/silver14.pdf).

Remember that we represent a stochastic policy as a mapping from a state $$ s $$ and an action $$ a $$ to the probability of taking action $$ a $$ given state $$ s $$:

$$

\pi: S \times A \rightarrow \mathbb{R}

$$

$$

\pi(s, a) = P(a \mid s)

$$

Parameterizing the space of policies then means choosing a parameter space $$ \Theta $$ where each point $$ \theta \in \Theta $$ in the parameter space corresponds to a policy; we'll denote this policy $$ \pi^{\theta}(s, a) $$

In our case, this means choosing a parameter space $$ \Theta $$ and a function $$ f $$ so that, given a point in the parameter space $$ \theta $$, a value is assigned to every cell in our policy matrix:

$$

\pi^{\theta}(s) = \begin{array}{c|rrr}
 & gw & gb & gr \\
\hline
w & f_{00}(\theta) & f_{01}(\theta) & f_{02}(\theta) \\
b & f_{10}(\theta) & f_{11}(\theta) & f_{12}(\theta) \\
\end{array}

$$

There are many ways to choose such a parameterization: one intuitive way, given that our rows are probabilities and must sum to one, is to take $$ \Theta $$ as the space of real 2 x 3 matrices, and apply softmax to the rows of this matrix to obtain a valid policy.

Given a point $$ \theta $$ in this space:

$$

\theta = \begin{array}{c|rrr}
 &  &  &  \\
\hline
 & \theta_{00} & \theta_{01} & \theta_{02} \\
 & \theta_{10} & \theta_{11} & \theta_{12} \\
\end{array}

$$

the associated policy would be the rowwise softmax of this matrix. This looks cumbersome but is very straightforward:

$$

f_{ij}(\theta) = \frac{e^{\theta_{ij}}}{\sum_{k=0}^K e^{\theta_{ik}}}

$$

As a numeric example, the point

$$

\theta = \begin{array}{c|rrr}
 &  &  &  \\
\hline
 & 1 & 2 & 3 \\
 & 1 & 1 & 1 \\
\end{array}

$$

Corresponds to the policy

$$

\pi^{\theta}(s) = \begin{array}{c|rrr}
 & gw & gb & gr \\
\hline
w & 0.09003057 &  0.24472847 &  0.66524096 \\
b & 0.33333333 &  0.33333333 &  0.33333333 \\
\end{array}

$$

It is even more straightforward in Theano, which has a built-in softmax op. So let's build a Theano graph to return both a policy function and a policy gradient function:

```python

def build_policy():
    theta = T.matrix('theta')
    a = T.iscalar('a')
    s = T.iscalar('s')
    
    policy = T.nnet.softmax(theta)[a, s]
    policy_gradient = T.grad(policy, theta)
    
    policy_f = theano.function(
        inputs = [theta, s, a],
        outputs = policy)
        
    policy_gradient_f = theano.function(
        inputs = [theta, s, a],
        outputs = policy_gradient)

    return policy_f, policy_gradient_f
    
```

### The optimization objective in terms of the policy

What do we want to maximize? Following [sutton1](#sutton1), we're going to formulate our objective $$ \rho(\pi) $$ as the long-term expected reward from following a policy $$ \pi $$ from a given start state $$ s_0 $$:

$$

\rho(\pi) = \mathbb{E} \left[ \sum^{\infty}_{t=1} \gamma^{t-1}r_t \mid s_0, \pi \right]

$$

If $$ \pi $$ is a function of the parameter $$ \theta $$, we can walk up the gradient of this function w.r.t $$ \theta $$,

$$ 

\frac{\partial \rho(\pi)}{\partial \theta} 

$$

to find a better policy!

It turns out this gradient has convenient properties [sutton1](#sutton1) that allow us to approximate it efficiently.

The REINFORCE algorithm [williams1](#williams1) is one such approximation to the gradient: we can use this quantity to update the parameters $$ \theta $$. If we make the dependence of the policy on the parameter explicit, this quantity is:

$$

\Delta\theta_t = \frac{\partial \pi(\theta, s_t, a_t)}{\partial \theta} R_t \frac{1}{\pi(\theta, s_t, a_t)}

$$

which we can calculate if we know how to calculate $$ \frac{\partial \rho(\pi)}{\partial \theta} $$; the $$ R_t $$ term is the discounted return associated to step $$ t $$,

$$

R_t = \sum^{\infty}_{k=1} \gamma^{k-1}r_{t + k}

$$

We can translate this easly to python code: we can calculate $$ \Delta\theta_t $$ via

```python
def calculate_delta_theta(
        theta, 
        a,
        s,
        ret,
        policy_f,
        policy_gradient_f):
        
    pi_grad = policy_gradient_f(theta, a, s)
    pi = policy_f(theta, a, s)
    return pi_grad * ret * (1 / pi)
```

So the parameter update step simply means applying this update combined with a learning rate:

```python
def policy_gradient_step(
        theta, 
        a,
        s,
        ret,
        policy_f,
        policy_gradient_f,
        learning_rate = 0.1):
            
    delta_theta = calculate_delta_theta(
        theta, 
        a,
        s,
        ret,
        policy_f,
        policy_gradient_f)
    
    return theta + delta_theta * learning_rate
```

How do we use this to actually *learn*?

### Interacting with the environment

If we look at the terms in the REINFORCE algorithm, we realize that estimating the return $$ R_t $$ is the critical part: given a policy, we can interact with the environment for $$ K $$ steps until changes in our $$ R_t $$ term are deemed small enough:

$$

R_t = \sum^{K}_{k=1} \gamma^{k-1}r_{t + k}

$$

we then update our policy and loop. In python code,

```python
def learn_iter(Env, theta, s, policy_f, policy_gradient_f, learning_rate, K):
    states = []
    actions = []
    rewards = []
    
    for k in xrange(K):
        a = sample_a(policy_f, s)
        s_prime, reward = Env(s, a)
        states.append(s)
        actions.append(a)
        rewards.append(r)
        
        s = s_prime
   
    ret = get_return(rewards, gamma)
    
    new_theta = policy_gradient_step(
        theta,
        a[0],
        s[0],
        ret,
        policy_f,
        policy_gradient_f,
        learning_rate)
    
    return new_theta
```


<a name="sutton1" href="https://webdocs.cs.ualberta.ca/~sutton/papers/SMSM-NIPS99.pdf">Policy Gradient Methods for
Reinforcement Learning with Function
Approximation</a>

<a name="williams1" href="http://incompleteideas.net/sutton/williams-92.pdf">Simple Statistical Gradient-Following Algorithms for
Connectionist Reinforcement Learning </a>


