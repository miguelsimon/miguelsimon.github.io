import numpy

import theano
import theano.tensor as T

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

def get_return(rewards, gamma):
    exps = numpy.arange(0, rewards.shape[0])
    return (rewards * (gamma ** exps)).sum()
    
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

def sample_a(policy_f, theta, s):
    num_actions = theta.shape[1]
    
    action_probs = []
    for a in range(num_actions):
        action_probs.append(policy_f(theta, s, a))
    action = numpy.random_choice(num_actions, p = action_probs)
    return a
    
def learn_iter(Env, theta, s, policy_f, policy_gradient_f, learning_rate, K):
    states = []
    actions = []
    rewards = []
    
    for k in xrange(K):
        a = sample_a(policy_f, theta, s)
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
    
    return new_theta, ret
    
def learn(Env, policy_f, policy_gradient_f, learning_rate, K, num_iters):
    theta = numpy.ones((2, 3))
    
    returns = []
    for i in range(num_iters):
        theta, ret = learn_iter(Env, theta, s, policy_f, policy_gradient_f, learning_rate, K)
        returns.append(ret)
    return theta, returns
        
    