import numpy
# states
cold = 0
hot = 1

state_names = ['cold', 'hot']
num_states = len(state_names)

#actions
go_cold = 0
go_hot = 1
go_random = 2

action_names = ['go_cold', 'go_hot', 'go_random']
num_actions = len(action_names)

# The reward function is represented as a tensor of shape (num_actions, num_states, num_states)
# where the first dimension indexes the action, the second indexes the state at time t, and the 
# third indexes the state at time t + 1
# Rewards in this problem are independent of the action, they just depend on the state transition.

R = numpy.zeros((num_actions, num_states, num_states))

R[go_random] = numpy.array([
    [-1, 1],
    [1, -1]])
    
R[go_hot] = R[go_random]
R[go_cold] = R[go_random]

# The transition probability function is represented as a tensor of shape (num_actions, num_states, num_states)
# as before, these dimensions correspond to the action, state at time t, and state at time t + 1

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

# The discount factor is called gamma

gamma = 0.8

def print_table(x_labels, rows):
    print
    num_cols = len(x_labels)
    row_format ="{:>16} |" * num_cols
    header = row_format.format(*x_labels)
    hline = ''.join('-' for _ in header)
    print header
    print hline
    for row in rows:
        print row_format.format(*row)
        print hline
        
def print_value_function(state_names, v):
    print_table(['s', 'V(s)'], zip(state_names, v))
    
def print_policy_function(state_names, action_names, policy):
    print_table(['s', 'policy(s)'], zip(state_names, [action_names[x] for x in policy]))

# Monte carlo simulator 
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
    
def print_policy_statistics(state_names, action_names, R, P, gamma, policy):	
    print 'policy function:'
    print_policy_function(state_names, action_names, policy)
    print 'estimated value function:'
    v = monte_carlo_value_estimate(R, P, gamma, policy)
    print_value_function(state_names, v)
    print
    
bad_policy = numpy.zeros(num_states, dtype = int)
bad_policy[cold] = go_cold
bad_policy[hot] = go_hot

print 'bad policy'
print_policy_statistics(state_names, action_names, R, P, gamma, bad_policy)
print

random_policy = numpy.zeros(num_states, dtype = int)
random_policy[cold] = go_random
random_policy[hot] = go_random

print 'random policy'
print_policy_statistics(state_names, action_names, R, P, gamma, random_policy)
print

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
        
    
#def test():
    #value = value_iteration(states, actions, reward, transition_p, gamma, max_iter = 200)
    #policy = calculate_policy(states, actions, reward, transition_p, gamma, value)
    #print policy
    #print monte_carlo_value_estimate(states, actions, reward, transition_p, gamma, policy)
    
#if __name__ == '__main__':
    #test()