import numpy
import value_iteration

white = 0
black = 1

state_names = ['white', 'black']
num_states = len(state_names)

go_white = 0
go_black = 1
go_random = 2

action_names = ['go_white', 'go_black', 'go_random']
num_actions = len(action_names)

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
    
#action-value functions Q are represented as a matrix of shape (num_states, num_actions)
    
#getting the deterministic policy associated to an action-value function is immediate
def get_policy(Q):
    return Q.argmax(1)
    
def epsilon_greedy_policy(Q, epsilon, s):
    num_actions = Q.shape[1]
    if numpy.random.random() < epsilon:
        return numpy.random.choice(num_actions)
    else:
        return Q[s].argmax()
        
def Q_learning_update(Q, alpha, gamma, s, a, r, s_prime):
    max_q_estimate = Q[s_prime].max()
    Q[s, a] = Q[s, a] + alpha * (r + gamma * max_q_estimate - Q[s, a])

def one_learning_iter(Q, Env, alpha, gamma, epsilon, s):
    a = epsilon_greedy_policy(Q, epsilon, s)
    s_prime, r = Env(s, a)
    Q_learning_update(Q, alpha, gamma, s, a, r, s_prime)
    return a, r, s_prime
    
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
    
def print_action_value_function(state_names, action_names, Q):
    row_labels = [' '] + action_names
    rows = []
    for name, values in zip(state_names, Q.tolist()):
        rows.append([name] + values)
    value_iteration.print_table(row_labels, rows)
    
    
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

