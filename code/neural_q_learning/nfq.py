import numpy
import numpy.linalg


actions = numpy.array([
    [0, 0], 
    [1., 0],
    [-1, 0],
    [0, 1],
    [0, -1]])

def make_env(dt, optimal_v, is_colliding):
    """
        state is a tuple of (position, velocity)
        a is an impulse vector
    """
    def env(s, a):
        p, v = s
        if is_colliding(p):
            reward = -100
            return (p, numpy.zeros(2)), reward
        else:
            v = v + a * dt
            p = p + dt * v
            reward = - abs(numpy.linalg.norm(v) - optimal_v)
            return (p, v), reward
    return env
     
def one_summation_term(Q, params, s, a, r, s_prime, alpha, gamma):
    """
    construct 1 term in the minimization problem of the parameters wrt
    the estimated action-value function
    """
    q_t = Q.get_q_values(params, s)[a]
    q_o = r + gamma * Q.get_q_values(params, s_prime).max()
    q_next = (1 - alpha) * q_t + alpha * q_o
    return s, a, q_next
    
def epsilon_greedy_policy(Q, params, epsilon, s):
    action_values = Q.get_q_values(params, s)
    if numpy.random.random() < epsilon:
        return numpy.random.choice(action_values.shape[0])
    else:
        return action_values.argmax()

def one_interaction(Q, params, Env, alpha, epsilon, s):
    a = epsilon_greedy_policy(Q, params, epsilon, s)
    s_prime, r = Env(s, actions[a])
    return a, r, s_prime
    
class Runner(object):
    def __init__(self, Q, Env, alpha, gamma, epsilon):
        self.Q = Q
        self.Env = Env
        self.alpha = alpha
        self.gamma = gamma
        self.epsilon = epsilon
        
        self.params = Q.initialize_params()
        self.state = self.initialize_state()
        
        self.history = []
        
    def step(self):
        s = self.state
        a, r, s_prime = one_interaction(
            self.Q, 
            self.params, 
            self.Env, 
            self.alpha, 
            self.epsilon,
            s)
        self.history.append((s, a, r, s_prime))
        self.state = s_prime

    def update_params(self):
        states = []
        actions = []
        targets = []
        
        for s, a, r, s_prime in self.history:
            s, a, target = one_summation_term(
                self.Q, 
                self.params, 
                s, 
                a, 
                r, 
                s_prime, 
                self.alpha, 
                self.gamma)
                
            states.append(s)
            actions.append(a)
            targets.append(target)
            
        grads = self.Q.get_gradient(self.params, states, actions, targets)
        for param, grad in zip(self.params, grads):
            param += 0.1 * grad
            #param += 0

    def reset(self):
        rs = []
        for s, a, r, s_prime in self.history:
            rs.append(r)
        rs = numpy.array(rs)
        print numpy.linalg.norm(s[1]), rs.mean(), len(self.history)
        self.history = []
        
    def initialize_state(self):
        return (numpy.zeros(2), numpy.zeros(2))
        
def test():
    import simple_q2 as simple_q
    Q = simple_q.Q(actions.shape[0])
    
    Env = make_env(0.1, 10., lambda x: False)
    
    runner = Runner(Q, Env, 0.1, 0.9, 0.1)
    
    while 1:
        for i in range(50):
            runner.step()
            
        runner.update_params()
        runner.reset()
        
    #runner.step()
    #runner.step()
    #runner.update_params()

if __name__ == '__main__':
    test()