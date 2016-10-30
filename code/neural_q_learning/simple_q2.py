import numpy

import theano
import theano.tensor as T

#State space:
    #(p, v)
    
def build(num_actions):
    W1 = T.matrix('W1')
    b1 = T.vector('b1')
    
    W2 = T.matrix('W2')
    b2 = T.vector('b2')
    
    ps = T.matrix('ps')
    vs = T.matrix('vs')
    
    cs = T.concatenate([ps, vs], axis = 1).transpose()

    input_actions = T.ivector('actions')
    target_qs = T.vector('target_qs')
    
    hidden = W1.dot(cs).transpose() + b1
    actions = W2.dot(hidden.transpose()).transpose() + b2
    one_hot_actions = T.extra_ops.to_one_hot(input_actions, b2.shape[0])
    
    qs = (actions * one_hot_actions).sum(1)
    costs = (qs - target_qs) ** 2
    cost = costs.sum()
    
    params = [W1, b1, W2, b2]
    gparams = [T.grad(cost, param) for param in params]
    
    get_gradient = theano.function(
        inputs=[W1, b1, W2, b2, ps, vs, input_actions, target_qs],
        outputs = gparams)
        
    get_action_values = theano.function(
         inputs=[W1, b1, W2, b2, ps, vs],
         outputs = actions)
   
    return get_gradient, get_action_values
    
def initialize_params(num_actions, num_hidden):
    W1 = numpy.random.normal(size = (num_hidden, 4))
    b1 = numpy.zeros(num_hidden)
    
    W2 = numpy.random.normal(size = (num_actions, num_hidden))
    b2 = numpy.zeros(num_actions)
    return (W1, b1, W2, b2)
    
class Q(object):
    def __init__(self, num_actions):
        self.num_actions = num_actions
        self.num_hidden = 128
        self._get_gradient, self._get_q_values = build(self.num_actions)
        
    def get_q_values(self, params, s):
        W1, b1, W2, b2 = params
        p, v = s
        
        return self._get_q_values(W1, b1, W2, b2, numpy.array([p]), numpy.array([v]))[0]
        
    def get_gradient(self, params, states, actions, targets):
        W1, b1, W2, b2 = params
        ps = []
        vs = []
        for p, v in states:
            ps.append(p)
            vs.append(v)
        #print targets
 
        return self._get_gradient(W1, b1, W2, b2, numpy.array(ps), numpy.array(vs), actions, targets)
        
    def initialize_params(self):
        return initialize_params(self.num_actions, self.num_hidden)
    
def test():
    params = initialize_params(9, 256)
    
    W1, b1, W2, b2 = params
    
    ps = numpy.ones((3, 2))
    vs = numpy.zeros((3, 2))
    
    get_gradient, get_action_values = build(9)
    print get_action_values(W1, b1, W2, b2, ps, vs)
    
    input_actions = numpy.array([0, 1, 0], dtype='int32')
    target_qs = numpy.array([1., 1., 1.])
    print get_gradient(W1, b1, W2, b2, ps, vs, input_actions, target_qs)
    

if __name__ == '__main__':
    test()
        