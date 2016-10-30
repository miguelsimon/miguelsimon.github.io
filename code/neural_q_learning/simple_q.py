import numpy

import theano
import theano.tensor as T

#State space:
    #(p, v)
    
def build(num_actions):
    W = T.matrix('W')
    b = T.vector('b')
    
    ps = T.matrix('ps')
    vs = T.matrix('vs')
    
    cs = T.concatenate([ps, vs], axis = 1).transpose()

    input_actions = T.ivector('actions')
    target_qs = T.vector('target_qs')
    
    actions = W.dot(cs).transpose() + b
    one_hot_actions = T.extra_ops.to_one_hot(input_actions, b.shape[0])
    
    qs = (actions * one_hot_actions).sum(1)
    costs = (qs - target_qs) ** 2
    cost = costs.sum()
    
    params = [W, b]
    gparams = [T.grad(cost, param) for param in params]
    
    get_gradient = theano.function(
        inputs=[W, b, ps, vs, input_actions, target_qs],
        outputs = gparams)
        
    get_action_values = theano.function(
         inputs=[W, b, ps, vs],
         outputs = actions)
   
    return get_gradient, get_action_values
    
def initialize_params(num_actions):
    W = numpy.random.normal(size = (num_actions, 4))
    b = numpy.zeros(num_actions)
    return (W, b)
    
class Q(object):
    def __init__(self, num_actions):
        self.num_actions = num_actions
        self._get_gradient, self._get_q_values = build(self.num_actions)
        
    def get_q_values(self, params, s):
        W, b = params
        p, v = s
        
        return self._get_q_values(W, b, numpy.array([p]), numpy.array([v]))[0]
        
    def get_gradient(self, params, states, actions, targets):
        W, b = params
        ps = []
        vs = []
        for p, v in states:
            ps.append(p)
            vs.append(v)
        #print targets
 
        return self._get_gradient(W, b, numpy.array(ps), numpy.array(vs), actions, targets)
        
    def initialize_params(self):
        return initialize_params(self.num_actions)
    
def test():
    params = initialize_params(9)
    
    W, b = params
    
    ps = numpy.ones((3, 2))
    vs = numpy.zeros((3, 2))
    
    get_gradient, get_action_values = build(9)
    print get_action_values(W, b, ps, vs)
    
    input_actions = numpy.array([0, 1, 0], dtype='int32')
    target_qs = numpy.array([1., 1., 1.])
    print get_gradient(W, b, ps, vs, input_actions, target_qs)
    

if __name__ == '__main__':
    test()
        