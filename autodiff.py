import uuid
import json
import math 
from collections import Counter
import random

class Number:
    def __init__(self, value=None, fun=None):
        self.fun = fun 
        self.value = value 
        self.uuid = str(uuid.uuid4())
        self.fungrads = None
        self.derivative = 1 
        self.parents = [] 

    def forward(self, values):
        self.value, self.fungrads = self.fun(values)
        for i in list(set(values)):
            i.parents.append(self)

    def backward(self):
        self.derivative = 0
        for i in self.parents:
            self.derivative += i.fungrads[self.uuid] * i.derivative 
    
    def __repr__(self):
        return json.dumps({"uuid": str(self.uuid), 
                        "value": self.value, 
                        "fungrads": self.fungrads,
                        "derivative": self.derivative,
                        "parents": [str(i.uuid) for i in self.parents]}, indent=2)
    

class Tape:

    def __init__(self):
        self.tape = []
    
    def node_add(self, values):
        mysum = sum([i.value for i in values])
        mygrads = dict(Counter([i.uuid for i in values]))
        return mysum, mygrads

    def node_multiply(self, values):
        myproduct = 1
        for i in values:
            myproduct *= i.value
        mygrads = {}

        uuids = {i.uuid: i for i in values}
        powers = dict(Counter([i.uuid for i in values]))
        
        for i in powers.keys():
            that_grad = 1
            for j in powers.keys():
                if i != j:
                    that_grad *= (uuids[j].value**(powers[j]))
                else:
                    that_grad *= (powers[j] * (uuids[j].value**(powers[j]-1)))
            mygrads[i] = that_grad
        return myproduct, mygrads


    def node_sigmoid(self, values):
        value = values[0]
        result = 1 / (1 + math.exp(-value.value))
        return result, {
            value.uuid: result * (1 - result)
        }


    def add(self, nodes):
        self.tape += list(set(nodes))
        newnode = Number(fun=self.node_add)
        newnode.forward(nodes)
        return newnode

    def multiply(self, nodes):
        self.tape += list(set(nodes)) 
        newnode = Number(fun=self.node_multiply)
        newnode.forward(nodes)
        return newnode

    def sigmoid(self, nodes):
        self.tape += list(set(nodes))
        newnode = Number(fun=self.node_sigmoid)
        newnode.forward(nodes)
        return newnode

    def grad(self, nodes):
        for i in self.tape[::-1]:
            i.backward()
        self.tape = []
        return [i.derivative for i in nodes]


tape = Tape()
three = Number(3)
five = Number(5)
result = tape.add([three, five])
print(result)
print(tape.tape)
print(tape.grad([three, five]))