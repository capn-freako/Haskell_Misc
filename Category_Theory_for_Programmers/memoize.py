#! /usr/bin/env python

from operator import mul
import time

class Memoized(object):
    def __init__(self, f):
        self.f = f
        self.memo = {}

    def call(self, *args):
        if(args in self.memo):
            return self.memo[args]
        else:
            res = self.f(*args)
            self.memo[args] = res
            return res

def fact(x):
    return reduce(mul, range(1, x + 1), 1)

def main():
    fact_m = Memoized(fact)
    arg = 10000

    t0 = time.time()
    fact_m.call(arg)
    t1 = time.time()
    res = fact_m.call(arg)
    t2 = time.time()

    print "{}! = {}".format(arg, res)
    print "First call took {} seconds.".format(t1 - t0)
    print "Second call took {} seconds.".format(t2 - t1)
    print "Keys of memo: {}".format(fact_m.memo.keys())

if(__name__ == "__main__"):
    main()

