#! /usr/bin/env python

import time
from random import random
from memoize import Memoized

def main():
    rand_m = Memoized(random)

    t0 = time.time()
    res1 = rand_m.call()
    t1 = time.time()
    res2 = rand_m.call()
    t2 = time.time()

    print "First result: {} ; Second result: {}".format(res1, res2)
    print "First call took {} seconds.".format(t1 - t0)
    print "Second call took {} seconds.".format(t2 - t1)
    print "Keys of memo: {}".format(rand_m.memo.keys())

if(__name__ == "__main__"):
    main()

