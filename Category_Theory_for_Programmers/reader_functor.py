#! /usr/bin/env python

def fmap(g, f):
    return lambda x: g(f(x))

def f(x):
    return x + 1

def g(x):
    return x * 2

def main():
    h = fmap(g, f)
    res = h(3)
    print res

if(__name__ == '__main__'):
    main()

