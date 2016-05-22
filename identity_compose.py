#! /usr/bin/env python

def identity(x):
    return x

def compose(g, h):
    return lambda x: g(h(x))

def main():
    def f(x):
        return x + 2

    g = compose(f, identity)
    h = compose(identity, f)

    for x in range(3):
        assert g(x) == f(x)
        assert h(x) == f(x)

    print "All tests passed."

if(__name__ == "__main__"):
    main()

