#! /usr/bin/env python

import abc


def ident(x):
    """
    Identity function - Just returns whatever is passed in.

    'id' is a built-in function, in Python, which doesn't give what we
    want.

    """

    return x


def compose(*functions):
    """
    Function composition for arbitrary number of inputs.

    Taken from:
    https://mathieularose.com/function-composition-in-python/#solution

    """

    return reduce(lambda f, g: lambda x: f(g(x)), functions, lambda x: x)


class Bifunctor(object):
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def bimap(self, g, h):
        pass

class Eq(object):
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def equals(self, other):
        pass

class Pair(Bifunctor, Eq):
    'Bifunctorial Product.'

    def __init__(self, x, y):
        self._x = x
        self._y = y

    def GetX(self):
        return self._x
    def SetX(self, val):
        self._x = val
    x = property(GetX, SetX)

    def GetY(self):
        return self._y
    def SetY(self, val):
        self._y = val
    y = property(GetY, SetY)

    def bimap(self, g, h):
        return Pair(g(self.x), h(self.y))

    def equals(self, other):
        if(self.x == other.x and self.y == other.y):
            return True
        else:
            return False


class K2(Bifunctor, Eq):
    'Bifunctorial constant.'

    def __init__(self, val):
        self._val = val

    def GetVal(self):
        return self._val
    def SetVal(self, val):
        self._val = val
    val = property(GetVal, SetVal)

    def bimap(self, g, h):
        return self

    def equals(self, other):
        if(self.val == other.val):
            return True
        else:
            return False


class Fst(Bifunctor, Eq):
    'First applicative bifunctor.'

    def __init__(self, val):
        self._val = val

    def GetVal(self):
        return self._val
    def SetVal(self, val):
        self._val = val
    val = property(GetVal, SetVal)

    def bimap(self, g, h):
        return Fst(g(self.val))

    def equals(self, other):
        if(self.val == other.val):
            return True
        else:
            return False


class Snd(Bifunctor, Eq):
    'Second applicative bifunctor.'

    def __init__(self, val):
        self._val = val

    def GetVal(self):
        return self._val
    def SetVal(self, val):
        self._val = val
    val = property(GetVal, SetVal)

    def bimap(self, g, h):
        return Snd(h(self.val))

    def equals(self, other):
        if(self.val == other.val):
            return True
        else:
            return False


def g1(x):
    return x + 1

def g2(x):
    return x + 2

def h1(x):
    return x * 2

def h2(x):
    return x * 3

def main():
    assert Pair(1, 2).bimap(ident, ident).equals(ident(Pair(1, 2)))
    assert Pair(1, 2).bimap(g1, h1).bimap(g2, h2).equals(Pair(1, 2).bimap(compose(g1, g2), compose(h1, h2)))
    assert K2(1).bimap(ident, ident).equals(ident(K2(1)))
    assert K2(1).bimap(g1, h1).bimap(g2, h2).equals(K2(1).bimap(compose(g1, g2), compose(h1, h2)))
    assert Fst(1).bimap(ident, ident).equals(ident(Fst(1)))
    assert Fst(1).bimap(g1, h1).bimap(g2, h2).equals(Fst(1).bimap(compose(g1, g2), compose(h1, h2)))
    assert Snd(1).bimap(ident, ident).equals(ident(Snd(1)))
    assert Snd(1).bimap(g1, h1).bimap(g2, h2).equals(Snd(1).bimap(compose(g1, g2), compose(h1, h2)))

    print "All assertions passed."

if(__name__ == '__main__'):
    main()

