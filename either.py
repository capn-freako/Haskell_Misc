#! /usr/bin/env python

class Either(object):
    def __init__(self, constructor, value):
        if(constructor == 'Left'):
            self._constructor = 'Left'
        elif(constructor == 'Right'):
            self._constructor = 'Right'
        else:
            raise RuntimeError('Unrecognized constructor: {}'.format(constructor))
        self._value = value

    def __str__(self):
        if(self._constructor == 'Left'):
            res = "Left "
        else:
            res = "Right "
        res += str(self._value)
        return res

    def _get_constructor(self):
        return _constructor
    constructor = property(_get_constructor)

    def _get_value(self):
        return _value
    value = property(_get_value)

def left(x):
    return Either('Left', x)

def right(x):
    return Either('Right', x)

def main():
    myLeft = left('Testing')
    print "left('Testing'): {}".format(myLeft)
    print "right(1): {}".format(right(1))
    print "Either('Middle', 2): {}".format(Either('Middle', 2))

if(__name__ == "__main__"):
    main()

