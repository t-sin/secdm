'''SECD machine object'''

class Object(object):
    pass

class Nil(Object):
    pass

class Symbol(Object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return '{}'.format(self.name.upper())

class Cons(Object):
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def __repr__(self):
        return '({} . {})'.format(self.car, self.cdr)

class Func(Object):
    def __init__(self, code, env):
        self.code = code
        self.env = env

    def __repr__(self):
        return 'fn<[...], {}>'.format(self.env)
