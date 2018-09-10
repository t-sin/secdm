'''SECD machine object'''


class Symbol(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return '{}'.format(self.name.upper())

class Cons(object):
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def __repr__(self):
        return '({} . {})'.format(self.car, self.cdr)

class Func(object):
    def __init__(self, code, env):
        self.code = code
        self.env = env

    def __repr__(self):
        return 'fn<[...], {}>'.format(self.env)
