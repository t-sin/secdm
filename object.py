'''SECD machine object'''

class Object(object):
    def __repr__(self):
        return '<OBJECT>'

class Nil(Object):
    def __repr__(self):
        return 'NIL'

class Symbol(Object):
    def __init__(self, name):
        self.name = name

    def __eq__(self, obj):
        if type(obj) is Symbol:
            return obj.name == self.name
        else:
            return False

    def __repr__(self):
        return '{}'.format(self.name.upper())

class String(Object):
    def __init__(self, str):
        self.str = str

    def __eq__(self, obj):
        if type(obj) is String:
            return obj.str == self.str
        else:
            return False

    def __repr__(self):
        return '{}'.format(self.str)

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
