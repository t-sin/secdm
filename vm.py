#!/usr/bin/ppython

from object import *


def rplaca(e, v):
    '''Replace environment stack head with specified value'''
    if e[0] == 'omega':
        e[0] = v
    else:
        raise Exception('e stack top is not "omega"')
    return e

# instructions
#
# they take few args; secd machine and others...
OPCODE = {
    # original instruction
    'ld': lambda m, n: ([m.e[n]] + m.s, m.e, m.c[1:], m.d),
    'ldc': lambda m, v: ([v] + m.s, m.e, m.c[1:], m.d),
    'ldf': lambda m, c: ([Func(m.c[0][1], m.e)] + m.s, m.e, m.c[1:], m.d),
    'ap': lambda m: ([], [m.s[1]] + m.e, m.s[0].code, [(m.s[2:], m.e, m.c[1:])] + m.d),
    'rtn': lambda m: ([m.s[0] if len(m.s) > 0 else []] + m.d[0][0], m.d[0][1], m.d[0][2], m.d[1:] if len(m.d) > 0 else []),
    'dum': lambda m: (m.s, ['omega'] + m.e, m.c[1:], m.d),
    'rap': lambda m: ([], rplaca(m.s[0].env, m.s[1]), m.s[0].code, [(m.s[2:], m.e, m.c[1:])] + m.d),
    'sel': lambda m, ct, cf: (m.s[1:], m.e, ct if m.s[0] else cf, [m.c[1:]] + m.d),
    'join': lambda m: (m.s, m.e, m.d[0], m.d[1:]),
    'car': lambda m: ([m.s[0].car] + m.s[1:], m.e, m.c[1:], m.d),
    'cdr': lambda m: ([m.s[0].cdr] + m.s[1:], m.e, m.c[1:], m.d),
    'atom': lambda m: ([type(m.s[0]) is Cons] + m.s[1:], m.e, m.c[1:], m.d),
    'cons': lambda m: ([Cons(m.s[0], m.s[1])] + m.s[2:], m.e, m.c[1:], m.d),
    'eq': lambda m: ([(m.s[0] == m.s[1])] + m.s[2:], m.e, m.c[1:], m.d),
    'add': lambda m: ([(m.s[0] + m.s[1])] + m.s[2:], m.e, m.c[1:], m.d),
    'sub': lambda m: ([(m.s[0] - m.s[1])] + m.s[2:], m.e, m.c[1:], m.d),
    'mul': lambda m: ([(m.s[0] * m.s[1])] + m.s[2:], m.e, m.c[1:], m.d),
    'div': lambda m: ([(m.s[0] / m.s[1])] + m.s[2:], m.e, m.c[1:], m.d),
    'rem': lambda m: ([(m.s[0] % m.s[1])] + m.s[2:], m.e, m.c[1:], m.d),
    'leq': lambda m: ([(m.s[0] <= m.s[1])] + m.s[2:], m.e, m.c[1:], m.d),
    'stop': lambda m: m._stop() or (m.s, m.e, m.c, m.d),

    # some my extention instruction
    'nil': lambda m: ([m.nil] + m.s[1:], m.e, m.c[1:], m.d),
    ## I/O
    'input': lambda m: ([String(input(''))] + m.s, m.e, m.c[1:], m.d),
    'print': lambda m: print(m.s[0], end='') or (m.s[1:], m.e, m.c[1:], m.d),
    'println': lambda m: print(m.s[0]) or (m.s[1:], m.e, m.c[1:], m.d),
    'toi': lambda m: ([int(m.s[0].str)] + m.s[1:], m.e, m.c[1:], m.d),
    'str': lambda m: ([str(m.s[0])] + m.s[1:], m.e, m.c[1:], m.d),
}


class Machine(object):
    '''SECD machine: state and runner'''

    def __init__(self):
        self.s = []
        self.e = []
        self.c = []
        self.d = []

        self.nil = Nil()

        self._stop_ = False
        self._debug_ = False

    def __getattr__(self, name):
        if name == 'st':
            return (self.s, self.e, self.c, self.d)
        else:
            return self.__getattribute__(name)

    def __repr__(self):
        return '''machine:
  s = {}
  e = {}
  c = {}
  d = {}
'''.format(*self.st)

    def _stop(self):
        self._stop_ = True

    def step(self):
        op = self.c[0]

        if self._debug_ is True:
            print(self)
            print('  OP: {}'.format(op))

        s, e, c, d = OPCODE[op[0].name](self, *op[1:])
        self.s = s
        self.e = e
        self.c = c
        self.d = d

    def run(self):
        while not self._stop_:
            if len(self.c) == 0:
                if self._debug_:
                    print('Machine stopped by empty code.')
                    print(self)
                return

            self.step()

        if self._debug_:
            print('Machine stopped.')
            print(self)
