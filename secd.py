#!/usr/bin/ppython

def rplaca(e, v):
    '''Replace specified value of environment??'''
    return None


class Machine(object):
    '''SECD machine: state and runner'''

    def __init__(self):
        self.s = []
        self.e = []
        self.c = []
        self.d = None

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

        s, e, c, d = opcode[op[0]](self, *op[1:])
        self.s = s
        self.e = e
        self.c = c
        self.d = d

    def run(self):
        while not self._stop_:
            if len(self.c) == 0:
                print('Machine stopped by empty code.')
                print(self)
                return

            self.step()

        print('Machine stopped.')
        print(self)


# instructions
#
# they take few args; secd machine and others...
opcode = {
    'ld': lambda m, n: ([e.n] + m.s, m.e, m.c[1:], m.d),
    'ldc': None,
    'ldf': None,
    'ap': None,
    'rtn': None,
    'dum': None,
    'rap': None,
    'sel': None,
    'join': None,
    'car': None,
    'cdr': None,
    'atom': None,
    'cons': None,
    'eq': None,
    'add': None,
    'sub': None,
    'mul': None,
    'div': None,
    'rem': None,
    'leq': None,
    'stop': lambda m: m._stop() or (m.s, m.e, m.c, m.d),
}


if __name__ == '__main__':
    code = [['stop']]
    m = Machine()
    m.c = code
    m._debug_ = True

    m.run()
