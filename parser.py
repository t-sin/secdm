'''A parser for S-expressions of SECD Machine code
'''

class Stream(object):
    def __init__(self, s):
        self.buffer = s
        self.head = 0

    def peek(self):
        if self.head >= len(self.buffer):
            return None
        else:
            return self.buffer[self.head]

    def read(self):
        if self.head >= len(self.buffer):
            return None
        else:
            ch = self.peek()
            self.head += 1
            return ch


def transition_sexp(s, ch):
    if ch is None:
        return 'end'
    elif ch in ' \n':
        return 'sexp'
    elif ch == '(':
        s.stack = [[]] + s.stack
        return 'list'
    elif ch == '"':
        s.tmps = ''
        return 'str'
    elif ch.isdigit():
        s.tmps = ch
        return 'int'
    else:
        s.tmps = ch
        return 'sym'

def transition_str(s, ch):
    if ch is None:
        return None
    elif ch != '"':
        s.tmps += ch
        return 'str'
    else:
        if s.stack == []:
            s.ast = s.tmps
            return 'sexp'
        else:
            s.stack[0].append(s.tmps)
            return 'list'

def transition_int(s, ch):
    if ch is None:
        if s.stack == []:
            s.ast = s.tmps
            return 'sexp'
        else:
            return None
    elif ch.isdigit():
        s.tmps += ch
        return 'int'
    else:
        if s.stack == []:
            s.ast = s.tmps
            return 'sexp'
        else:
            s.stack[0].append(s.tmps)
            return 'list'

def transition_sym(s, ch):
    if ch is None:
        if s.stack == []:
            s.ast = s.tmps
            return 'sexp'
        else:
            return None
    elif ch.isalpha():
        s.tmps += ch
        return 'sym'
    else:
        if s.stack == []:
            s.ast = s.tmps
            return 'sexp'
        else:
            s.stack[0].append(s.tmps)
            return 'list'

def transition_list(s, ch):
    if ch is None:
        s.ast.append(s.stack[0])
        return None
    elif ch in ' \n':
        return 'list'
    elif ch == '(':
        s.stack = [[]] + s.stack
        return 'list'
    elif ch == ')':
        s.ast.append(s.stack[0])
        s.stack = s.stack[1:]
        return 'sexp'
    elif ch == '"':
        s.tmps = ''
        return 'str'
    elif ch.isdigit():
        s.tmps = ch
        return 'int'
    else:
        s.tmps = ch
        return 'sym'

automaton = {
    'sexp': (transition_sexp, ['end', 'sexp', 'str', 'int', 'sym', 'list']),
    'str': (transition_str, ['str', 'sexp', 'list']),
    'int': (transition_int, ['int', 'sexp', 'list']),
    'sym': (transition_sym, ['sym', 'sexp', 'list']),
    'list': (transition_list, ['sexp', 'str', 'int', 'sym', 'list']),
    'end': (None, None),
}

class MachineCodeParser(object):
    def __init__(self):
        self.node = 'sexp'
        self.stack = []
        self.tmps = ''
        self.ast = None

    def __repr__(self):
        return '(node, stack) = ({}, {})'.format(self.node, self.stack)

    def set_state(self, node, stack):
        self.node = node
        self.stack = stack

    def parse_one(self, stream):
        while True:
            ch = stream.read()
            print('ch: {}, {}'.format(repr(ch), repr(self)))
            print('ast: {}'.format(repr(self.ast)))
            edge = automaton[self.node]
            self.node = edge[0](self, ch)

            if self.node == 'end':
                print('accepted!')
                return self.ast
            elif self.node == None:
                print('rejected...')
                return self.ast

if __name__ == '__main__':
    s = Stream(input('> '))
    p = MachineCodeParser()
    p.parse_one(s)
