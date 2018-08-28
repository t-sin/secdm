
def locate(e, n):
    '''Returns element of environment at index `n`'''
    return e[n]

def rplaca(e, v):
    '''Replace specified value of environment??'''
    return None

opcode = {
    'ld': None,
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
    'stop': None,
}

class SECD(object):
    '''SECD machine: state and runner'''

    def __init__(self):
        self.s = []
        self.e = []
        self.c = []
        self.d = None

    def __init__(self, s, e, c):
        self.s = s
        self.e = e
        self.c = c
        self.d = None

    def run():
        pass
