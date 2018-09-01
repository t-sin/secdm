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

class ParseState(object):
    def __init__(self):
        self.node = 'atom_start'
        self.stack = []
        self.buffer = None

class SexpParser(object):
    def __init__(self, stream):
        self.stream = stream
        self.state = ParseState()
        self.result = []

    def parse_str(self):
        self.state.node = 'str_start'
        self.state.buffer = ''

        while True:
            ch = self.stream.peek()
            if ch is None:
                return None

            if ch == ')':
                if self.state.stack != []:
                    result = self.state.buffer
                    self.state.buffer = None
                    self.state.node = 'list_start'
                    return result
                else:
                    raise Exception('Illegal ")"')

            if ch == '"':
                self.stream.read()
                result = self.state.buffer
                self.state.buffer = None
                self.state.node = 'atom_start'
                return result

            self.state.buffer += self.stream.read()

    def parse_int(self):
        self.state.node = 'int_start'
        self.state.buffer = ''

        while True:
            ch = self.stream.peek()
            if ch is None or ch in ' \n':
                result = int(self.state.buffer)
                self.state.buffer = None
                self.state.node = 'atom_start'
                return result

            self.state.buffer += self.stream.read()

    def parse_sym(self):
        self.state.node = 'sym_start'
        self.state.buffer = ''

        while True:
            ch = self.stream.peek()
            if ch is None or ch in ' \n':
                result = self.state.buffer
                self.state.buffer = None
                self.state.node = 'atom_start'
                return result

            self.state.buffer += self.stream.read()

    def append_result(self, result):
        stk = self.result
        depth = 1
        while len(stk) > 0 and type(stk[-1]) is list:
            print('Depth: {}, stack: {}'.format(depth, repr(stk)))
            stk = stk[-1]
            depth += 1

        print('after stack: {}'.format(repr(stk)))
        if depth == len(self.state.stack):
             stk.append(result)
        elif depth < len(self.state.stack):
             for d in range(0, len(self.state.stack) - depth):
                 stk.append([])
                 stk = stk[-1]
             else:
                 stk.append(result)

    def parse(self):
        while True:
            print(self.state.stack)
            ch = self.stream.peek()
            result = None

            # skips whitespaces
            while ch is not None and ch in ' \n':
                self.stream.read()
                ch = self.stream.peek()

            # end of stream
            if ch is None:
                if self.state.stack == []:
                    self.state.node = 'end'
                return (self.result, self.state)
            elif ch == '(':
                self.stream.read()
                self.state.node = 'list_start'
                self.state.stack = ['list'] + self.state.stack
                continue
            elif ch == ')':
                self.stream.read()
                self.state.node = 'atom_start'
                self.state.stack = self.state.stack[1:]
            elif ch == '"':
                self.stream.read()
                result = self.parse_str()
            elif ch in '0123456789':
                result = self.parse_int()
            else:
                result = self.parse_sym()


            print(repr(result))
            if result is None:
                return (None, self.state)
            else:
                self.append_result(result)


if __name__ == '__main__':
    s = Stream(input('> '))
    p = SexpParser(s)
    print(p.parse())
