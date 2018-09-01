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
        self.node = 'sexp'
        self.stack = []
        self.buffer = None

class SexpParser(object):
    def __init__(self, stream):
        self.stream = stream
        self.state = ParseState()
        self.result = []

    def parse_str(self):
        self.state.node = 'str'
        self.state.buffer = ''

        while True:
            ch = self.stream.peek()
            if ch is None:
                return None

            if ch == '"':
                self.stream.read()
                result = self.state.buffer
                self.state.buffer = None
                return result

            self.state.buffer += self.stream.read()

    def parse_int(self):
        self.state.node = 'int'
        self.state.buffer = ''

        while True:
            ch = self.stream.peek()
            if ch is None or ch in ' \n':
                result = int(self.state.buffer)
                self.state.buffer = None
                self.state.node = 'sexp'
                return result

            self.state.buffer += self.stream.read()

    def parse_sym(self):
        self.state.node = 'sym'
        self.state.buffer = ''

        while True:
            ch = self.stream.peek()
            if ch is None or ch in ' \n':
                result = self.state.buffer
                self.state.buffer = None
                self.state.node = 'sexp'
                return result

            self.state.buffer += self.stream.read()

    def append_result(self, result):
        print('---------------\nres: {}, stack: {}'.format(repr(result), self.state.stack))
        deepest_tail = self.result
        stack_len = len(self.state.stack)
        depth = 1
        while type(deepest_tail) is list and len(deepest_tail) > 0 and type(deepest_tail[-1]) is list:
            deepest_tail = deepest_tail[-1]
            depth += 1
            print('Depth: {}, deepest: {}'.format(depth, repr(deepest_tail)))

        if depth == stack_len:
             print('depth == len(stack)')
             deepest_tail.append(result)

        elif depth < stack_len:
            print('depth < len(stack)')
            for d in range(0, stack_len - depth):
                deepest_tail.append([])
                deepest_tail = deepest_tail[-1]
            else:
                deepest_tail.append(result)

        elif depth > stack_len:
            print('depth > len(stack)')
            target = self.result
            print('self.result = {}'.format(target))
            for d in range(0, depth - stack_len):
                target = target[-1]
            else:
                print('pop and target: {}'.format(target))
                target.append(result)

    def parse(self):
        while True:
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
                self.state.stack = [self.state.node] + self.state.stack
                self.state.node = 'list'
                continue

            elif len(self.state.stack) > 0 and ch == ')':
                self.stream.read()
                self.state.node = self.state.stack[0]
                self.state.stack = self.state.stack[1:]
                continue

            elif ch == '"':
                self.stream.read()
                result = self.parse_str()

            elif ch in '0123456789':
                result = self.parse_int()

            else:
                result = self.parse_sym()

            if result is None:
                return (None, self.state)
            else:
                self.append_result(result)


if __name__ == '__main__':
    s = Stream(input('> '))
    p = SexpParser(s)
    print(p.parse())
