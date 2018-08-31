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

class SexpParser(object):
    def __init__(self, stream):
        self.result = None
        self.stream = stream

    def parse(self):

        while True:
            ch = self.stream.peek()
            result = None

            while ch == ' ':
                self.stream.read()
                ch = self.stream.peek()

            if ch is None:
                return (self.result, self.stream)

            elif ch == '"':
                self.stream.read()
                ch = self.stream.peek()
                _str_result = ''

                while ch is not None and ch != '"':
                    _str_result += self.stream.read()
                    ch = self.stream.peek()

                result = _str_result

            elif ch in '0123456789':
                _int_result = ''
                ch = self.stream.peek()

                while ch is not None and ch in '0123456789':
                    _int_result += self.stream.read()
                    ch = self.stream.peek()

                result = None if len(_int_result) == 0 else int(_int_result)

            elif ch == '(':
                self.stream.read()
                ch = self.stream.peek()
                while ch != ')':
                    pass

            else:
                ch = self.stream.peek()
                print(ch)
                _sym_result = ''

                while ch is not None and ch != ' ':
                    _sym_result += self.stream.read()
                    ch = self.stream.peek()

                result = _sym_result


            if self.result is None:
                self.result = result

            elif type(self.result) is list:
                self.result.append(result)

            else:
                return (self.result, self.stream)


if __name__ == '__main__':
    s = Stream('   abcd')
    p = SexpParser(s)
    print(p.parse())
    print(s.buffer[s.head:])
