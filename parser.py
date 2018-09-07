'''A parser for S-expressions of SECD Machine code
'''

from io import StringIO

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

class ParseError(Exception):
    pass

class EOFError(ParseError):
    pass

class MachineCodeReader(object):
    def __init__(self, stream):
        self.stream = stream
        self.read_buffer = StringIO()

    def append_input(self, new_str):
        str_read = self.read_buffer.getvalue()
        self.stream = Stream(str_read + new_str)
        self.read_buffer = StringIO(str_read)

    def _read_ch(self):
        ch = self.stream.read()
        self.read_buffer.write(ch)
        return ch

    def _skip_whitespace(self):
        while True:
            ch = self.stream.peek()
            if ch is None:
                return None
            elif ch in ' \n':
                self._read_ch()
            else:
                break

    def skip_comment(self):
        while True:
            ch = self.stream.peek()
            if ch is None or ch == '\n':
                self.stream.read()
                return
            else:
                self.stream.read()

    def read_str(self):
        buf = StringIO()
        while True:
            ch = self.stream.peek()
            if ch is None:
                raise EOFError()
            elif ch == '"':
                self._read_ch()
                return buf.getvalue()
            else:
                buf.write(self._read_ch())

    def read_int(self):
        buf = StringIO()
        ch = self.stream.peek()
        if ch == '-':
            buf.write(self._read_ch())
        while True:
            ch = self.stream.peek()
            if ch is None:
                return int(buf.getvalue())
            elif ch.isdigit():
                buf.write(self._read_ch())
            else:
                return int(buf.getvalue())

    def read_sym(self):
        buf = StringIO()
        while True:
            ch = self.stream.peek()
            if ch is None:
                return buf.getvalue()
            elif ch in '"() \n;':
                return buf.getvalue()
            else:
                buf.write(self._read_ch())

    def read_list(self):
        lis = []
        while True:
            self._skip_whitespace()

            ch = self.stream.peek()
            if ch is None:
                raise EOFError()
            elif ch == ')':
                self._read_ch()
                return lis
            else:
                lis.append(self.read_one())

    def read_one(self):
        self._skip_whitespace()
        ch = self.stream.peek()

        if ch is not None and ch == ';':
            self.skip_comment()
            return self.read_one()
        elif ch is None:
            return None
        elif ch == '(':
            self._read_ch()
            return self.read_list()
        elif ch == '"':
            self._read_ch()
            return self.read_str()
        elif ch == '-' or ch.isdigit():
            return self.read_int()
        else:
            return self.read_sym()
