import unittest

from parser import *


def parse_str(str):
    stream = Stream(str)
    parser = MachineCodeParser()
    return parser.parse_one(stream)

class TestParser(unittest.TestCase):
    def test_null(self):
        self.assertEqual(parse_str(''), (True, None))
        self.assertEqual(parse_str(' '), (True, None))
        self.assertEqual(parse_str('\n'), (True, None))
        self.assertEqual(parse_str('     \n\n'), (True, None))

    def test_string(self):
        self.assertEqual(parse_str('""'), (True, ''))
        self.assertEqual(parse_str('" "'), (True, ' '))

        self.assertEqual(parse_str('"abcd"'), (True, 'abcd'))
        self.assertEqual(parse_str('"1234"'), (True, '1234'))

        self.assertEqual(parse_str('  " "'), (True, ' '))

    def test_integer(self):
        self.assertEqual(parse_str('1'), (True, 1))
        self.assertEqual(parse_str('12'), (True, 12))
        self.assertEqual(parse_str('12345'), (True, 12345))

        self.assertEqual(parse_str('123err'), (False, None))

    def test_symbol(self):
        self.assertEqual(parse_str('a'), (True, 'a'))
        self.assertEqual(parse_str('ab'), (True, 'ab'))
        self.assertEqual(parse_str('abcde'), (True, 'abcde'))

        self.assertEqual(parse_str('err1234'), (False, None))

    def test_list(self):
        self.assertEqual(parse_str('()'), (True, []))
        self.assertEqual(parse_str('(a)'), (True, ['a']))
        self.assertEqual(parse_str('(abc)'), (True, ['abc']))

        self.assertEqual(parse_str('( a)'), (True, ['a']))
        self.assertEqual(parse_str('(a )'), (True, ['a']))
        self.assertEqual(parse_str('( a )'), (True, ['a']))

        self.assertEqual(parse_str('(a b)'), (True, ['a', 'b']))
        self.assertEqual(parse_str('(a b c)'), (True, ['a', 'b', 'c']))
        self.assertEqual(parse_str('(a b c d e)'), (True, ['a', 'b', 'c', 'd', 'e']))

    def test_parse_one(self):
        self.assertEqual(parse_str('"foo" "bar"'), (True, 'foo'))
        self.assertEqual(parse_str('123 abc'), (True, 123))
        self.assertEqual(parse_str('abc def'), (True, 'abc'))

        self.assertEqual(parse_str('"foo"123'), (True, 'foo'))
        self.assertEqual(parse_str('"foo""bar"'), (True, 'foo'))
        self.assertEqual(parse_str('"foo"bar'), (True, 'foo'))

    def test_complex_list(self):
        self.assertEqual(parse_str('((a) b)'), (True, [['a'], 'b']))
        self.assertEqual(parse_str('(a (b))'), (True, ['a', ['b']]))
        self.assertEqual(parse_str('((a b))'), (True, [['a', 'b']]))

        self.assertEqual(parse_str('(a (b c))'), (True, ['a', ['b', 'c']]))
        self.assertEqual(parse_str('(a (b) c)'), (True, ['a', ['b'], 'c']))
        self.assertEqual(parse_str('((a b) c)'), (True, [['a', 'b'], 'c']))

        self.assertEqual(parse_str('(a b (c d (e f)))'),
                        (True, ['a', 'b', ['c', 'd', ['e', 'f']]]))
        self.assertEqual(parse_str('(a b (c d (e) f))'),
                        (True, ['a', 'b', ['c', 'd', ['e'], 'f']]))
        self.assertEqual(parse_str('(a b (c d (e f) g))'),
                        (True, ['a', 'b', ['c', 'd', ['e', 'f'], 'g']]))
