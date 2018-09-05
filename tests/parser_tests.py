import unittest

from parser import *


def parse_str(str):
    stream = Stream(str)
    reader = MachineCodeReader(stream)
    return reader.read_one()

class TestParser(unittest.TestCase):
    def test_null(self):
        self.assertEqual(parse_str(''), None)
        self.assertEqual(parse_str(' '), None)
        self.assertEqual(parse_str('\n'), None)
        self.assertEqual(parse_str('     \n\n'), None)

    def test_string(self):
        self.assertEqual(parse_str('""'), '')
        self.assertEqual(parse_str('" "'), ' ')

        self.assertEqual(parse_str('"abcd"'), 'abcd')
        self.assertEqual(parse_str('"1234"'), '1234')

        self.assertEqual(parse_str('  " "'), ' ')

    def test_integer(self):
        self.assertEqual(parse_str('1'), 1)
        self.assertEqual(parse_str('12'), 12)
        self.assertEqual(parse_str('12345'), 12345)

        self.assertEqual(parse_str('123err'), 123)

    def test_symbol(self):
        self.assertEqual(parse_str('a'), 'a')
        self.assertEqual(parse_str('ab'), 'ab')
        self.assertEqual(parse_str('abcde'), 'abcde')

        self.assertEqual(parse_str('sym1234'), 'sym1234')

    def test_list(self):
        self.assertEqual(parse_str('()'), [])
        self.assertEqual(parse_str('(a)'), ['a'])
        self.assertEqual(parse_str('(abc)'), ['abc'])

        self.assertEqual(parse_str('( a)'), ['a'])
        self.assertEqual(parse_str('(a )'), ['a'])
        self.assertEqual(parse_str('( a )'), ['a'])

        self.assertEqual(parse_str('(a b)'), ['a', 'b'])
        self.assertEqual(parse_str('(a b c)'), ['a', 'b', 'c'])
        self.assertEqual(parse_str('(a b c d e)'), ['a', 'b', 'c', 'd', 'e'])

    def test_parse_one(self):
        self.assertEqual(parse_str('"foo" "bar"'), 'foo')
        self.assertEqual(parse_str('123 abc'), 123)
        self.assertEqual(parse_str('abc def'), 'abc')

        self.assertEqual(parse_str('"foo"123'), 'foo')
        self.assertEqual(parse_str('"foo""bar"'), 'foo')
        self.assertEqual(parse_str('"foo"bar'), 'foo')

    def test_complex_list(self):
        self.assertEqual(parse_str('((a) b)'), [['a'], 'b'])
        self.assertEqual(parse_str('(a (b))'), ['a', ['b']])
        self.assertEqual(parse_str('((a b))'), [['a', 'b']])

        self.assertEqual(parse_str('(a (b c))'), ['a', ['b', 'c']])
        self.assertEqual(parse_str('(a (b) c)'), ['a', ['b'], 'c'])
        self.assertEqual(parse_str('((a b) c)'), [['a', 'b'], 'c'])

        self.assertEqual(parse_str('(a b (c d (e f)))'),
                         ['a', 'b', ['c', 'd', ['e', 'f']]])
        self.assertEqual(parse_str('(a b (c d (e) f))'),
                         ['a', 'b', ['c', 'd', ['e'], 'f']])
        self.assertEqual(parse_str('(a b (c d (e f) g))'),
                         ['a', 'b', ['c', 'd', ['e', 'f'], 'g']])
