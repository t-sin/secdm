import sys

from parser import Stream, MachineCodeReader
from vm import Machine

usage = '''SECD machine toys

usage: secdm.py [-hd] [PROGRAM]
'''

def secdm_repl(debug=False):
    vm = Machine()
    vm._debug_ = debug
    while True:
        code_list = []
        line = input('>> ')
        stream = Stream(line)
        reader = MachineCodeReader(stream)

        while True:
            code = reader.read_one()
            if code is None:
                break
            code_list.append(code)

        vm.c = code_list
        vm.run()

def secdm_run(filename):
    with open(filename) as f:
        stream = Stream(f.read(None))

    vm = Machine()
    vm._debug_ = debug
    reader = MachineCodeReader(stream)
    code_list = []

    while True:
        code = reader.read_one()
        if code is None:
            break
        code_list.append(code)

    vm.c = code_list
    vm.run()

if __name__ == '__main__':
    debug = '--debug' in sys.argv or '-d' in sys.argv
    help = '--help' in sys.argv or '-h' in sys.argv
    _argv = [arg for arg in sys.argv[1:]
             if not (arg == '--debug' or arg == '-d' or arg == '--help' or arg == '-h')]

    if help:
        print(usage)

    elif len(_argv) > 0:
        secdm_run(_argv[0])

    else:
        secdm_repl(debug=debug)
