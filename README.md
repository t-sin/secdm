# secdm

SECD machine toys.

## How to run

Clone it, 

```sh
$ git clone https://github.com/t-sin/secdm.git
$ cd secdm
```

and run!

```
$ python3 secdm.py --help
SECD machine toys

USAGE: secdm.py [-hd] [PROGRAM]

secdm.py is a machine code interpreter for Landin's SECD Machine. SECD Machine has
only 21 instructions with some my extension about I/O.

Running secdm.py without PROGRAM, it enters REPL mode.

OPTIONS:
        -d --debug   Print state of virtual machine on each steps.
        -h --help    Print this message.

# with interpreter
$ python3 secdm.py

# from source code
$ python3 secdm.py program/test.s
```

## References

I references this paper:

- Miloˇs Radovanovi, Mirjana Ivanovi, "AN IMPLEMENTATION OF LISPKIT LISP IN JAVA"
  https://perun.pmf.uns.ac.rs/radovanovic/publications/2002-prim-lisp.pdf

## Author

- TANAKA Shinichi (<shinichi.tanaka45@gmail.com>)

## License

This program *secdm* is licensed under the GNU General Public License Version 3. See [COPYING](COPYING) for details.
