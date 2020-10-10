# secdm

SECD machine toys.

*SECD machine* is a abscract machine to implement pure functional programming languages.
It is introduced for the purpose of mechanically evaluating expressions of Lambda Calculus.

It has four registers, they point each stacks;

- Stack: data stack
- Environment: values bound by function applying
- Code: code stack
- Dump: stack to store temporary data like other stacks

## Versions

This is second version of *secdm*.
At first time, I wrote this with Python3 but I cannot understood it, especially D register.
I heard that D register is a continuation.

In second version, I set those to goal:

- Understanding continuations and D register
- Introducing side effects like `setq`
- Introducing dynamic scope
- Compiling from small Lisp onto SECD machine

Here is a list of all version of *secdm*:

- [first version (written in Python)](https://github.com/t-sin/secdm/releases/tag/python-implementation)
- [second version (written in Common Lisp)](https://github.com/t-sin/secdm/)

## References

I references this paper:

- Miloˇs Radovanovi, Mirjana Ivanovi, "AN IMPLEMENTATION OF LISPKIT LISP IN JAVA"
  https://perun.pmf.uns.ac.rs/radovanovic/publications/2002-prim-lisp.pdf

## Author

- TANAKA Shinichi (<shinichi.tanaka45@gmail.com>)

## License

This program *secdm* is licensed under the GNU General Public License Version 3. See [COPYING](COPYING) for details.
