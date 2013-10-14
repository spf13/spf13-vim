# Prelude Python Quickstart

## Python Mode

Emacs comes with Python programming support through the built-in
Python-mode. Whenever you are editing Python code run `C-h m` to
look at the Python mode key bindings. Alternatively look at the
menu bar entries under Python. To toggle the menu bar press `F12`.

## Syntax checking

Prelude ships with [Flycheck](https://github.com/flycheck/flycheck),
an on the fly syntax checker. Flycheck has support for two Python
syntax checkers, [Pylint](http://www.pylint.org/) and
[Flake8](http://flake8.readthedocs.org/en/latest/). In
order to have Flycheck support on the fly syntax checking for
Python you need to have either of these installed and accessible to
Emacs. In order to manually choose a checker run `C-c ! s`.
