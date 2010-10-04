
"""
Implementation of the command-line I{pyflakes} tool.
"""

import _ast
import sys
import os

checker = __import__('pyflakes.checker').checker

def check(codeString, filename):
    try:
        tree = compile(codeString, filename, 'exec', _ast.PyCF_ONLY_AST)
    except (SyntaxError, IndentationError):
        value = sys.exc_info()[1]
        try:
            (lineno, offset, line) = value[1][1:]
        except IndexError:
            print >> sys.stderr, 'could not compile %r' % (filename,)
            return 1
        if line.endswith("\n"):
            line = line[:-1]
        print >> sys.stderr, '%s:%d: could not compile' % (filename, lineno)
        print >> sys.stderr, line
        print >> sys.stderr, " " * (offset-2), "^"
        return 1
    else:
        w = checker.Checker(tree, filename)
        w.messages.sort(lambda a, b: cmp(a.lineno, b.lineno))
        for warning in w.messages:
            print warning
        return len(w.messages)


def checkPath(filename):
    """
    Check the given path, printing out any warnings detected.

    @return: the number of warnings printed
    """
    if os.path.exists(filename):
        return check(file(filename, 'U').read() + '\n', filename)
    else:
        print >> sys.stderr, '%s: no such file' % (filename,)
        return 1

def main():
    warnings = 0
    args = sys.argv[1:]
    if args:
        for arg in args:
            if os.path.isdir(arg):
                for dirpath, dirnames, filenames in os.walk(arg):
                    for filename in filenames:
                        if filename.endswith('.py'):
                            warnings += checkPath(os.path.join(dirpath, filename))
            else:
                warnings += checkPath(arg)
    else:
        warnings += check(sys.stdin.read(), '<stdin>')

    raise SystemExit(warnings > 0)
