
import textwrap

from twisted.trial import unittest

from pyflakes import checker, ast


class Test(unittest.TestCase):

    def flakes(self, input, *expectedOutputs):
        w = checker.Checker(ast.parse(textwrap.dedent(input)))
        outputs = [type(o) for o in w.messages]
        expectedOutputs = list(expectedOutputs)
        outputs.sort()
        expectedOutputs.sort()
        self.assert_(outputs == expectedOutputs, '''\
for input:
%s
expected outputs:
%s
but got:
%s''' % (input, repr(expectedOutputs), '\n'.join([str(o) for o in w.messages])))
        return w
