
"""
Tests for L{pyflakes.scripts.pyflakes}.
"""

import sys
from StringIO import StringIO

from twisted.python.filepath import FilePath
from twisted.trial.unittest import TestCase

from pyflakes.scripts.pyflakes import checkPath

def withStderrTo(stderr, f):
    """
    Call C{f} with C{sys.stderr} redirected to C{stderr}.
    """
    (outer, sys.stderr) = (sys.stderr, stderr)
    try:
        return f()
    finally:
        sys.stderr = outer



class CheckTests(TestCase):
    """
    Tests for L{check} and L{checkPath} which check a file for flakes.
    """
    def test_missingTrailingNewline(self):
        """
        Source which doesn't end with a newline shouldn't cause any
        exception to be raised nor an error indicator to be returned by
        L{check}.
        """
        fName = self.mktemp()
        FilePath(fName).setContent("def foo():\n\tpass\n\t")
        self.assertFalse(checkPath(fName))


    def test_checkPathNonExisting(self):
        """
        L{checkPath} handles non-existing files.
        """
        err = StringIO()
        count = withStderrTo(err, lambda: checkPath('extremo'))
        self.assertEquals(err.getvalue(), 'extremo: no such file\n')
        self.assertEquals(count, 1)
