# (c) 2005-2008 Divmod, Inc.
# See LICENSE file for details

"""
Tests for various Pyflakes behavior.
"""

from sys import version_info

from pyflakes import messages as m
from pyflakes.test import harness


class Test(harness.Test):

    def test_duplicateArgs(self):
        self.flakes('def fu(bar, bar): pass', m.DuplicateArgument)

    def test_localReferencedBeforeAssignment(self):
        self.flakes('''
        a = 1
        def f():
            a; a=1
        f()
        ''', m.UndefinedName)
    test_localReferencedBeforeAssignment.todo = 'this requires finding all assignments in the function body first'

    def test_redefinedFunction(self):
        """
        Test that shadowing a function definition with another one raises a
        warning.
        """
        self.flakes('''
        def a(): pass
        def a(): pass
        ''', m.RedefinedFunction)

    def test_redefinedClassFunction(self):
        """
        Test that shadowing a function definition in a class suite with another
        one raises a warning.
        """
        self.flakes('''
        class A:
            def a(): pass
            def a(): pass
        ''', m.RedefinedFunction)

    def test_functionDecorator(self):
        """
        Test that shadowing a function definition with a decorated version of
        that function does not raise a warning.
        """
        self.flakes('''
        from somewhere import somedecorator

        def a(): pass
        a = somedecorator(a)
        ''')

    def test_classFunctionDecorator(self):
        """
        Test that shadowing a function definition in a class suite with a
        decorated version of that function does not raise a warning.
        """
        self.flakes('''
        class A:
            def a(): pass
            a = classmethod(a)
        ''')

    def test_unaryPlus(self):
        '''Don't die on unary +'''
        self.flakes('+1')



class Python25Test(harness.Test):
    """
    Tests for checking of syntax only available in Python 2.5 and newer.
    """
    if version_info < (2, 5):
        skip = "Python 2.5 required for if-else and with tests"

    def test_ifexp(self):
        """
        Test C{foo if bar else baz} statements.
        """
        self.flakes("a = 'moo' if True else 'oink'")
        self.flakes("a = foo if True else 'oink'", m.UndefinedName)
        self.flakes("a = 'moo' if True else bar", m.UndefinedName)


    def test_withStatementNoNames(self):
        """
        No warnings are emitted for using inside or after a nameless C{with}
        statement a name defined beforehand.
        """
        self.flakes('''
        from __future__ import with_statement
        bar = None
        with open("foo"):
            bar
        bar
        ''')

    def test_withStatementSingleName(self):
        """
        No warnings are emitted for using a name defined by a C{with} statement
        within the suite or afterwards.
        """
        self.flakes('''
        from __future__ import with_statement
        with open('foo') as bar:
            bar
        bar
        ''')


    def test_withStatementTupleNames(self):
        """
        No warnings are emitted for using any of the tuple of names defined by
        a C{with} statement within the suite or afterwards.
        """
        self.flakes('''
        from __future__ import with_statement
        with open('foo') as (bar, baz):
            bar, baz
        bar, baz
        ''')


    def test_withStatementSingleNameUndefined(self):
        """
        An undefined name warning is emitted if the name first defined by a
        C{with} statement is used before the C{with} statement.
        """
        self.flakes('''
        from __future__ import with_statement
        bar
        with open('foo') as bar:
            pass
        ''', m.UndefinedName)


    def test_withStatementTupleNamesUndefined(self):
        """
        An undefined name warning is emitted if a name first defined by a the
        tuple-unpacking form of the C{with} statement is used before the
        C{with} statement.
        """
        self.flakes('''
        from __future__ import with_statement
        baz
        with open('foo') as (bar, baz):
            pass
        ''', m.UndefinedName)


    def test_withStatementSingleNameRedefined(self):
        """
        A redefined name warning is emitted if a name bound by an import is
        rebound by the name defined by a C{with} statement.
        """
        self.flakes('''
        from __future__ import with_statement
        import bar
        with open('foo') as bar:
            pass
        ''', m.RedefinedWhileUnused)


    def test_withStatementTupleNamesRedefined(self):
        """
        A redefined name warning is emitted if a name bound by an import is
        rebound by one of the names defined by the tuple-unpacking form of a
        C{with} statement.
        """
        self.flakes('''
        from __future__ import with_statement
        import bar
        with open('foo') as (bar, baz):
            pass
        ''', m.RedefinedWhileUnused)


    def test_withStatementUndefinedInside(self):
        """
        An undefined name warning is emitted if a name is used inside the
        body of a C{with} statement without first being bound.
        """
        self.flakes('''
        from __future__ import with_statement
        with open('foo') as bar:
            baz
        ''', m.UndefinedName)


    def test_withStatementNameDefinedInBody(self):
        """
        A name defined in the body of a C{with} statement can be used after
        the body ends without warning.
        """
        self.flakes('''
        from __future__ import with_statement
        with open('foo') as bar:
            baz = 10
        baz
        ''')


    def test_withStatementUndefinedInExpression(self):
        """
        An undefined name warning is emitted if a name in the I{test}
        expression of a C{with} statement is undefined.
        """
        self.flakes('''
        from __future__ import with_statement
        with bar as baz:
            pass
        ''', m.UndefinedName)

        self.flakes('''
        from __future__ import with_statement
        with bar as bar:
            pass
        ''', m.UndefinedName)

    def test_listNestedListComprehension(self):
        self.flakes('''
        root = [['213', '123'], ['4354']]
        foo = [int(c) for group in root for c in group]
        ''')

