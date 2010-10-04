import ast
from pyflakes import messages
import __builtin__


allowed_before_future = (ast.Module, ast.ImportFrom, ast.Expr, ast.Str)
defined_names = set(('__file__', '__builtins__'))

class Binding(object):
    """
    @ivar used: pair of (L{Scope}, line-number) indicating the scope and
                line number that this binding was last used
    """
    def __init__(self, name, source):
        self.name = name
        self.source = source
        self.used = False

    def __str__(self):
        return self.name

    def __repr__(self):
        return '<%s object %r from line %r at 0x%x>' % (self.__class__.__name__,
                                                        self.name,
                                                        self.source.lineno,
                                                        id(self))

class UnBinding(Binding):
    '''Created by the 'del' operator.'''

class Importation(Binding):
    def __init__(self, name, source):
        name = name.split('.')[0]
        super(Importation, self).__init__(name, source)

class Assignment(Binding):
    pass

class FunctionDefinition(Binding):
    _property_decorator = False


class Scope(dict):
    import_starred = False       # set to True when import * is found

    def __repr__(self):
        return '<%s at 0x%x %s>' % (self.__class__.__name__, id(self), dict.__repr__(self))

    def __init__(self):
        super(Scope, self).__init__()

class ClassScope(Scope):
    pass



class FunctionScope(Scope):
    """
    I represent a name scope for a function.

    @ivar globals: Names declared 'global' in this function.
    """
    def __init__(self):
        super(FunctionScope, self).__init__()
        self.globals = {}



class ModuleScope(Scope):
    pass

class Checker(ast.NodeVisitor):
    def __init__(self, tree, filename='(none)', builtins = None):
        ast.NodeVisitor.__init__(self)

        self.deferred = []
        self.dead_scopes = []
        self.messages = []
        self.filename = filename
        self.scope_stack = [ModuleScope()]
        self.futures_allowed = True
        self.builtins = frozenset(builtins or [])

        self.visit(tree)
        for handler, scope in self.deferred:
            self.scope_stack = scope
            handler()
        del self.scope_stack[1:]
        self.pop_scope()
        self.check_dead_scopes()

    def defer(self, callable):
        '''Schedule something to be called after just before completion.

        This is used for handling function bodies, which must be deferred
        because code later in the file might modify the global scope. When
        `callable` is called, the scope at the time this is called will be
        restored, however it will contain any new bindings added to it.
        '''
        self.deferred.append( (callable, self.scope_stack[:]) )

    def check_dead_scopes(self):
        # Check for modules that were imported but unused
        for scope in self.dead_scopes:
            for importation in scope.itervalues():
                if isinstance(importation, Importation) and not importation.used:
                    self.report(messages.UnusedImport, importation.source.lineno, importation.name)

    def push_function_scope(self):
        self.scope_stack.append(FunctionScope())

    def push_class_scope(self):
        self.scope_stack.append(ClassScope())

    def pop_scope(self):
        scope = self.scope_stack.pop()
        self.dead_scopes.append(scope)

    @property
    def scope(self):
        return self.scope_stack[-1]

    def report(self, message_class, *args, **kwargs):
        self.messages.append(message_class(self.filename, *args, **kwargs))

    def visit_Import(self, node):
        for name_node in node.names:
            # "import bar as foo" -> name=bar, asname=foo
            name = name_node.asname or name_node.name
            self.add_binding(node, Importation(name, node))

    def visit_GeneratorExp(self, node):
        for generator in node.generators:
            self.visit(generator.iter)
            self.assign_vars(generator.target)

        for generator in node.generators:
            if hasattr(node, 'elt'):
                self.visit(node.elt)

            self.visit_nodes(generator.ifs)

    visit_ListComp = visit_GeneratorExp

    def visit_For(self, node):
        '''
        Process bindings for loop variables.
        '''
        self.visit_nodes(node.iter)

        for var in self.flatten(node.target):
            upval = self.scope.get(var.id)
            if isinstance(upval, Importation) and upval.used:
                self.report(messages.ImportShadowedByLoopVar,
                            node.lineno, node.col_offset, var.id, upval.source.lineno)

            self.add_binding(var, Assignment(var.id, var))

        self.visit_nodes(node.body + node.orelse)

    def visit_FunctionDef(self, node):

        try:
            decorators = node.decorator_list
        except AttributeError:
            # Use .decorators for Python 2.5 compatibility
            decorators = node.decorators

        self.visit_nodes(decorators)

        # Check for property decorator
        func_def = FunctionDefinition(node.name, node)

        for decorator in decorators:
            if getattr(decorator, 'attr', None) in ('setter', 'deleter'):
                func_def._property_decorator = True

        self.add_binding(node, func_def)

        self.visit_Lambda(node)

    def visit_Lambda(self, node):
        self.visit_nodes(node.args.defaults)

        def run_function():
            self.push_function_scope()

            # Check for duplicate arguments
            argnames = set()
            for arg in self.flatten(node.args.args):
                if arg.id in argnames:
                    self.report(messages.DuplicateArgument, arg.lineno, arg.col_offset, arg.id)
                argnames.add(arg.id)

            self.assign_vars(node.args.args, report_redef=False)
            if node.args.vararg is not None:
                self.add_binding(node, Assignment(node.args.vararg, node), False)
            if node.args.kwarg is not None:
                self.add_binding(node, Assignment(node.args.kwarg, node), False)
            self.visit_nodes(node.body)
            self.pop_scope()

        self.defer(run_function)

    def visit_Name(self, node):
        '''
        Locate names in locals / function / globals scopes.
        '''
        scope, name = self.scope, node.id

        # try local scope
        import_starred = scope.import_starred
        try:
            scope[name].used = (scope, node.lineno, node.col_offset)
        except KeyError:
            pass
        else:
            return

        # try enclosing function scopes
        for func_scope in self.scope_stack[-2:0:-1]:
            import_starred = import_starred or func_scope.import_starred
            if not isinstance(func_scope, FunctionScope):
                continue
            try:
                func_scope[name].used = (scope, node.lineno, node.col_offset)
            except KeyError:
                pass
            else:
                return

        # try global scope
        import_starred = import_starred or self.scope_stack[0].import_starred
        try:
            self.scope_stack[0][node.id].used = (scope, node.lineno, node.col_offset)
        except KeyError:
            if not import_starred and not self.is_builtin(name):
                self.report(messages.UndefinedName, node.lineno, node.col_offset, name)

    def assign_vars(self, targets, report_redef=True):
        scope = self.scope

        for target in self.flatten(targets):
            name = target.id
            # if the name hasn't already been defined in the current scope
            if isinstance(scope, FunctionScope) and name not in scope:
                # for each function or module scope above us
                for upscope in self.scope_stack[:-1]:
                    if not isinstance(upscope, (FunctionScope, ModuleScope)):
                        continue

                    upval = upscope.get(name)
                    # if the name was defined in that scope, and the name has
                    # been accessed already in the current scope, and hasn't
                    # been declared global
                    if upval is not None:
                        if upval.used and upval.used[0] is scope and name not in scope.globals:
                            # then it's probably a mistake
                            self.report(messages.UndefinedLocal,
                                        upval.used[1], upval.used[2], name, upval.source.lineno, upval.source.col_offset)

            self.add_binding(target, Assignment(name, target), report_redef)

    def visit_Assign(self, node):
        for target in node.targets:
            self.visit_nodes(node.value)
            self.assign_vars(node.targets)

    def visit_Delete(self, node):
        for target in self.flatten(node.targets):
            if isinstance(self.scope, FunctionScope) and target.id in self.scope.globals:
                del self.scope.globals[target.id]
            else:
                self.add_binding(target, UnBinding(target.id, target))

    def visit_With(self, node):
        self.visit(node.context_expr)

        # handle new bindings made by optional "as" part
        if node.optional_vars is not None:
            self.assign_vars(node.optional_vars)

        self.visit_nodes(node.body)

    def visit_ImportFrom(self, node):
        if node.module == '__future__':
            if not self.futures_allowed:
                self.report(messages.LateFutureImport, node.lineno, node.col_offset, [alias.name for alias in node.names])
        else:
            self.futures_allowed = False

        for alias in node.names:
            if alias.name == '*':
                self.scope.import_starred = True
                self.report(messages.ImportStarUsed, node.lineno, node.col_offset, node.module)
                continue
            name = alias.asname or alias.name
            importation = Importation(name, node)
            if node.module == '__future__':
                importation.used = (self.scope, node.lineno, node.col_offset)
            self.add_binding(node, importation)

    def visit_Global(self, node):
        '''
        Keep track of global declarations.
        '''
        scope = self.scope
        if isinstance(scope, FunctionScope):
            scope.globals.update(dict.fromkeys(node.names))

    def visit_ClassDef(self, node):
        try:
            decorators = node.decorator_list
        except AttributeError:
            # Use .decorators for Python 2.5 compatibility
            decorators = getattr(node, 'decorators', [])

        self.visit_nodes(decorators)

        self.add_binding(node, Assignment(node.name, node))
        self.visit_nodes(node.bases)

        self.push_class_scope()
        self.visit_nodes(node.body)
        self.pop_scope()

    def visit_excepthandler(self, node):
        if node.type is not None:
            self.visit(node.type)
        if node.name is not None:
            self.assign_vars(node.name)
        self.visit_nodes(node.body)

    visit_ExceptHandler = visit_excepthandler # in 2.6, this was CamelCased

    def flatten(self, nodes):
        if isinstance(nodes, ast.Attribute):
            self.visit(nodes)
            return []
        elif isinstance(nodes, ast.Subscript):
            self.visit(nodes.value)
            self.visit(nodes.slice)
            return []
        elif isinstance(nodes, ast.Name):
            return [nodes]
        elif isinstance(nodes, (ast.Tuple, ast.List)):
            return self.flatten(nodes.elts)

        flattened_nodes = []
        for node in nodes:
            if hasattr(node, 'elts'):
                flattened_nodes += self.flatten(node.elts)
            elif node is not None:
                flattened_nodes += self.flatten(node)

        return flattened_nodes

    def add_binding(self, node, value, report_redef=True):
        line, col, scope, name = node.lineno, node.col_offset, self.scope, value.name

        # Check for a redefined function
        func = scope.get(name)
        if (isinstance(func, FunctionDefinition) and isinstance(value, FunctionDefinition)):
            # Property-decorated functions (@x.setter) should have duplicate names
            if not value._property_decorator:
                self.report(messages.RedefinedFunction, line, name, func.source.lineno)

        # Check for redefining an unused import
        if report_redef and not isinstance(scope, ClassScope):
            for up_scope in self.scope_stack[::-1]:
                upval = up_scope.get(name)
                if isinstance(upval, Importation) and not upval.used:
                    self.report(messages.RedefinedWhileUnused, line, col, name, upval.source.lineno)

        # Check for "del undefined_name"
        if isinstance(value, UnBinding):
            try:
                del scope[name]
            except KeyError:
                self.report(messages.UndefinedName, line, col, name)
        else:
            scope[name] = value

    def visit(self, node):
        if not isinstance(node, allowed_before_future):
            self.futures_allowed = False

        return super(Checker, self).visit(node)

    def visit_nodes(self, nodes):
        try:
            nodes = list(getattr(nodes, 'elts', nodes))
        except TypeError:
            nodes = [nodes]

        for node in nodes:
            self.visit(node)

    def is_builtin(self, name):
        if hasattr(__builtin__, name):
            return True
        if name in defined_names:
            return True
        if name in self.builtins:
            return True

        return False

