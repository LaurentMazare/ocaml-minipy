import ast, inspect, json, textwrap, types

def node_to_python(node):
  if not isinstance(node, ast.AST):
    raise ValueError('not an ast node ' + str(type(node)))
  fields = {}
  for name, value in ast.iter_fields(node):
    if isinstance(value, ast.AST):
      value = node_to_python(value)
    elif isinstance(value, list):
      value = [node_to_python(v) for v in value]
    fields[name] = value
  name = node.__class__.__name__
  return (name, fields) if fields else (name,)

class PyAst(object):
  def __init__(self, fn):
    filename = inspect.getsourcefile(fn)
    lines, lineno = inspect.getsourcelines(fn)
    code = textwrap.dedent(''.join(lines[1:]))
    tree = ast.parse(code)
    self._tree = node_to_python(tree)
    self._fn = fn
    self._filename = filename
    self._lineno = lineno

  def __call__(self, *args, **kwargs):
    return self._fn(*args, **kwargs)

  def write(self, fobj):
    json.dump(self._tree, fobj)

def pyast(fn):
  if not isinstance(fn, types.FunctionType):
    raise ValueError('not a function')
  return PyAst(fn)

@pyast
def test(x):
  print(x)
  return x + 1

with open('/tmp/test.json', 'w') as fobj:
  test.write(fobj)

