"""Runs only the TX / TW-generic / WWD / WWE part of run_tests.py (fast iteration).
The full run_tests.py suite is the authoritative check. Usage: python3 tx_only.py"""
import ast, os, sys
HERE = os.path.dirname(os.path.abspath(__file__))
src = open(os.path.join(HERE, 'run_tests.py')).read()
tree = ast.parse(src)
need = {'lst', 'P', 'near', 'chk', 'cfg_run', 'fresh', 'settings', 'add', 'mkline', 'walls_now', 'masters_now',
        'find_line', 'snapshot', 'run_cmd', 'field',
        'build', 'mk', 'master_set', 'mset', 'geo', 'lines_match_masters', 'same_lines', 'expected', 'lisp_walls', 'wall', 'spans', 'undo_rec'}
head = [n for n in tree.body if isinstance(n, (ast.Import, ast.ImportFrom))
        or isinstance(n, ast.FunctionDef)
        or (isinstance(n, ast.Assign) and any(getattr(t, 'id', '') in ('cfgdir', 'fails', 'EPS', 'HERE') for t in n.targets))]
start = src.index('# =========================== TX (geometry-first')
end = src.index("\nprint(f'\\n{fails} failure(s)')")
g = {'__file__': os.path.join(HERE, 'run_tests.py')}
sys.path.insert(0, HERE); sys.setrecursionlimit(10000)
exec(compile(ast.Module(body=head, type_ignores=[]), 'head', 'exec'), g)
exec("A.OUTPUT = False\nA.load(os.path.join(HERE, '..', 'WallTool.lsp'))\nA.ev('(setq *wt:cfg* *wt:cfg-defaults*)')\nfails = 0\n", g)
exec(src[start:end] + "\nprint(f'\\n{fails} failure(s)')", g)
