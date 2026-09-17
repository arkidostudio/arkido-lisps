"""Minimal AutoLISP subset interpreter for testing WallTool's pure geometry core."""
import math, re, sys

class Sym(str):
    pass

class Pair:
    def __init__(s, a, d): s.a, s.d = a, d
    def __repr__(s): return f"({s.a!r} . {s.d!r})"

T = Sym('T')

def tokenize(src):
    toks, i = [], 0
    while i < len(src):
        c = src[i]
        if c in ' \t\r\n': i += 1
        elif c == ';':
            while i < len(src) and src[i] != '\n': i += 1
        elif c in "()'": toks.append(c); i += 1
        elif c == '"':
            j, buf = i + 1, ''
            while src[j] != '"':
                if src[j] == '\\':
                    j += 1; buf += {'n': '\n', 't': '\t'}.get(src[j], src[j])
                else: buf += src[j]
                j += 1
            toks.append(('STR', buf)); i = j + 1
        else:
            j = i
            while j < len(src) and src[j] not in ' \t\r\n()\';"': j += 1
            toks.append(src[i:j]); i = j
    return toks

def atom(t):
    if isinstance(t, tuple): return t[1]
    try: return int(t)
    except ValueError: pass
    try: return float(t)
    except ValueError: return Sym(t.upper())

def parse(toks):
    out, stack = [], []
    def push(x):
        (stack[-1] if stack else out).append(x)
    quote_depth = []
    i = 0
    def read(i):
        t = toks[i]
        if t == "'":
            x, i = read(i + 1); return [Sym('QUOTE'), x], i
        if t == '(':
            lst, i = [], i + 1
            while toks[i] != ')':
                if toks[i] == '.':
                    d, i = read(i + 1)
                    return Pair(lst[0], d), i + 1
                x, i = read(i); lst.append(x)
            return (lst or None), i + 1
        return atom(t), i + 1
    while i < len(toks):
        x, i = read(i); out.append(x)
    return out

class LispError(Exception): pass

G = {}
SPECIAL = {}

def truthy(x): return x is not None

STACK = []
def lookup(s, env):
    for e in reversed(STACK):
        if s in e: return e[s]
    return G.get(s)

def setvar(s, v, env):
    for e in reversed(STACK):
        if s in e: e[s] = v; return
    G[s] = v

def lisp_eval(x, env):
    if isinstance(x, Sym):
        if x == 'T': return T
        if x == 'NIL': return None
        return lookup(x, env)
    if not isinstance(x, list): return x
    h = x[0]
    if isinstance(h, Sym) and h in SPECIAL: return SPECIAL[h](x[1:], env)
    f = lisp_eval(h, env) if not isinstance(h, list) else lisp_eval(h, env)
    args = [lisp_eval(a, env) for a in x[1:]]
    if f is None: raise LispError(f"not a function: {h!r}")
    return call(f, args, env)

def call(f, args, env):
    if callable(f): return f(*args)
    if isinstance(f, Sym): return call(lookup(f, env), args, env)
    if isinstance(f, list) and f[0] == 'LAMBDA':
        return apply_lambda(f[1], f[2:], args, env)
    raise LispError(f"not a function: {f!r}")

def apply_lambda(params, body, args, env):
    params = params or []
    if '/' in params:
        k = params.index('/'); ps, locs = params[:k], params[k + 1:]
    else: ps, locs = params, []
    if len(ps) != len(args): raise LispError(f"arg count {ps} {args}")
    frame = dict(zip(ps, args)); frame.update({l: None for l in locs})
    STACK.append(frame)
    r = None
    try:
        for b in body: r = lisp_eval(b, env)
    finally: STACK.pop()
    return r

def sp(name):
    def d(fn): SPECIAL[Sym(name)] = fn; return fn
    return d

@sp('QUOTE')
def _q(a, env): return a[0]
@sp('FUNCTION')
def _fn(a, env): return a[0]
@sp('LAMBDA')
def _lam(a, env): return [Sym('LAMBDA')] + a
@sp('DEFUN')
def _defun(a, env):
    G[a[0]] = [Sym('LAMBDA'), a[1]] + a[2:]; return a[0]
@sp('SETQ')
def _setq(a, env):
    r = None
    for i in range(0, len(a), 2):
        r = lisp_eval(a[i + 1], env); setvar(a[i], r, env)
    return r
@sp('IF')
def _if(a, env):
    if truthy(lisp_eval(a[0], env)): return lisp_eval(a[1], env)
    return lisp_eval(a[2], env) if len(a) > 2 else None
@sp('COND')
def _cond(a, env):
    for cl in a:
        v = lisp_eval(cl[0], env)
        if truthy(v):
            for b in cl[1:]: v = lisp_eval(b, env)
            return v
    return None
@sp('PROGN')
def _progn(a, env):
    r = None
    for b in a: r = lisp_eval(b, env)
    return r
@sp('WHILE')
def _while(a, env):
    r = None
    while truthy(lisp_eval(a[0], env)):
        for b in a[1:]: r = lisp_eval(b, env)
    return r
@sp('REPEAT')
def _repeat(a, env):
    r = None
    for _ in range(lisp_eval(a[0], env)):
        for b in a[1:]: r = lisp_eval(b, env)
    return r
@sp('FOREACH')
def _foreach(a, env):
    r = None
    frame = {a[0]: None}
    lst = lisp_eval(a[1], env) or []
    STACK.append(frame)
    try:
        for v in lst:
            frame[a[0]] = v
            for b in a[2:]: r = lisp_eval(b, env)
    finally: STACK.pop()
    return r
@sp('AND')
def _and(a, env):
    for b in a:
        if not truthy(lisp_eval(b, env)): return None
    return T
@sp('OR')
def _or(a, env):
    for b in a:
        if truthy(lisp_eval(b, env)): return T
    return None

def L(xs):
    xs = list(xs); return xs or None

def cons(a, d):
    if d is None: return [a]
    if isinstance(d, list): return [a] + d
    return Pair(a, d)
def car(x):
    if x is None: return None
    return x.a if isinstance(x, Pair) else x[0]
def cdr(x):
    if x is None: return None
    if isinstance(x, Pair): return x.d
    return L(x[1:])

def num_eq(a, b):
    if isinstance(a, (int, float)) and isinstance(b, (int, float)): return a == b
    return a == b if isinstance(a, str) and isinstance(b, str) else a is b

def equal(a, b, fuzz=0):
    if isinstance(a, (int, float)) and isinstance(b, (int, float)): return abs(a - b) <= fuzz
    if isinstance(a, list) and isinstance(b, list):
        return len(a) == len(b) and all(equal(x, y, fuzz) for x, y in zip(a, b))
    if isinstance(a, Pair) and isinstance(b, Pair): return equal(a.a, b.a, fuzz) and equal(a.d, b.d, fuzz)
    return a == b

def div(*a):
    r = a[0]
    for b in a[1:]:
        if isinstance(r, int) and isinstance(b, int): r = int(r / b)
        else: r = r / b
    return r

def lisptype(x):
    if x is None: return None
    if isinstance(x, bool): return Sym('SYM')
    if isinstance(x, int): return Sym('INT')
    if isinstance(x, float): return Sym('REAL')
    if isinstance(x, Sym): return Sym('SYM')
    if isinstance(x, str): return Sym('STR')
    if isinstance(x, list) and x and x[0] == 'LAMBDA': return Sym('USUBR')
    if isinstance(x, (list, Pair)): return Sym('LIST')
    if isinstance(x, Ename): return Sym('ENAME')
    return Sym('SUBR')

def distof(s, mode=2):
    try:
        if not re.fullmatch(r'\s*[-+]?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?\s*', s): return None
        return float(s)
    except Exception: return None

def rtos(x, mode=2, prec=4): return f"{x:.{prec}f}"

def wcmatch(s, pat):
    for p in pat.split(','):
        rx = '^' + re.escape(p).replace(r'\*', '.*').replace(r'\?', '.') + '$'
        if re.match(rx, s): return T
    return None

def mapcar(f, *lists):
    lists = [l or [] for l in lists]
    return L(call(f, list(xs), []) for xs in zip(*lists))

def B(v): return T if v else None

_files = {}
def lopen(path, mode):
    fh = open(path, mode); _files[id(fh)] = fh; return fh
def read_line(fh):
    l = fh.readline()
    return l.rstrip('\n').rstrip('\r') if l else None

def princ(x=None, *_):
    if x is not None and OUTPUT: sys.stdout.write(x if isinstance(x, str) else repr(x))
    return x
OUTPUT = True

def cxr(path):
    def f(x):
        for c in reversed(path): x = car(x) if c == 'a' else cdr(x)
        return x
    return f

B_ = {
 '+': lambda *a: sum(a) if a else 0, '-': lambda a, *b: (a - sum(b)) if b else -a,
 '*': lambda *a: math.prod(a), '/': div, '1+': lambda a: a + 1, '1-': lambda a: a - 1,
 '<': lambda *a: B(all(x < y for x, y in zip(a, a[1:]))), '>': lambda *a: B(all(x > y for x, y in zip(a, a[1:]))),
 '<=': lambda *a: B(all(x <= y for x, y in zip(a, a[1:]))), '>=': lambda *a: B(all(x >= y for x, y in zip(a, a[1:]))),
 '=': lambda a, b: B(num_eq(a, b)), '/=': lambda a, b: B(not num_eq(a, b)),
 'ABS': abs, 'SQRT': math.sqrt, 'SIN': math.sin, 'COS': math.cos,
 'ATAN': lambda y, x=None: math.atan(y) if x is None else math.atan2(y, x),
 'MAX': max, 'MIN': min, 'FIX': lambda x: int(x), 'FLOAT': float, 'REM': lambda a, b: math.fmod(a, b) if isinstance(a, float) or isinstance(b, float) else int(math.fmod(a, b)),
 'NUMBERP': lambda x: B(isinstance(x, (int, float))), 'MINUSP': lambda x: B(x < 0), 'ZEROP': lambda x: B(x == 0),
 'NOT': lambda x: B(x is None), 'NULL': lambda x: B(x is None), 'EQ': lambda a, b: B(a is b or (isinstance(a, (str, int, float)) and a == b)),
 'EQUAL': lambda a, b, f=0: B(equal(a, b, f)),
 'LIST': lambda *a: L(a), 'CONS': cons, 'CAR': car, 'CDR': cdr,
 'NTH': lambda n, l: (l[n] if l and n < len(l) else None), 'LAST': lambda l: l[-1] if l else None,
 'LENGTH': lambda l: len(l) if l else 0, 'REVERSE': lambda l: L(reversed(l or [])),
 'APPEND': lambda *ls: L(x for l in ls for x in (l or [])),
 'MEMBER': lambda x, l: next((L(l[i:]) for i in range(len(l or [])) if equal(x, l[i])), None),
 'ASSOC': lambda k, l: next((e for e in (l or []) if equal(car(e), k)), None),
 'LISTP': lambda x: B(x is None or isinstance(x, (list, Pair))), 'TYPE': lisptype,
 'MAPCAR': mapcar, 'APPLY': lambda f, args: call(f, list(args or []), []), 'EVAL': lambda x: lisp_eval(x, {}), 'SET': lambda k, v: (G.__setitem__(k, v), v)[1],
 'STRCAT': lambda *a: ''.join(a), 'STRLEN': len, 'SUBSTR': lambda s, i, n=None: s[i - 1:] if n is None else s[i - 1:i - 1 + n],
 'STRCASE': lambda s, lower=None: s.lower() if lower else s.upper(), 'CHR': chr, 'ASCII': lambda s: ord(s[0]) if s else 0,
 'ITOA': str, 'ATOI': lambda s: int(float(s)) if distof(s) is not None else 0, 'ATOF': lambda s: distof(s) or 0.0,
 'RTOS': rtos, 'DISTOF': distof, 'WCMATCH': wcmatch,
 'SNVALID': lambda s, *_: B(s and not re.search(r'[<>/\\":;?*|,=`]', s)),
 'PRINC': princ, 'PROMPT': princ, 'TERPRI': lambda: None,
 'OPEN': lopen, 'READ-LINE': read_line, 'CLOSE': lambda f: f.close(),
 'FINDFILE': lambda f: FINDFILE.get(f),
}
FINDFILE = {}
for k, v in B_.items(): G[Sym(k)] = v
for p in ['aa', 'ad', 'da', 'dd', 'add', 'ddd', 'addd', 'dda', 'ada']:
    G[Sym('C' + p.upper() + 'R')] = cxr(p)

def load(path):
    r = None
    for form in parse(tokenize(open(path).read())): r = lisp_eval(form, [])
    return r

def ev(src):
    r = None
    for form in parse(tokenize(src)): r = lisp_eval(form, [])
    return r

# ---------------------------------------------------------------------------
# Fake drawing database: enough of entmake/entget/entmod/entdel/ssget for the
# wall rebuild, XW and EW code paths. Entities live in model space.
class Ename:
    n = 0
    def __init__(s): Ename.n += 1; s.id = Ename.n
    def __repr__(s): return f'<E{s.id}>'
DB = {}          # Ename -> data list (without -1)
DELETED = set()
LAST = [None]
SSFIRST = [None]

def _clean(d):
    return [e for e in d if car(e) != -1]
def entmake(d):
    e = Ename(); DB[e] = _clean(d); LAST[0] = e; return d
def _xd(d):
    return next((x for x in d if car(x) == -3), None)
def entget(e, apps=None):
    if e is None or e not in DB or e in DELETED: return None
    d = [x for x in DB[e] if car(x) != -3]
    x = _xd(DB[e])
    if apps and x is not None:
        keep = [a for a in (cdr(x) or []) if any(wcmatch(car(a).upper(), w.upper()) for w in apps)]
        if keep: d.append(L([-3] + keep))
    return [Pair(-1, e)] + d
def entmod(d):
    e = next(x.d for x in d if isinstance(x, Pair) and x.a == -1)
    if e not in DB or e in DELETED: return None
    old, new = _xd(DB[e]), _xd(d)
    apps = {car(a): a for a in (cdr(old) or [])} if old is not None else {}
    for a in (cdr(new) or []) if new is not None else []: apps[car(a)] = a
    DB[e] = [x for x in _clean(d) if car(x) != -3] + ([L([-3] + list(apps.values()))] if apps else [])
    return d
def entdel(e):
    if e in DELETED: DELETED.discard(e)
    else: DELETED.add(e)
    return e
def _match_one(data, f):
    k = car(f)
    if k == 410: return True
    if k == -3:
        x = _xd(data)
        have = [car(a).upper() for a in (cdr(x) or [])] if x is not None else []
        return all(any(wcmatch(h, car(w).upper()) for h in have) for w in (cdr(f) or []))
    v = cdr(f)
    got = cdr(next((x for x in data if car(x) == k), Pair(k, None)))
    if k in (0, 2, 8): return bool(got) and bool(wcmatch(got.upper(), v.upper()))
    return got == v
def _match(data, filt):
    items, pos = list(filt or []), [0]
    def one():
        f = items[pos[0]]; pos[0] += 1
        if car(f) == -4 and cdr(f).upper() in ('<OR', '<AND'):
            res = []
            while not (car(items[pos[0]]) == -4 and cdr(items[pos[0]]).upper() in ('OR>', 'AND>')):
                res.append(one())
            pos[0] += 1
            return any(res) if cdr(f).upper() == '<OR' else all(res)
        return _match_one(data, f)
    ok = True
    while pos[0] < len(items): ok = one() and ok
    return ok
def ssget(*args):
    if args and args[0] == '_I':
        sel = SSFIRST[0] or []
        filt = args[1] if len(args) > 1 else None
        out = [e for e in sel if e in DB and e not in DELETED and _match(DB[e], filt)]
        return out or None
    if args and args[0] == '_X':
        out = [e for e in DB if e not in DELETED and _match(DB[e], args[1] if len(args) > 1 else None)]
        return out or None
    raise LispError('interactive ssget not supported in tests')
def sssetfirst(*a): SSFIRST[0] = None; return None
for k, v in {'ENTMAKE': entmake, 'ENTGET': entget, 'ENTMOD': entmod, 'ENTDEL': entdel, 'ENTLAST': lambda: LAST[0],
             'SSGET': ssget, 'SSLENGTH': len, 'SSNAME': lambda s, i: s[i], 'SSSETFIRST': sssetfirst,
             'GETVAR': lambda n: {'CTAB': 'Model'}.get(n.upper(), 0), 'TBLSEARCH': lambda *a: T,
             'REDRAW': lambda *a: None, 'ENTMAKEX': lambda d: (entmake(d), LAST[0])[1],
             'DISTANCE': lambda a, b: math.dist(list(a)[:2], list(b)[:2]), 'REGAPP': lambda a: a,
             'NAMEDOBJDICT': lambda: None, 'DICTSEARCH': lambda *a: None,
             'VL-REMOVE': lambda x, l: L(y for y in (l or []) if not equal(x, y)),
             'VL-REMOVE-IF': lambda f, l: L(y for y in (l or []) if call(f, [y], []) is None),
             'VL-REMOVE-IF-NOT': lambda f, l: L(y for y in (l or []) if call(f, [y], []) is not None), 'SUBST': lambda new, old, l: L(new if equal(x, old) else x for x in (l or []))}.items():
    G[Sym(k)] = v

def db_reset():
    DB.clear(); DELETED.clear(); LAST[0] = None; SSFIRST[0] = None
def db_lines(layer):
    out = []
    for e, d in DB.items():
        if e in DELETED: continue
        g = {car(x): cdr(x) for x in d}
        if g.get(0) == 'LINE' and g.get(8, '').upper() == layer.upper():
            out.append((e, (g[10][0], g[10][1]), (g[11][0], g[11][1])))
    return out

# --- command/UI stubs for PickFirst and keyword tests
PICKPTS = {}          # Ename -> (x, y) recorded pick point (ssnamex method 1)
KWQUEUE = []
def command_s(*a):
    SSFIRST[0] = None  # like AutoCAD: running a command clears the implied selection
    return T
def ssnamex(ss, i=None):
    if i is not None and not isinstance(i, int): raise LispError('bad argument type: numberp')
    items = [ss[i]] if i is not None else list(ss or [])
    out = []
    for x in items:
        if x in PICKPTS: out.append([1, x, 0, [0, [PICKPTS[x][0], PICKPTS[x][1], 0.0]]])
        else: out.append([0, x, 0])
    return out
for k, v in {'COMMAND-S': command_s, 'SETVAR': lambda *a: None, 'SSNAMEX': ssnamex,
             'INITGET': lambda *a: None, 'GETKWORD': lambda *a: KWQUEUE.pop(0) if KWQUEUE else None}.items():
    G[Sym(k)] = v

# --- WWO stubs
ENTSELQ = []
DISTQ = []
ERRNO = [0]
def entsel(*a):
    if not ENTSELQ: ERRNO[0] = 52; return None
    x = ENTSELQ.pop(0)
    if x is None: ERRNO[0] = 7
    return x
def getvar2(n):
    n = n.upper()
    if n == 'ERRNO': return ERRNO[0]
    return {'CTAB': 'Model'}.get(n, 0)
def setvar2(n, v):
    if n.upper() == 'ERRNO': ERRNO[0] = v
for k, v in {'ENTSEL': entsel, 'GETDIST': lambda *a: DISTQ.pop(0) if DISTQ else None,
             'GETVAR': getvar2, 'SETVAR': setvar2, 'TRANS': lambda p, a, b: p}.items():
    G[Sym(k)] = v

# --- TW stubs
POINTQ = []
G[Sym('GETPOINT')] = lambda *a: POINTQ.pop(0) if POINTQ else None
G[Sym('GETCORNER')] = lambda *a: POINTQ.pop(0) if POINTQ else None

# --- WW loop stubs
REALQ = []
G[Sym('GETREAL')] = lambda *a: REALQ.pop(0) if REALQ else None
