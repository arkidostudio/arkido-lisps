"""Runs the SB solver (real LISP source) against cases A-I.
Uses the AutoLISP interpreter from ../../AKD WallTool/test/alisp.py.
Usage: python3 run_tests.py"""
import math, os, sys, functools, itertools
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, '..', '..', 'AKD WallTool', 'test'))
sys.setrecursionlimit(10000)
import alisp as A

A.OUTPUT = '-v' in sys.argv
Sym = type(next(iter(A.G)))
A.G[Sym('PI')] = math.pi
A.G[Sym('HANDENT')] = lambda h: None
A.G[Sym('VL-SORT')] = lambda l, f: A.L(sorted(l or [], key=functools.cmp_to_key(
    lambda a, b: -1 if A.truthy(A.call(f, [a, b], [])) else (1 if A.truthy(A.call(f, [b, a], [])) else 0))))
A.G[Sym('VL-POSITION')] = lambda x, l: next((i for i, y in enumerate(l or []) if A.equal(x, y)), None)
for n in (2, 3, 4):
    for p in itertools.product('ad', repeat=n):
        A.G[Sym('C' + ''.join(p).upper() + 'R')] = A.cxr(''.join(p))
A.load(os.path.join(HERE, '..', 'SB.lsp'))
if '-d' in sys.argv: A.ev('(setq *sb-debug* T)')

def P(p): return f'(list {float(p[0])} {float(p[1])})'
def poly(*pts, closed=True):
    pts = list(pts) + ([pts[0]] if closed else [])
    return list(zip(pts, pts[1:]))
def rot(segs, deg, about=(0, 0)):
    c, s = math.cos(math.radians(deg)), math.sin(math.radians(deg))
    f = lambda p: (about[0] + (p[0]-about[0])*c - (p[1]-about[1])*s, about[1] + (p[0]-about[0])*s + (p[1]-about[1])*c)
    return [(f(a), f(b)) for a, b in segs], f

def solve(segs, picks, maxb=1200.0):
    src = '(list ' + ' '.join(f'(list {P(a)} {P(b)})' for a, b in segs) + ')'
    A.ev(f'(setq net (sb:build-virtual-network {src} {maxb}))')
    out = []
    for p in picks:
        r = A.ev(f'(sb:solve-pick net {P(p)} {maxb})')
        out.append((None, r[1]) if r[0] is None else ([tuple(q) for q in r[0]], list(r[1])))
    return out

def area(pts): return abs(sum(p[0]*q[1] - p[1]*q[0] for p, q in zip(pts, pts[1:] + pts[:1]))) / 2

fails = 0
def check(name, cond, info=''):
    global fails
    print(('PASS ' if cond else 'FAIL ') + name + ('' if cond else f'   {info}'))
    fails += not cond

def room_ok(name, res, exp_area, exp_n=4):
    pts, _ = res
    check(name, pts is not None and abs(area(pts) - exp_area) < 1 and len(pts) == exp_n, res)

def fail_ok(name, res, text):
    check(name, res[0] is None and text in res[1], res)

W, H = 6000, 4000
top_l_r = [((0, 0), (0, H)), ((0, H), (W, H)), ((W, H), (W, 0))]

# A closed rectangle (+ a dangling stub inside, removed as a spike)
room_ok('A closed rectangle', solve(poly((0, 0), (W, 0), (W, H), (0, H)) + [((1000, 2000), (1000, 3000))], [(3000, 2000)])[0], W*H)

# B 900 door gap
B = top_l_r + [((0, 0), (2000, 0)), ((2900, 0), (W, 0))]
room_ok('B 900 door gap bridged', solve(B, [(3000, 2000)])[0], W*H)

# C gap 1800 > Max Bridge
C = top_l_r + [((0, 0), (2000, 0)), ((3800, 0), (W, 0))]
fail_ok('C 1800 gap refused', solve(C, [(3000, 2000)])[0], '1800')
fail_ok('B@1500 gap refused (1500 > 1200)', solve(top_l_r + [((0, 0), (2000, 0)), ((3500, 0), (W, 0))], [(3000, 2000)])[0], '1500')

# D close but perpendicular endpoints (missing corner)
D = [((0, 600), (0, H)), ((0, H), (W, H)), ((W, H), (W, 0)), ((W, 0), (400, 0))]
check('D perpendicular ends not bridged', solve(D, [(3000, 2000)])[0][0] is None)

# E two doors
E = [((0, 0), (0, H)), ((W, H), (W, 0)), ((0, 0), (1000, 0)), ((1900, 0), (W, 0)), ((0, H), (3000, H)), ((3900, H), (W, H))]
room_ok('E two doors', solve(E, [(3000, 2000)])[0], W*H)

# F two rooms, shared wall, a door each
F = [((0, 0), (0, H)), ((0, H), (8000, H)), ((8000, H), (8000, 0)), ((4000, 0), (4000, H)),
     ((0, 0), (1000, 0)), ((1900, 0), (5000, 0)), ((5900, 0), (8000, 0))]
r1, r2 = solve(F, [(2000, 2000), (6000, 2000)])
room_ok('F room 1', r1, 16e6); room_ok('F room 2', r2, 16e6)
check('F rooms distinct', r1[1] != r2[1])

# G 2400 open-plan connection must not merge silently
G = poly((0, 0), (8000, 0), (8000, H), (0, H)) + [((4000, 0), (4000, 800)), ((4000, 3200), (4000, H))]
fail_ok('G 2400 opening refused', solve(G, [(2000, 2000)])[0], '2400')
G2 = poly((0, 0), (8000, 0), (8000, H), (0, H)) + [((4000, 0), (4000, 1550)), ((4000, 2450), (4000, H))]
r1, r2 = solve(G2, [(2000, 2000), (6000, 2000)])
room_ok('G2 900 partition door room 1', r1, 16e6); room_ok('G2 room 2', r2, 16e6)

# H angled: case B rotated 30 deg, plus an irregular quad with a skewed door
HB, f = rot(B, 30)
room_ok('H rotated door room', solve(HB, [f((3000, 2000))])[0], W*H)
q = [(0, 0), (5000, 800), (4200, 4500), (-600, 3600)]
u = ((q[1][0]-q[0][0])/math.dist(q[0], q[1]), (q[1][1]-q[0][1])/math.dist(q[0], q[1]))
a, b = (u[0]*2000, u[1]*2000), (u[0]*2900, u[1]*2900)
HQ = poly(q[1], q[2], q[3], q[0], closed=False) + [(q[0], a), (b, q[1])]
room_ok('H irregular quad with skewed door', solve(HQ, [(2000, 2000)])[0], area(q))

# I double-line walls, 200 thick, jambs drawn; two rooms, partition door + external door
t = 200
I = [((-t, -t), (2000, -t)), ((2900, -t), (8200 + t, -t)), ((8200 + t, -t), (8200 + t, H + t)),
     ((8200 + t, H + t), (-t, H + t)), ((-t, H + t), (-t, -t)),                     # outer faces
     ((0, 0), (2000, 0)), ((2900, 0), (8200, 0)), ((8200, 0), (8200, H)), ((8200, H), (0, H)), ((0, H), (0, 0)),  # inner faces
     ((2000, 0), (2000, -t)), ((2900, 0), (2900, -t)),                                # external door jambs
     ((4000, 0), (4000, 1000)), ((4000, 1900), (4000, H)), ((4200, 0), (4200, 1000)), ((4200, 1900), (4200, H)),  # partition faces
     ((4000, 1000), (4200, 1000)), ((4000, 1900), (4200, 1900))]                     # partition door jambs
r1, r2 = solve(I, [(2000, 2000), (6000, 2000)])
room_ok('I double-line room 1 follows inner faces', r1, 4000*H)
room_ok('I double-line room 2 follows inner faces', r2, 4000*H)

# D2: perpendicular near-miss in a real room must not create a bridge that splits it
D2 = poly((0, 0), (W, 0), (W, H), (0, H)) + [((2000, 0), (2000, 1500)), ((2600, 1800), (3600, 1800))]
room_ok('D2 unrelated stubs ignored', solve(D2, [(4500, 3000)])[0], W*H)

# J single-line rectangle: doors on left and right walls (same height), 1200 window on top wall
JW, JH = 4500, 4000
J = [((0, 0), (JW, 0)),
     ((0, 0), (0, 1500)), ((0, 2400), (0, JH)), ((JW, 0), (JW, 1500)), ((JW, 2400), (JW, JH)),
     ((0, JH), (1650, JH)), ((2850, JH), (JW, JH))]
room_ok('J 2x900 + 1200 openings, Max Bridge 1200', solve(J, [(2000, 2000)], 1200.0)[0], JW*JH)
fail_ok('J2 same room, Max Bridge 1000 -> leaks through 1200, refused', solve(J, [(2000, 2000)], 1000.0)[0], '1200')
Jn = [((0, 0), (JW, 0)),
      ((0, 0), (0, 1500)), ((0, 2400), (0, JH)), ((JW, 0), (JW, 1500)), ((JW, 2400), (JW, JH)),
      ((0, JH), (1650, JH)), ((2850.0000001, JH), (JW, JH))]
room_ok('J3 1200.0000001 opening accepted at Max Bridge 1200', solve(Jn, [(2000, 2000)], 1200.0)[0], JW*JH)

# K realistic double-line walls, t=200, jamb returns at every opening (inner room 0..4500 x 0..4000)
def dl_room(w, h, t, left, right, top, glazing=False):
    """left/right = (y0, y1) door in side walls, top = (x0, x1) window. Returns LINE list."""
    s = []
    def wall_x(x_in, x_out, y0, y1, gap):       # vertical wall faces with a jambed gap
        for x in (x_in, x_out):
            ya, yb = (0 if x == x_in else -t), (h if x == x_in else h + t)
            s.extend([((x, ya), (x, gap[0])), ((x, gap[1]), (x, yb))])
        s.extend([((x_in, gap[0]), (x_out, gap[0])), ((x_in, gap[1]), (x_out, gap[1]))])
    wall_x(0, -t, 0, h, left)
    wall_x(w, w + t, 0, h, right)
    s.extend([((0, 0), (w, 0)), ((-t, -t), (w + t, -t))])                   # bottom wall, closed
    for y_in, y_out in ((h, h + t),):
        s.extend([((0, y_in), (top[0], y_in)), ((top[1], y_in), (w, y_in)),
                  ((-t, y_out), (top[0], y_out)), ((top[1], y_out), (w + t, y_out)),
                  ((top[0], y_in), (top[0], y_out)), ((top[1], y_in), (top[1], y_out))])
        if glazing:
            s.extend([((top[0], h + t/3), (top[1], h + t/3)), ((top[0], h + 2*t/3), (top[1], h + 2*t/3))])
    return s
K = dl_room(JW, JH, 200, (1500, 2400), (1500, 2400), (1650, 2850))
room_ok('K double-line 2x900 doors + 1200 window', solve(K, [(2000, 2000)], 1200.0)[0], JW*JH)
fail_ok('K2 double-line, Max Bridge 1000 refused', solve(K, [(2000, 2000)], 1000.0)[0], '1200')
room_ok('K3 double-line window with glazing lines', solve(dl_room(JW, JH, 200, (1500, 2400), (1500, 2400), (1650, 2850), True), [(2000, 2000)], 1200.0)[0], JW*JH)
room_ok('K4 double-line doors offset (not opposite)', solve(dl_room(JW, JH, 200, (600, 1500), (2600, 3500), (1650, 2850)), [(2000, 2000)], 1200.0)[0], JW*JH)
# corridor 1100 wide with opposite doors: jamb returns face each other across it - must not split it
C2 = dl_room(1100, 6000, 200, (2000, 2900), (2000, 2900), (100, 1000))
room_ok('K5 1100 corridor, opposite doors not bridged across', solve(C2, [(550, 1000)], 1200.0)[0], 1100*6000)

print('\n%d failure(s)' % fails)
sys.exit(1 if fails else 0)
