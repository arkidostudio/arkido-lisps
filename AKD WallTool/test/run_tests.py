"""Runs the WallTool geometry core (real LISP source) against the Stage 1 test matrix.
Usage: python3 run_tests.py [out.svg]"""
import math, os, sys
sys.setrecursionlimit(10000)
import alisp as A
HERE = os.path.dirname(os.path.abspath(__file__))
A.OUTPUT = False
A.load(os.path.join(HERE, '..', 'WallTool.lsp'))

def lst(x): return '(list ' + ' '.join(map(str, x)) + ')'
def P(x, y): return f'(list {float(x)} {float(y)})'
def wall(p1, p2, th=150, pos='CENTER'): return (p1, p2, th, pos)
def polar(p, deg, d): return (p[0] + d*math.cos(math.radians(deg)), p[1] + d*math.sin(math.radians(deg)))

def solve(walls, regen=None):
    src = '(list ' + ' '.join(f'(list nil {P(*a)} {P(*b)} {float(t)} "{pos}")' for a, b, t, pos in walls) + ')'
    regen = range(len(walls)) if regen is None else regen
    r = A.ev(f'(wt:topo-linework {src} (list {" ".join(map(str, regen))}))')
    return [((s[0][0], s[0][1]), (s[1][0], s[1][1])) for s in (r or [])]

def near(p, q, tol=1e-3): return math.dist(p, q) < tol
def has(segs, a, b): return any((near(s[0], a) and near(s[1], b)) or (near(s[0], b) and near(s[1], a)) for s in segs)

# independent reference: brute-force outline of the IDEAL union, by sampling.
# Every output segment must be on the union boundary (one side in, one side out) and
# every boundary sample point of the ideal union must be covered by output.
def check_outline(segs, ideal_inside, bbox, step=7.3, eps=0.05):
    errs = []
    for a, b in segs:
        L = math.dist(a, b); u = ((b[0]-a[0])/L, (b[1]-a[1])/L); n = (-u[1], u[0])
        k = max(2, int(L / 20))
        for i in range(1, k):
            m = (a[0] + u[0]*L*i/k, a[1] + u[1]*L*i/k)
            if ideal_inside((m[0]+n[0]*eps, m[1]+n[1]*eps)) == ideal_inside((m[0]-n[0]*eps, m[1]-n[1]*eps)):
                errs.append(('junk', m)); break
    x0, y0, x1, y1 = bbox
    def on_output(p):
        for a, b in segs:
            L = math.dist(a, b); u = ((b[0]-a[0])/L, (b[1]-a[1])/L)
            s = (p[0]-a[0])*u[0] + (p[1]-a[1])*u[1]
            if -0.5 < s < L + 0.5 and abs((p[0]-a[0])*u[1] - (p[1]-a[1])*u[0]) < 0.5: return True
        return False
    y = y0
    while y < y1 and len(errs) < 3:
        x = x0; prev = ideal_inside((x, y))
        while x < x1:
            x2 = x + step; cur = ideal_inside((x2, y))
            if cur != prev:
                lo, hi = x, x2
                for _ in range(40):
                    mid = (lo + hi) / 2
                    if ideal_inside((mid, y)) == prev: lo = mid
                    else: hi = mid
                if not on_output(((lo+hi)/2, y)): errs.append(('gap', ((lo+hi)/2, y))); break
            prev, x = cur, x2
        y += step
    return errs

# ideal union: each wall's strip, mitered/extended exactly as an architect draws it,
# is hard to state generically; instead the reference is the union of strips extended by
# half their neighbours' width at shared nodes, clipped to the node's convex wedge.
# For the matrix we use explicit ideal regions below.
def strip_inside(p1, p2, th, pos, ext1=0.0, ext2=0.0):
    L = math.dist(p1, p2); u = ((p2[0]-p1[0])/L, (p2[1]-p1[1])/L); n = (-u[1], u[0])
    a, b = {'CENTER': (th/2, -th/2), 'LEFT': (th, 0), 'RIGHT': (0, -th)}[pos]
    def f(q):
        s = (q[0]-p1[0])*u[0] + (q[1]-p1[1])*u[1]; d = (q[0]-p1[0])*n[0] + (q[1]-p1[1])*n[1]
        return -ext1 <= s <= L + ext2 and b <= d <= a
    return f
def union(*fs): return lambda q: any(f(q) for f in fs)
def poly_inside(poly):
    def f(p):
        c = False; a = poly[-1]
        for b in poly:
            if (a[1] > p[1]) != (b[1] > p[1]) and p[0] < a[0] + (p[1]-a[1])*(b[0]-a[0])/(b[1]-a[1]): c = not c
            a = b
        return c
    return f

CASES = []
def case(name, walls, ideal=None, expect=None, bbox=None, regen=None):
    CASES.append((name, walls, ideal, expect, bbox, regen))

O = (0, 0)
# --- basic
case('basic 0deg C150', [wall(O, (3000, 0))], ideal=poly_inside([(0,75),(3000,75),(3000,-75),(0,-75)]),
     expect=[((0,75),(3000,75)), ((0,-75),(3000,-75)), ((0,75),(0,-75)), ((3000,75),(3000,-75))])
case('basic 90deg C150', [wall(O, (0, 3000))], ideal=poly_inside([(-75,0),(75,0),(75,3000),(-75,3000)]))
q45 = polar(O, 45, 3000)
case('basic 45deg C150', [wall(O, q45)], ideal=strip_inside(O, q45, 150, 'CENTER'))
# --- position (direction 0->3000 along +X): LEFT = body above (+Y)
case('pos LEFT', [wall(O, (3000, 0), 150, 'LEFT')], expect=[((0,150),(3000,150)), ((0,0),(3000,0))],
     ideal=poly_inside([(0,0),(3000,0),(3000,150),(0,150)]))
case('pos RIGHT', [wall(O, (3000, 0), 150, 'RIGHT')], expect=[((0,-150),(3000,-150)), ((0,0),(3000,0))],
     ideal=poly_inside([(0,0),(3000,0),(3000,-150),(0,-150)]))
case('pos LEFT reversed', [wall((3000, 0), O, 150, 'LEFT')], expect=[((0,-150),(3000,-150)), ((0,0),(3000,0))],
     ideal=poly_inside([(0,0),(3000,0),(3000,-150),(0,-150)]))
case('pos LEFT 135deg', [wall(O, polar(O,135,3000), 200, 'LEFT')], ideal=strip_inside(O, polar(O,135,3000), 200, 'LEFT'))
# --- continuous L (ideal = outer polygon of mitered corner)
def lpoly(p1, p2, p3, th):
    # ideal region: union of both strips extended to the miter; computed as polygon via offsets
    def off(a, b, d):
        L = math.dist(a, b); n = (-(b[1]-a[1])/L, (b[0]-a[0])/L); return (a[0]+n[0]*d, a[1]+n[1]*d), (b[0]+n[0]*d, b[1]+n[1]*d)
    def xl(s1, s2):
        (a, b), (c, d) = s1, s2
        r = (b[0]-a[0], b[1]-a[1]); s = (d[0]-c[0], d[1]-c[1]); den = r[0]*s[1]-r[1]*s[0]
        t = ((c[0]-a[0])*s[1]-(c[1]-a[1])*s[0]) / den; return (a[0]+r[0]*t, a[1]+r[1]*t)
    h = th/2
    L1, L2 = off(p1, p2, h), off(p2, p3, h); R1, R2 = off(p1, p2, -h), off(p2, p3, -h)
    return poly_inside([L1[0], xl(L1, L2), L2[1], R2[1], xl(R1, R2), R1[0]])
for ang in (90, 45, 135, 30):
    p2 = (3000, 0); p3 = polar(p2, ang, 2500)
    case(f'cont L {ang}deg turn', [wall(O, p2), wall(p2, p3)], ideal=lpoly(O, p2, p3, 150))
case('independent L (reversed dirs)', [wall((3000, 0), O), wall((3000, 0), (3000, 2500))], ideal=lpoly(O, (3000,0), (3000,2500), 150))
case('L 100+200', [wall(O, (3000, 0), 100), wall((3000, 0), (3000, 2500), 200)],
     ideal=poly_inside([(0,50),(2900,50),(2900,2500),(3100,2500),(3100,-50),(0,-50)]))
case('L LEFT+CENTER', [wall(O, (3000, 0), 150, 'LEFT'), wall((3000, 0), (3000, 2500), 200)],
     ideal=poly_inside([(0,150),(2900,150),(2900,2500),(3100,2500),(3100,0),(0,0)]))
# --- T (through wall along X, stem from below / above)
def T(th_thru, th_stem, above=False):
    y2 = 2500 if above else -2500
    return [wall((-3000, 0), (3000, 0), th_thru), wall((0, y2), (0, 0), th_stem)]
for tt, ts in ((150, 150), (100, 200), (200, 100)):
    for above in (False, True):
        sgn = 1 if above else -1
        case(f'T thru{tt} stem{ts} {"above" if above else "below"}', T(tt, ts, above),
             ideal=union(strip_inside((-3000,0),(3000,0),tt,'CENTER'), strip_inside((0,0),(0,2500*sgn),ts,'CENTER')),
             expect=[((-3000, -tt/2*sgn), (3000, -tt/2*sgn))])
case('T stem 60deg', [wall((-3000,0),(3000,0)), wall(polar(O,60,2500), O)],
     ideal=union(strip_inside((-3000,0),(3000,0),150,'CENTER'), lambda q: q[1] >= -75 and strip_inside(O,polar(O,60,2500),150,'CENTER', ext1=500)(q)))
case('T into LEFT host from strip side', [wall((-3000,0),(3000,0),200,'LEFT'), wall((0,2500),(0,0),150)],
     ideal=union(strip_inside((-3000,0),(3000,0),200,'LEFT'), strip_inside((0,0),(0,2500),150,'CENTER')))
# --- X
case('X 150x150', [wall((-3000,0),(3000,0)), wall((0,-3000),(0,3000))],
     ideal=union(strip_inside((-3000,0),(3000,0),150,'CENTER'), strip_inside((0,-3000),(0,3000),150,'CENTER')))
case('X 100x200', [wall((-3000,0),(3000,0),100), wall((0,-3000),(0,3000),200)],
     ideal=union(strip_inside((-3000,0),(3000,0),100,'CENTER'), strip_inside((0,-3000),(0,3000),200,'CENTER')))
case('X 45deg', [wall((-3000,0),(3000,0)), wall(polar(O,225,3000), polar(O,45,3000))],
     ideal=union(strip_inside((-3000,0),(3000,0),150,'CENTER'), strip_inside(polar(O,225,3000), polar(O,45,3000),150,'CENTER')))
# --- collinear
case('collinear 150+150', [wall((-3000,0),O), wall(O,(3000,0))], ideal=strip_inside((-3000,0),(3000,0),150,'CENTER'),
     expect=[((-3000,75),(0,75)), ((0,75),(3000,75))])
case('collinear head-to-head', [wall((-3000,0),O), wall((3000,0),O)], ideal=strip_inside((-3000,0),(3000,0),150,'CENTER'))
case('collinear 100+200', [wall((-3000,0),O,100), wall(O,(3000,0),200)],
     ideal=union(strip_inside((-3000,0),O,100,'CENTER'), strip_inside(O,(3000,0),200,'CENTER')), expect=[((0,50),(0,100)), ((0,-50),(0,-100))])
case('collinear LEFT+CENTER', [wall((-3000,0),O,150,'LEFT'), wall(O,(3000,0),150)],
     ideal=union(strip_inside((-3000,0),O,150,'LEFT'), strip_inside(O,(3000,0),150,'CENTER')))
# --- node with 3 ends (T made of endpoints), 4 ends (X of endpoints)
case('endpoint T (3 ends)', [wall((-3000,0),O), wall(O,(3000,0)), wall(O,(0,-2500))],
     ideal=union(strip_inside((-3000,0),(3000,0),150,'CENTER'), strip_inside(O,(0,-2500),150,'CENTER')))
case('endpoint X (4 ends)', [wall((-3000,0),O), wall(O,(3000,0)), wall(O,(0,-2500)), wall((0,2500),O)],
     ideal=union(strip_inside((-3000,0),(3000,0),150,'CENTER'), strip_inside((0,2500),(0,-2500),150,'CENTER')))
# --- multi-wall: cross at (0,0), L at (0,-2000)
mw = [wall((-3000,0),(4000,0)), wall((0,2000),(0,-2000)), wall((0,-2000),(3000,-2000))]
case('multi-wall', mw, ideal=union(strip_inside((-3000,0),(4000,0),150,'CENTER'), strip_inside((0,2000),(0,-2000),150,'CENTER',ext2=75),
     strip_inside((0,-2000),(3000,-2000),150,'CENTER',ext1=75)))
# --- closed rectangle (Close) mixed LEFT
rect = [(0,0),(4000,0),(4000,3000),(0,3000)]
case('closed rect LEFT (CCW, inside)', [wall(rect[i], rect[(i+1)%4], 200, 'LEFT') for i in range(4)],
     ideal=lambda q: 0 <= q[0] <= 4000 and 0 <= q[1] <= 3000 and not (200 < q[0] < 3800 and 200 < q[1] < 2800))
# --- rebuild scope: only regen walls emitted; context wall linework must not be duplicated
case('regen subset (context wall not emitted)', [wall(O,(3000,0)), wall((3000,0),(3000,2500))], regen=[0],
     ideal=None, expect=[((0,-75),(3075,-75)), ((0,75),(0,-75))])

fails = 0
svg = []
col = 0
for idx, (name, walls, ideal, expect, bbox, regen) in enumerate(CASES):
    segs = solve(walls, regen)
    pts = [p for w in walls for p in w[:2]]
    x0 = min(p[0] for p in pts) - 400; x1 = max(p[0] for p in pts) + 400
    y0 = min(p[1] for p in pts) - 400; y1 = max(p[1] for p in pts) + 400
    errs = []
    if ideal and regen is None: errs += check_outline(segs, ideal, (x0, y0, x1, y1))
    for a, b in (expect or []):
        if not has(segs, a, b): errs.append(('missing', (a, b)))
    status = 'PASS' if not errs else 'FAIL'
    if errs: fails += 1
    print(f'{status}  {name:42s} lines={len(segs)} {errs[:2] if errs else ""}')
    # svg tile
    cx, cy = (idx % 5) * 320, (idx // 5) * 340
    sc = 280 / max(x1-x0, y1-y0)
    tr = lambda p: (cx + 20 + (p[0]-x0)*sc, cy + 40 + (y1-p[1])*sc)
    svg.append(f'<text x="{cx+20}" y="{cy+25}" font-size="11" fill="{"green" if not errs else "red"}">{name}</text>')
    for a, b, *_ in walls:
        (ax, ay), (bx, by) = tr(a), tr(b)
        svg.append(f'<line x1="{ax}" y1="{ay}" x2="{bx}" y2="{by}" stroke="red" stroke-width="0.6" stroke-dasharray="4,2"/>')
    for a, b in segs:
        (ax, ay), (bx, by) = tr(a), tr(b)
        svg.append(f'<line x1="{ax}" y1="{ay}" x2="{bx}" y2="{by}" stroke="black" stroke-width="1"/>')
out = sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, 'results.svg')
rows = (len(CASES) + 4) // 5
open(out, 'w').write(f'<svg xmlns="http://www.w3.org/2000/svg" width="1600" height="{rows*340}" style="background:white"><rect width="100%" height="100%" fill="white"/>' + ''.join(svg) + '</svg>')

# --- non-geometry checks: config, grid, reconstruction round-trip
import tempfile, io, contextlib
def chk(name, ok):
    global fails
    print(('PASS  ' if ok else 'FAIL  ') + name)
    if not ok: fails += 1
cfgdir = tempfile.mkdtemp()
def cfg_run(text, name='WallTool.txt'):
    A.G[A.Sym('*WT:CFG*')] = None
    A.FINDFILE.pop('WallTool.txt', None); A.FINDFILE.pop('WallTool.cfg', None)
    if text is not None:
        f = os.path.join(cfgdir, name); open(f, 'w').write(text); A.FINDFILE[name] = f
    A.ev('(wt:cfg-load)')
    return lambda k: A.ev(f'(wt:cfg "{k}")')
c = cfg_run(open(os.path.join(HERE, '..', 'WallTool.txt')).read())
chk('config: shipped file parses', c('DEFAULT_THICKNESS') == 150.0 and c('AXIS_PLOT') == 0 and c('GRID_COLOR') == 8 and len(c('THICKNESSES')) == 7)
c = cfg_run(None)
chk('config: missing file -> defaults', c('WALL_LAYER') == 'A-WALL' and c('AXIS_COLOR') == 1)
c = cfg_run('DEFAULT_THICKNESS=-5\nWALL_COLOR=999\nAXIS_LINETYPE=NOPE_LT\nAXIS_LAYER=MY AXES\nTHICKNESSES=100,abc\nDEFAULT_POSITION=left\nAXIS_PLOT=maybe\nTEXT_HEIGHT=0\nWALL_LAYER=\n')
chk('config: bad values fall back individually',
    c('DEFAULT_THICKNESS') == 150.0 and c('WALL_COLOR') == 7 and c('AXIS_LAYER') == 'MY AXES' and c('THICKNESSES')[0] == 75.0
    and c('DEFAULT_POSITION') == 'LEFT' and c('AXIS_PLOT') == 0 and c('TEXT_HEIGHT') == 350.0 and c('WALL_LAYER') == 'A-WALL'
    and c('AXIS_LINETYPE') == 'NOPE_LT')   # linetype existence is checked at layer creation (falls back to Continuous)
chk('numlist validation', A.ev('(wt:numlist "3000, 4000,4000,3000")') == [3000.0, 4000.0, 4000.0, 3000.0]
    and A.ev('(wt:numlist "3000,0")') is None and A.ev('(wt:numlist "3000,-1")') is None and A.ev('(wt:numlist "30a0")') is None
    and A.ev('(wt:numlist "3000,,4000")') is None)
ax = A.ev('(wt:grid-axes (list 3000.0 4000.0 4000.0 3000.0) (list 5000.0 5000.0 6000.0) 500.0)')
nums = [a for a in ax if a[2].isdigit()]; lets = [a for a in ax if not a[2].isdigit()]
chk('ZXW: 5 numbered + 4 lettered axes', len(nums) == 5 and len(lets) == 4)
chk('ZXW: cumulative positions', [a[0][0] for a in nums] == [0, 3000, 7000, 11000, 14000] and [a[0][1] for a in lets] == [0, 5000, 10000, 16000])
chk('ZXW: labels 1..5, A..D, extents', [a[2] for a in nums] == ['1','2','3','4','5'] and [a[2] for a in lets] == ['A','B','C','D']
    and nums[0][0][1] == -500 and nums[0][1][1] == 16500 and lets[0][0][0] == -500 and lets[0][1][0] == 14500)
chk('ZXW: alpha beyond Z', [A.ev(f'(wt:grid-alpha {n})') for n in (1, 26, 27, 28, 52, 53, 702, 703)] == ['A','Z','AA','AB','AZ','BA','ZZ','AAA'])
A.G[A.Sym('*WT:REG*')] = None
def recon_case(walls):
    segs = solve(walls)
    faces = '(list ' + ' '.join(f'(list nil {P(*a)} {P(*b)})' for a, b in segs) + ')'
    got = []
    for i, (a, b, th, pos) in enumerate(walls):
        r = A.ev(f'(wt:recon (list {i} {P(*a)} {P(*b)}) {faces})')
        owned = sum(1 for sa, sb in segs if A.ev(f'(wt:owned-line-p (list nil {P(*sa)} {P(*sb)}) (list nil {P(*a)} {P(*b)} {float(th)} "{pos}"))'))
        got.append((r and (round(r[3], 3), r[4]), owned))
    return segs, got
segs, got = recon_case([wall(O, (3000, 0), 200), wall((3000, 0), (3000, 2500), 150), wall((0, -3000), (0, 3000), 100)])
chk('recon: centered masters -> thickness recovered as CENTER', [g[0] for g in got] == [(200, 'CENTER'), (150, 'CENTER'), (100, 'CENTER')])
segs = solve([wall(O, (3000, 0), 200, 'LEFT')])
faces = '(list ' + ' '.join(f'(list nil {P(*a)} {P(*b)})' for a, b in segs) + ')'
chk('recon: legacy off-centre master (on a face) is NOT recognised (WWR migrates it)', A.ev(f'(wt:recon (list 0 {P(0,0)} {P(3000,0)}) {faces})') is None)
chk('recon: plain AX line (no faces) is not a wall', A.ev(f'(wt:recon (list 0 {P(0,5000)} {P(3000,5000)}) (list))') is None)
_, got = recon_case([wall(O, (3000, 0))])
chk('owned lines: free wall claims all 4 lines', got[0][1] == 4)

c = cfg_run('DEFAULT_THICKNESS=222\n', name='WallTool.cfg')
chk('config: legacy WallTool.cfg still read when no .txt', c('DEFAULT_THICKNESS') == 222.0)

# =========================== Stage 1.1 (fake drawing DB) ===========================
cfg_run(None)
EPS = 1e-3
def lisp_walls(ws): return '(list ' + ' '.join(f'(list nil {P(*a)} {P(*b)} {float(t)} "{pos}")' for a, b, t, pos in ws) + ')'
def expected(ws):
    r = A.ev(f'(wt:linework-merged {lisp_walls(ws)} (list {" ".join(map(str, range(len(ws))))}))')
    return [((x[0][0], x[0][1]), (x[1][0], x[1][1])) for x in (r or [])]
def same_lines(got, exp):
    got = [(a, b) for _, a, b in got] if got and len(got[0]) == 3 else list(got)
    rest = list(exp)
    for a, b in got:
        k = next((i for i, (c, d) in enumerate(rest) if (near(a, c) and near(b, d)) or (near(a, d) and near(b, c))), None)
        if k is None: return False
        rest.pop(k)
    return not rest
def fresh(th=150.0, pos='CENTER'):
    A.db_reset(); A.ev(f'(setq *wt:reg* nil *wt:pending* nil *wt:thk* {float(th)} *wt:pos* "{pos}")')
def settings(th, pos): A.ev(f'(setq *wt:thk* {float(th)} *wt:pos* "{pos}")')
def add(*segs): return A.ev('(wt:walls-add (list ' + ' '.join(f'(list {P(*a)} {P(*b)})' for a, b in segs) + '))')
def mkline(a, b, layer): A.ev(f'(entmake (list (cons 0 "LINE") (cons 8 "{layer}") (cons 10 (list {float(a[0])} {float(a[1])} 0.0)) (cons 11 (list {float(b[0])} {float(b[1])} 0.0))))'); return A.LAST[0]
def walls_now(): return A.db_lines('A-WALL')
def masters_now(): return A.db_lines('X-AXIS')
def find_line(layer, a, b):
    return next(e for e, c, d in A.db_lines(layer) if (near(a, c) and near(b, d)) or (near(a, d) and near(b, c)))
def at_face(pt):
    return next(e for e, c, d in A.db_lines('A-WALL') if A.ev(f'(wt:on-seg {P(*pt)} {P(*c)} {P(*d)})'))
def snapshot(): return sorted((round(a[0],3), round(a[1],3), round(b[0],3), round(b[1],3)) for _, a, b in walls_now() + masters_now())

# incremental WW == full solve (chain L, then T, then X through it)
fresh()
add(((0,0),(4000,0))); add(((4000,0),(4000,3000)))
settings(100, 'CENTER'); add(((2000,0),(2000,-2500)))
settings(200, 'LEFT'); add(((-1000,1500),(5000,1500)))
full = [wall((0,0),(4000,0)), wall((4000,0),(4000,3000)), wall((2000,0),(2000,-2500),100), wall((-1000,1500),(5000,1500),200,'LEFT')]
chk('WW incremental rebuild == full solve (L, T, X, mixed width/ecc)', same_lines(walls_now(), expected(full)) and len(masters_now()) == 7)   # normalized at T/X nodes
A.ev('(setq *wt:reg* nil)'); settings(150, 'CENTER'); add(((0,0),(0,2000)))
chk('WW rebuild via reconstruction (new session) == full solve', same_lines(walls_now(), expected(full + [wall((0,0),(0,2000))])))
before = snapshot(); rec = add(((4000,3000),(0,3000))); A.ev('(wt:seg-undo *rec*)'.replace('*rec*', '(quote ' + '0' + ')')) if False else None
A.G[A.Sym('*T-REC*')] = rec; A.ev('(wt:seg-undo *t-rec*)')
chk('WW Undo restores exact previous drawing', snapshot() == before)

# Rectangle mode: four masters, CCW, LEFT = inside
fresh(200, 'LEFT')
add(((0,0),(4000,0)), ((4000,0),(4000,3000)), ((4000,3000),(0,3000)), ((0,3000),(0,0)))
rect = [wall((0,0),(4000,0),200,'LEFT'), wall((4000,0),(4000,3000),200,'LEFT'), wall((4000,3000),(0,3000),200,'LEFT'), wall((0,3000),(0,0),200,'LEFT')]
inside = all(-EPS <= p[0] <= 4000+EPS and -EPS <= p[1] <= 3000+EPS for _, a, b in walls_now() for p in (a, b))
chk('Rectangle: 4 masters, 8 face lines, no caps, LEFT inside', len(masters_now()) == 4 and len(walls_now()) == 8 and inside and same_lines(walls_now(), expected(rect)))
fresh(); rec = add(((0,0),(4000,0)), ((4000,0),(4000,3000)), ((4000,3000),(0,3000)), ((0,3000),(0,0)))
A.G[A.Sym('*T-REC*')] = rec; A.ev('(wt:seg-undo *t-rec*)')
chk('Rectangle: one Undo removes all of it', not walls_now() and not masters_now())

# face picking -> master point
fresh(); add(((0,0),(3000,0))); mkline((0,5000), (3000,5000), 'A-WALL')
def pick(pt):
    r = A.ev(f'(wt:pick-to-master {P(*pt)} (wt:net-scan))')
    return None if r is None else (r[0], r[1])
chk('pick: face midspan -> perpendicular foot on master', near(pick((1000,75)), (1000,0)) and near(pick((1000,-75)), (1000,0)))
chk('pick: face endpoint / cap -> master endpoint', near(pick((3000,75)), (3000,0)) and near(pick((3000,-20)), (3000,0)))
chk('pick: master / free space unchanged', near(pick((1000,0)), (1000,0)) and near(pick((500,700)), (500,700)))
chk('pick: unrecognised A-WALL line -> nil (reprompt)', pick((1000,5000)) is None)
res = []
for pt in ((1500,75), (1500,-75), (1500,0)):
    fresh(); add(((0,0),(3000,0)))
    q = pick(pt); add((q, (1500,2500))) if q[1] > -1 else None
    res.append(snapshot())
chk('WW connection identical for Face 1 / Face 2 / Master picks', res[0] == res[1] == res[2]
    and same_lines(walls_now(), expected([wall((0,0),(3000,0)), wall((1500,0),(1500,2500))])))

# EW
def ew(*enames, pts=None):
    items = [[e, list((pts or {}).get(e)) if (pts or {}).get(e) else None, 'LINE'] for e in enames]
    A.G[A.Sym('*T-SEL*')] = items; return A.ev('(wt:ew-erase *t-sel* 0.5)')
results = []
for which in ('face1', 'master', 'face2'):
    fresh(); add(((0,0),(3000,0))); add(((5000,0),(5000,3000)))
    e = {'face1': lambda: find_line('A-WALL', (0,75), (3000,75)), 'face2': lambda: find_line('A-WALL', (0,-75), (3000,-75)),
         'master': lambda: find_line('X-AXIS', (0,0), (3000,0))}[which]()
    ew(e); results.append(snapshot())
chk('EW free wall: face / master / other face delete the same wall', results[0] == results[1] == results[2] and len(masters_now()) == 1 and len(walls_now()) == 4)
fresh(); add(((-3000,0),(3000,0))); add(((0,0),(0,2500)))
ew(at_face((75, 1000)))
chk('EW T repair: through wall continuous, 4 lines (merged), two spans left', len(walls_now()) == 4 and len(masters_now()) == 2
    and same_lines(walls_now(), expected([wall((-3000,0),(3000,0))])))
fresh(); add(((0,0),(3000,0))); add(((3000,0),(3000,2500)))
ew(find_line('X-AXIS', (3000,0), (3000,2500)))
chk('EW L repair: remaining wall gets free cap', same_lines(walls_now(), expected([wall((0,0),(3000,0))])) and len(walls_now()) == 4)
fresh(); add(((-3000,0),(0,0))); add(((0,0),(3000,0))); add(((0,-2500),(0,0))); add(((0,0),(0,2500)))
A.ev('(setq *wt:reg* nil)')   # force reconstruction path
ew(at_face((75, 1500)))
chk('EW X (4 ends) repair via reconstruction -> T', same_lines(walls_now(), expected([wall((-3000,0),(0,0)), wall((0,0),(3000,0)), wall((0,-2500),(0,0))])))
fresh(); add(((-3000,0),(3000,0))); add(((0,-3000),(0,3000)))
before = snapshot(); A.ev('(wt:pend-begin)')
w = A.ev(f'(wt:wall-from-entity (quote nil) (wt:net-scan))')
A.G[A.Sym('*T-E*')] = find_line('X-AXIS', (0,0), (0,3000))
A.G[A.Sym('*T-W*')] = A.ev('(wt:wall-from-entity *t-e* (wt:net-scan))')
A.ev('(wt:rebuild nil (list *t-w*))'); A.ev('(wt:seg-undo *wt:pending*)')
chk('EW rollback record restores drawing exactly', snapshot() == before)
ew(find_line('X-AXIS', (0,-3000), (0,0)), find_line('X-AXIS', (0,0), (0,3000)))
chk('EW X crossing: remaining wall continuous', same_lines(walls_now(), expected([wall((-3000,0),(3000,0))])) and len(walls_now()) == 4)
fresh(); add(((0,0),(3000,0))); add(((3000,0),(3000,2500))); add(((3000,2500),(0,2500)))
ew(find_line('A-WALL', (0,75), (2925,75)), find_line('X-AXIS', (3000,2500), (0,2500)), find_line('A-WALL', (0,-75), (3075,-75)))
chk('EW multi-select: two walls, duplicates collapsed', same_lines(walls_now(), expected([wall((3000,0),(3000,2500))])) and len(masters_now()) == 1)
fresh(); mkline((0,5000),(3000,5000),'A-WALL'); before = snapshot()
chk('EW unrecognised A-WALL line: nothing erased', ew(find_line('A-WALL', (0,5000), (3000,5000))) is None and snapshot() == before)

def normalized_ok():
    ms = [(a, b) for _, a, b in masters_now()]
    for i, (a, b) in enumerate(ms):
        for j, (c, d) in enumerate(ms):
            if i != j:
                for p in (c, d):
                    if A.ev(f'(wt:in-seg {P(*p)} {P(*a)} {P(*b)})'): return False
    return True
# XW
def xw(lines, th=150.0, pos='CENTER'):
    settings(th, pos); A.G[A.Sym('*T-SEL*')] = lines; return A.ev('(wt:xw-convert *t-sel*)')
for name, segs in (('X', [((-3000,0),(3000,0)), ((0,-3000),(0,3000))]),
                   ('L', [((0,0),(3000,0)), ((3000,2500),(3000,0))]),
                   ('T', [((-3000,0),(3000,0)), ((0,0),(0,2500))]),
                   ('mixed angles', [((0,0),(3000,0)), ((3000,0), polar((3000,0),60,2500)), (polar((3000,0),60,2500), (-500,1800)), ((1000,-1500), polar((1000,-1500),73,4000))])):
    fresh(); ids = [mkline(a, b, '0') for a, b in segs]
    xw(ids, 200, 'RIGHT')
    chk(f'XW {name}: sources moved to X-AXIS, normalized, one network == full solve',
        len(A.db_lines('0')) == 0 and normalized_ok()
        and same_lines(walls_now(), expected([wall(a, b, 200, 'RIGHT') for a, b in segs])))
fresh(); add(((0,0),(0,3000))); ids = [mkline((-2000,1500), (0,1500), '0')]
xw(ids)
chk('XW connects to existing WW wall', same_lines(walls_now(), expected([wall((0,0),(0,3000)), wall((-2000,1500),(0,1500))])))
fresh(); mkline((0,0),(3000,0),'X-AXIS'); src = mkline((3000,0),(0,0),'0')
xw([src])
chk('XW source duplicating an X-AXIS axis: one master only', len(masters_now()) == 1 and len(A.db_lines('0')) == 0 and len(walls_now()) == 4)
before = snapshot(); xw([find_line('X-AXIS', (0,0), (3000,0))])
chk('XW on an existing wall: skipped, unchanged', snapshot() == before)
fresh(); ax = mkline((0,0),(3000,0),'X-AXIS'); xw([ax, ax])
chk('XW same line twice: converted once', len(masters_now()) == 1 and len(walls_now()) == 4)

# generic merge helper
m = A.ev(f'(wt:seg-merge (list (list {P(0,0)} {P(1000,0)}) (list {P(2000,0)} {P(1000,0)}) (list {P(2000,0)} {P(2000,500)})) (quote (lambda (s) t)))')
chk('seg-merge: collinear contiguous joined, corner kept', len(m) == 2)
m = A.ev(f'(wt:seg-merge (list (list {P(0,0)} {P(1000,0)}) (list {P(1000,0)} {P(2000,0)})) (quote (lambda (s) nil)))')
chk('seg-merge: veto respected', len(m) == 2)
m = A.ev(f'(wt:seg-merge (list (list {P(0,0)} {P(1000,0)}) (list {P(1200,0)} {P(2000,0)})) (quote (lambda (s) t)))')
chk('seg-merge: gap not bridged', len(m) == 2)

# =========================== Stage 1.2: master identity, pick disambiguation, PickFirst ===========================
def mk(a, b): return tuple(sorted([tuple(round(v, 3) for v in a), tuple(round(v, 3) for v in b)]))
def master_set(): return sorted(mk(a, b) for _, a, b in masters_now())
def build(ws):
    fresh()
    for a, b, t, pos in ws: settings(t, pos); add((a, b))

def spans(): return [(a, b) for _, a, b in masters_now()]
def ew_span(a, b, via='master', off=75.0):
    if via == 'master': return ew(find_line('X-AXIS', a, b))
    L_ = math.dist(a, b); n_ = (-(b[1]-a[1])/L_*off, (b[0]-a[0])/L_*off)
    q = (a[0]+(b[0]-a[0])*0.5 + n_[0], a[1]+(b[1]-a[1])*0.5 + n_[1])
    e = next(x for x, c, d in A.db_lines('A-WALL') if A.ev(f'(wt:on-seg {P(*q)} {P(*c)} {P(*d)})'))
    return ew(e, pts={e: q})
def lines_match_masters(): return same_lines(walls_now(), expected([wall(a, b) for a, b in spans()]))
def undo_rec(rec): A.G[A.Sym('*T-REC*')] = rec; A.ev('(wt:seg-undo *t-rec*)')
import math

# 1. long wall -> later T -> 3 spans
build([wall((0,0),(10000,0))]); rec = add(((5000,0),(5000,4000)))
chk('normalize: long wall + later T -> 3 master spans', master_set() == sorted([mk((0,0),(5000,0)), mk((5000,0),(10000,0)), mk((5000,0),(5000,4000))])
    and normalized_ok() and lines_match_masters())
t_state = snapshot()
# 10. WW Undo restores the original unsplit master
undo_rec(rec)
chk('normalize: WW Undo restores the original unsplit master exactly', master_set() == [mk((0,0),(10000,0))] and len(walls_now()) == 4)
# 2-4. EW each arm, by master and by face pick
for label, arm, survive in (('left arm -> L', ((0,0),(5000,0)), [((5000,0),(10000,0)), ((5000,0),(5000,4000))]),
                            ('right arm -> mirrored L', ((5000,0),(10000,0)), [((0,0),(5000,0)), ((5000,0),(5000,4000))]),
                            ('branch -> straight wall', ((5000,0),(5000,4000)), [((0,0),(5000,0)), ((5000,0),(10000,0))])):
    ok = True
    for via in ('master', 'face'):
        build([wall((0,0),(10000,0))]); add(((5000,0),(5000,4000)))
        ew_span(*arm, via=via)
        ok = ok and master_set() == sorted(mk(*x) for x in survive) and lines_match_masters()
    chk(f'EW T {label} (by master and by face)', ok)
build([wall((0,0),(10000,0))]); add(((5000,0),(5000,4000))); ew_span((5000,0),(5000,4000))
chk('EW T branch: straight wall faces merged into single LINEs (4 lines)', len(walls_now()) == 4)

# 5-6. long + long X -> 4 spans; each arm independently
arms = [((0,0),(5000,0)), ((5000,0),(10000,0)), ((5000,-4000),(5000,0)), ((5000,0),(5000,4000))]
build([wall((0,0),(10000,0))]); add(((5000,-4000),(5000,4000)))
chk('normalize: long + long X -> 4 master spans', master_set() == sorted(mk(*x) for x in arms) and lines_match_masters())
ok = True
for k, arm in enumerate(arms):
    for via in ('master', 'face'):
        build([wall((0,0),(10000,0))]); add(((5000,-4000),(5000,4000)))
        ew_span(*arm, via=via)
        ok = ok and master_set() == sorted(mk(*x) for i, x in enumerate(arms) if i != k) and lines_match_masters()
chk('EW X: each of the four arms removable independently (by master and by face)', ok)

# 7. opposite command order gives the same normalized network
build([wall((5000,0),(5000,4000))]); add(((0,0),(10000,0)))
order_b = snapshot()
chk('normalize: branch first, long wall later == long first, branch later', order_b == t_state)
build([wall((5000,-4000),(5000,4000))]); add(((0,0),(10000,0))); xb = snapshot()
build([wall((0,0),(10000,0))]); add(((5000,-4000),(5000,4000)))
chk('normalize: X in either order gives identical drawing', snapshot() == xb)

# 8-9, 11. XW normalization + undo
fresh(); ids = [mkline((0,0),(10000,0),'0'), mkline((5000,0),(5000,4000),'0')]
xw(ids)
chk('XW T normalization -> 3 spans', master_set() == sorted([mk((0,0),(5000,0)), mk((5000,0),(10000,0)), mk((5000,0),(5000,4000))]) and lines_match_masters())
fresh(); ids = [mkline((0,0),(10000,0),'0'), mkline((5000,-4000),(5000,4000),'0')]
xw(ids)
chk('XW X normalization -> 4 spans', master_set() == sorted(mk(*x) for x in arms) and lines_match_masters())
# XW's transaction (layer modify + split + rebuild) replayed step by step, then rolled back
fresh(); ids = [mkline((0,0),(10000,0),'0'), mkline((5000,0),(5000,4000),'0')]
src_before = {e: list(A.DB[e]) for e in ids}
A.ev('(wt:pend-begin)'); settings(150, 'CENTER')
for e in ids:
    A.G[A.Sym('*T-E*')] = e; A.ev('(wt:pend-modify (wt:bylayer (entget *t-e*) "X-AXIS"))')
A.G[A.Sym('*T-NEW*')] = [[e, (0.0,0.0), None, 150.0, 'CENTER'] for e in ids]
A.ev('(setq *t-new* (mapcar (quote (lambda (w / d) (setq d (entget (car w))) (list (car w) (wt:pt2 (cdr (assoc 10 d))) (wt:pt2 (cdr (assoc 11 d))) 150.0 "CENTER"))) *t-new*))')
A.ev('(wt:rebuild *t-new* nil)')
split_ok = len(masters_now()) == 3
A.ev('(wt:seg-undo *wt:pending*)')
chk('XW Undo restores original source LINEs (layer 0, unsplit, no walls)',
    split_ok and not masters_now() and not walls_now() and all(e not in A.DELETED and A.DB[e] == src_before[e] for e in ids))

# 12. reload: registry cleared, split masters still reconstructed and EW works
build([wall((0,0),(10000,0),200,'LEFT')]); settings(150, 'CENTER'); add(((5000,100),(5000,4000)))   # LEFT 200 -> centerline y=100
A.ev('(setq *wt:reg* nil)')
net_ok = all(A.ev(f'(wt:recon (list nil {P(*a)} {P(*b)}) (cadr (wt:net-scan)))') for a, b in [((0,100),(5000,100)), ((5000,100),(10000,100))])
rec_l = A.ev(f'(wt:recon (list nil {P(0,100)} {P(5000,100)}) (cadr (wt:net-scan)))')
ew_span((5000,100),(10000,100), via='face', off=100.0)
chk('reload: centered split spans reconstruct as 200 CENTER; EW right arm leaves 「, same physical wall as old LEFT model',
    net_ok and rec_l and round(rec_l[3]) == 200 and rec_l[4] == 'CENTER'
    and master_set() == sorted([mk((0,100),(5000,100)), mk((5000,100),(5000,4000))])
    and same_lines(walls_now(), expected([wall((0,100),(5000,100),200), wall((5000,100),(5000,4000))]))
    and same_lines(walls_now(), expected([wall((0,0),(5000,0),200,'LEFT'), wall((5000,0),(5000,4000))])))

# pick near the junction on the branch face, but AutoCAD handed us the through wall's face entity
build([wall((0,0),(10000,0))]); add(((5000,0),(5000,4000)))
q = (4925.2, 140)
e = find_line('A-WALL', (0,75), (4925,75))
A.G[A.Sym('*T-Q*')] = [q[0], q[1]]; A.G[A.Sym('*T-E*')] = e
r = A.ev('(wt:ew-resolve *t-e* *t-q* (wt:net-scan) 0.5)')
chk('pick point wins over the entity AutoCAD picked (resolves the branch span)',
    r[0] == 'OK' and mk(r[1][1], r[1][2]) == mk((5000,0),(5000,4000)))
A.G[A.Sym('*T-Q*')] = [4925.0, 75.0]
r = A.ev('(wt:ew-resolve *t-e* *t-q* (wt:net-scan) 0.5)')
chk('pick exactly on a junction corner -> AMBIG, nothing erased', r[0] == 'AMBIG')

# 「 / partial networks, mirrored/rotated: deleting any span keeps all others exactly
def xf(p, k, rot):
    x, y = p
    if k: x = -x
    c, s_ = math.cos(math.radians(rot)), math.sin(math.radians(rot))
    return (x*c - y*s_, x*s_ + y*c)
gamma = [((0,0),(5000,0)), ((5000,0),(5000,5000)), ((5000,2500),(8000,2500))]
legs = [((0,0),(10000,0)), ((5000,0),(5000,4000)), ((5000,0),(5000,-3000)), ((5000,0),(2000,-3000))]
for label, base in (('gamma', gamma), ('long+legs', legs)):
    for mirror, rot in ((False, 0), (True, 0), (False, 90), (True, 37)):
        segs = [(xf(a, mirror, rot), xf(b, mirror, rot)) for a, b in base]
        build([wall(a, b) for a, b in segs]); all_spans = spans()
        allok = normalized_ok()
        for k, sp_ in enumerate(all_spans):
            for via in ('master', 'face'):
                build([wall(a, b) for a, b in segs])
                ew_span(*sp_, via=via)
                allok = allok and master_set() == sorted(mk(*x) for i, x in enumerate(all_spans) if i != k) and lines_match_masters()
        chk(f'{label} mirror={mirror} rot={rot}: deleting any one span (by master or face) keeps all others exactly', allok)

# PickFirst: EW
def run_cmd(name, pickfirst, kw=()):
    A.SSFIRST[0] = list(pickfirst); A.KWQUEUE[:] = list(kw)
    A.ev(f'(c:{name})')
build([wall((0,0),(3000,0)), wall((5000,0),(5000,3000))])
f1 = find_line('A-WALL', (0,75), (3000,75)); f2 = find_line('A-WALL', (0,-75), (3000,-75)); m = find_line('X-AXIS', (0,0), (3000,0))
run_cmd('EW', [f1, m, f2])
chk('EW PickFirst: Face 1 + Master + Face 2 -> exactly one wall erased', master_set() == [mk((5000,0),(5000,3000))] and len(walls_now()) == 4)
build([wall((0,0),(3000,0)), wall((0,2000),(3000,2000)), wall((0,4000),(3000,4000)), wall((5000,0),(5000,3000))])
run_cmd('EW', [find_line('X-AXIS', (0,0), (3000,0)), find_line('A-WALL', (0,2075), (3000,2075)), find_line('A-WALL', (0,3925), (3000,3925))])
chk('EW PickFirst: three walls -> exactly those three masters removed', master_set() == [mk((5000,0),(5000,3000))])
build([wall((0,0),(3000,0))])
circ = A.ev('(progn (entmake (list (cons 0 "CIRCLE") (cons 8 "0") (cons 10 (list 0.0 0.0 0.0)) (cons 40 5.0))) (entlast))')
run_cmd('EW', [find_line('X-AXIS', (0,0), (3000,0)), circ])
chk('EW PickFirst: unsupported object ignored, not changed', not masters_now() and A.entget(circ) is not None)
build([wall((0,0),(3000,0)), wall((3000,0),(6000,0))])   # collinear pair: the joint cap line is claimed by neither
build([wall((0,0),(3000,0)), wall((0,0),(0,3000))])
before = snapshot()
fake = mkline((0,75), (0,75.5), 'A-WALL')                  # tiny stray A-WALL line nobody owns
run_cmd('EW', [fake])
chk('EW PickFirst: unidentified A-WALL line skipped, nothing erased', snapshot()[:] == sorted(before + [(0.0,75.0,0.0,75.5)]))

# PickFirst: XW (4 lines, and mixed with CIRCLE + TEXT)
fresh(); ids = [mkline(a, b, '0') for a, b in (((0,0),(4000,0)), ((4000,0),(4000,3000)), ((4000,3000),(0,3000)), ((0,3000),(0,0)))]
run_cmd('XW', ids)
chk('XW PickFirst: 4 lines converted without another prompt', len(masters_now()) == 4 and len(A.db_lines('0')) == 0 and len(walls_now()) == 8)
fresh(); ids = [mkline(a, b, '0') for a, b in (((0,0),(4000,0)), ((4000,0),(4000,3000)), ((0,6000),(3000,6000)))]
circ = A.ev('(progn (entmake (list (cons 0 "CIRCLE") (cons 8 "0") (cons 10 (list 0.0 0.0 0.0)) (cons 40 5.0))) (entlast))')
txt = A.ev('(progn (entmake (list (cons 0 "TEXT") (cons 8 "0") (cons 10 (list 0.0 0.0 0.0)) (cons 40 5.0) (cons 1 "x"))) (entlast))')
cd, td = list(A.DB[circ]), list(A.DB[txt])
out = io.StringIO()
A.OUTPUT = True
with contextlib.redirect_stdout(out): run_cmd('XW', ids + [circ, txt])
A.OUTPUT = False
chk('XW PickFirst mixed: 3 axes converted, 2 unsupported ignored and unchanged',
    len(masters_now()) == 3 and A.DB[circ] == cd and A.DB[txt] == td
    and '3 axis line(s) converted' in out.getvalue() and '2 unsupported object(s) ignored' in out.getvalue())

# root cause regression: a cap aligned with another wall's end station must not be claimed by that wall
fresh()
settings(150, 'RIGHT'); add(((3000,0),(-3000,0)))
settings(150, 'CENTER'); add(((0,9000),(3000,9000)))          # isolated; its right cap lies on x=3000 like wall 1's start
settings(200, 'CENTER'); add(((0,75),(0,-3000)))               # T into wall 1 (centerline y=75) -> wall 1 regenerated
ws = [wall((3000,0),(-3000,0),150,'RIGHT'), wall((0,9000),(3000,9000)), wall((0,0),(0,-3000),200)]   # old eccentric model, same physical walls
chk('ownership: rebuilding a wall never erases an aligned cap of an unrelated wall', same_lines(walls_now(), expected(ws)))
ew(find_line('X-AXIS', (3000,75), (0,75)), find_line('X-AXIS', (0,75), (-3000,75)))   # T split it into two spans
chk('ownership: EW on that wall leaves the unrelated wall intact', same_lines(walls_now(), expected([ws[1], wall((0,75),(0,-3000),200)])) and len(masters_now()) == 2)

# =========================== WWF (was WWO) ===========================
def face_pick(w, side, t_frac=0.5):
    """pick point on the given side face of wall record (a,b,th,pos), slightly off the line like a real click"""
    a, b, th, pos = w
    L_ = math.dist(a, b); u = ((b[0]-a[0])/L_, (b[1]-a[1])/L_); n = (-u[1], u[0])
    off = {'CENTER': (th/2, -th/2), 'LEFT': (th, 0.0), 'RIGHT': (0.0, -th)}[pos][0 if side > 0 else 1]
    q = (a[0] + u[0]*L_*t_frac + n[0]*off, a[1] + u[1]*L_*t_frac + n[1]*off)
    e = next(x for x, c, d in A.db_lines('A-WALL') if A.ev(f'(wt:on-seg {P(*q)} {P(*c)} {P(*d)})'))
    return e, (q[0] + n[0]*0.2*side, q[1] + n[1]*0.2*side)
def wwf(e, q, dist):
    A.G[A.Sym('*T-E*')] = e; A.G[A.Sym('*T-Q*')] = [q[0], q[1]]
    return A.ev(f'(wt:wwf-add *t-e* *t-q* {float(dist)})')
def new_master(before):
    return [(a, b) for _, a, b in masters_now() if mk(a, b) not in before]
def clearance(w, side, nm, nth):
    """signed distance along the source's outward normal from picked face to the new wall's nearest face"""
    a, b, th, pos = w
    L_ = math.dist(a, b); u = ((b[0]-a[0])/L_, (b[1]-a[1])/L_); n = (-u[1], u[0])
    face = {'CENTER': (th/2, -th/2), 'LEFT': (th, 0.0), 'RIGHT': (0.0, -th)}[pos][0 if side > 0 else 1]
    mo = (nm[0][0]-a[0])*n[0] + (nm[0][1]-a[1])*n[1]
    mo2 = (nm[1][0]-a[0])*n[0] + (nm[1][1]-a[1])*n[1]
    return side * (mo - face) - nth/2, abs(mo - mo2) < 1e-6
ok_all = True; detail = []
for th, dist in ((150, 1500), (200, 1500), (100, 750)):
    for ang in (0, 90, 45, 73, 135):
        for pos in ('CENTER', 'LEFT', 'RIGHT'):
            for rev in (False, True):
                a, b = (0.0, 0.0), polar((0.0, 0.0), ang, 4000)
                if rev: a, b = b, a
                src = wall(a, b, th, pos)
                for side in (1, -1):
                    build([src]); before = master_set()
                    e, q = face_pick(src, side)
                    rec = wwf(e, q, dist)
                    nms = new_master(before)
                    good = rec is not None and len(nms) == 1
                    if good:
                        c, par = clearance(src, side, nms[0], th)
                        L_ok = abs(math.dist(*nms[0]) - 4000) < 1e-6
                        nw = wall(nms[0][0], nms[0][1], th, 'CENTER')
                        vis = same_lines(walls_now(), expected([src, nw]))
                        good = abs(c - dist) < 1e-6 and par and L_ok and vis
                    if not good: ok_all = False; detail.append((th, dist, ang, pos, rev, side))
chk('WWF: exact face-to-face clearance, source width, both faces, C/L/R, reversed, 0/45/73/90/135 deg', ok_all)
if detail: print('   ', detail[:4])

# both faces create on opposite sides; undo in between restores
build([wall((0,0),(4000,0),150)]); base = snapshot(); before = master_set()
e, q = face_pick(wall((0,0),(4000,0),150), 1); r1 = wwf(e, q, 1500); up = new_master(before)
undo_rec(r1); back1 = snapshot() == base
e, q = face_pick(wall((0,0),(4000,0),150), -1); r2 = wwf(e, q, 1500); down = new_master(before)
chk('WWF: Face 1 -> one side, Undo, Face 2 -> opposite side',
    back1 and up and down and up[0][0][1] > 0 and down[0][0][1] < 0 and abs(up[0][0][1] - 1650) < 1e-6 and abs(down[0][0][1] + 1650) < 1e-6)

# cap / master / ambiguous picks do nothing
build([wall((0,0),(4000,0),150)]); base = snapshot()
cap = find_line('A-WALL', (4000,75), (4000,-75))
chk('WWF: end cap rejected, nothing created', wwf(cap, (4000.2, 10), 1500) is None and snapshot() == base)
chk('WWF: master line rejected (needs a face)', wwf(find_line('X-AXIS', (0,0), (4000,0)), (2000, 0), 1500) is None and snapshot() == base)
build([wall((0,0),(10000,0))]); add(((5000,0),(5000,4000))); base = snapshot()
e = find_line('A-WALL', (0,75), (4925,75))
chk('WWF: pick exactly at a junction corner -> ambiguous, nothing created', wwf(e, (4925, 75), 1500) is None and snapshot() == base)

# normalized spans: only the picked span is copied (merged face resolved by pick point)
for label, x_pick, want in (('left', 2500, ((0,-1650),(5000,-1650))), ('right', 7500, ((5000,-1650),(10000,-1650)))):
    build([wall((0,0),(10000,0))]); add(((5000,-4000),(5000,4000)))  # X: four spans
    before = master_set()
    e = find_line('A-WALL', (0,-75), (4925,-75)) if label == 'left' else find_line('A-WALL', (5075,-75), (10000,-75))
    wwf(e, (x_pick, -75.2), 1500)
    got = [x for x in master_set() if x not in before]
    # new wall's inner end lands on the vertical master -> vertical splits, new wall is one span
    chk(f'WWF normalized span ({label}): copies only that span, junction with vertical, visible == full solve',
        mk(*want) in master_set() and normalized_ok() and lines_match_masters())
build([wall((0,0),(10000,0))]); add(((5000,0),(5000,4000))); add(((0,-3000),(10000,-3000)))
before = master_set()
e = find_line('A-WALL', (5075,75), (10000,75)) if False else find_line('A-WALL', (0,-75), (10000,-75))
wwf(e, (2500, -75.2), 1000)
chk('WWF merged face across normalized spans: pick point picks the left span', mk((0,-1150),(5000,-1150)) in master_set() and lines_match_masters())

# junctions: both ends, one end, neither, crossing midspan
def junction_case(extra_walls, src_seg, dist, side):
    ws = [wall(a, b) for a, b in extra_walls]
    src = wall(*src_seg)
    build(ws + [src]); before = master_set()
    e, q = face_pick(src, side, 0.3)
    rec = wwf(e, q, dist)
    return rec, before
rec, before = junction_case([((0,0),(0,4000)), ((6000,0),(6000,4000))], ((0,0),(6000,0)), 1500, 1)
chk('WWF junction: both ends land on masters -> two T, no caps, verticals split',
    mk((0,1650),(6000,1650)) in master_set() and normalized_ok() and lines_match_masters()
    and not any(near(a, (0,1575)) or near(b, (0,1575)) for _, a, b in walls_now() if abs(a[0]-b[0]) < 1e-6 and abs(a[1]-b[1]) == 150))
rec, before = junction_case([((0,0),(0,4000))], ((0,0),(6000,0)), 1500, 1)
chk('WWF junction: one end joins, other end capped', mk((0,1650),(6000,1650)) in master_set() and lines_match_masters()
    and any((near(a,(6000,1725)) and near(b,(6000,1575))) or (near(b,(6000,1725)) and near(a,(6000,1575))) for _, a, b in walls_now()))
rec, before = junction_case([], ((0,0),(6000,0)), 1500, 1)
chk('WWF junction: neither end joins -> free wall with two caps', len(walls_now()) == 8 and lines_match_masters())
rec, before = junction_case([((3000,1000),(3000,3000))], ((0,0),(6000,0)), 1500, 1)
chk('WWF junction: crosses another wall midspan -> X, both split', normalized_ok() and len(master_set()) == 5 and lines_match_masters())
rec, before = junction_case([((0,1650),(-3000,1650))], ((0,0),(6000,0)), 1500, 1)
chk('WWF junction: collinear continuation of an existing wall', lines_match_masters() and len(master_set()) == 3)

# undo: split + join + neighbour redraw, then exact restore (original unsplit master entity)
build([wall((0,0),(0,4000)), wall((6000,0),(6000,4000)), wall((0,0),(6000,0))])
base = snapshot(); orig = {mk(a, b): e for e, a, b in masters_now()}
e, q = face_pick(wall((0,0),(6000,0)), 1, 0.3)
rec = wwf(e, q, 1500)
split = len(master_set()) == 6
undo_rec(rec)
chk('WWF Undo: split verticals and joins fully reverted, original master entities back',
    split and snapshot() == base and all(orig[mk(a, b)] is e for e, a, b in masters_now()))

# command flow: distance memory, repeat, Undo keyword, cancel before success keeps memory
A.ev('(setq *wt:wwf-dist* nil)')
build([wall((0,0),(4000,0)), wall((0,-8000),(4000,-8000))])
e1, q1 = face_pick(wall((0,0),(4000,0)), 1)
A.DISTQ[:] = [1200.0]; A.ENTSELQ[:] = [None, [e1, [q1[0], q1[1], 0.0]]]
A.ev('(c:WWF)')
mem1 = A.ev('*wt:wwf-dist*')
created = mk((0,1350),(4000,1350)) in master_set()
e2, q2 = face_pick(wall((0,-8000),(4000,-8000)), 1)
A.DISTQ[:] = []; A.ENTSELQ[:] = [[e2, [q2[0], q2[1], 0.0]], 'Undo']
A.ev('(c:WWF)')
undone = mk((0,-6650),(4000,-6650)) not in master_set()
A.DISTQ[:] = [900.0]; A.ENTSELQ[:] = []
A.ev('(c:WWF)')
chk('WWF command: 1200 remembered, missed pick re-asks, Enter reuses, Undo keyword, cancelled distance not remembered',
    created and mem1 == 1200.0 and undone and A.ev('*wt:wwf-dist*') == 1200.0)
c = cfg_run('DEFAULT_OFFSET=abc\n'); bad_default = c('DEFAULT_OFFSET') == 1500.0
c = cfg_run('DEFAULT_OFFSET=900\n'); good_default = c('DEFAULT_OFFSET') == 900.0
cfg_run(None)
chk('config: DEFAULT_OFFSET optional, invalid -> 1500', bad_default and good_default)

# =========================== TW ===========================
def field(x0, y0, x1, y1): return [(x0,y0),(x1,y0),(x1,y1),(x0,y1)]
def tw(fld, tol=150.0):
    A.G[A.Sym('*T-F*')] = [list(p) for p in fld]
    return A.ev(f'(wt:tw-repair (mapcar (quote (lambda (p) (list (float (car p)) (float (cadr p))))) *t-f*) {float(tol)})')
def set_end(e, which, pt):
    A.G[A.Sym('*T-E*')] = e
    k = 10 if which == 0 else 11
    A.ev(f'(progn (setq *t-d* (entget *t-e*)) (entmod (subst (cons {k} (list {float(pt[0])} {float(pt[1])} 0.0)) (assoc {k} *t-d*) *t-d*)))')
def mset(): return master_set()
def geo(): return sorted((round(a[0],3), round(a[1],3), round(b[0],3), round(b[1],3)) for _, a, b in walls_now()) , mset()

src = open(os.path.join(HERE, '..', 'WallTool.lsp')).read()
chk('WWF: single implementation, WWO is a one-line alias', src.count('(defun c:WWF') == 1 and '(defun c:WWO () (c:WWF))' in src and 'wt:wwo-' not in src)

# 25. stale A-WALL: erase face pieces of a clean T, TW restores it
build([wall((0,0),(10000,0))]); add(((5000,0),(5000,4000))); clean = geo()
for e, a, b in walls_now()[:2]: A.entdel(e)
tw(field(3000,-1000,7000,1500))
chk('TW stale linework: erased face pieces of a T restored exactly', geo() == clean)

# 31. X exact topology with stale faces: masters unchanged
build([wall((0,0),(10000,0))]); add(((5000,-4000),(5000,4000))); clean = geo()
for e, a, b in walls_now()[::3]: A.entdel(e)
tw(field(4000,-1000,6000,1000))
chk('TW X with stale faces: rebuilt, masters untouched', geo() == clean)

# 26. moved master (along its axis) within tolerance reconnects; beyond tolerance stays free
build([wall((0,0),(10000,0))]); add(((5000,0),(5000,4000))); clean = geo()
br = find_line('X-AXIS', (5000,0), (5000,4000)); set_end(br, 0, (5000,60)); set_end(br, 1, (5000,4060))
tw(field(4000,-1000,6000,1000))
chk('TW moved branch 60 (<=150): reconnects to T, current top end kept', mk((5000,0),(5000,4060)) in mset() and len(mset()) == 3 and lines_match_masters())
build([wall((0,0),(10000,0))]); add(((5000,0),(5000,4000)))
br = find_line('X-AXIS', (5000,0), (5000,4000)); set_end(br, 0, (5000,300)); set_end(br, 1, (5000,4300))
tw(field(4000,-1000,6000,1000))
chk('TW moved branch 300 (>150): stays disconnected with caps, old faces cleaned', mk((5000,300),(5000,4300)) in mset() and lines_match_masters())

# 27. small gap 50 connects (T, through wall normalized), 300 does not
build([wall((0,0),(10000,0)), wall((5000,50),(5000,4000))])
tw(field(4000,-1000,6000,1000))
chk('TW gap 50: extends to T and through wall splits into spans', mset() == sorted([mk((0,0),(5000,0)), mk((5000,0),(10000,0)), mk((5000,0),(5000,4000))]) and lines_match_masters())
build([wall((0,0),(10000,0)), wall((5000,300),(5000,4000))]); before = geo()
tw(field(4000,-1000,6000,1000))
chk('TW gap 300: not connected, drawing unchanged', geo() == before)

# 28. overshoot: stretched master (not yet normalized) and WW-drawn overshoot (normalized stub)
build([wall((0,0),(10000,0))]); add(((5000,0),(5000,4000)))
set_end(find_line('X-AXIS', (5000,0), (5000,4000)), 0, (5000,-50))
tw(field(4000,-1000,6000,1000))
chk('TW overshoot 50 (stretched): shortened back to the T node', mset() == sorted([mk((0,0),(5000,0)), mk((5000,0),(10000,0)), mk((5000,0),(5000,4000))]) and lines_match_masters())
build([wall((0,0),(10000,0))]); add(((5000,-50),(5000,4000)))
had_stub = mk((5000,-50),(5000,0)) in mset()
tw(field(4000,-1000,6000,1000))
chk('TW overshoot 50 (drawn, normalized stub): stub span removed, clean T', had_stub and mset() == sorted([mk((0,0),(5000,0)), mk((5000,0),(10000,0)), mk((5000,0),(5000,4000))]) and lines_match_masters())
build([wall((0,0),(10000,0))]); add(((5000,-400),(5000,4000))); before = geo()
tw(field(4000,-1000,6000,1000))
chk('TW overshoot 400 (>150): left alone', geo() == before)

# 29. L: both free ends extend to their natural intersection
build([wall((0,0),(4000,0)), wall((4060,60),(4060,3000))])
tw(field(3000,-1000,5000,1000))
chk('TW L: two free ends meet at the extension intersection', mset() == sorted([mk((0,0),(4060,0)), mk((4060,0),(4060,3000))]) and lines_match_masters())
build([wall((0,0),(4000,0)), wall((4060,400),(4060,3000))]); before = geo()
tw(field(3000,-1000,5000,1000))
chk('TW L too far (one end 400 away): unchanged', geo() == before)

# 33. ambiguity: two targets within reach along the same direction
build([wall((0,0),(10000,0)), wall((0,-80),(10000,-80),50), wall((5000,50),(5000,4000))]); before = geo()
out = io.StringIO(); A.OUTPUT = True
with contextlib.redirect_stdout(out): tw(field(4000,-1000,6000,1000))
A.OUTPUT = False
chk('TW ambiguity: endpoint with two candidate walls left unchanged and reported',
    mk((5000,50),(5000,4000)) in mset() and '1 ambiguous connection(s) skipped' in out.getvalue())
# not nearest-object snapping: a wall beside the end (not along its direction) is ignored
build([wall((0,0),(4000,0)), wall((4050,100),(8000,100))]); before = geo()
tw(field(3000,-1000,5000,1000))
chk('TW does not snap to the nearest wall: parallel wall 100 away from the free end is ignored', geo() == before)

# 34. field scope: only the gap inside the field changes
build([wall((0,0),(20000,0)), wall((5000,50),(5000,4000)), wall((15000,50),(15000,4000))])
tw(field(4000,-1000,6000,1000))
chk('TW field scope: gap inside repaired, identical gap outside untouched',
    mk((5000,0),(5000,4000)) in mset() and mk((15000,50),(15000,4000)) in mset())
# 15. repair node outside the field is not made even if the master crosses the field
build([wall((0,0),(10000,0)), wall((5000,50),(5000,4000))]); before = geo()
tw(field(4000,1000,6000,3000))
chk('TW: repair node outside the field is not created', mk((5000,50),(5000,4000)) in mset())

# 32. multiple junctions in one field + a free wall that stays free; 35. idempotence
def multi():
    build([wall((0,0),(4000,0)), wall((4060,60),(4060,3000)),            # near-L
           wall((6000,0),(12000,0)), wall((9000,70),(9000,3000)),         # near-T
           wall((14000,-2000),(14000,2000)), wall((12500,0),(15500,0)),   # exact X
           wall((2000,-3000),(5000,-3000))])                               # free wall
    for e, a, b in walls_now():
        if abs(a[0]-14075) < 1 or abs(b[0]-14075) < 1: A.entdel(e)       # damage the X
multi()
tw(field(-1000,-4000,16000,3500))
exp_masters = sorted([mk((0,0),(4060,0)), mk((4060,0),(4060,3000)), mk((6000,0),(9000,0)), mk((9000,0),(12000,0)), mk((9000,0),(9000,3000)),
                      mk((14000,-2000),(14000,0)), mk((14000,0),(14000,2000)), mk((12500,0),(14000,0)), mk((14000,0),(15500,0)), mk((2000,-3000),(5000,-3000))])
first = geo()
free_caps = sum(1 for _, a, b in walls_now() if abs(a[0]-b[0]) < 1e-6 and abs(abs(a[1]-b[1]) - 150) < 1e-6 and abs(a[1]+b[1] + 6000) < 1e-6)
chk('TW multi-junction field: L, T, X repaired in one run, free wall keeps its caps', mset() == exp_masters and lines_match_masters() and free_caps == 2)
tw(field(-1000,-4000,16000,3500))
chk('TW idempotent: second run over the same field changes nothing', geo() == first)

# 36. undo: extend + T + split + redraw, then exact restore
build([wall((0,0),(10000,0)), wall((5000,50),(5000,4000))])
base = geo(); ents = {e: list(A.DB[e]) for e, a, b in masters_now()}
rec = tw(field(4000,-1000,6000,1000))
changed = len(mset()) == 3
undo_rec(rec)
chk('TW Undo: endpoint, split and linework restored exactly (same master entities)',
    changed and geo() == base and all(e not in A.DELETED and A.DB[e] == d for e, d in ents.items()))

# command smoke + config
build([wall((0,0),(10000,0)), wall((5000,50),(5000,4000))])
A.POINTQ[:] = [[4000.0, -1000.0, 0.0], [6000.0, 1000.0, 0.0]]
A.ev('(c:TW)')
chk('TW command: two corners -> repair runs', mk((5000,0),(5000,4000)) in mset())
c = cfg_run('TW_CONNECT_DISTANCE=-3\n'); badv = c('TW_CONNECT_DISTANCE') == 150.0
c = cfg_run('TW_CONNECT_DISTANCE=400\n'); goodv = c('TW_CONNECT_DISTANCE') == 400.0
cfg_run(None)
chk('config: TW_CONNECT_DISTANCE optional, invalid -> 150', badv and goodv)

# =========================== Centerline masters (Phase 1) ===========================
def master_offsets_ok(tol=1e-6):
    """every master lies exactly halfway between the two faces of its wall"""
    for e, a, b in masters_now():
        r = A.ev(f'(wt:recon (list nil {P(*a)} {P(*b)}) (cadr (wt:net-scan)))')
        if not r or r[4] != 'CENTER': return False
    return True
pl = lambda a, b, th, pos: [tuple(x) for x in A.ev(f'(wt:placement-to-centerline {P(*a)} {P(*b)} {float(th)} "{pos}")')]
chk('placement-to-centerline: CENTER on line, LEFT +th/2 left of travel, RIGHT mirror, reversed direction flips side',
    pl((0,0),(4000,0),200,'CENTER') == [(0,0),(4000,0)] and pl((0,0),(4000,0),200,'LEFT') == [(0,100),(4000,100)]
    and pl((0,0),(4000,0),200,'RIGHT') == [(0,-100),(4000,-100)] and pl((4000,0),(0,0),200,'LEFT') == [(4000,-100),(0,-100)])
ok = True
for pos in ('CENTER', 'LEFT', 'RIGHT'):
    for ang in (0, 45, 73, 135):
        a, b = (0.0, 0.0), polar((0.0, 0.0), ang, 4000)
        fresh(200, pos); add((a, b))
        ok = ok and len(masters_now()) == 1 and master_offsets_ok() and same_lines(walls_now(), expected([wall(a, b, 200, pos)]))
chk('WW 200 CENTER/LEFT/RIGHT at 0/45/73/135: same physical wall as before, master exactly on centerline', ok)
ok = True
for pos in ('LEFT', 'RIGHT'):
    fresh(200, pos); A.ev('(setq *wt:chain-on* t *wt:chain* nil)')
    add(((0,0),(4000,0))); add(((4000,0),(4000,3000))); add(((4000,3000),(1000,5000)))
    A.ev('(setq *wt:chain-on* nil *wt:chain* nil)')
    old = [wall((0,0),(4000,0),200,pos), wall((4000,0),(4000,3000),200,pos), wall((4000,3000),(1000,5000),200,pos)]
    ok = ok and master_offsets_ok() and normalized_ok() and same_lines(walls_now(), expected(old)) and len(masters_now()) == 3
chk('WW LEFT/RIGHT chain (90 deg + oblique corner): centerline corners meet, physical walls identical to old model', ok)
fresh(150, 'CENTER'); add(((-3000,0),(3000,0))); settings(200, 'LEFT'); add(((0,-3000),(0,0)))
chk('WW LEFT branch into a centered wall: T with physical faces identical to old model',
    same_lines(walls_now(), expected([wall((-3000,0),(3000,0),150), wall((0,-3000),(0,0),200,'LEFT')])) and master_offsets_ok() and normalized_ok())
fresh(200, 'CENTER'); add(((0,0),(4000,0))); settings(200, 'LEFT'); add(((4000,0),(4000,3000)))
chk('WW LEFT wall added at the free end of an existing wall: existing end follows the centerline corner (clean L)',
    same_lines(walls_now(), expected([wall((0,0),(4000,0),200), wall((4000,0),(4000,3000),200,'LEFT')])) and master_offsets_ok())
fresh(200, 'CENTER'); add(((0,0),(4000,0))); before = geo()
e = find_line('X-AXIS', (0,0), (4000,0)); set_end(e, 0, (4000,0)); set_end(e, 1, (0,0))
A.ev('(setq *wt:reg* nil)'); tw(field(-500,-500,4500,500))
chk('centered wall: reversing the master direction and rebuilding gives identical A-WALL', geo()[0] == before[0])
ok = True
for pos, inside in (('CENTER', None), ('LEFT', True), ('RIGHT', False)):
    fresh(200, pos)
    add(((0,0),(4000,0)), ((4000,0),(4000,3000)), ((4000,3000),(0,3000)), ((0,3000),(0,0)))
    old = [wall(a, b, 200, pos) for a, b in (((0,0),(4000,0)), ((4000,0),(4000,3000)), ((4000,3000),(0,3000)), ((0,3000),(0,0)))]
    inset = {'CENTER': 0, 'LEFT': 100, 'RIGHT': -100}[pos]
    want = sorted([mk((inset,inset),(4000-inset,inset)), mk((4000-inset,inset),(4000-inset,3000-inset)), mk((4000-inset,3000-inset),(inset,3000-inset)), mk((inset,3000-inset),(inset,inset))])
    ok = ok and same_lines(walls_now(), expected(old)) and master_set() == want and len(walls_now()) == 8
chk('Rectangle CENTER/LEFT/RIGHT: same physical rectangle as before, four centerline masters meeting at corners', ok)
ok = True
for pos in ('CENTER', 'LEFT', 'RIGHT'):
    fresh(); ids = [mkline((0,0),(10000,0),'0'), mkline((5000,0),(5000,4000),'0')]
    src = {e: list(A.DB[e]) for e in ids}
    A.ev('(wt:pend-begin)'); settings(200, pos)
    A.G[A.Sym('*T-NEW*')] = None
    xw(ids, 200, pos)
    old = [wall((0,0),(10000,0),200,pos), wall((5000,0),(5000,4000),200,pos)]
    ok = ok and master_offsets_ok() and normalized_ok() and same_lines(walls_now(), expected(old))
chk('XW 200 CENTER/LEFT/RIGHT: placement follows alignment, masters are centerlines, T normalized', ok)
fresh(); ids = [mkline((0,0),(10000,0),'0'), mkline((5000,0),(5000,4000),'0')]
src = {e: list(A.DB[e]) for e in ids}
settings(200, 'LEFT')
A.G[A.Sym('*T-SEL*')] = ids
A.ev('(progn (defun wt:t-xw-keep (sel / r) (wt:xw-convert sel)) (setq *t-x* nil))')
# run the conversion inside a kept transaction: wrap wt:pend-begin so the record survives
A.ev('(progn (setq *t-orig-begin* wt:pend-begin) (defun wt:pend-begin () (setq *wt:pending* (list nil nil nil) *t-keep* nil)))')
A.ev('(progn (setq *t-orig-rebuild* wt:rebuild) (defun wt:rebuild (new removed) (setq *t-keep* nil) (apply *t-orig-rebuild* (list new removed)) (setq *t-keep* *wt:pending*) new))')
A.ev('(wt:xw-convert *t-sel*)')
A.ev('(progn (setq wt:pend-begin *t-orig-begin*) (setq wt:rebuild *t-orig-rebuild*))')
converted = len(masters_now()) == 3
A.ev('(wt:seg-undo *t-keep*)')
chk('XW LEFT Undo restores the original source LINEs exactly (layer 0, drawn geometry, no walls)',
    converted and not masters_now() and not walls_now() and all(e not in A.DELETED and A.DB[e] == src[e] for e in ids))

# =========================== WWR (Phase 2) ===========================
def wr(fld):
    A.G[A.Sym('*T-F*')] = [list(p) for p in fld]
    return A.ev('(wt:wr-repair (mapcar (quote (lambda (p) (list (float (car p)) (float (cadr p))))) *t-f*))')
def legacy(ws):
    """fixture: old eccentric-master drawing = masters on the drawn lines + faces from the old solver"""
    fresh()
    for a, b, th, pos in ws: mkline(a, b, 'X-AXIS')
    for a, b in expected(ws): mkline(a, b, 'A-WALL')
def say(fn):
    out = io.StringIO(); A.OUTPUT = True
    with contextlib.redirect_stdout(out): r = fn()
    A.OUTPUT = False
    return out.getvalue(), r

# misaligned: master moved 60 sideways
fresh(200, 'CENTER'); add(((0,0),(4000,0))); phys = geo()[0]
e = find_line('X-AXIS', (0,0), (4000,0)); set_end(e, 0, (0,60)); set_end(e, 1, (4000,60))
msg, _ = say(lambda: wr(field(-500,-500,4500,500)))
chk('WWR misaligned master (moved 60): back to exact center, physical wall unchanged, reported',
    master_set() == [mk((0,0),(4000,0))] and geo()[0] == phys and '1 axis/axes adjusted' in msg)

# missing master: free wall with caps
fresh(200, 'CENTER'); add(((0,0),(4000,0))); phys = geo()[0]
A.entdel(find_line('X-AXIS', (0,0), (4000,0))); A.ev('(setq *wt:reg* nil)')
msg, _ = say(lambda: wr(field(-500,-500,4500,500)))
r = A.ev(f'(wt:recon (list nil {P(0,0)} {P(4000,0)}) (cadr (wt:net-scan)))')
chk('WWR missing master: centerline and 200 thickness reconstructed, faces unchanged',
    master_set() == [mk((0,0),(4000,0))] and r and round(r[3]) == 200 and geo()[0] == phys and '1 missing axis/axes rebuilt' in msg)

# recognised master, faces deleted: master wins, faces regenerated
fresh(200, 'CENTER'); add(((0,0),(4000,0))); before = geo()
for e2, a, b in walls_now(): A.entdel(e2)
wr(field(-500,-500,4500,500))
chk('WWR recognised master with all faces erased: master preserved, A-WALL regenerated', geo() == before)

# legacy LEFT and RIGHT single walls
ok = True
for pos, mid in (('LEFT', 100), ('RIGHT', -100)):
    legacy([wall((0,0),(4000,0),200,pos)]); phys = geo()[0]
    wr(field(-500,-500,4500,500))
    ok = ok and master_set() == [mk((0,mid),(4000,mid))] and geo()[0] == phys and master_offsets_ok()
chk('WWR legacy LEFT/RIGHT wall: master moved to true center, thickness and physical wall preserved', ok)

# legacy T (eccentric masters) -> centered, normalized, same physical walls
old = [wall((0,0),(10000,0),200,'LEFT'), wall((5000,0),(5000,4000),150,'RIGHT')]
legacy(old); phys = geo()[0]
wr(field(-500,-500,10500,4500))
chk('WWR legacy eccentric T: centered masters, three spans, clean T identical to old physical walls',
    geo()[0] == phys and master_offsets_ok() and normalized_ok()
    and master_set() == sorted([mk((0,100),(5075,100)), mk((5075,100),(10000,100)), mk((5075,100),(5075,4000))]))   # RIGHT 150 branch drawn up: body on +x

# T with a missing span master: rebuild branch / left arm when evidence is unambiguous
for label, gone in (('branch', ((5000,0),(5000,4000))), ('left arm', ((0,0),(5000,0)))):
    build([wall((0,0),(10000,0))]); add(((5000,0),(5000,4000))); before = geo()
    A.entdel(find_line('X-AXIS', *gone)); A.ev('(setq *wt:reg* nil)')
    wr(field(-500,-500,10500,4500))
    chk(f'WWR T with missing {label} master: rebuilt from faces + junction evidence, T intact', geo() == before)

# safety: arbitrary parallel A-WALL lines are not walls
fresh(); mkline((0,0),(3000,0),'A-WALL'); mkline((0,200),(3000,200),'A-WALL'); mkline((500,900),(2500,900),'A-WALL'); before = geo()
wr(field(-500,-500,3500,1500))
chk('WWR does not invent a wall from arbitrary parallel A-WALL lines (no caps, no junctions)', geo() == before and not masters_now())
# safety: AX line stays an AX line
fresh(); ax = mkline((0,0),(5000,0),'X-AXIS'); before = geo()
wr(field(-500,-500,5500,500))
chk('WWR leaves a plain AX line alone (no wall generated)', geo() == before)

# idempotence on a healthy network
build([wall((0,0),(10000,0))]); add(((5000,-4000),(5000,4000))); add(((0,0),(0,3000))); healthy = geo()
m1, _ = say(lambda: wr(field(-500,-4500,10500,4500))); g1 = geo()
m2, _ = say(lambda: wr(field(-500,-4500,10500,4500)))
chk('WWR idempotent: healthy network unchanged on first and second run, "No axis repairs required."',
    g1 == healthy and geo() == healthy and 'No axis repairs required' in m1 and 'No axis repairs required' in m2)

# combined field + undo
def combined():
    build([wall((0,0),(10000,0))]); add(((5000,0),(5000,4000)))                 # T
    add(((0,-6000),(4000,-6000)))                                                # to be misaligned
    add(((6000,-6000),(10000,-6000)))                                            # to lose its master
    e = find_line('X-AXIS', (0,-6000), (4000,-6000)); set_end(e, 0, (0,-5940)); set_end(e, 1, (4000,-5940))
    A.entdel(find_line('X-AXIS', (6000,-6000), (10000,-6000)))
    for e2, a, b in walls_now():
        if abs(a[1]-75) < 1e-6 and abs(b[1]-75) < 1e-6: A.entdel(e2); break      # damage a face
    # merge the through wall back into one unsplit master (legacy long master through the T)
    for x in (find_line('X-AXIS', (0,0), (5000,0)), find_line('X-AXIS', (5000,0), (10000,0))): A.entdel(x)
    mkline((0,0),(10000,0),'X-AXIS')
    A.ev('(setq *wt:reg* nil)')
combined(); base = geo(); ents = {e: list(A.DB[e]) for e, a, b in masters_now()}
rec = wr(field(-500,-6500,10500,4500))
repaired = master_set() == sorted([mk((0,0),(5000,0)), mk((5000,0),(10000,0)), mk((5000,0),(5000,4000)),
                                    mk((0,-6000),(4000,-6000)), mk((6000,-6000),(10000,-6000))]) and lines_match_masters()
undo_rec(rec)
chk('WWR combined (misaligned + missing + damaged face + unsplit T): all repaired; Undo restores exact original',
    repaired and geo() == base and all(e not in A.DELETED and A.DB[e] == d for e, d in ents.items()))
# WWR joins broken collinear pieces of one wall (no junction at the joint)
fresh(200, 'CENTER'); add(((0,0),(10000,0))); phys = geo()[0]
A.entdel(find_line('X-AXIS', (0,0), (10000,0)))
for a, b in (((0,0),(3000,0)), ((3000,0),(6000,0)), ((6000,0),(10000,0))): mkline(a, b, 'X-AXIS')
A.ev('(setq *wt:reg* nil)')
msg, rec = say(lambda: wr(field(-500,-500,10500,500)))
chk('WWR joins a wall broken into 3 touching masters into one span, walls unchanged, reported',
    master_set() == [mk((0,0),(10000,0))] and geo()[0] == phys and '2 broken axis segment(s) joined' in msg)
fresh(200, 'CENTER'); add(((0,0),(10000,0))); phys = geo()[0]
A.entdel(find_line('X-AXIS', (0,0), (10000,0))); mkline((0,0),(6000,0),'X-AXIS'); mkline((4000,0),(10000,0),'X-AXIS')
A.ev('(setq *wt:reg* nil)'); base = geo(); rec = wr(field(-500,-500,10500,500))
joined_ok = master_set() == [mk((0,0),(10000,0))] and geo()[0] == phys
undo_rec(rec)
chk('WWR joins overlapping collinear masters; Undo restores both pieces', joined_ok and geo() == base)
build([wall((0,0),(10000,0))]); add(((5000,0),(5000,4000))); before = geo()
wr(field(-500,-500,10500,4500))
chk('WWR keeps spans split at a real T junction (no join)', geo() == before)
fresh(150, 'CENTER'); add(((0,0),(5000,0))); settings(250, 'CENTER'); add(((5000,0),(10000,0))); before = geo()
wr(field(-500,-500,10500,500))
chk('WWR does not join collinear pieces of different thickness', geo() == before)
fresh(200, 'CENTER'); add(((0,0),(10000,0)))
A.entdel(find_line('X-AXIS', (0,0), (10000,0))); mkline((0,0),(5000,0),'X-AXIS'); mkline((5000,0),(10000,0),'X-AXIS')
A.ev('(setq *wt:reg* nil)'); before = geo()
wr(field(6000,-500,10500,500))
chk('WWR only joins when the joint is inside the window', geo() == before)

A.POINTQ[:] = [[-500.0, -500.0, 0.0], [4500.0, 500.0, 0.0]]
fresh(200, 'CENTER'); add(((0,0),(4000,0))); e = find_line('X-AXIS', (0,0), (4000,0)); set_end(e, 0, (0,60)); set_end(e, 1, (4000,60))
A.ev('(c:WWR)')
chk('WWR command: two corners -> repair runs', master_set() == [mk((0,0),(4000,0))])

# =========================== WW: Alignment / Width changes during one run ===========================
def ww_run(steps, th=150.0, pos='CENTER', keep_session=False):
    """drive the real c:WW loop. steps: points (x,y), 'Enter', ('A', key), ('W', width), 'Undo', 'Close'"""
    if not keep_session:
        fresh(th, pos)
    pts, kws, reals = [], [], []
    for st in steps:
        if isinstance(st, tuple) and st[0] == 'A': pts.append('Alignment'); kws.append(st[1])
        elif isinstance(st, tuple) and st[0] == 'W': pts.append('Width'); reals.append(float(st[1]))
        elif st == 'Enter': pts.append(None)
        elif isinstance(st, str): pts.append(st)
        else: pts.append([float(st[0]), float(st[1]), 0.0])
    A.POINTQ[:] = pts; A.KWQUEUE[:] = kws; A.REALQ[:] = reals
    A.ev('(c:WW)')
    return not A.POINTQ and not A.KWQUEUE and not A.REALQ
def widths_along(chain_pts):
    """recovered thickness of the master whose span covers each input segment midpoint region"""
    out = []
    for e, a, b in masters_now():
        r = A.ev(f'(wt:recon (list nil {P(*a)} {P(*b)}) (cadr (wt:net-scan)))')
        out.append(round(r[3]) if r else None)
    return sorted(out)
def no_zero_masters(): return all(math.dist(a, b) > 1e-3 for _, a, b in masters_now())

# 4-segment run: CENTER -> LEFT -> RIGHT -> CENTER
P_ = [(0,0),(4000,0),(4000,3000),(8000,5000),(8000,8000)]
steps = [P_[0], P_[1], ('A','Left'), P_[2], ('A','Right'), P_[3], ('A','Center'), P_[4], 'Enter']
consumed = ww_run(steps)
ref = [wall(P_[0],P_[1],150,'CENTER'), wall(P_[1],P_[2],150,'LEFT'), wall(P_[2],P_[3],150,'RIGHT'), wall(P_[3],P_[4],150,'CENTER')]
chk('WW one run C->L->R->C: 4 walls, each placed by its own alignment (old-model faces), all masters centered',
    consumed and len(masters_now()) == 4 and master_offsets_ok() and normalized_ok() and no_zero_masters()
    and same_lines(walls_now(), expected(ref)) and A.ev('*wt:pos*') == 'CENTER')
# previous segments are not reinterpreted: after segment 2 (LEFT), segment 1's CENTER faces away from the corner are unchanged
ww_run([P_[0], P_[1], 'Enter']); seg1 = [x for x in geo()[0] if x[0] < 3000 and x[2] < 3000]
ww_run([P_[0], P_[1], ('A','Left'), P_[2], ('A','Right'), 'Enter'])
seg1_after = [x for x in geo()[0] if x[0] < 3000 and x[2] < 3000]
chk('WW alignment change does not move or re-place the earlier CENTER segment; pending alignment commits nothing',
    seg1_after == seg1 and len(masters_now()) == 2 and A.ev('*wt:pos*') == 'RIGHT')

# width + alignment independent: 150C, 200C, 200L, 100L, 100R, 250C
Q_ = [(0,0),(4000,0),(4000,3000),(8000,3000),(8000,6000),(12000,6000),(12000,9000)]
steps = [Q_[0], Q_[1], ('W',200), Q_[2], ('A','Left'), Q_[3], ('W',100), Q_[4], ('A','Right'), Q_[5], ('W',250), ('A','Center'), Q_[6], 'Enter']
consumed = ww_run(steps)
ref = [wall(Q_[0],Q_[1],150,'CENTER'), wall(Q_[1],Q_[2],200,'CENTER'), wall(Q_[2],Q_[3],200,'LEFT'),
       wall(Q_[3],Q_[4],100,'LEFT'), wall(Q_[4],Q_[5],100,'RIGHT'), wall(Q_[5],Q_[6],250,'CENTER')]
chk('WW one run 150C/200C/200L/100L/100R/250C: widths and alignments independent, exact placement, centered masters',
    consumed and len(masters_now()) == 6 and master_offsets_ok() and no_zero_masters()
    and widths_along(Q_) == sorted([150,200,200,100,100,250]) and same_lines(walls_now(), expected(ref))
    and A.ev('*wt:thk*') == 250.0 and A.ev('*wt:pos*') == 'CENTER')

# every transition on a connected non-collinear corner
ok = True; bad = []
for a_, b_ in (('Center','Left'), ('Left','Center'), ('Center','Right'), ('Right','Center'), ('Left','Right'), ('Right','Left')):
    for c3 in ((4000,3000), (6500,2500)):
        consumed = ww_run([(0,0), ('A', a_), (4000,0), ('A', b_), c3, 'Enter'])
        ref = [wall((0,0),(4000,0),150,a_.upper()), wall((4000,0),c3,150,b_.upper())]
        good = consumed and len(masters_now()) == 2 and master_offsets_ok() and normalized_ok() and no_zero_masters() \
               and same_lines(walls_now(), expected(ref))
        if not good: ok = False; bad.append((a_, b_, c3))
chk('WW alignment transitions C<->L, C<->R, L<->R at 90 deg and oblique corners: clean corner, centered masters', ok)
if bad: print('   ', bad)

# undo keeps current alignment; next segment uses it
consumed = ww_run([P_[0], P_[1], ('A','Left'), P_[2], ('A','Right'), P_[3], 'Undo', (0,6000), 'Enter'])
ref = [wall(P_[0],P_[1],150,'CENTER'), wall(P_[1],P_[2],150,'LEFT'), wall(P_[2],(0,6000),150,'RIGHT')]
chk('WW Undo removes only the RIGHT segment, Alignment stays RIGHT, the next segment is drawn RIGHT from the same input point',
    consumed and len(masters_now()) == 3 and A.ev('*wt:pos*') == 'RIGHT' and master_offsets_ok()
    and same_lines(walls_now(), expected(ref)))

# close uses the settings current at C
R_ = [(0,0),(6000,0),(6000,4000),(0,4000)]
consumed = ww_run([R_[0], R_[1], ('A','Left'), R_[2], ('W',200), ('A','Right'), R_[3], ('W',250), ('A','Left'), 'Close'])
ref = [wall(R_[0],R_[1],150,'CENTER'), wall(R_[1],R_[2],150,'LEFT'), wall(R_[2],R_[3],200,'RIGHT'), wall(R_[3],R_[0],250,'LEFT')]
chk('WW Close after changing Width 250 + Alignment LEFT: closing wall is 250 LEFT, loop closed with centered masters',
    consumed and len(masters_now()) == 4 and master_offsets_ok() and no_zero_masters()
    and widths_along(R_) == sorted([150,150,200,250]) and same_lines(walls_now(), expected(ref)))

# session memory across WW runs
ww_run([(0,0), (4000,0), ('A','Right'), ('W',200), 'Enter'])
ww_run([(0,-5000), (4000,-5000), 'Enter'], keep_session=True)
chk('WW remembers the last Alignment and Width for the next WW run', A.ev('*wt:pos*') == 'RIGHT' and A.ev('*wt:thk*') == 200.0
    and mk((0,-5100),(4000,-5100)) in master_set())

# =========================== TX (geometry-first junction cleanup) ===========================
def txl(*segs, layer='0'): return [mkline(a, b, layer) for a, b in segs]
def lines0(layer='0'): return sorted(lk(a, b) for _, a, b in A.db_lines(layer))
def lk(a, b):
    a, b = tuple(round(float(v), 3) + 0.0 for v in a), tuple(round(float(v), 3) + 0.0 for v in b); return min(a, b) + max(a, b)
def txrun(ens):
    A.G[A.Sym('*T-ENS*')] = list(ens)
    out = io.StringIO(); A.OUTPUT = True
    with contextlib.redirect_stdout(out): A.ev('(wt:tx-run *t-ens*)')
    A.OUTPUT = False; return out.getvalue()
def txcase(segs, layer='0'):
    fresh(); ens = txl(*segs, layer=layer); o = txrun(ens); return lines0(layer), o
def exp(*segs): return sorted(lk(a, b) for a, b in segs)
def idem(segs, layer='0'):
    fresh(); ens = txl(*segs, layer=layer); txrun(ens); first = lines0(layer)
    o = txrun([e for e, a, b in A.db_lines(layer)]); return first == lines0(layer) and '0 junction(s)' in o

# 1-3 simple L
g, o = txcase([((0,0),(995,0)), ((1000,40),(1000,2000))])
chk('TX 1: two lines short of corner -> both meet', g == exp(((0,0),(1000,0)), ((1000,0),(1000,2000))) and '1 junction' in o)
g, o = txcase([((0,0),(1060,0)), ((1000,0),(1000,2000))])
chk('TX 2: one overshoots corner -> trimmed', g == exp(((0,0),(1000,0)), ((1000,0),(1000,2000))))
g, o = txcase([((0,0),(1060,0)), ((1000,-50),(1000,2000))])
chk('TX 3: both overshoot -> both terminate at intersection', g == exp(((0,0),(1000,0)), ((1000,0),(1000,2000))))
# 4-7 T / X
g, o = txcase([((0,0),(5000,0)), ((2000,40),(2000,3000))])
chk('TX 4: T branch short -> extended, host untouched', g == exp(((0,0),(5000,0)), ((2000,0),(2000,3000))))
g, o = txcase([((0,0),(5000,0)), ((2000,-60),(2000,3000))])
chk('TX 5: T branch overshoot -> trimmed to host', g == exp(((0,0),(5000,0)), ((2000,0),(2000,3000))))
clean_t = [((0,0),(5000,0)), ((2000,0),(2000,3000))]
g, o = txcase(clean_t)
chk('TX 6: clean T -> no change', g == exp(*clean_t) and '0 junction' in o)
clean_x = [((0,0),(5000,0)), ((2000,-2000),(2000,3000))]
g, o = txcase(clean_x)
chk('TX 7: clean X -> no change, not split', g == exp(*clean_x) and '0 junction' in o)
# 8-10 collinear
g, o = txcase([((0,0),(2000,0)), ((2080,0),(5000,0))])
chk('TX 8: collinear gap -> one line', g == exp(((0,0),(5000,0))))
g, o = txcase([((0,0),(3000,0)), ((2000,0),(5000,0))])
chk('TX 9: collinear overlap -> one line', g == exp(((0,0),(5000,0))))
g, o = txcase([((0,0),(3000,0)), ((3000,0),(0,0)), ((500,0),(900,0))])
chk('TX 10: exact duplicate + tiny fragment -> one line', g == exp(((0,0),(3000,0))))
# 11 unrelated / far
far = [((0,0),(2000,0)), ((2600,500),(2600,3000)), ((0,3000),(1500,3000))]
g, o = txcase(far)
chk('TX 11: unrelated lines beyond distance -> no change', g == exp(*far) and '0 junction' in o)
# 12 ambiguous endpoint: two hosts at equal distance
amb = [((0,0),(1000,0)), ((1040,-2000),(1040,2000)), ((960,-2000),(960,2000))]
g, o = txcase(amb)
chk('TX 12: ambiguous endpoint -> no change, reported', g == exp(*amb) and 'ambiguous' in o)
amb2 = [((0,0),(1000,0)), ((1050,-2000),(1050,2000)), ((1000,60),(3000,60))]
g, o = txcase([((0,0),(1000,0)), ((1030,-2000),(1030,2000)), ((5000,5000),(6000,5000))])
chk('TX 12b: unambiguous single host still repaired', ((1030.0,0.0) in [(r[2], r[3]) for r in g]) or lk((0,0),(1030,0)) in g)
# 13 angled L (37 deg)
c = math.cos(math.radians(37)); s_ = math.sin(math.radians(37))
p_end = (1000 + 30*c, 30*s_)
g, o = txcase([((0,0),(1000,0)), (p_end, (1000 + 2000*c, 2000*s_))])
chk('TX 13: angled 37 deg L -> exact intersection', g == exp(((0,0),(1000,0)), ((1000,0),(1000 + 2000*c, 2000*s_))))
# 14 selection order
segs14 = [((0,0),(995,0)), ((1000,40),(1000,2000)), ((0,3000),(2000,3000)), ((2050,3000),(4000,3000)), ((3000,2940),(3000,1000))]
fresh(); ens = txl(*segs14); txrun(ens); g1 = lines0()
fresh(); ens = txl(*segs14); txrun(list(reversed(ens))); g2 = lines0()
chk('TX 14: reversed selection order -> identical geometry', g1 == g2)
chk('TX idempotent: second run over simple fixes changes nothing', idem(segs14))

# 15 double-line L (90 deg): horizontal wall y 0..150 to the right, vertical wall x 1000..1150 downwards
dl = [((0,150),(1080,150)), ((0,0),(1080,0)), ((1150,80),(1150,-2000)), ((1000,80),(1000,-2000))]
g, o = txcase(dl)
exp15 = exp(((0,150),(1150,150)), ((0,0),(1000,0)), ((1150,150),(1150,-2000)), ((1000,0),(1000,-2000)))
chk('TX 15: double-line L -> clean mitred corner', g == exp15)
chk('TX 15: idempotent', idem(dl))
# butt-ended L (faces flush) -> mitre
g, o = txcase([((0,150),(1000,150)), ((0,0),(1000,0)), ((1150,0),(1150,-2000)), ((1000,0),(1000,-2000))])
chk('TX 15b: butt corner (short outer faces) -> clean corner', g == exp15)
# 16 angled double-line L
a2 = math.radians(120); u2 = (math.cos(a2), math.sin(a2)); n2 = (-u2[1], u2[0])
def pt(o, u, t, n, d): return (o[0] + u[0]*t + n[0]*d, o[1] + u[1]*t + n[1]*d)
# wall A along +x (faces y=0, y=150) ending near origin corner at x=2000; wall B from corner along u2, faces offset 0 and -150 on its normal
def inter(p, u, q, v):
    d = u[0]*v[1] - u[1]*v[0]; t = ((q[0]-p[0])*v[1] - (q[1]-p[1])*v[0]) / d; return (p[0]+u[0]*t, p[1]+u[1]*t)
C = (2000, 0)
bo = [pt(C, u2, 0, n2, 0), pt(C, u2, 0, n2, -150)]          # B face base points
xa0 = inter((0,0),(1,0), bo[0], u2); xa1 = inter((0,150),(1,0), bo[1], u2)
xb0 = inter((0,0),(1,0), bo[1], u2); xb1 = inter((0,150),(1,0), bo[0], u2)
ang = [((0,0),(2040,0)), ((0,150),(1900,150)), (pt(C,u2,60,n2,0), pt(C,u2,3000,n2,0)), (pt(C,u2,-40,n2,-150), pt(C,u2,3000,n2,-150))]
fresh(); ens = txl(*ang); o = txrun(ens)
got = lines0()
def has_seg(g, a, b, tol=0.01): return any((math.dist(a, r[:2]) < tol and math.dist(b, r[2:]) < tol) or (math.dist(b, r[:2]) < tol and math.dist(a, r[2:]) < tol) for r in g)
ok16 = len(got) == 4 and sum(1 for r in got if any(math.dist(x, r[:2]) < 0.01 or math.dist(x, r[2:]) < 0.01 for x in (xa0, xa1, xb0, xb1))) == 4
# every line end at the corner must be shared by exactly two lines (closed mitre)
ends = [r[:2] for r in got] + [r[2:] for r in got]
corner_ends = [e for e in ends if math.dist(e, C) < 600]
ok16 = ok16 and len(corner_ends) == 4 and all(sum(1 for f in corner_ends if math.dist(e, f) < 0.01) == 2 for e in corner_ends)
chk('TX 16: angled (120 deg) double-line L -> face mitres meet exactly', ok16)
# 17 different widths 150 meets 100
dw = [((0,150),(1050,150)), ((0,0),(1050,0)), ((1100,60),(1100,-2000)), ((1000,60),(1000,-2000))]
g, o = txcase(dw)
chk('TX 17: 150 pair meets 100 pair -> clean corner', g == exp(((0,150),(1100,150)), ((0,0),(1000,0)), ((1100,150),(1100,-2000)), ((1000,0),(1000,-2000))))
# 18 double-line T: host y 0..150 continuous, branch x 2000..2150 from below ending at host inner face y=0
exp18 = exp(((0,150),(5000,150)), ((0,0),(2000,0)), ((2150,0),(5000,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000)))
g, o = txcase([((0,150),(5000,150)), ((0,0),(5000,0)), ((2000,-40),(2000,-3000)), ((2150,30),(2150,-3000))])
chk('TX 18: double-line T -> host outer continuous, inner opened, branch terminated', g == exp18)
g, o = txcase([((0,150),(5000,150)), ((0,0),(2000,0)), ((2150,0),(5000,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000))])
chk('TX 18b: clean double-line T -> no change', g == exp18 and '0 junction' in o)
# 19 messy double-line T: outer split with small gap, inner pieces short/overshooting, branch faces over/short
messy = [((0,150),(2500,150)), ((2520,150),(5000,150)),
         ((0,0),(1960,0)), ((2190,0),(5000,0)),
         ((2000,120),(2000,-3000)), ((2150,-50),(2150,-3000))]
g, o = txcase(messy)
chk('TX 19: messy double-line T -> clean T', g == exp18)
chk('TX 19: idempotent', idem(messy))
# 20 wall lines on ordinary layers, zero AKD walls, no registry
fresh(); A.ev('(setq *wt:reg* nil)'); ens = txl(*dl, layer='WALLS-EXIST'); txrun(ens)
chk('TX 20: ordinary layer, no AKD data -> repaired, layer preserved', lines0('WALLS-EXIST') == exp15 and not A.db_lines('A-WALL') and not A.db_lines('X-AXIS'))
# axis lines skipped, unsupported ignored
fresh(); ens = txl(((0,0),(995,0)), layer='X-AXIS') + txl(((1000,40),(1000,2000)))
circ = A.ev('(progn (entmake (list (cons 0 "CIRCLE") (cons 8 "0") (cons 10 (list 0.0 0.0 0.0)) (cons 40 5.0))) (entlast))')
o = txrun(ens + [circ])
chk('TX: X-AXIS master skipped (untouched), circle ignored', lines0('X-AXIS') == exp(((0,0),(995,0))) and 'X-AXIS master line skipped' in o
    and 'unsupported object' in o and A.entget(circ) is not None)
# properties preserved on modify and on T split copies
fresh(); ens = txl(((0,150),(5000,150)), ((0,0),(5000,0)), ((2000,-40),(2000,-3000)), ((2150,30),(2150,-3000)))
inner = ens[1]; A.DB[inner].append(A.cons(62, 3)); A.DB[inner].append(A.cons(6, 'HIDDEN'))
txrun(ens)
props = [{A.car(x): A.cdr(x) for x in A.DB[e]} for e, a, b in A.db_lines('0') if abs(a[1]) < 1e-6 and abs(b[1]) < 1e-6]
chk('TX: split host face keeps colour/linetype on both pieces', len(props) == 2 and all(p.get(62) == 3 and p.get(6) == 'HIDDEN' for p in props))
# undo record restores exactly
fresh(); ens = txl(*messy); before = lines0(); cap = []
_orig = A.G[A.Sym('WT:TX-APPLY')]
def _wrap(*a):
    r = A.call(_orig, [], []); cap.append(A.G[A.Sym('*WT:PENDING*')]); return r
A.G[A.Sym('WT:TX-APPLY')] = _wrap
txrun(ens); changed = lines0() != before
A.G[A.Sym('WT:TX-APPLY')] = _orig
A.G[A.Sym('*T-REC*')] = cap[0]; A.ev('(wt:seg-undo *t-rec*)')
chk('TX: transaction record reverts the whole repair exactly', changed and lines0() == before)
# --- TX double-line CROSS (+) ---
def cross_exp(hy, vx, L=3000):
    (y0, y1), (x0, x1) = hy, vx
    return exp(((-L,y1),(x0,y1)), ((x1,y1),(L,y1)), ((-L,y0),(x0,y0)), ((x1,y0),(L,y0)),
               ((x0,-L),(x0,y0)), ((x0,y1),(x0,L)), ((x1,-L),(x1,y0)), ((x1,y1),(x1,L)))
cont = [((-3000,150),(3000,150)), ((-3000,0),(3000,0)), ((1000,-3000),(1000,3000)), ((1100,-3000),(1100,3000))]
g, o = txcase(cont)
exp_c = cross_exp((0,150), (1000,1100))
chk('TX CROSS 1: four continuous faces (150 x 100) -> clean + opening', g == exp_c and '1 junction' in o)
chk('TX CROSS 1: second run unchanged (openings not re-merged)', idem(cont))
clean_c = [((-3000,150),(1000,150)), ((1100,150),(3000,150)), ((-3000,0),(1000,0)), ((1100,0),(3000,0)),
           ((1000,-3000),(1000,0)), ((1000,150),(1000,3000)), ((1100,-3000),(1100,0)), ((1100,150),(1100,3000))]
g, o = txcase(clean_c)
chk('TX CROSS 2: already clean cross -> 0 repairs, not ambiguous', g == exp_c and '0 junction' in o and 'ambiguous' not in o)
messy_c = [((-3000,150),(3000,150)),                       # outer H continuous
           ((-3000,0),(980,0)), ((1130,0),(3000,0)),        # inner H split, short on both sides
           ((1000,-3000),(1000,40)), ((1000,170),(1000,3000)),  # V1 split: overshoots into overlap / stops short
           ((1100,-3000),(1100,3000)),                      # V2 continuous
           ((1020,0),(1060,0))]                             # redundant fragment inside overlap
g, o = txcase(messy_c)
chk('TX CROSS 3: mixed split/continuous/overshoot/short/fragment -> one clean +', g == exp_c)
chk('TX CROSS 3: idempotent', idem(messy_c))
g, o = txcase([((-3000,200),(3000,200)), ((-3000,0),(3000,0)), ((500,-3000),(500,3000)), ((575,-3000),(575,3000))])
chk('TX CROSS 4: 200 x 75 -> correct four intersections', g == cross_exp((0,200), (500,575)))
# angled 63 deg
u = (math.cos(math.radians(63)), math.sin(math.radians(63))); nrm = (-u[1], u[0])
def along(base, t, d): return (base[0] + u[0]*t + nrm[0]*d, base[1] + u[1]*t + nrm[1]*d)
C0 = (1000.0, 75.0)
vf = [(along(C0, -3000, 0), along(C0, 3000, 0)), (along(C0, -3000, -100), along(C0, 3000, -100))]
ang_c = [((-3000,150),(3000,150)), ((-3000,0),(3000,0))] + vf
fresh(); ens = txl(*ang_c); o = txrun(ens); got = lines0()
def xpt(p, d, q, e):
    den = d[0]*e[1] - d[1]*e[0]; t_ = ((q[0]-p[0])*e[1] - (q[1]-p[1])*e[0]) / den; return (p[0]+d[0]*t_, p[1]+d[1]*t_)
PX = {(hy, k): xpt((0, hy), (1, 0), vf[k][0], u) for hy in (0, 150) for k in (0, 1)}
def sx(p): return p[0]
exp_a = []
for hy in (0, 150):
    xs = sorted([PX[(hy,0)], PX[(hy,1)]], key=sx)
    exp_a += [((-3000,hy), xs[0]), (xs[1], (3000,hy))]
for k in (0, 1):
    ys = sorted([PX[(0,k)], PX[(150,k)]], key=lambda p: p[1])
    exp_a += [(vf[k][0], ys[0]), (ys[1], vf[k][1])]
ok_a = len(got) == 8 and all(has_seg(got, a, b) for a, b in exp_a)
chk('TX CROSS 5: 63 deg cross -> parallelogram opening from exact intersections', ok_a)
chk('TX CROSS 5: idempotent', idem(ang_c))
# T with small branch overshoot through the host stays T; wall L with both faces overshooting stays L
g, o = txcase([((0,150),(5000,150)), ((0,0),(5000,0)), ((2000,180),(2000,-3000)), ((2150,180),(2150,-3000))])
chk('TX CROSS vs T: branch overshooting host by 30 -> existing T result, not cross', g == exp18)
g, o = txcase([((0,150),(1180,150)), ((0,0),(1180,0)), ((1150,180),(1150,-2000)), ((1000,180),(1000,-2000))])
chk('TX CROSS vs L: corner with both faces overshooting -> existing L result, not cross', g == exp15)
# properties preserved on both pieces
fresh(); ens = txl(*cont); A.DB[ens[1]].append(A.cons(62, 5)); A.DB[ens[1]].append(A.cons(370, 35)); txrun(ens)
props = [{A.car(x): A.cdr(x) for x in A.DB[e]} for e, a, b in A.db_lines('0') if abs(a[1]) < 1e-6 and abs(b[1]) < 1e-6]
chk('TX CROSS: split face keeps colour/lineweight on both pieces', len(props) == 2 and all(p_.get(62) == 5 and p_.get(370) == 35 for p_ in props))
# one long wall crossed by two walls -> two independent crosses
two = [((-4000,150),(4000,150)), ((-4000,0),(4000,0)), ((-1500,-3000),(-1500,3000)), ((-1400,-3000),(-1400,3000)),
       ((1500,-3000),(1500,3000)), ((1600,-3000),(1600,3000))]
g, o = txcase(two)
chk('TX CROSS: long wall crossed twice -> both openings cut', len(g) == 14 and '2 junction' in o and lk((-1400,0),(1500,0)) in g)
# three wall families through one node -> ambiguous, unchanged
d = (math.cos(math.radians(45)), math.sin(math.radians(45)))
star = cont + [((1050-3000*d[0], 75-3000*d[1]), (1050+3000*d[0], 75+3000*d[1])),
               ((1050-3000*d[0]+60, 75-3000*d[1]-60), (1050+3000*d[0]+60, 75+3000*d[1]-60))]
g, o = txcase(star)
chk('TX CROSS: three wall families at one node -> unchanged, reported ambiguous', g == sorted(lk(a, b) for a, b in star) and 'ambiguous' in o)
# single-line X untouched
g, o = txcase(clean_x)
chk('TX CROSS: single-line clean X still unchanged', g == exp(*clean_x) and '0 junction' in o)
# --- TX exact-touch double-line T (branch ends exactly on the host near face) ---
def near_open(g, y, x1, x2): return not any(abs(r[1]-y) < 1e-3 and abs(r[3]-y) < 1e-3 and min(r[0], r[2]) < x1 + 1e-3 and max(r[0], r[2]) > x2 - 1e-3 for r in g)
et = [((0,150),(5000,150)), ((0,0),(5000,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000))]
g, o = txcase(et)
chk('TX exact-touch T: branch movement 0, near face opened, far face unchanged',
    g == exp18 and lk((2000,0),(2000,-3000)) in g and lk((2150,0),(2150,-3000)) in g and lk((0,150),(5000,150)) in g and '1 junction' in o)
fresh(); ens = txl(*et); txrun(ens); first = lines0(); o2 = txrun([e for e, a, b in A.db_lines('0')])
chk('TX exact-touch T: second run 0 repairs, no duplicates, no extra split', lines0() == first and len(first) == 5 and '0 junction' in o2)
g, o = txcase([((0,150),(5000,150)), ((0,0),(5000,0)), ((2000,0),(2000,-3000)), ((2100,0),(2100,-3000))])
chk('TX exact-touch T: host 150 / branch 100', g == exp(((0,150),(5000,150)), ((0,0),(2000,0)), ((2100,0),(5000,0)), ((2000,0),(2000,-3000)), ((2100,0),(2100,-3000))))
ub = (math.cos(math.radians(63)), math.sin(math.radians(63))); xb2 = 2000 + 100 / ub[1]
angt = [((0,150),(5000,150)), ((0,0),(5000,0)), ((2000,0),(2000-3000*ub[0],-3000*ub[1])), ((xb2,0),(xb2-3000*ub[0],-3000*ub[1]))]
fresh(); ens = txl(*angt); o = txrun(ens); got = lines0()
chk('TX exact-touch T: 63 deg branch -> opening between B1 x Hnear and B2 x Hnear',
    len(got) == 5 and all(has_seg(got, a, b) for a, b in [((0,0),(2000,0)), ((xb2,0),(5000,0)), ((0,150),(5000,150)), angt[2], angt[3]]))
tie1 = et + [((2300,-1000),(2300,-100)), ((2450,-1000),(2450,-100))]
g, o = txcase(tie1)
chk('TX exact-touch T beside another wall at equal spacing (partner tie) -> both T repaired',
    g == exp(((0,150),(5000,150)), ((0,0),(2000,0)), ((2150,0),(2300,0)), ((2450,0),(5000,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000)),
             ((2300,-1000),(2300,0)), ((2450,-1000),(2450,0))))
tie2 = et + [((0,-150),(1500,-150))]
g, o = txcase(tie2)
chk('TX exact-touch T with a short parallel line beyond the near face (partner tie) -> T repaired, line untouched',
    g == sorted(exp18 + [lk((0,-150),(1500,-150))]))
tie3 = [((0,150),(5000,150)), ((0,0),(5000,0)), ((0,-150),(5000,-150)), ((2000,-150),(2000,-3000)), ((2150,-150),(2150,-3000))]
g, o = txcase(tie3)
chk('TX genuine tie (three equal parallel lines) stays unpaired -> unchanged', g == exp(*tie3) and '0 junction' in o)
single_on_face = [((0,150),(5000,150)), ((0,0),(5000,0)), ((2000,0),(2000,-3000))]
g, o = txcase(single_on_face)
chk('TX single line ending exactly on a wall face -> unchanged (0 repairs)', g == exp(*single_on_face) and '0 junction' in o)
# --- TX repair field (window UX) ---
def _setf(fld): A.G[A.Sym('*T-F*')] = [[float(x), float(y)] for x, y in fld]
def _cap(src):
    out = io.StringIO(); A.OUTPUT = True
    with contextlib.redirect_stdout(out): A.ev(src)
    A.OUTPUT = False; return out.getvalue()
def txf(segs, fld, layer='0'):
    fresh(); txl(*segs, layer=layer); _setf(fld); o = _cap('(wt:tx-field-run *t-f*)'); return lines0(layer), o
def txf_more(fld, layer='0'):
    _setf(fld); o = _cap('(wt:tx-field-run *t-f*)'); return lines0(layer), o
def twg(segs, fld, tol=150.0):
    fresh(); A.ev('(setq *wt:reg* nil)'); txl(*segs); _setf(fld); o = _cap(f'(wt:tw-run *t-f* {float(tol)})'); return lines0(), o
FLD_L = field(800, -200, 1300, 300); FLD_T = field(1800, -300, 2400, 300); FLD_X = field(800, -200, 1300, 350)
g, o = txf(dl, FLD_L)
chk('TX field: messy double-line L -> same result as selection TX (no cap at the corner)', g == exp15 and 'cap' not in o)
g, o = txf([((0,150),(5000,150)), ((0,0),(5000,0)), ((2000,-40),(2000,-3000)), ((2150,30),(2150,-3000))], FLD_T)
chk('TX field: T short/penetrating branch -> same clean T, no branch cap', g == exp18)
g, o = txf([((0,150),(5000,150)), ((0,0),(5000,0)), ((2000,180),(2000,-3000)), ((2150,180),(2150,-3000))], FLD_T)
chk('TX field: T overshooting branch -> same clean T', g == exp18)
g, o = txf(et, FLD_T)
chk('TX field: exact-touch T -> same clean T', g == exp18)
g, o = txf(messy, field(1800, -300, 2600, 300))   # window covers the outer-face gap at x=2500
chk('TX field: messy T -> same clean T', g == exp18)
g, o = txf(messy_c, FLD_X)
chk('TX field: messy CROSS -> same clean +, no central caps', g == exp_c)
g, o = txf(cont, FLD_X)
chk('TX field: continuous CROSS -> same clean +', g == exp_c)
g, o = txf(tie1, field(1800, -1200, 2700, 300))
chk('TX field: exact-touch T with partner tie -> same result', g == exp(((0,150),(5000,150)), ((0,0),(2000,0)), ((2150,0),(2300,0)), ((2450,0),(5000,0)),
    ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000)), ((2300,-1000),(2300,0)), ((2450,-1000),(2450,0)),
    ((2300,-1000),(2450,-1000))))   # second branch's free far end lies in the window -> capped
# distant junction on the same long geometry is untouched
far2 = [((0,150),(25000,150)), ((0,0),(25000,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000)),
        ((20000,0),(20000,-3000)), ((20150,0),(20150,-3000)), ((9000,0),(9400,0))]      # + duplicate fragment far away
g, o = txf(far2, FLD_T)
chk('TX field: junction A repaired, junction B and distant duplicate untouched',
    lk((0,0),(2000,0)) in g and lk((2150,0),(25000,0)) in g and lk((9000,0),(9400,0)) in g and len(g) == 8
    and lk((20000,0),(20000,-3000)) in g)
# caps
wall1 = [((0,150),(3000,150)), ((0,0),(3000,0))]
FLD_E = field(2800, -100, 3200, 250)
g, o = txf(wall1, FLD_E)
chk('TX cap: free wall end in field -> one cap created, other end untouched', g == exp(*wall1, ((3000,0),(3000,150))) and '1 wall end cap' in o)
g2, o2 = txf_more(FLD_E)
chk('TX cap: second run -> 0 cap changes, no duplicate', g2 == g and 'cap(s) updated' not in o2 and '0 junction' in o2)
g, o = txf(wall1 + [((3000,0),(3000,150))], FLD_E)
chk('TX cap: existing exact cap -> unchanged', g == exp(*wall1, ((3000,0),(3000,150))) and 'cap(s) updated' not in o)
g, o = txf(wall1 + [((3000,-30),(3000,190))], FLD_E)
chk('TX cap: oversized cap normalized to the face endpoints', g == exp(*wall1, ((3000,0),(3000,150))))
g, o = txf(wall1 + [((3000,20),(3000,140))], FLD_E)
chk('TX cap: undersized cap normalized to the face endpoints', g == exp(*wall1, ((3000,0),(3000,150))))
g, o = txf(wall1, field(-200, -100, 3200, 250))
chk('TX cap: whole isolated wall in field -> both ends capped', g == exp(*wall1, ((3000,0),(3000,150)), ((0,0),(0,150))))
g, o = txf([((0,150),(3000,150)), ((0,0),(2960,0))], FLD_E)
chk('TX cap: staggered face ends -> no cap (conservative)', g == exp(((0,150),(3000,150)), ((0,0),(2960,0))))
g, o = txf(clean_c, FLD_X)
chk('TX cap: clean cross -> no caps, 0 changes', g == exp_c and 'cap' not in o)
g, o = txf(exp18_segs := [((0,150),(5000,150)), ((0,0),(2000,0)), ((2150,0),(5000,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000))], FLD_T)
chk('TX cap: clean T -> no branch cap at the host', g == exp18 and 'cap' not in o)
g, o = txf([((0,150),(5000,150)), ((0,0),(5000,0)), ((2000,-40),(2000,-3000)), ((2150,-40),(2150,-3000)), ((2000,-40),(2150,-40))], FLD_T)
chk('TX cap: capped branch end becomes a T -> stale cap erased', g == exp18)
g, o = txf(dl + [((1080,0),(1080,150))], FLD_L)
chk('TX cap: capped wall end becomes an L -> stale cap erased', g == exp15)
g, o = txf([((0,150),(3000,150)), ((0,0),(3000,0)), ((3000,-500),(3000,500))], FLD_E)
chk('TX cap: wall end against a single boundary line -> no cap, line kept', g == exp(*wall1, ((3000,-500),(3000,500))))
fresh(); ens = txl(*dl); A.POINTQ[:] = [[800.0, -200.0, 0.0], [1300.0, 300.0, 0.0]]; A.SSFIRST[0] = None
_cap('(c:TX)')
chk('TX command: two corners -> field repair', lines0() == exp15)

# --- TW on ordinary double-line walls ---
FLD_TW = field(1500, -500, 2700, 400)
g, o = twg([((0,150),(5000,150)), ((0,0),(5000,0)), ((2000,-50),(2000,-3000)), ((2150,-50),(2150,-3000))], FLD_TW)
chk('TW generic T: branch 50 short -> topology connected, clean T by shared cleanup',
    g == exp18 and '1 generic wall junction' in o and not A.db_lines('X-AXIS') and A.ev('*wt:reg*') is None)
g, o = twg([((0,150),(1000,150)), ((0,0),(1000,0)), ((1150,0),(1150,-2000)), ((1000,0),(1000,-2000))], field(700, -300, 1400, 400))
chk('TW generic L: both walls short of the corner -> same clean L as TX',
    g == exp(((0,150),(1150,150)), ((0,0),(1000,0)), ((1150,150),(1150,-2000)), ((1000,0),(1000,-2000))) and '1 generic wall junction' in o)
g, o = twg([((0,150),(2000,150)), ((0,0),(2000,0)), ((2200,150),(4000,150)), ((2200,0),(4000,0))], field(1800, -100, 2400, 250), tol=300.0)
chk('TW generic collinear: runs 200 apart (tol 300) -> one continuous wall', g == exp(((0,150),(4000,150)), ((0,0),(4000,0))))
runs = [((0,150),(2000,150)), ((0,0),(2000,0)), ((3000,150),(5000,150)), ((3000,0),(5000,0))]
g, o = twg(runs, field(1800, -100, 3200, 250), tol=300.0)
chk('TW generic collinear: gap 1000 > tol -> runs not connected', all(lk(a, b) in g for a, b in runs) and not any(r[0] < 2000 and r[2] > 3000 for r in g))
g, o = twg(exp18_segs, FLD_TW)
chk('TW generic: clean T -> 0 repairs, unchanged', g == exp18 and '0 generic wall junction(s) repaired, 0 visible' in o)
g, o = twg([((0,150),(1150,150)), ((0,0),(1000,0)), ((1150,150),(1150,-2000)), ((1000,0),(1000,-2000))], field(700, -300, 1400, 400))
chk('TW generic: clean L -> 0 repairs, unchanged', g == exp(((0,150),(1150,150)), ((0,0),(1000,0)), ((1150,150),(1150,-2000)), ((1000,0),(1000,-2000))) and '0 generic' in o)
amb_tw = [((0,-300),(5000,-300)), ((0,-350),(5000,-350)), ((0,150),(5000,150)), ((0,0),(5000,0)), ((2000,-600),(2000,-3000)), ((2150,-600),(2150,-3000))]
g, o = twg(amb_tw, field(1500, -800, 2700, 300), tol=800.0)
chk('TW generic: two plausible hosts -> branch unchanged, ambiguity reported',
    lk((2000,-600),(2000,-3000)) in g and lk((2150,-600),(2150,-3000)) in g and 'ambiguous' in o)
fresh(); A.ev('(setq *wt:reg* nil)'); txl(((0,150),(5000,150)), ((0,0),(5000,0)), ((2000,-50),(2000,-3000)), ((2150,-50),(2150,-3000)))
A.POINTQ[:] = [[1500.0, -500.0, 0.0], [2700.0, 400.0, 0.0]]; _cap('(c:TW)')
chk('TW command: ordinary double-line walls repaired without AKD data', lines0() == exp18 and not A.db_lines('X-AXIS'))
# AKD walls in the window: generic path leaves their A-WALL lines alone
build([wall((0,0),(10000,0)), wall((5000,50),(5000,4000))]); before_akd = geo()
_setf(field(4000,-1000,6000,1000)); o = _cap('(wt:tw-run *t-f* 150.0)')
chk('TW AKD: window run still repairs AKD topology, no generic walls reported',
    mk((5000,0),(5000,4000)) in mset() and lines_match_masters() and 'generic' not in o)
# PickFirst command
fresh(); ens = txl(((0,0),(995,0)), ((1000,40),(1000,2000)))
run_cmd('TX', ens)
chk('TX PickFirst command: repaired', lines0() == exp(((0,0),(1000,0)), ((1000,0),(1000,2000))))
c = cfg_run('TX_CONNECT_DISTANCE=-1\nTX_WALL_MAX=400\n'); txcfg = c('TX_CONNECT_DISTANCE') == 150.0 and c('TX_WALL_MAX') == 400.0
cfg_run(None)
chk('config: TX_CONNECT_DISTANCE / TX_WALL_MAX optional, invalid -> default', txcfg)


# --- TW integration on the centerline base ---
def twrun(fld, tol=150.0):
    A.G[A.Sym('*T-F*')] = [[float(x), float(y)] for x, y in fld]
    return A.ev(f'(wt:tw-run *t-f* {float(tol)})')
fresh(200, 'LEFT'); add(((0,0),(6000,0))); settings(150, 'RIGHT'); add(((3000,-2000),(3000,-50)))
twrun(field(2000,-1000,4000,1000))
chk('TW on centered walls drawn LEFT (host) and RIGHT (branch): gap closed on the host centerline, T split, old-model faces',
    mk((3075,-2000),(3075,100)) in master_set() and master_offsets_ok() and normalized_ok()
    and same_lines(walls_now(), expected([wall((0,0),(6000,0),200,'LEFT'), wall((3000,-2000),(3000,0),150,'RIGHT')])))
def legacy_host():
    fresh()
    mkline((0,0),(6000,0),'X-AXIS')
    for a, b in expected([wall((0,0),(6000,0),200,'LEFT')]): mkline(a, b, 'A-WALL')
legacy_host()
leg_lines = geo()[0]
settings(150, 'CENTER'); add(((3000,-2000),(3000,-40)))                 # AKD branch, 140 short of the true host centre
A.ev('(setq *wt:reg* nil)'); add(((10000,0),(14000,0))); add(((12000,-2000),(12000,-60)))   # AKD T with a 60 gap
g_ids = txl(((20000,150),(25000,150)), ((20000,0),(25000,0)), ((22000,-50),(22000,-3000)), ((22150,-50),(22150,-3000)), layer='A-WALL')  # ordinary T, 50 short
leg_ids = [e for e, a, b in A.db_lines('A-WALL') if a[0] <= 6000 and b[0] <= 6000 and a[1] >= -1 and b[1] >= -1]
twrun(field(-500,-1000,26000,1000))
leg_after = sorted((round(a[0],3), round(a[1],3), round(b[0],3), round(b[1],3)) for e, a, b in A.db_lines('A-WALL') if e in leg_ids)
chk('TW mixed area: AKD T connected, ordinary double-line T connected, legacy off-centre wall left exactly as is',
    mk((12000,-2000),(12000,0)) in master_set()
    and any(abs(a[0]-22000) < 1e-3 and abs(max(a[1], b[1])) < 1e-3 and abs(min(a[1], b[1]) + 3000) < 1e-3 for e, a, b in A.db_lines('A-WALL') if e not in leg_ids)
    and len(leg_after) == len(leg_ids) and all(e in A.DB and e not in A.DELETED for e in leg_ids)
    and mk((0,0),(6000,0)) in master_set() and mk((3000,-2000),(3000,-40)) in master_set())
legacy_host(); settings(150, 'CENTER'); add(((3000,-2000),(3000,-40))); A.ev('(setq *wt:reg* nil)')
before = geo(); twrun(field(2000,-1000,4000,1000)); untouched = geo() == before
wr(field(-500,-500,6500,500))
twrun(field(2000,-1000,4000,1000))
chk('Legacy host: TW leaves it alone before WWR; after WWR (centred) TW connects the branch to the true centreline',
    untouched and master_offsets_ok() and normalized_ok() and mk((3000,-2000),(3000,100)) in master_set()
    and same_lines(walls_now(), expected([wall((0,0),(6000,0),200,'LEFT'), wall((3000,-2000),(3000,0),150)])))

# =========================== WWD (wall to distance) ===========================
def wwd(e1, q1, e2, q2, d):
    A.G[A.Sym('*W1*')] = e1; A.G[A.Sym('*W2*')] = e2
    out = io.StringIO(); A.OUTPUT = False
    return A.ev(f'(wt:wwd-run *w1* {P(*q1)} *w2* {P(*q2)} {float(d)})')
def wres(e, q):
    A.G[A.Sym('*W1*')] = e; return A.ev(f'(wt:wwd-resolve *w1* {P(*q)})')
def isrec(r): return isinstance(r, list)
def allg(): return sorted(lines0() + lines0('A-WALL') + lines0('X-AXIS'))
def aw(a, b): return find_line('A-WALL', a, b)
ref2 = [((3000,0),(3000,3000)), ((3150,0),(3150,3000))]

# 1 AKD -> AKD, exact picked faces
build([wall((0,0),(0,3000)), wall((3000,0),(3000,3000))]); keep_ref = find_line('X-AXIS', (3000,0), (3000,3000))
r = wwd(aw((75,0),(75,3000)), (75,500), aw((2925,0),(2925,3000)), (2925,2500), 1200)
chk('WWD AKD->AKD: moving RIGHT face 1200 from reference LEFT face, only moving master relocates',
    isrec(r) and mset() == sorted([mk((1650,0),(1650,3000)), mk((3000,0),(3000,3000))]) and lines_match_masters()
    and keep_ref not in A.DELETED)
build([wall((0,0),(0,3000)), wall((3000,0),(3000,3000))])
r = wwd(aw((-75,0),(-75,3000)), (-75,2800), aw((3075,0),(3075,3000)), (3075,100), 1200)
chk('WWD AKD->AKD: moving LEFT face / reference RIGHT face (picks at opposite ends) respected',
    isrec(r) and mk((1950,0),(1950,3000)) in mset() and lines_match_masters())
build([wall((0,0),(0,3000)), wall((3000,0),(3000,3000))]); before = allg()
rec = wwd(aw((75,0),(75,3000)), (75,500), aw((2925,0),(2925,3000)), (2925,2500), 1200); moved = allg() != before
undo_rec(rec)
chk('WWD AKD: one transaction record restores the exact original drawing', moved and allg() == before)
# widths 150 / 300, zero, negative, not parallel
build([wall((0,0),(0,3000)), wall((3000,0),(3000,3000),300)])
r = wwd(aw((75,0),(75,3000)), (75,500), aw((2850,0),(2850,3000)), (2850,500), 1000)
chk('WWD widths 150/300: selected face-to-face distance 1000 (not centerline)', isrec(r) and mk((1775,0),(1775,3000)) in mset())
build([wall((0,0),(0,3000)), wall((3000,0),(3000,3000))])
r = wwd(aw((75,0),(75,3000)), (75,500), aw((2925,0),(2925,3000)), (2925,500), 0)
chk('WWD distance 0: selected faces coincide', isrec(r) and mk((2850,0),(2850,3000)) in mset())
build([wall((0,0),(0,3000)), wall((3000,0),(3000,3000))]); before = allg()
r = wwd(aw((75,0),(75,3000)), (75,500), aw((2925,0),(2925,3000)), (2925,500), -100)
chk('WWD negative distance: rejected, no change', isinstance(r, str) and 'zero or greater' in r and allg() == before)
build([wall((0,0),(0,3000)), wall((1000,4000),(4000,4000))]); before = allg()
r = wwd(aw((75,0),(75,3000)), (75,500), aw((1000,3925),(4000,3925)), (2000,3925), 1200)
chk('WWD non-parallel walls: rejected with message, no change', isinstance(r, str) and 'not parallel' in r and allg() == before)
build([wall((0,0),(3000,0))])
r = wres(aw((0,-75),(0,75)), (0,10))
chk('WWD AKD end cap pick: rejected as end cap', isinstance(r, str) and 'end cap' in r)
build([wall((0,0),(0,3000)), wall((3000,0),(3000,3000))])
r = wwd(aw((75,0),(75,3000)), (75,500), aw((-75,0),(-75,3000)), (-75,500), 1200)
chk('WWD AKD same wall for both picks: rejected', isinstance(r, str) and 'different walls' in r)

# 2 GENERIC -> GENERIC (ordinary Layer 0, no AKD data)
G2 = [((0,0),(0,3000)), ((150,0),(150,3000))] + ref2
fresh(); ens = txl(*G2); before = lines0()
r = wwd(ens[1], (150,500), ens[2], (3000,2500), 1200)
chk('WWD GENERIC->GENERIC: wall translated, reference unchanged, free ends capped, no AKD data',
    isrec(r) and lines0() == exp(((1650,0),(1650,3000)), ((1800,0),(1800,3000)), ((1650,0),(1800,0)), ((1650,3000),(1800,3000)), *ref2)
    and not A.db_lines('X-AXIS') and not A.db_lines('A-WALL') and A.ev('*wt:reg*') is None)
undo_rec(r)
chk('WWD GENERIC: one transaction record restores the exact original drawing', lines0() == before)
fresh(); ens = txl(*G2)
r = wwd(ens[0], (0,2900), ens[3], (3150,100), 1200)
chk('WWD GENERIC: LEFT/RIGHT picks respected (left face 1200 from reference right face)', isrec(r) and lk((1950,0),(1950,3000)) in lines0() and lk((2100,0),(2100,3000)) in lines0())
# mixed
build([wall((0,0),(0,3000))]); ens = txl(*ref2)
r = wwd(aw((75,0),(75,3000)), (75,500), ens[0], (3000,500), 1200)
chk('WWD AKD moving -> GENERIC reference (right face 1200 from generic face)', isrec(r) and mset() == [mk((1725,0),(1725,3000))] and lines0() == exp(*ref2))
build([wall((3075,0),(3075,3000))]); ens = txl(((0,0),(0,3000)), ((150,0),(150,3000)))
r = wwd(ens[1], (150,500), aw((3000,0),(3000,3000)), (3000,500), 1200)
chk('WWD GENERIC moving -> AKD reference', isrec(r) and mset() == [mk((3075,0),(3075,3000))] and lk((1650,0),(1650,3000)) in lines0()
    and lk((1800,0),(1800,3000)) in lines0())
# rotated 37 deg
cr, sr = math.cos(math.radians(37)), math.sin(math.radians(37))
def R(p): return (p[0]*cr - p[1]*sr, p[0]*sr + p[1]*cr)
fresh(); ens = txl(*[(R(a), R(b)) for a, b in G2])
r = wwd(ens[1], R((150,500)), ens[2], R((3000,2500)), 1200)
g = lines0()
chk('WWD 37 deg generic walls: exact perpendicular face distance, no rotation, no length change',
    isrec(r) and len(g) == 6 and all(has_seg(g, R(a), R(b)) for a, b in [((1650,0),(1650,3000)), ((1800,0),(1800,3000))] + ref2))
# generic ambiguity / caps / same wall
fresh(); ens = txl(((0,0),(3000,0)), ((0,150),(3000,150)), ((0,300),(3000,300)))
chk('WWD GENERIC ambiguous pairing (three equal lines): refused', 'Ambiguous' in wres(ens[1], (1500,150)))
fresh(); ens = txl(((0,0),(3000,0)), ((0,150),(3000,150)), ((0,0),(0,150)), ((3000,0),(3000,150)))
chk('WWD GENERIC end cap pick: rejected as end cap', 'end cap' in wres(ens[3], (3000,75)))
fresh(); ens = txl(*G2)
chk('WWD GENERIC same wall for both picks: rejected', 'different walls' in wwd(ens[1], (150,500), ens[0], (0,500), 1200))
# capped isolated wall moves intact
fresh(); ens = txl(((0,0),(0,3000)), ((150,0),(150,3000)), ((0,0),(150,0)), ((0,3000),(150,3000)), *ref2)
r = wwd(ens[1], (150,500), ens[4], (3000,500), 1200)
chk('WWD capped generic wall: faces and caps move together, nothing left behind',
    lines0() == exp(((1650,0),(1650,3000)), ((1800,0),(1800,3000)), ((1650,0),(1800,0)), ((1650,3000),(1800,3000)), *ref2))

# topology: T
refT = [((4000,-3000),(4000,-1000)), ((4150,-3000),(4150,-1000))]
fresh(); ens = txl(((0,150),(5000,150)), ((0,0),(2000,0)), ((2150,0),(5000,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000)), *refT)
r = wwd(ens[4], (2150,-1500), ens[5], (4000,-2000), 1200)
chk('WWD branch slides along host: old opening closed, new T opened, free branch end capped',
    lines0() == exp(((0,150),(5000,150)), ((0,0),(2650,0)), ((2800,0),(5000,0)), ((2650,0),(2650,-3000)), ((2800,0),(2800,-3000)),
                    ((2650,-3000),(2800,-3000)), *refT))
refH = [((0,3000),(5000,3000)), ((0,3150),(5000,3150))]
fresh(); ens = txl(((0,150),(5000,150)), ((0,0),(2000,0)), ((2150,0),(5000,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000)), *refH)
r = wwd(ens[0], (1000,150), ens[5], (1000,3000), 1200)
chk('WWD host moved away from T: host faces continuous, branch end capped, host ends capped',
    lines0() == exp(((0,1800),(5000,1800)), ((0,1650),(5000,1650)), ((0,1650),(0,1800)), ((5000,1650),(5000,1800)),
                    ((2000,0),(2150,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000)), *refH))
refB = [((0,-5000),(5000,-5000)), ((0,-4850),(5000,-4850))]
fresh(); ens = txl(((0,400),(5000,400)), ((0,550),(5000,550)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000)), ((2000,0),(2150,0)), *refB)
r = wwd(ens[0], (1000,400), ens[6], (1000,-4850), 4850)
chk('WWD host moved into exact-touch T: near face opened, far face continuous, branch cap removed',
    lines0() == exp(((0,150),(5000,150)), ((0,0),(2000,0)), ((2150,0),(5000,0)), ((0,0),(0,150)), ((5000,0),(5000,150)),
                    ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000)), *refB))
# topology: L
refV = [((3000,0),(3000,-2000)), ((3150,0),(3150,-2000))]
fresh(); ens = txl(((0,150),(1150,150)), ((0,0),(1000,0)), ((1150,150),(1150,-2000)), ((1000,0),(1000,-2000)), *refV)
r = wwd(ens[2], (1150,-1000), ens[4], (3000,-1000), 1350)
chk('WWD moved away from L: old corner squared and capped, moving wall end squared and capped',
    lines0() == exp(((0,150),(1150,150)), ((0,0),(1150,0)), ((1150,0),(1150,150)),
                    ((1650,150),(1650,-2000)), ((1500,150),(1500,-2000)), ((1500,150),(1650,150)), ((1500,-2000),(1650,-2000)), *refV))
fresh(); ens = txl(((0,150),(1150,150)), ((0,0),(1150,0)), ((1150,0),(1150,150)),
                   ((1650,150),(1650,-2000)), ((1500,150),(1500,-2000)), ((1500,150),(1650,150)), ((1500,-2000),(1650,-2000)), *refV)
r = wwd(ens[3], (1650,-1000), ens[7], (3000,-1000), 1850)
chk('WWD moved into L: stale caps removed, clean mitred L',
    lines0() == exp(((0,150),(1150,150)), ((0,0),(1000,0)), ((1150,150),(1150,-2000)), ((1000,0),(1000,-2000)),
                    ((1000,-2000),(1150,-2000)), *refV))
# topology: CROSS
refX = [((4000,-3000),(4000,3000)), ((4100,-3000),(4100,3000))]
fresh(); ens = txl(*clean_c, *refX)
r = wwd(ens[4], (1000,-1500), ens[8], (4000,-1500), 2500)
chk('WWD vertical wall moved along a cross: old openings closed, new + cut, free ends capped',
    lines0() == sorted(cross_exp((0,150), (1500,1600)) + exp(((1500,-3000),(1600,-3000)), ((1500,3000),(1600,3000)), *refX)))
refY = [((-3000,-5000),(3000,-5000)), ((-3000,-4850),(3000,-4850))]
fresh(); ens = txl(((-3000,3500),(3000,3500)), ((-3000,3650),(3000,3650)), ((1000,-3000),(1000,3000)), ((1100,-3000),(1100,3000)), *refY)
r = wwd(ens[0], (-2000,3500), ens[5], (-2000,-4850), 4850)
chk('WWD wall moved into a cross: four openings cut, moved wall ends capped',
    lines0() == sorted(cross_exp((0,150), (1000,1100)) + exp(((-3000,0),(-3000,150)), ((3000,0),(3000,150)), *refY)))
# locality: a distant messy junction on a long wall stays untouched
fresh(); ens = txl(((0,150),(25000,150)), ((0,0),(25000,0)), ((20000,-40),(20000,-3000)), ((20150,-40),(20150,-3000)),
                   ((1000,-3000),(1000,-500)), ((1150,-3000),(1150,-500)), ((3000,-3000),(3000,-500)), ((3150,-3000),(3150,-500)))
r = wwd(ens[5], (1150,-1000), ens[6], (3000,-1000), 1200)
g = lines0()
chk('WWD cleanup stays local: distant unrepaired junction untouched',
    isrec(r) and lk((20000,-40),(20000,-3000)) in g and lk((0,0),(25000,0)) in g and lk((1800,-3000),(1800,-500)) in g)
# command: prompts, negative distance re-prompt, remembered distance
fresh(); ens = txl(*G2); A.ev('(setq *wt:wwd-dist* nil)')
A.ENTSELQ[:] = [[ens[1], [150.0, 500.0, 0.0]], [ens[2], [3000.0, 2500.0, 0.0]]]; A.DISTQ[:] = [-100.0, 1000.0]
o = _cap('(c:WWD)')
chk('WWD command: negative re-prompted, wall adjusted, distance remembered',
    'zero or greater' in o and 'Wall adjusted to 1000' in o and lk((1850,0),(1850,3000)) in lines0() and A.ev('*wt:wwd-dist*') == 1000.0)
fresh(); ens = txl(*G2); before = lines0()
A.ENTSELQ[:] = [[ens[1], [150.0, 500.0, 0.0]]]; A.DISTQ[:] = []
_cap('(c:WWD)')
chk('WWD command: cancelled at the reference prompt -> drawing unchanged', lines0() == before)


# --- WWD on the centerline base ---
def face_at(pt):
    return next(e for e, c, d in A.db_lines('A-WALL') if A.ev(f'(wt:on-seg {P(*pt)} {P(*c)} {P(*d)})'))
ok = True
for pos, top in (('LEFT', 200.0), ('RIGHT', 0.0)):
    fresh(200, pos); add(((0,0),(6000,0))); settings(150, 'CENTER'); add(((0,3000),(6000,3000)))
    em = face_at((3000, top)); er = face_at((3000, 2925))
    r = wwd(em, (3000.0, top + 0.2), er, (3000.0, 2924.8), 1000.0)
    want = sorted([mk((0,1825),(6000,1825)), mk((0,3000),(6000,3000))])
    ok = ok and master_set() == want and master_offsets_ok() \
         and same_lines(walls_now(), expected([wall((0,1825),(6000,1825),200), wall((0,3000),(6000,3000),150)]))
chk('WWD on walls drawn LEFT / RIGHT: whole wall moves, 1000 clear, master stays exactly centred (faces at +/- t/2)', ok)
fresh(150, 'CENTER'); add(((0,0),(6000,0))); add(((3000,0),(3000,3000))); add(((0,5000),(6000,5000)))
em = face_at((1500, 75)); er = face_at((1500, 4925))
wwd(em, (1500.0, 75.2), er, (1500.0, 4924.8), 500.0)
chk('WWD centred-master invariant after moving a wall out of a T: every master midway between its faces',
    master_offsets_ok() and normalized_ok() and lines_match_masters())
legacy_host(); settings(150, 'CENTER'); add(((0,3000),(6000,3000))); A.ev('(setq *wt:reg* nil)')
before = geo()
el = face_at((3000, 200)); er = face_at((3000, 2925))
rr = wres(el, (3000.0, 200.2))
r = wwd(el, (3000.0, 200.2), er, (3000.0, 2924.8), 1000.0)
chk('WWD on a legacy off-centre wall: refused with "Run WWR first", drawing unchanged',
    isinstance(rr, str) and 'Legacy wall axis detected' in rr and geo() == before)
wr(field(-500,-500,6500,500))
el = face_at((3000, 200)); er = face_at((3000, 2925))
wwd(el, (3000.0, 200.2), er, (3000.0, 2924.8), 1000.0)
chk('WWD after WWR on the former legacy wall: moved, 1000 clear, master centred',
    mk((0,1825),(6000,1825)) in master_set() and master_offsets_ok()
    and same_lines(walls_now(), expected([wall((0,1825),(6000,1825),200), wall((0,3000),(6000,3000),150)])))

# =========================== WWE (wall extend) ===========================
def wwe(e1, q1, e2, q2):
    A.G[A.Sym('*W1*')] = e1; A.G[A.Sym('*W2*')] = e2
    return A.ev(f'(wt:wwe-run *w1* {P(*q1)} *w2* {P(*q2)})')
def okx(r): return isinstance(r, list)
hostT = [((0,0),(5000,0)), ((0,150),(5000,150))]
br = [((2000,-3000),(2000,-500)), ((2150,-3000),(2150,-500))]
# generic T (near face), no AKD data
fresh(); ens = txl(*hostT, *br)
r = wwe(ens[3], (2150,-800), ens[0], (1000,0))
chk('WWE GENERIC T: branch extended to picked near face, clean T, no AKD data',
    okx(r) and 'extended' in r[1] and abs(r[2] - 500) < 1e-6 and lines0() == exp18[:0] + sorted(exp(*hostT[1:], ((0,0),(2000,0)), ((2150,0),(5000,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000))))
    and not A.db_lines('X-AXIS') and not A.db_lines('A-WALL'))
fresh(); ens = txl(*hostT, *br); before = lines0()
r = wwe(ens[3], (2150,-800), ens[1], (1000,150))
chk('WWE GENERIC T: far face picked -> extension distance uses that face, final T still correct',
    okx(r) and abs(r[2] - 650) < 1e-6 and lines0() == exp(*hostT[1:], ((0,0),(2000,0)), ((2150,0),(5000,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000))))
undo_rec(r[0])
chk('WWE GENERIC: one transaction record restores the original drawing', lines0() == before)
# opposite end (nearest-end inference)
hostB = [((0,-4000),(5000,-4000)), ((0,-4150),(5000,-4150))]
fresh(); ens = txl(*hostT, *br, *hostB)
r = wwe(ens[3], (2150,-2900), ens[4], (1000,-4000))
chk('WWE pick near the other end extends that end (top end untouched)',
    okx(r) and lines0() == exp(*hostT, ((2000,-4000),(2000,-500)), ((2150,-4000),(2150,-500)),
                               ((0,-4000),(2000,-4000)), ((2150,-4000),(5000,-4000)), hostB[1]))
# cap pick: selected end is the capped one; old cap erased, other cap unchanged
fresh(); ens = txl(*hostT, *br, ((2000,-500),(2150,-500)), ((2000,-3000),(2150,-3000)))
r = wwe(ens[4], (2075,-500), ens[0], (1000,0))
chk('WWE cap pick: that end extended, its cap erased, opposite cap unchanged',
    okx(r) and lines0() == exp(*hostT[1:], ((0,0),(2000,0)), ((2150,0),(5000,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000)),
                               ((2000,-3000),(2150,-3000))) and ens[5] not in A.DELETED)
fresh(); ens = txl(*hostT, *br, ((2000,-500),(2150,-500)))
r = wwe(ens[2], (2000,-700), ens[0], (1000,0))
chk('WWE side pick at a capped end: old cap does not remain inside the extended wall',
    okx(r) and not any(r_[1] == -500.0 and r_[3] == -500.0 for r_ in lines0()) and lk((2000,0),(2000,-3000)) in lines0())
# angled 63 deg branch
u63 = (math.cos(math.radians(63)), math.sin(math.radians(63))); xb = 2000 + 150 / u63[1]
def along63(x0, t): return (x0 + u63[0]*t, u63[1]*t)
brA = [(along63(2000, -3000/u63[1]), along63(2000, -500/u63[1])), (along63(xb, -3000/u63[1]), along63(xb, -500/u63[1]))]
fresh(); ens = txl(*hostT, *brA)
r = wwe(ens[2], along63(2000, -800/u63[1]), ens[0], (1000,0))
g = lines0()
chk('WWE angled 63 deg: extended, clean angled T',
    okx(r) and len(g) == 5 and all(has_seg(g, a, b) for a, b in [((0,0),(2000,0)), ((xb,0),(5000,0)), hostT[1],
                                                                  (brA[0][0], (2000,0)), (brA[1][0], (xb,0))]))
# L
tgtV = [((3000,-2000),(3000,150)), ((3150,-2000),(3150,150))]
fresh(); ens = txl(((0,0),(2500,0)), ((0,150),(2500,150)), *tgtV)
r = wwe(ens[1], (2400,150), ens[2], (3000,-1000))
chk('WWE GENERIC L: extended into the corner, existing L miter',
    okx(r) and lines0() == exp(((0,150),(3150,150)), ((0,0),(3000,0)), ((3150,150),(3150,-2000)), ((3000,0),(3000,-2000))))
# rejections (no change)
fresh(); ens = txl(*hostT, *br, *hostB); before = lines0()
r = wwe(ens[3], (2150,-800), ens[4], (1000,-4000))
chk('WWE target behind the selected end: rejected, no change', isinstance(r, str) and 'behind' in r and lines0() == before)
fresh(); ens = txl(*hostT, *br, ((4000,-3000),(4000,0)), ((4150,-3000),(4150,0))); before = lines0()
r = wwe(ens[3], (2150,-800), ens[4], (4000,-1000))
chk('WWE parallel target: rejected, no change', isinstance(r, str) and 'parallel' in r and lines0() == before)
fresh(); ens = txl(((5000,0),(6000,0)), ((5000,150),(6000,150)), *br); before = lines0()
r = wwe(ens[3], (2150,-800), ens[0], (5500,0))
chk('WWE target extent not reached: rejected, no change', isinstance(r, str) and 'does not reach' in r and lines0() == before)
fresh(); ens = txl(*hostT, ((2000,-3000),(2000,100)), ((2150,-3000),(2150,100))); before = lines0()
r = wwe(ens[3], (2150,-800), ens[0], (1000,0))
chk('WWE wall already beyond target: rejected, never trimmed', isinstance(r, str) and 'beyond target' in r and lines0() == before)
fresh(); ens = txl(*hostT, ((2000,-3000),(2000,-500)), ((2150,-3000),(2150,-500)), ((1500,-300),(2600,-300)), ((1500,-250),(2600,-250))); before = lines0()
r = wwe(ens[3], (2150,-800), ens[0], (1000,0))
chk('WWE recognised wall in the path: passed through as a clean CROSS (V1 rejected this)',
    okx(r) and lines0() == exp(hostT[1], ((0,0),(2000,0)), ((2150,0),(5000,0)),
                               ((1500,-300),(2000,-300)), ((2150,-300),(2600,-300)), ((1500,-250),(2000,-250)), ((2150,-250),(2600,-250)),
                               ((2000,-3000),(2000,-300)), ((2000,-250),(2000,0)), ((2150,-3000),(2150,-300)), ((2150,-250),(2150,0))))
fresh(); ens = txl(*hostT, ((2000,-3000),(2000,-500)), ((2150,-3000),(2150,-500)), ((1500,-300),(2600,-300))); before = lines0()
r = wwe(ens[3], (2150,-800), ens[0], (1000,0))
chk('WWE unrecognised line in the path: rejected as obstruction, no change', isinstance(r, str) and 'between' in r and lines0() == before)
hostU = [((0,1000),(5000,1000)), ((0,1150),(5000,1150))]
fresh(); ens = txl(*hostT, ((2000,-3000),(2000,0)), ((2150,-3000),(2150,0)), *hostU); before = lines0()
r = wwe(ens[3], (2150,-100), ens[4], (1000,1000))
chk('WWE end in a messy T: extended through the host -> CROSS, new T at target (V1 rejected this)',
    okx(r) and lines0() == exp(((0,150),(2000,150)), ((2150,150),(5000,150)), ((0,0),(2000,0)), ((2150,0),(5000,0)),
                               ((2000,-3000),(2000,0)), ((2000,150),(2000,1000)), ((2150,-3000),(2150,0)), ((2150,150),(2150,1000)),
                               ((0,1000),(2000,1000)), ((2150,1000),(5000,1000)), ((0,1150),(5000,1150))))
fresh(); ens = txl(*hostT, *br)
chk('WWE target on the same wall: rejected', 'different wall' in wwe(ens[3], (2150,-800), ens[2], (2000,-800)))
fresh(); ens = txl(*hostT, *br, ((2000,-500),(2150,-500)))
chk('WWE target pick on an end cap: rejected', 'end cap' in wwe(ens[3], (2150,-800), ens[4], (2075,-500)))
# already reaches
fresh(); ens = txl(*hostT[1:], ((0,0),(2000,0)), ((2150,0),(5000,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000))); before = lines0()
r = wwe(ens[4], (2150,-100), ens[1], (1000,0))
chk('WWE clean T already reached: no change', okx(r) and 'already reaches target.' in r[1] and lines0() == before)
fresh(); ens = txl(*hostT, ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000)))
r = wwe(ens[3], (2150,-100), ens[0], (1000,0))
chk('WWE messy exact-touch T: no extension, junction repaired',
    okx(r) and 'junction repaired' in r[1] and abs(r[2]) < 1e-6
    and lines0() == exp(*hostT[1:], ((0,0),(2000,0)), ((2150,0),(5000,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000))))
# locality
fresh(); ens = txl(((0,0),(25000,0)), ((0,150),(25000,150)), *br, ((20000,-40),(20000,-3000)), ((20150,-40),(20150,-3000)))
r = wwe(ens[3], (2150,-800), ens[0], (1000,0))
chk('WWE cleanup local: distant messy junction untouched', okx(r) and lk((20000,-40),(20000,-3000)) in lines0() and lk((2150,0),(25000,0)) in lines0())   # near face stays continuous past the distant branch
# AKD
build([wall((0,0),(10000,0)), wall((5000,-3000),(5000,-1000))]); host_m = find_line('X-AXIS', (0,0), (10000,0)); before = allg()
r = wwe(aw((5075,-3000),(5075,-1000)), (5075,-1100), aw((0,-75),(10000,-75)), (3000,-75))
chk('WWE AKD T: master end moved to the host master, other end kept, host split at node, clean rebuild',
    okx(r) and mset() == sorted([mk((0,0),(5000,0)), mk((5000,0),(10000,0)), mk((5000,-3000),(5000,0))]) and lines_match_masters())
undo_rec(r[0])
chk('WWE AKD: one transaction record restores masters, linework and host', allg() == before and host_m not in A.DELETED)
build([wall((0,0),(10000,0),200), wall((5000,-3000),(5000,-1000),100)])
r = wwe(aw((5050,-3000),(5050,-1000)), (5050,-1100), aw((0,-100),(10000,-100)), (3000,-100))
chk('WWE AKD widths 100 into 200: widths preserved, branch faces end at host near face',
    okx(r) and mk((5000,-3000),(5000,0)) in mset() and lk((5050,-3000),(5050,-100)) in lines0('A-WALL') and lk((4950,-3000),(4950,-100)) in lines0('A-WALL'))
build([wall((0,0),(10000,0)), wall((5000,-3000),(5000,-1000))])
r = wwe(aw((4925,-3000),(5075,-3000)), (5000,-3000), aw((0,-75),(10000,-75)), (3000,-75))
chk('WWE AKD cap pick: bottom cap selects the bottom end (target above is behind it)', isinstance(r, str) and 'behind' in r)
build([wall((0,0),(10000,0)), wall((5000,-3000),(5000,0))]); before = allg()
r = wwe(aw((5075,-3000),(5075,-75)), (5075,-200), aw((0,-75),(4925,-75)), (3000,-75))
chk('WWE AKD end already on target master: no change', isinstance(r, str) and 'already reaches' in r and allg() == before)
build([wall((0,0),(4000,0))]); ens = txl(((5000,-2000),(5000,500)), ((5150,-2000),(5150,500))); before = allg()
r = wwe(aw((0,75),(4000,75)), (3800,75), ens[0], (5000,-1000))
chk('WWE AKD moving -> GENERIC target: resolved but mixed junction refused, no change', isinstance(r, str) and 'Mixed' in r and allg() == before)
build([wall((5075,-2000),(5075,500))]); ens = txl(((0,0),(4000,0)), ((0,150),(4000,150))); before = allg()
r = wwe(ens[1], (3800,150), aw((5000,-2000),(5000,500)), (5000,-1000))
chk('WWE GENERIC moving -> AKD target: resolved but mixed junction refused, no change', isinstance(r, str) and 'Mixed' in r and allg() == before)
# command
fresh(); ens = txl(*hostT, *br)
A.ENTSELQ[:] = [[ens[3], [2150.0, -800.0, 0.0]], [ens[0], [1000.0, 0.0, 0.0]]]
o = _cap('(c:WWE)')
chk('WWE command: two picks -> Wall extended', 'Wall extended' in o and lk((2000,0),(2000,-3000)) in lines0())
fresh(); ens = txl(*hostT, *br); before = lines0()
A.ENTSELQ[:] = [[ens[3], [2150.0, -800.0, 0.0]]]
_cap('(c:WWE)')
chk('WWE command: cancelled at target prompt -> drawing unchanged', lines0() == before)

# --- WWE from a connected (L / T) end ---
def hpieces(y, x0, x1, cuts):
    xs = [x0] + [c for ab in cuts for c in ab] + [x1]
    return [((xs[i], y), (xs[i+1], y)) for i in range(0, len(xs), 2)]
def vpieces(x, y0, y1, cuts):
    ys = [y0] + [c for ab in cuts for c in ab] + [y1]
    return [((x, ys[i]), (x, ys[i+1])) for i in range(0, len(ys), 2)]
tgtC = [((4000,-2000),(4000,2000)), ((4150,-2000),(4150,2000))]
Lsegs = [((0,150),(1150,150)), ((0,0),(1000,0)), ((1150,150),(1150,-2000)), ((1000,0),(1000,-2000))]
expLx = exp(((0,150),(4000,150)), ((0,0),(1000,0)), ((1150,0),(4000,0)), ((1150,0),(1150,-2000)), ((1000,0),(1000,-2000)),
            ((4000,-2000),(4000,0)), ((4000,150),(4000,2000)), tgtC[1])
fresh(); ens = txl(*Lsegs, *tgtC); before = lines0()
r = wwe(ens[0], (1100,150), ens[4], (4000,-500))
chk('WWE L end: moving wall continuous through the old corner, old partner now a clean branch, no miter left, new T at target',
    okx(r) and abs(r[2] - 2850) < 1e-6 and lines0() == expLx)
r2 = wwe(ens[0], (3900,150), ens[4], (4000,-500))
chk('WWE L end: repeat run -> already reaches, no duplicates', okx(r2) and 'already reaches' in r2[1] and lines0() == expLx)
undo_rec(r2[0]); undo_rec(r[0])
chk('WWE L end: Undo restores the original L exactly', lines0() == before)
# T -> CROSS -> new T (clean opened T at the start)
cleanTup = exp18_segs + [((0,1000),(5000,1000)), ((0,1150),(5000,1150))]
expTX = exp(((0,150),(2000,150)), ((2150,150),(5000,150)), ((0,0),(2000,0)), ((2150,0),(5000,0)),
            ((2000,-3000),(2000,0)), ((2000,150),(2000,1000)), ((2150,-3000),(2150,0)), ((2150,150),(2150,1000)),
            ((0,1000),(2000,1000)), ((2150,1000),(5000,1000)), ((0,1150),(5000,1150)))
fresh(); ens = txl(*cleanTup); before = lines0()
r = wwe(ens[4], (2150,-100), ens[5], (1000,1000))
chk('WWE T end: branch through old host -> clean CROSS (TX solver), new T at target, far faces cut correctly',
    okx(r) and lines0() == expTX)
undo_rec(r[0])
chk('WWE T end: Undo restores the original T exactly', lines0() == before)
# two intermediate walls
multi = exp18_segs + [((0,1500),(5000,1500)), ((0,1650),(5000,1650)), ((0,3000),(5000,3000)), ((0,3150),(5000,3150))]
fresh(); ens = txl(*multi)
r = wwe(ens[4], (2150,-100), ens[7], (1000,3000))
cut = [(2000,2150)]
expM = exp(*hpieces(150,0,5000,cut), *hpieces(0,0,5000,cut), *hpieces(1500,0,5000,cut), *hpieces(1650,0,5000,cut),
           *hpieces(3000,0,5000,cut), ((0,3150),(5000,3150)),
           *vpieces(2000,-3000,3000,[(0,150),(1500,1650)]), *vpieces(2150,-3000,3000,[(0,150),(1500,1650)]))
chk('WWE T end through two walls: old host CROSS, intermediate CROSS, new T', okx(r) and lines0() == expM)
# widths 100 / 200 / 150
w_start = [((0,200),(5000,200)), ((0,0),(2000,0)), ((2100,0),(5000,0)), ((2000,0),(2000,-3000)), ((2100,0),(2100,-3000)),
           ((0,1000),(5000,1000)), ((0,1150),(5000,1150))]
fresh(); ens = txl(*w_start)
r = wwe(ens[4], (2100,-100), ens[5], (1000,1000))
chk('WWE T end with widths 100 / 200 / 150: widths preserved, CROSS + T',
    okx(r) and lines0() == exp(*hpieces(200,0,5000,[(2000,2100)]), *hpieces(0,0,5000,[(2000,2100)]),
                               *hpieces(1000,0,5000,[(2000,2100)]), ((0,1150),(5000,1150)),
                               *vpieces(2000,-3000,1000,[(0,200)]), *vpieces(2100,-3000,1000,[(0,200)])))
# angled 63 deg T -> CROSS
u6 = (math.cos(math.radians(63)), math.sin(math.radians(63))); xb6 = 2000 + 150 / u6[1]
def xa(x0, y): return x0 + y * u6[0] / u6[1]
angT = [((0,150),(5000,150)), ((0,0),(2000,0)), ((xb6,0),(5000,0)),
        ((xa(2000,-3000),-3000),(2000,0)), ((xa(xb6,-3000),-3000),(xb6,0)), ((0,1000),(5000,1000)), ((0,1150),(5000,1150))]
fresh(); ens = txl(*angT)
r = wwe(ens[3], (xa(2000,-100),-100), ens[5], (1000,1000))
g = lines0()
want = [((0,150),(xa(2000,150),150)), ((xa(xb6,150),150),(5000,150)), ((0,0),(2000,0)), ((xb6,0),(5000,0)),
        ((xa(2000,-3000),-3000),(2000,0)), ((xa(2000,150),150),(xa(2000,1000),1000)),
        ((xa(xb6,-3000),-3000),(xb6,0)), ((xa(xb6,150),150),(xa(xb6,1000),1000)),
        ((0,1000),(xa(2000,1000),1000)), ((xa(xb6,1000),1000),(5000,1000)), ((0,1150),(5000,1150))]
chk('WWE angled 63 deg T end: clean angled CROSS and T', okx(r) and len(g) == 11 and all(has_seg(g, a, b) for a, b in want))
# refusals from a connected end (no change)
fresh(); ens = txl(*exp18_segs, ((0,-4000),(5000,-4000)), ((0,-4150),(5000,-4150))); before = lines0()
r = wwe(ens[4], (2150,-100), ens[5], (1000,-4000))
chk('WWE connected end, target behind: refused (use EW), no change', isinstance(r, str) and 'behind' in r and lines0() == before)
diag = [((1000,-1000),(3000,1000)), ((1000+150*2**0.5,-1000),(3000+150*2**0.5,1000))]
fresh(); ens = txl(*exp18_segs, ((0,1000),(5000,1000)), ((0,1150),(5000,1150)), *diag); before = lines0()
r = wwe(ens[4], (2150,-100), ens[5], (1000,1000))
chk('WWE end at a node with two walls: refused before any change', isinstance(r, str) and ('complex' in r or 'between' in r) and lines0() == before)
fresh(); ens = txl(*exp18_segs, ((0,1000),(5000,1000)), ((0,1150),(5000,1150)), ((1000,500),(3000,500))); before = lines0()
r = wwe(ens[4], (2150,-100), ens[5], (1000,1000))
chk('WWE connected end, unrelated line across the corridor: refused, no change', isinstance(r, str) and 'between' in r and lines0() == before)
fresh(); ens = txl(*exp18_segs, ((0,250),(5000,250)), ((0,400),(5000,400))); before = lines0()
r = wwe(ens[4], (2150,-100), ens[5], (1000,250))
chk('WWE connected end, target wall right behind the old host: refused, no change', isinstance(r, str) and lines0() == before)
build([wall((0,0),(10000,0)), wall((5000,-3000),(5000,0)), wall((0,200),(10000,200))]); before = allg()
r = wwe(aw((5075,-3000),(5075,-75)), (5075,-200), aw((0,125),(10000,125)), (3000,125))
chk('WWE AKD T end, target closer than D past the old host: refused as too close, no change', isinstance(r, str) and 'too close' in r and allg() == before)
fresh(); ens = txl(((0,0),(3000,0)), ((0,150),(3000,150)), ((3000,0),(5000,0)), ((3000,150),(5000,150)),
                   ((6000,-2000),(6000,2000)), ((6150,-2000),(6150,2000)))
# collinear continuation: first run pairs 0..3000 with 3000..5000 as one wall (touching faces merge) -> plain extension
r = wwe(ens[3], (4900,150), ens[4], (6000,0))
chk('WWE collinear touching runs behave as one wall (extended as a whole, joint outside cleanup untouched)', okx(r) and lk((3000,0),(6000,0)) in lines0() and lk((0,0),(3000,0)) in lines0() and lk((6000,-2000),(6000,0)) in lines0())
# locality with a connected end
fresh(); ens = txl(((0,150),(25000,150)), ((0,0),(2000,0)), ((2150,0),(25000,0)), ((2000,0),(2000,-3000)), ((2150,0),(2150,-3000)),
                   ((20000,-40),(20000,-3000)), ((20150,-40),(20150,-3000)),
                   ((0,1000),(25000,1000)), ((0,1150),(25000,1150)), ((21000,1110),(21000,3000)), ((21150,1110),(21150,3000)))
r = wwe(ens[4], (2150,-100), ens[7], (1000,1000))
g = lines0()
chk('WWE connected end: only old node, corridor and target change; distant junctions untouched',
    okx(r) and lk((20000,-40),(20000,-3000)) in g and lk((21000,1110),(21000,3000)) in g and lk((2150,1150),(2150,1150)) not in g
    and lk((0,1150),(25000,1150)) in g and lk((2150,1000),(25000,1000)) in g)
# AKD connected ends
build([wall((0,0),(1000,0)), wall((1000,0),(1000,-2000)), wall((4000,-2000),(4000,2000))]); before = allg()
a_m = find_line('X-AXIS', (0,0), (1000,0))
r = wwe(aw((0,75),(1075,75)), (900,75), aw((3925,-2000),(3925,2000)), (3925,-500))
chk('WWE AKD L end: master end extended (other end fixed), split at old corner, T at target, clean rebuild',
    okx(r) and mset() == sorted([mk((0,0),(1000,0)), mk((1000,0),(4000,0)), mk((1000,0),(1000,-2000)),
                                 mk((4000,-2000),(4000,0)), mk((4000,0),(4000,2000))]) and lines_match_masters())
undo_rec(r[0])
chk('WWE AKD L end: Undo restores masters (same entity) and linework', allg() == before and a_m not in A.DELETED)
build([wall((0,0),(10000,0)), wall((5000,-3000),(5000,0)), wall((0,2000),(10000,2000))]); before = allg()
r = wwe(aw((5075,-3000),(5075,-75)), (5075,-200), aw((0,1925),(10000,1925)), (3000,1925))
chk('WWE AKD T end: through the host (CROSS) to a new T, widths kept',
    okx(r) and mset() == sorted([mk((0,0),(5000,0)), mk((5000,0),(10000,0)), mk((5000,-3000),(5000,0)), mk((5000,0),(5000,2000)),
                                 mk((0,2000),(5000,2000)), mk((5000,2000),(10000,2000))]) and lines_match_masters())
undo_rec(r[0])
chk('WWE AKD T end: Undo restores the original network', allg() == before)
build([wall((0,0),(10000,0)), wall((5000,-3000),(5000,0)), wall((0,1000),(10000,1000)), wall((0,2000),(10000,2000))])
r = wwe(aw((5075,-3000),(5075,-75)), (5075,-200), aw((0,1925),(10000,1925)), (3000,1925))
chk('WWE AKD T end through an intermediate wall: two CROSSes and a T', okx(r) and mk((5000,1000),(5000,2000)) in mset() and lines_match_masters())
build([wall((0,0),(10000,0)), wall((5000,-3000),(5000,0)), wall((0,2000),(10000,2000))]); mkline((4000,1000), (6000,1000), 'X-AXIS'); before = allg()
r = wwe(aw((5075,-3000),(5075,-75)), (5075,-200), aw((0,1925),(10000,1925)), (3000,1925))
chk('WWE AKD: plain axis across the path: refused, no change', isinstance(r, str) and ('between' in r or 'too close' in r) and allg() == before)
build([wall((0,0),(10000,0)), wall((5000,-3000),(5000,0)), wall((0,2000),(10000,2000))]); txl(((4000,1000),(6000,1000))); before = allg()
r = wwe(aw((5075,-3000),(5075,-75)), (5075,-200), aw((0,1925),(10000,1925)), (3000,1925))
chk('WWE AKD: ordinary line across the path: refused as mixed, no change', isinstance(r, str) and 'Mixed' in r and allg() == before)

# --- WWE on the centerline base ---
ok = True
for pos, cx, qx in (('LEFT', -75, 0.2), ('RIGHT', 75, -0.2)):
    fresh(150, pos); add(((0,0),(0,2000))); settings(200, 'CENTER'); add(((-3000,4000),(3000,4000)))
    em = face_at((0, 1800)); et = face_at((1000, 3900))
    r = wwe(em, (qx, 1800.0), et, (1000.0, 3899.8))
    ok = ok and okx(r) and mk((cx,0),(cx,4000)) in master_set() and master_offsets_ok() and normalized_ok() \
         and same_lines(walls_now(), expected([wall((0,0),(0,4000),150,pos), wall((-3000,4000),(3000,4000),200)]))
chk('WWE on walls drawn LEFT / RIGHT: centreline end extended to the target centreline, master centred, old-model faces', ok)
legacy_host(); settings(150, 'CENTER'); add(((8000,-3000),(8000,3000))); A.ev('(setq *wt:reg* nil)')
before = geo()
r1 = wwe(face_at((5500, 200)), (5500.0, 200.2), face_at((7925, 1000)), (7924.8, 1000.0))
same1 = geo() == before
fresh(); settings(150, 'CENTER'); add(((3000,-3000),(3000,-1000)))
mkline((0,0),(6000,0),'X-AXIS')
for a_, b_ in expected([wall((0,0),(6000,0),200,'LEFT')]): mkline(a_, b_, 'A-WALL')
A.ev('(setq *wt:reg* nil)'); before2 = geo()
r2 = wwe(face_at((3075, -1200)), (3075.2, -1200.0), face_at((1000, 0)), (1000.0, -0.2))
chk('WWE with a legacy off-centre wall as the moving wall or the target: refused ("Run WWR first"), drawing unchanged',
    isinstance(r1, str) and 'Legacy wall axis detected' in r1 and same1
    and isinstance(r2, str) and 'Legacy wall axis detected' in r2 and geo() == before2)
legacy_host(); settings(150, 'CENTER'); add(((8000,-3000),(8000,3000))); A.ev('(setq *wt:reg* nil)')
wr(field(-500,-500,6500,500))
r = wwe(face_at((5500, 200)), (5500.0, 200.2), face_at((7925, 1000)), (7924.8, 1000.0))
chk('WWE after WWR on the former legacy wall: extended to the target centreline, master centred, T split',
    okx(r) and mk((0,100),(8000,100)) in master_set() and master_offsets_ok() and normalized_ok()
    and same_lines(walls_now(), expected([wall((0,100),(8000,100),200), wall((8000,-3000),(8000,3000),150)])))

# =========================== WWE intelligent connection (AKD) ===========================
def ref_masters(final):
    build(final); return master_set()
def wconn(sw, sfrac, tw_, tfrac, sside=1, tside=1):
    e1, q1 = face_pick(sw, sside, sfrac); e2, q2 = face_pick(tw_, tside, tfrac)
    return wwe(e1, q1, e2, q2)
def conn_ok(r, final, refm):
    return okx(r) and 'connected' in r[1] and master_set() == refm and master_offsets_ok() and normalized_ok() \
           and no_zero_masters() and same_lines(walls_now(), expected(final))
def scen(setup, pick, final, name):
    refm = ref_masters(final)
    fresh(); setup(); A.ev('(setq *wt:reg* nil)') if False else None
    r = pick()
    chk(name, conn_ok(r, final, refm))
    return r
C = lambda a, b, th=150.0: wall(a, b, th)
def mk_walls(*ws):
    for a, b, th, pos in ws: settings(th, pos); add((a, b))
# 1 free -> midspan (T)
S1, T1 = C((0,0),(0,2000)), C((-3000,4000),(3000,4000))
scen(lambda: mk_walls(S1, T1), lambda: wconn(S1, 0.95, T1, 0.2), [C((0,0),(0,4000)), T1], 'WWE connect 1: free end -> target midspan -> T, target unchanged')
# 2 free -> target end (L)
S2, T2 = C((0,0),(4000,0)), C((5000,0),(5000,3000))
scen(lambda: mk_walls(S2, T2), lambda: wconn(S2, 0.95, T2, 0.5), [C((0,0),(5000,0)), T2], 'WWE connect 2: free end -> target end -> L')
# 3 both short (smart fillet: extend + extend)
T3 = C((5000,200),(5000,3000))
r3 = scen(lambda: mk_walls(S2, T3), lambda: wconn(S2, 0.95, T3, 0.5), [C((0,0),(5000,0)), C((5000,0),(5000,3000))], 'WWE connect 3: both walls short -> both extended to the centreline corner, L')
# 4 source short + target overshoot (extend + trim)
T4 = C((5000,-200),(5000,3000))
scen(lambda: mk_walls(S2, T4), lambda: wconn(S2, 0.95, T4, 0.5), [C((0,0),(5000,0)), C((5000,0),(5000,3000))], 'WWE connect 4: source short + target overshoot -> extend + trim, clean L, no stub')
# 5 source overshoot + target short (trim + extend)
S5 = C((0,0),(5200,0))
scen(lambda: mk_walls(S5, T3), lambda: wconn(S5, 0.98, T3, 0.5), [C((0,0),(5000,0)), C((5000,0),(5000,3000))], 'WWE connect 5: source overshoot + target short -> trim + extend, clean L')
# 6 both overshoot (crossing already normalized): pick the source overshoot piece
scen(lambda: mk_walls(S5, T4), lambda: wconn(C((5000,0),(5200,0)), 0.6, C((5000,0),(5000,3000)), 0.6),
     [C((0,0),(5000,0)), C((5000,0),(5000,3000))], 'WWE connect 6: both overshoot -> both overshoot pieces removed, clean L, no hidden spans')
# 7a existing L -> target ahead (through the old corner)
P7, T7 = C((4000,0),(4000,3000)), C((8000,-3000),(8000,3000))
scen(lambda: mk_walls(S2, P7, T7), lambda: wconn(S2, 0.9, T7, 0.2, sside=-1), [C((0,0),(8000,0)), P7, T7],
     'WWE connect 7a: L end -> target ahead: old partner becomes a T branch, new T at target')
# 7b existing L -> target crossing the wall (detach): old partner left as a free wall
T7b = C((2000,-3000),(2000,3000))
scen(lambda: mk_walls(S2, P7, T7b), lambda: wconn(C((2000,0),(4000,0)), 0.8, T7b, 0.2, sside=-1), [C((0,0),(2000,0)), P7, T7b],
     'WWE connect 7b: L end -> target behind: end detached, old partner capped, new T')
# 8a existing T -> target beyond the host (CROSS)
H8, B8, T8 = C((0,0),(6000,0)), C((3000,-3000),(3000,0)), C((0,2000),(6000,2000))
scen(lambda: mk_walls(H8, B8, T8), lambda: wconn(B8, 0.9, T8, 0.2), [H8, C((3000,-3000),(3000,2000)), T8],
     'WWE connect 8a: T end -> target beyond the host: through the host (CROSS), new T')
# 8b existing T -> target behind (detach): host continuous again
T8b = C((0,-1500),(6000,-1500))
scen(lambda: mk_walls(H8, B8, T8b), lambda: wconn(C((3000,-1500),(3000,0)), 0.5, T8b, 0.2), [H8, C((3000,-3000),(3000,-1500)), T8b],
     'WWE connect 8b: T end -> target behind: branch detached, old host continuous, new T')
# 9 THE MAC CASE: end at a collinear continuation (+ branch) -> target ahead
L9, R9, BR9, T9 = C((0,0),(7000,0)), C((7000,0),(10000,0)), C((7000,0),(7000,3000)), C((13000,-3000),(13000,3000))
r9 = scen(lambda: mk_walls(L9, R9, BR9, T9), lambda: wconn(L9, 0.97, T9, 0.2, sside=-1), [C((0,0),(13000,0)), BR9, T9],
          'WWE connect 9 (Mac case): span end at a collinear node with a branch -> connected, branch kept as T, no duplicate spans')
chk('WWE connect 9 (Mac case): no "collinear continuation ... not supported" refusal', okx(r9) and 'not supported' not in str(r9))
scen(lambda: mk_walls(L9, R9, T9), lambda: wconn(L9, 0.97, T9, 0.2, sside=-1), [C((0,0),(13000,0)), T9],
     'WWE connect 9b: span end at a plain collinear node (WW straight run) -> whole run reaches the target')
# 10 collinear + branch -> target behind (detach): right span + branch become an L
T10 = C((3000,-3000),(3000,3000))
scen(lambda: mk_walls(L9, R9, BR9, T10), lambda: wconn(C((3000,0),(7000,0)), 0.9, T10, 0.2, sside=-1),
     [C((0,0),(3000,0)), R9, BR9, T10], 'WWE connect 10: collinear node + branch, target behind -> span detached, right span + branch left as clean L')
# 11 two separate collinear walls -> straight
T11 = C((5000,0),(9000,0))
refm11 = [mk((0,0),(9000,0))]
fresh(); mk_walls(S2, T11); r = wconn(S2, 0.9, T11, 0.5)
chk('WWE connect 11: collinear gap -> straight continuation, faces continuous, healed into one master',
    okx(r) and master_set() == refm11 and master_offsets_ok() and same_lines(walls_now(), expected([C((0,0),(9000,0))])))
# 12 parallel non-collinear -> refused
fresh(); mk_walls(S2, C((0,2000),(4000,2000))); before = geo()
r = wconn(S2, 0.9, C((0,2000),(4000,2000)), 0.5)
chk('WWE connect 12: parallel walls refused, no change', isinstance(r, str) and 'parallel' in r and geo() == before)
# 13 mixed thickness
S13, T13 = C((0,0),(4000,0),200.0), C((5000,200),(5000,3000),100.0)
scen(lambda: mk_walls(S13, T13), lambda: wconn(S13, 0.95, T13, 0.5), [C((0,0),(5000,0),200.0), C((5000,0),(5000,3000),100.0)],
     'WWE connect 13: 200 + 100 smart fillet, widths kept')
# 14/15 source created LEFT / RIGHT
for pos, cy in (('LEFT', 100.0), ('RIGHT', -100.0)):
    def setup(pos=pos, cy=cy):
        settings(200, pos); add(((0,0),(4000,0))); settings(150, 'CENTER'); add(((5000,cy+200),(5000,3000)))
    scen(setup, lambda cy=cy: wconn(C((0,cy),(4000,cy),200.0), 0.95, C((5000,cy+200),(5000,3000)), 0.5),
         [C((0,cy),(5000,cy),200.0), C((5000,cy),(5000,3000))], f'WWE connect {14 if pos=="LEFT" else 15}: source drawn {pos} -> centreline corner, master centred')
# 16/17 target created LEFT / RIGHT
for pos, cx in (('LEFT', 4900.0), ('RIGHT', 5100.0)):
    def setup(pos=pos):
        settings(150, 'CENTER'); add(((0,0),(4000,0))); settings(200, pos); add(((5000,200),(5000,3000)))
    scen(setup, lambda cx=cx: wconn(S2, 0.95, C((cx,200),(cx,3000),200.0), 0.5),
         [C((0,0),(cx,0)), C((cx,0),(cx,3000),200.0)], f'WWE connect {16 if pos=="LEFT" else 17}: target drawn {pos} -> centreline corner, master centred')
# 20 ambiguity / reach refusals
fresh(); mk_walls(S2, T2, C((5000,0),(7000,-2000))); before = geo()
r = wconn(S2, 0.95, T2, 0.5)
chk('WWE connect 20: corner already a junction of other walls -> refused, no change', isinstance(r, str) and 'junction' in r and geo() == before)
fresh(); mk_walls(S2, T3, C((5000,200),(7000,200))); before = geo()
r = wconn(S2, 0.95, T3, 0.5)
chk('WWE connect: target end connected elsewhere -> refused, no change', isinstance(r, str) and 'connected elsewhere' in r and geo() == before)
scen(lambda: mk_walls(S2, C((5000,500),(5000,3000))), lambda: wconn(S2, 0.95, C((5000,500),(5000,3000)), 0.5),
     [C((0,0),(5000,0)), C((5000,0),(5000,3000))], 'WWE connect: free target end 500 away (> WWE_CORNER_DISTANCE) -> connected (explicit target)')
fresh(); mk_walls(S2, C((-1000,-3000),(-1000,3000))); before = geo()
r = wconn(S2, 0.95, C((-1000,-3000),(-1000,3000)), 0.5)
chk('WWE connect: corner behind the fixed end -> refused, no change', isinstance(r, str) and 'behind' in r and geo() == before)
# 21 / 22 undo
fresh(); mk_walls(S1, T1); before = geo(); ents = {e: list(A.DB[e]) for e, a, b in masters_now()}
r = wconn(S1, 0.95, T1, 0.2); undo_rec(r[0])
chk('WWE connect 21: Undo after a source-only move restores the drawing exactly', geo() == before and all(e not in A.DELETED and A.DB[e] == d for e, d in ents.items()))
fresh(); mk_walls(S2, T3); before = geo(); ents = {e: list(A.DB[e]) for e, a, b in masters_now()}
r = wconn(S2, 0.95, T3, 0.5); undo_rec(r[0])
chk('WWE connect 22: Undo after source + target moves restores the drawing exactly', geo() == before and all(e not in A.DELETED and A.DB[e] == d for e, d in ents.items()))
# 23 rollback when a later step fails
fresh(); mk_walls(S2, T3); before = geo()
A.ev('(progn (setq *t-rb* wt:rebuild *t-n* 0) (defun wt:rebuild (new removed) (setq *t-n* (1+ *t-n*)) (if (> *t-n* 1) (wt:t-boom)) (apply *t-rb* (list new removed))))')
failed = False
try: wconn(S2, 0.95, T3, 0.5)
except A.LispError: failed = True
A.ev('(progn (setq wt:rebuild *t-rb*) (wt:error "test abort"))')
chk('WWE connect 23: a failure after the old spans were removed rolls everything back', failed and geo() == before)
# command prompts + message
fresh(); mk_walls(S2, T3)
e1, q1 = face_pick(S2, 1, 0.95); e2, q2 = face_pick(T3, 1, 0.5)
A.ENTSELQ[:] = [[e1, [q1[0], q1[1], 0.0]], [e2, [q2[0], q2[1], 0.0]]]
out = io.StringIO(); A.OUTPUT = True
with contextlib.redirect_stdout(out): A.ev('(c:WWE)')
A.OUTPUT = False
src_l = open(os.path.join(HERE, '..', 'WallTool.lsp')).read()
chk('WWE command: "Select wall end to connect" / "Select target wall", reports Wall connected',
    'Wall connected' in out.getvalue() and '"\\nSelect wall end to connect: "' in src_l and '"\\nSelect target wall: "' in src_l
    and mk((0,0),(5000,0)) in master_set())

# =========================== Local axis healing ===========================
def nmast(): return len(masters_now())
# WW
ww_run([(0,0),(4000,0),'Enter'])
chk('HEAL 1: WW free segment -> one master, nothing else changed', master_set() == [mk((0,0),(4000,0))] and nmast() == 1)
fresh(); add(((0,0),(6000,0)))
ww_run([(3000,0),(3000,3000),'Enter'], keep_session=True)
chk('HEAL 2: WW branch into a wall -> T split kept (3 spans)', nmast() == 3 and normalized_ok() and lines_match_masters())
fresh(); add(((0,0),(6000,0)))
ww_run([(3000,-3000),(3000,3000),'Enter'], keep_session=True)
chk('HEAL 3: WW crossing wall -> X splits kept (4 spans)', nmast() == 4 and normalized_ok() and lines_match_masters())
A.POINTQ[:] = []
fresh(); A.POINTQ[:] = ['Rectangle', [0.0,0.0,0.0], [4000.0,3000.0,0.0], None]; A.KWQUEUE[:] = []; A.ev('(c:WW)')
chk('HEAL 4: WW Rectangle -> four corner spans kept', nmast() == 4 and normalized_ok() and lines_match_masters())
ww_run([(0,0),(3000,0),(6000,0),'Enter'])
chk('HEAL 4b: WW straight run in two clicks -> redundant node healed into one master', master_set() == [mk((0,0),(6000,0))] and lines_match_masters())
ww_run([(0,0),(3000,0),(6000,0),(6000,3000),'Enter'], th=150.0, pos='LEFT')
chk('HEAL 4c: WW LEFT straight run + corner -> straight node healed, corner still joins the healed master',
    master_set() == sorted([mk((0,75),(5925,75)), mk((5925,75),(5925,3000))]) and master_offsets_ok()
    and same_lines(walls_now(), expected([wall((0,0),(3000,0),150,'LEFT'), wall((3000,0),(6000,0),150,'LEFT'), wall((6000,0),(6000,3000),150,'LEFT')])))
ww_run([(0,0),(3000,0),'Enter']); one = geo()
ww_run([(0,0),(3000,0),(6000,0),'Undo','Enter'])
chk('HEAL 11: WW Undo after a healed straight segment -> exactly the one-segment state', geo() == one)
fresh(); add(((0,10000),(3000,10000))); add(((3000,10000),(6000,10000)))     # unhealed pair elsewhere (plain add)
ww_run([(0,0),(3000,0),(6000,0),'Enter'], keep_session=True)
chk('HEAL 10a: WW heals only its own nodes (an unrelated split pair elsewhere is untouched)',
    mk((0,10000),(3000,10000)) in master_set() and mk((3000,10000),(6000,10000)) in master_set() and mk((0,0),(6000,0)) in master_set())
chk('WWF / XW / plain add: no local healing (collinear continuation keeps its spans)', True if False else
    (lambda: (fresh(), add(((0,0),(3000,0))), add(((3000,0),(6000,0))), nmast() == 2)[-1])())
# WWE
fresh(); mk_walls(H8, B8, T8b)
r = wconn(C((3000,-1500),(3000,0)), 0.5, T8b, 0.2)
chk('HEAL 5: WWE detaches a T branch -> old host split healed (one host master)', okx(r) and mk((0,0),(6000,0)) in master_set())
fresh(); mk_walls(S2, P7, T7b)
r = wconn(C((2000,0),(4000,0)), 0.8, T7b, 0.2, sside=-1)
chk('HEAL 6: WWE detaches an L -> old partner a single free master, no leftover node',
    okx(r) and mk((4000,0),(4000,3000)) in master_set() and nmast() == 4 and normalized_ok())
fresh(); mk_walls(L9, R9, BR9, T9)
r = wconn(L9, 0.97, T9, 0.2, sside=-1)
chk('HEAL 7: WWE through a collinear continuation -> T node with the branch kept, nothing else split',
    okx(r) and master_set() == sorted([mk((0,0),(7000,0)), mk((7000,0),(13000,0)), mk((7000,0),(7000,3000)),
                                        mk((13000,-3000),(13000,0)), mk((13000,0),(13000,3000))]))
fresh(); mk_walls(S2, T3); r = wconn(S2, 0.95, T3, 0.5)
chk('HEAL 8: WWE smart fillet -> exactly two masters meeting at the corner', okx(r) and master_set() == sorted([mk((0,0),(5000,0)), mk((5000,0),(5000,3000))]))
fresh(); mk_walls(S5, T3); r = wconn(S5, 0.98, T3, 0.5)
chk('HEAL 9: WWE trim + extend -> exactly two masters', okx(r) and master_set() == sorted([mk((0,0),(5000,0)), mk((5000,0),(5000,3000))]))
fresh(); mk_walls(S2, T3); add(((0,10000),(3000,10000))); add(((3000,10000),(6000,10000)))
r = wconn(S2, 0.95, T3, 0.5)
chk('HEAL 10b: WWE heals only its own nodes (an unrelated split pair elsewhere is untouched)',
    okx(r) and mk((0,10000),(3000,10000)) in master_set() and mk((3000,10000),(6000,10000)) in master_set())
fresh(); mk_walls(H8, B8, T8b); before = geo(); ents = {e: list(A.DB[e]) for e, a, b in masters_now()}
r = wconn(C((3000,-1500),(3000,0)), 0.5, T8b, 0.2); undo_rec(r[0])
chk('HEAL 12: WWE Undo including the healed host -> exact original (split host back)',
    geo() == before and all(e not in A.DELETED and A.DB[e] == d for e, d in ents.items()))

# =========================== WWE far explicit targets ===========================
for dd in (100, 300, 301, 1500, 5000):
    Tf = C((5000,dd),(5000,dd+3000))
    scen(lambda Tf=Tf: mk_walls(S2, Tf), lambda Tf=Tf: wconn(S2, 0.95, Tf, 0.5), [C((0,0),(5000,0)), C((5000,0),(5000,dd+3000))],
         f'WWE far: free target end {dd} beyond the corner -> connected (L)')
fresh(); mk_walls(S2, C((5000,1500),(5000,4500)), C((4000,800),(6000,800))); before = geo()
r = wconn(S2, 0.95, C((5000,1500),(5000,4500)), 0.5)
chk('WWE far: another wall across the target extension -> refused, no change', isinstance(r, str) and 'between' in r and geo() == before)
fresh(); mk_walls(S2, C((5000,1500),(5000,4500)), C((5000,1500),(7000,1500))); before = geo()
r = wconn(S2, 0.95, C((5000,1500),(5000,4500)), 0.5)
chk('WWE far: target end already connected (L elsewhere) -> refused, topology kept', isinstance(r, str) and 'connected elsewhere' in r and geo() == before)
fresh(); mk_walls(S2, C((0,2000),(4000,2000))); before = geo()
r = wconn(S2, 0.9, C((0,2000),(4000,2000)), 0.5)
chk('WWE far: parallel target still refused', isinstance(r, str) and 'parallel' in r and geo() == before)
T21a = C((5000,-250),(5000,3000))
scen(lambda: mk_walls(S2, T21a), lambda: wconn(S2, 0.95, T21a, 0.5), [C((0,0),(5000,0)), C((5000,0),(5000,3000))],
     'WWE classify: corner 250 from a free target end (<= WWE_CORNER_DISTANCE) -> L, target trimmed')
T21b = C((5000,-400),(5000,3000))
scen(lambda: mk_walls(S2, T21b), lambda: wconn(S2, 0.95, T21b, 0.5), [C((0,0),(5000,0)), T21b],
     'WWE classify: corner 400 from the target end (> WWE_CORNER_DISTANCE) -> T, target unchanged')
S24, T24 = C((0,0),(4000,0),200.0), C((5000,2000),(5000,5000),100.0)
scen(lambda: mk_walls(S24, T24), lambda: wconn(S24, 0.95, T24, 0.5), [C((0,0),(5000,0),200.0), C((5000,0),(5000,5000),100.0)],
     'WWE far: 200 + 100 smart fillet with the target 2000 away')

# =========================== Native STRETCH compatibility ===========================
def stretch(x0, y0, x1, y1, dx, dy, layers=('X-AXIS', 'A-WALL')):
    """what AutoCAD STRETCH does to LINEs: endpoints inside the crossing window move"""
    for lyr in layers:
        for e, a, b in A.db_lines(lyr):
            for k, p in ((0, a), (1, b)):
                if x0 <= p[0] <= x1 and y0 <= p[1] <= y1:
                    set_end(e, k, (p[0] + dx, p[1] + dy))
def rec_of_face(pt):
    A.G[A.Sym('*T-E*')] = face_at(pt)
    return A.ev('(wt:wall-from-entity *t-e* (wt:net-scan))')
W25 = C((0,0),(4000,0),200.0)
def stretched_free():
    fresh(); mk_walls(W25); stretch(3900,-200,4200,200,1000,0)
stretched_free()
r = rec_of_face((2000,100))
chk('STRETCH 25/32: complete end stretch -> recognised from current geometry (5000 long, 200, centred), session thickness kept',
    r and mk(r[1], r[2]) == mk((0,0),(5000,0)) and round(r[3]) == 200 and master_offsets_ok()
    and same_lines(walls_now(), expected([C((0,0),(5000,0),200.0)])))
stretched_free(); ew(face_at((4500,100)), pts={face_at((4500,100)): (4500.0, 100.2)})
chk('STRETCH 26: EW on the stretched wall removes it', not masters_now() and not walls_now())
stretched_free(); e, q = face_pick(C((0,0),(5000,0),200.0), 1, 0.5); wwf(e, q, 1500)
chk('STRETCH 27: WWF from the stretched wall copies its new length', mk((0,1700),(5000,1700)) in master_set() and master_offsets_ok())
stretched_free(); settings(150, 'CENTER'); add(((0,3000),(5000,3000)))
wwd(face_at((2500,100)), (2500.0, 100.2), face_at((2500,2925)), (2500.0, 2924.8), 1000.0)
chk('STRETCH 28: WWD moves the stretched wall', mk((0,1825),(5000,1825)) in master_set() and master_offsets_ok())
stretched_free(); settings(150, 'CENTER'); add(((7000,-3000),(7000,3000)))
r = wconn(C((0,0),(5000,0),200.0), 0.95, C((7000,-3000),(7000,3000)), 0.2)
chk('STRETCH 29: WWE extends the stretched wall to a target', okx(r) and mk((0,0),(7000,0)) in master_set() and normalized_ok())
fresh(); mk_walls(S2, P7); stretch(3800,-300,4300,3300,1000,0)
chk('STRETCH 30: complete L-corner stretch (partner moved whole) -> still a valid, recognised L',
    master_set() == sorted([mk((0,0),(5000,0)), mk((5000,0),(5000,3000))]) and master_offsets_ok()
    and same_lines(walls_now(), expected([C((0,0),(5000,0)), C((5000,0),(5000,3000))])))
fresh(); mk_walls(H8, B8); stretch(2800,-3200,3200,-2800,0,-1000)
chk('STRETCH 31: complete T-branch end stretch -> valid T',
    mk((3000,-4000),(3000,0)) in master_set() and master_offsets_ok()
    and same_lines(walls_now(), expected([H8, C((3000,-4000),(3000,0))])))
fresh(); mk_walls(H8, B8); stretch(5800,-200,6200,200,1000,0)
chk('STRETCH 31b: complete T-host end stretch (normalized host with T node) -> valid, node kept',
    master_set() == sorted([mk((0,0),(3000,0)), mk((3000,0),(7000,0)), mk((3000,-3000),(3000,0))]) and master_offsets_ok()
    and same_lines(walls_now(), expected([C((0,0),(7000,0)), B8])))
fresh(); mk_walls(W25); stretch(3900,-200,4200,200,1000,0, layers=('A-WALL',))
r = rec_of_face((2000,100)); damaged = not same_lines(walls_now(), expected([W25]))
chk('STRETCH 33: faces stretched, axis not -> the wall is still the axis (4000), not a new 5000 wall; mismatch visible',
    r and mk(r[1], r[2]) == mk((0,0),(4000,0)) and damaged)
wr(field(-500,-500,5500,500))
chk('STRETCH 35: WWR restores the faces from the axis', master_set() == [mk((0,0),(4000,0))] and same_lines(walls_now(), expected([W25])))
fresh(); mk_walls(W25); stretch(3900,-200,4200,200,1000,0, layers=('X-AXIS',))
r = rec_of_face((2000,100))
chk('STRETCH 34: axis stretched, faces not -> axis wins (5000), faces not treated as a separate wall',
    r and mk(r[1], r[2]) == mk((0,0),(5000,0)) and not same_lines(walls_now(), expected([C((0,0),(5000,0),200.0)])))
wr(field(-500,-500,5500,500))
chk('STRETCH 35b: WWR rebuilds the faces to the stretched axis', master_set() == [mk((0,0),(5000,0))] and same_lines(walls_now(), expected([C((0,0),(5000,0),200.0)])))
fresh(); mk_walls(W25); stretch(-500,-500,4500,500,0,60, layers=('X-AXIS',))
wr(field(-500,-500,4500,500))
chk('STRETCH 36: axis stretched sideways as a whole -> WWR re-centres it, faces unchanged',
    master_set() == [mk((0,0),(4000,0))] and same_lines(walls_now(), expected([W25])))
fresh(); mk_walls(W25); stretch(3900,-200,4200,200,0,300, layers=('X-AXIS',)); before = geo()
msg, _ = say(lambda: wr(field(-500,-500,4500,500)))
e = face_at((2000,100)); A.G[A.Sym('*T-E*')] = e
res = A.ev('(wt:ew-resolve *t-e* (list 2000.0 100.2) (wt:net-scan) 0.5)')
chk('STRETCH 37: axis stretched at one end only (skewed) -> not a wall for EW, WWR skips it as ambiguous, nothing changed',
    res[0] != 'OK' and geo() == before and 'ambiguous' in msg)

# --- TX protects AKD wall geometry (integration) ---
build([wall((0,0),(6000,0))]); add(((3000,0),(3000,3000))); akd_before = geo()
akd_ids = [e for e, a, b in walls_now()] + [e for e, a, b in masters_now()]
g1 = mkline((0,5000),(2950,5000),'A-WALL'); g2 = mkline((3000,5050),(3000,7000),'A-WALL')     # ordinary lines on A-WALL, near-L
g3 = mkline((8000,0),(10000,0),'0'); g4 = mkline((10050,50),(10050,2000),'0')                  # ordinary lines elsewhere
A.SSFIRST[0] = akd_ids + [g1, g2, g3, g4]
out = io.StringIO(); A.OUTPUT = True
with contextlib.redirect_stdout(out): A.ev('(c:TX)')
A.OUTPUT = False
lines_g = [(a, b) for e, a, b in A.db_lines('A-WALL') + A.db_lines('0') if e not in akd_ids]
chk('TX selection over AKD walls: masters, faces and caps untouched; ordinary A-WALL and layer-0 lines still repaired',
    geo() [1] == akd_before[1] and all(e in A.DB and e not in A.DELETED for e in akd_ids)
    and sorted((round(a[0],3), round(a[1],3), round(b[0],3), round(b[1],3)) for e, a, b in walls_now() if e in akd_ids) == akd_before[0]
    and any(near(p, (3000,5000)) for a, b in lines_g for p in (a, b))
    and any(near(p, (10050,0)) for a, b in lines_g for p in (a, b))
    and 'AKD wall line(s) left to the wall tools' in out.getvalue())
build([wall((0,0),(6000,0))]); add(((3000,0),(3000,3000))); akd_before = geo()
A.POINTQ[:] = [[-1000.0, -1000.0, 0.0], [7000.0, 4000.0, 0.0]]
A.ev('(c:TX)')
chk('TX window over AKD walls: nothing changed', geo() == akd_before)
legacy_fix = [wall((0,0),(6000,0),200,'LEFT')]
fresh()
for a, b, th, pos in legacy_fix: mkline(a, b, 'X-AXIS')
for a, b in expected(legacy_fix): mkline(a, b, 'A-WALL')
before = geo()
A.POINTQ[:] = [[-1000.0, -1000.0, 0.0], [7000.0, 1000.0, 0.0]]
A.ev('(c:TX)')
chk('TX window over a legacy off-centre wall: its faces and caps are protected (left to WWR)', geo() == before)

# Position submenu keys (shared by WW and XW)
res = []
for key, want in (('Q', 'LEFT'), ('W', 'CENTER'), ('E', 'RIGHT'), ('Left', 'LEFT'), ('Right', 'RIGHT'), ('Center', 'CENTER')):
    A.KWQUEUE[:] = [key]; A.ev('(wt:ww-position)'); res.append(A.ev('*wt:pos*') == want)
A.KWQUEUE[:] = []; settings(150, 'RIGHT'); A.ev('(wt:ww-position)')
enter_keeps = A.ev('*wt:pos*') == 'RIGHT'
A.KWQUEUE[:] = ['XYZ']; A.ev('(wt:ww-position)')
chk('Alignment submenu: Left/Center/Right and Q/W/E, Enter keeps current, invalid input leaves it unchanged',
    all(res) and enter_keeps and A.ev('*wt:pos*') == 'RIGHT')
src = open(os.path.join(HERE, '..', 'WallTool.lsp')).read()
chk('Prompts: WW shows Alignment in start and next-point prompts, XW keeps [Width/posiTion], submenu keys valid',
    '[Width/Alignment/Rectangle/Settings]' in src and '[Width/Alignment/Rectangle/Undo/Settings]' in src
    and '[Width/Alignment/Rectangle/Undo/Close/Settings]' in src and '"Width Alignment Rectangle Undo Close Settings"' in src
    and '[Width/posiTion]' in src and 'Eccentricity' not in src and '"Left Center Right Q W E"' in src
    and 'Alignment [Left/Center/Right] <' in src)

# =========================== Openings hook (AKD WinDoor) ===========================
def hole_cut(x0, x1, y=0.0, th=150.0):   # what WinDoor's HH does to an AKD wall along +X
    h = th / 2
    for f in [e for e, a, b in walls_now() if abs(a[1]-b[1]) < 1e-6 and abs(abs(a[1]-y)-h) < 1e-6 and min(a[0],b[0]) < x0 and max(a[0],b[0]) > x1]:
        _, a, b = next(l for l in walls_now() if l[0] == f); lo, hi = sorted([a[0], b[0]])
        A.entdel(f)
        mkline((lo, a[1]), (x0, a[1]), 'A-WALL'); mkline((x1, a[1]), (hi, a[1]), 'A-WALL')
    mkline((x0, y-h), (x0, y+h), 'A-WALL'); mkline((x1, y-h), (x1, y+h), 'A-WALL')
def covered(pt): return any(A.ev(f'(wt:on-seg {P(*pt)} {P(*a)} {P(*b)})') for _, a, b in walls_now())
def caps_at(x): return sum(1 for _, a, b in walls_now() if near(a, (x,-75)) and near(b, (x,75)) or near(a, (x,75)) and near(b, (x,-75)))
def opening_ok(): return (not covered((2450,75)) and not covered((2450,-75)) and covered((1000,75)) and covered((3500,-75))
                          and caps_at(2000) == 1 and caps_at(2900) == 1)
A.ev('(defun t:openings () (list (list (list 2450.0 0.0 0.0) (list 1.0 0.0 0.0) 900.0)))')
A.ev('(setq *wt:opening-fns* nil)')
build([wall((0,0),(6000,0))]); hole_cut(2000, 2900)
add(((4000,0),(4000,3000)))
chk('openings: without a provider a rebuild fills the hole (the reported bug)', covered((2450,75)))
A.ev("(setq *wt:opening-fns* (list 't:undefined-fn 't:openings))")
build([wall((0,0),(6000,0))]); hole_cut(2000, 2900); before = snapshot()
rec = add(((4000,0),(4000,3000)))
chk('openings: WW T into a wall with a hole keeps the hole and one cap per jamb', opening_ok())
undo_rec(rec)
chk('openings: Undo restores the holed wall exactly', snapshot() == before)
add(((0,0),(0,3000))); add(((6000,0),(6000,3000)))
chk('openings: L corners at both ends keep the hole', opening_ok())
ew(*[e for e, a, b in masters_now() if abs(a[1]) < 1e-6 and abs(b[1]) < 1e-6])
chk('openings: EW of the holed wall removes its jamb caps too', caps_at(2000) == 0 and caps_at(2900) == 0 and not covered((1000,75)))
build([wall((0,0),(6000,0))])
chk('openings: a wall drawn through a registered opening is cut on creation', opening_ok())
build([wall((0,500),(6000,500))])
chk('openings: an opening outside the wall band is ignored', covered((2450,575)) and covered((2450,425)))
A.ev('(setq *wt:opening-fns* nil)')

print(f'\n{fails} failure(s)')
sys.exit(1 if fails else 0)
