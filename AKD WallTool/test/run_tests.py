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
segs, got = recon_case([wall(O, (3000, 0), 200, 'LEFT'), wall((3000, 0), (3000, 2500), 150), wall((0, -3000), (0, 3000), 100, 'RIGHT')])
chk('recon: thickness/position recovered (LEFT, CENTER, RIGHT)', [g[0] for g in got] == [(200, 'LEFT'), (150, 'CENTER'), (100, 'RIGHT')])
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
build([wall((0,0),(10000,0),200,'LEFT')]); settings(150, 'CENTER'); add(((5000,0),(5000,4000)))
A.ev('(setq *wt:reg* nil)')
net_ok = all(A.ev(f'(wt:recon (list nil {P(*a)} {P(*b)}) (cadr (wt:net-scan)))') for a, b in [((0,0),(5000,0)), ((5000,0),(10000,0))])
rec_l = A.ev(f'(wt:recon (list nil {P(0,0)} {P(5000,0)}) (cadr (wt:net-scan)))')
ew_span((5000,0),(10000,0), via='face', off=200.0)
chk('reload: split spans reconstruct with inherited 200/LEFT; EW right arm leaves 「',
    net_ok and rec_l and round(rec_l[3]) == 200 and rec_l[4] == 'LEFT'
    and master_set() == sorted([mk((0,0),(5000,0)), mk((5000,0),(5000,4000))])
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
settings(200, 'CENTER'); add(((0,0),(0,-3000)))                # T into wall 1 -> wall 1 regenerated
ws = [wall((3000,0),(-3000,0),150,'RIGHT'), wall((0,9000),(3000,9000)), wall((0,0),(0,-3000),200)]
chk('ownership: rebuilding a wall never erases an aligned cap of an unrelated wall', same_lines(walls_now(), expected(ws)))
ew(find_line('X-AXIS', (3000,0), (0,0)), find_line('X-AXIS', (0,0), (-3000,0)))   # T split it into two spans
chk('ownership: EW on that wall leaves the unrelated wall intact', same_lines(walls_now(), expected([ws[1], ws[2]])) and len(masters_now()) == 2)

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

# Position submenu keys (shared by WW and XW)
res = []
for key, want in (('Q-left', 'LEFT'), ('W-center', 'CENTER'), ('E-right', 'RIGHT'), ('Left', 'LEFT'), ('Right', 'RIGHT')):
    A.KWQUEUE[:] = [key]; A.ev('(wt:ww-position)'); res.append(A.ev('*wt:pos*') == want)
A.KWQUEUE[:] = []; settings(150, 'RIGHT'); A.ev('(wt:ww-position)')
chk('Position submenu: Q=LEFT W=CENTER E=RIGHT, Enter keeps current', all(res) and A.ev('*wt:pos*') == 'RIGHT')
src = open(os.path.join(HERE, '..', 'WallTool.lsp')).read()
chk('Prompts: posiTion shown in WW/XW, no Eccentricity, submenu keys valid',
    '[Width/posiTion/Rectangle/Settings]' in src and '[Width/posiTion/Undo/Close]' in src and '[Width/posiTion]' in src
    and 'Eccentricity' not in src and '"Q-left W-center E-right Left Center Right"' in src
    and src.count('"Width posiTion') == 3)

print(f'\n{fails} failure(s)')
sys.exit(1 if fails else 0)
