"""AKD WallTool + AKD WinDoor integration checks (real WallTool.lsp and AKDDoorWin.lsp).
Usage: python3 integration_tests.py"""
import math, os, re, subprocess, sys
sys.setrecursionlimit(10000)
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import alisp as A
WT = os.path.join(HERE, '..', 'WallTool.lsp')
WD = os.path.join(HERE, '..', '..', 'AKD WinDoor', 'AKDDoorWin.lsp')
A.OUTPUT = False

fails = 0
def chk(name, ok):
    global fails
    print(('PASS  ' if ok else 'FAIL  ') + name)
    if not ok: fails += 1

def sub(code):
    """run code in a fresh interpreter (load-order checks); returns stdout"""
    pre = f"import sys; sys.setrecursionlimit(10000); sys.path.insert(0, {HERE!r}); import alisp as A; A.OUTPUT = False\n"
    r = subprocess.run([sys.executable, '-c', pre + code], capture_output=True, text=True)
    return r.stdout + r.stderr

# ---------------------------------------------------------------- load: C (WallTool then WinDoor)
A.load(WT); A.ev('(setq *wt:cfg* *wt:cfg-defaults*)'); A.load(WD)

def P(x, y): return f'(list {float(x)} {float(y)})'
def P3(p): return f'(list {float(p[0])} {float(p[1])} 0.0)'
def near(p, q, tol=1e-3): return math.dist(p, q) < tol
def fresh():
    A.db_reset(); A.ev('(setq *wt:reg* nil *wt:pending* nil *wt:thk* 150.0 *wt:pos* "CENTER")')
def settings(th): A.ev(f'(setq *wt:thk* {float(th)} *wt:pos* "CENTER")')
def add(*segs, th=150.0):
    settings(th); return A.ev('(wt:walls-add (list ' + ' '.join(f'(list {P(*a)} {P(*b)})' for a, b in segs) + '))')
def walls_now(): return A.db_lines('A-WALL')
def masters_now(): return A.db_lines('X-AXIS')
def mk(a, b): return tuple(sorted([tuple(round(v, 3) for v in a), tuple(round(v, 3) for v in b)]))
def master_set(): return sorted(mk(a, b) for _, a, b in masters_now())
def alive(e): return e in A.DB and e not in A.DELETED
def on(p, a, b, tol=1e-3):
    L_ = math.dist(a, b); u = ((b[0]-a[0])/L_, (b[1]-a[1])/L_)
    s = (p[0]-a[0])*u[0] + (p[1]-a[1])*u[1]
    return -tol <= s <= L_ + tol and abs((p[0]-a[0])*u[1] - (p[1]-a[1])*u[0]) < tol
def covered(p): return any(on(p, a, b) for _, a, b in walls_now())
def face_line(p): return next(e for e, a, b in walls_now() if on(p, a, b))
def jambs_x(x, y=0.0, h=75.0):
    return sum(1 for _, a, b in walls_now() if near(a, (x, y-h)) and near(b, (x, y+h)) or near(a, (x, y+h)) and near(b, (x, y-h)))
def jambs_y(y, x=0.0, h=75.0):
    return sum(1 for _, a, b in walls_now() if near(a, (x-h, y)) and near(b, (x+h, y)) or near(a, (x+h, y)) and near(b, (x-h, y)))
def hole_x(x0, x1, y=0.0, h=75.0):
    """horizontal wall at y: gap in both faces over (x0, x1), one jamb at each end"""
    m = (x0 + x1) / 2
    return (not covered((m, y+h)) and not covered((m, y-h)) and not covered((x0+1, y+h)) and not covered((x1-1, y-h))
            and covered((x0-1, y+h)) and covered((x1+1, y-h)) and jambs_x(x0, y, h) == 1 and jambs_x(x1, y, h) == 1)
def solid_x(x0, x1, y=0.0, h=75.0):
    return all(covered((x, y+h)) and covered((x, y-h)) for x in (x0+1, (x0+x1)/2, x1-1)) and jambs_x(x0, y, h) == 0 and jambs_x(x1, y, h) == 0
def snap(): return {e: list(d) for e, d in A.DB.items() if e not in A.DELETED}
def undo_rec(rec): A.G[A.Sym('*T-REC*')] = rec; A.ev('(wt:seg-undo *t-rec*)')
def lines_geo(): return sorted(mk(a, b) + (l,) for l in ('A-WALL', 'X-AXIS') for _, a, b in A.db_lines(l))

gid = [0]
def opening(mid, w, d=(1.0, 0.0), kind='door', label=True, name=None):
    """a placed WinDoor object as AD/AW leave it: tagged INSERT + linked label"""
    gid[0] += 1
    app, lay = ('ADOOR', 'A-DOOR') if kind == 'door' else ('AWIN', 'A-WINDOW')
    name = name or (f'AKD-DS-{int(w)}' if kind == 'door' else f'AKD-WF-{int(w)}-1')
    gn = f'{app}-T{gid[0]}'
    A.ev(f'(entmake (list (cons 0 "INSERT") (cons 8 "{lay}") (cons 2 "{name}") (cons 10 {P3(mid)}) (cons 50 {math.atan2(d[1], d[0])})))')
    e = A.LAST[0]; A.G[A.Sym('*T-E*')] = e
    if kind == 'door': A.ev(f'(_tagdoor *t-e* {float(w)} "S" 1 1.0 {P3(mid)} {P3(d)} "{gn}")')
    else: A.ev(f'(_tagwin *t-e* {float(w)} 1 1.0 {P3(mid)} {P3(d)} "{gn}")')
    lbl = None
    if label:
        A.ev(f'(entmake (list (cons 0 "CIRCLE") (cons 8 "X-TAGS") (cons 10 {P3((mid[0], mid[1]+450))}) (cons 40 225.0)))')
        lbl = A.LAST[0]; A.G[A.Sym('*T-L*')] = lbl
        A.ev(f'({"_tag-doorlbl" if kind == "door" else "_tag-winlbl"} *t-l* "{gn}")')
    return e, lbl
def place(mid, w, d=(1.0, 0.0), kind='door'):
    """AD/AW on an AKD WallTool wall: object first, then WallTool cuts the opening"""
    e, l = opening(mid, w, d, kind)
    A.ev(f'(akd:wt-regen (list (list {P3(mid)} {P3(d)} {float(w)})))')
    return e, l
def ins_pt(e): return tuple(A.cdr(next(p for p in A.DB[e] if A.car(p) == 10)))[:2]
def xd_mid(e):
    x = next(i for i in A.DB[e] if A.car(i) == -3)
    items = A.cdr(A.car(A.cdr(x)))
    it = next(i for i in items if A.car(i) == 1011)
    return tuple(A.cdr(it))[:2]
def ew(*enames):
    A.G[A.Sym('*T-SEL*')] = [[e, None, 'LINE'] for e in enames]; return A.ev('(wt:ew-erase *t-sel* 0.5)')
def tw(x0, y0, x1, y1):
    A.G[A.Sym('*T-F*')] = [[float(x0), float(y0)], [float(x1), float(y0)], [float(x1), float(y1)], [float(x0), float(y1)]]
    return A.ev('(wt:tw-repair *t-f* 150.0)')
def wwr(x0, y0, x1, y1):
    A.G[A.Sym('*T-F*')] = [[float(x0), float(y0)], [float(x1), float(y0)], [float(x1), float(y1)], [float(x0), float(y1)]]
    return A.ev('(wt:wr-repair *t-f*)')
def wwd(e1, q1, e2, q2, d):
    A.G[A.Sym('*W1*')] = e1; A.G[A.Sym('*W2*')] = e2
    return A.ev(f'(wt:wwd-run *w1* {P(*q1)} *w2* {P(*q2)} {float(d)})')
def wwe(e1, q1, e2, q2):
    A.G[A.Sym('*W1*')] = e1; A.G[A.Sym('*W2*')] = e2
    return A.ev(f'(wt:wwe-run *w1* {P(*q1)} *w2* {P(*q2)})')
def wwf(e, q, dist):
    A.G[A.Sym('*T-E*')] = e; A.G[A.Sym('*T-Q*')] = [float(q[0]), float(q[1])]
    return A.ev(f'(wt:wwf-add *t-e* *t-q* {float(dist)})')
def xw(ids): A.G[A.Sym('*T-SEL*')] = ids; return A.ev('(wt:xw-convert *t-sel*)')
def mkline(a, b, layer):
    A.ev(f'(entmake (list (cons 0 "LINE") (cons 8 "{layer}") (cons 10 {P3(a)}) (cons 11 {P3(b)})))'); return A.LAST[0]
def info(e): A.G[A.Sym('*T-E*')] = e; return A.ev('(cw:read-xd *t-e*)')
def reg_count(): return [len([x for x in (A.ev(s) or []) if x == 'AKD:WT-' + t]) for s, t in
                         (('*wt:opening-fns*', 'OPENINGS'), ('*wt:wall-moved-fns*', 'MOVED'), ('*wt:opening-removed-fns*', 'REMOVED'))]

# ---------------------------------------------------------------- E / AN / AO: command names
def cmds(path): return set(m.upper() for m in re.findall(r'\(defun\s+c:([^\s()]+)', open(path).read(), re.I))
cw_, cd_ = cmds(WT), cmds(WD)
chk('AO: complete command-name audit: WallTool and WinDoor define no common command', not (cw_ & cd_))
chk('AM/AN: WallTool defines WWR and no WR; WinDoor uses EDW / WRN, not EW / WR',
    'WWR' in cw_ and 'WR' not in cw_ and {'EDW', 'WRN'} <= cd_ and not ({'EW', 'WR'} & cd_))
chk('E: after loading both, EW and WWR are WallTool\'s, EDW and WRN are WinDoor\'s',
    A.ev("c:EW") == A.ev("c:EW") and 'WT:EW-ERASE' in str(A.ev("c:EW")) and 'WT:WR-REPAIR' in str(A.ev("c:WWR"))
    and 'EW:DO-ONE' in str(A.ev("c:EDW")) and A.ev("c:WR") is None)
wtsrc = open(WT).read()
chk('WallTool core: no XData, no VL/VLA/VLAX, no WinDoor names',
    not re.search(r'\(\s*vla?x?-', wtsrc, re.I) and not re.search(r"[('\s]-3[\s)]", re.sub(r';[^\n]*', '', wtsrc))
    and not re.search(r'ADOOR|AWIN|akd:', re.sub(r';[^\n]*', '', wtsrc), re.I))

# ---------------------------------------------------------------- AK / C: registration
chk('C: WallTool then WinDoor: providers registered once', reg_count() == [1, 1, 1])
A.load(WD); A.load(WD)
chk('AK: reloading WinDoor twice keeps a single registration per hook', reg_count() == [1, 1, 1])

# ---------------------------------------------------------------- F / G / AE: placement + WW rebuild
fresh(); add(((0,0),(6000,0)))
d1, l1 = place((2000,0), 900)
chk('AE: door placed on a WallTool wall -> WallTool cuts the opening with jambs', hole_x(1550, 2450))
data = list(A.DB[d1])
add(((4500,0),(4500,3000)))
chk('F: door opening survives a WW T nearby; door block untouched', hole_x(1550, 2450) and A.DB[d1] == data and alive(l1))
fresh(); add(((0,0),(6000,0)))
w1, _ = place((3000,0), 1200, kind='window')
add(((6000,0),(6000,3000))); add(((0,0),(0,3000)))
chk('G: window opening survives WW L corners at both ends', hole_x(2400, 3600))

# real hole:do with a WallTool face (post = object placement stub)
fresh(); add(((0,0),(6000,0)))
def post(b1a, b1b, b2a, b2b):
    m = ((b1a[0]+b1b[0]+b2a[0]+b2b[0])/4, (b1a[1]+b1b[1]+b2a[1]+b2b[1])/4)
    opening(m, math.dist(b1a, b1b))
A.G[A.Sym('T:POST')] = post
f = face_line((3000,75)); A.G[A.Sym('*T-E*')] = f
A.ev("(hole:do *t-e* (list 3000.0 75.0 0.0) 't:post)")
chk('AE: AD flow (hole:do) on a WallTool face: WallTool opening at the face centre', hole_x(2550, 3450))
A.G[A.Sym('*T-E*')] = face_line((1000,75))
A.ev("(hole:do *t-e* (list 1000.0 75.0 0.0) 't:post)")   # centre mode: face piece (0..2550) centre 1275 -> fits
chk('AE: second AD on the same wall face piece -> second WallTool opening', hole_x(825, 1725) and hole_x(2550, 3450))

# ---------------------------------------------------------------- H / AB: WWE
fresh(); add(((0,0),(4000,0))); add(((4000,0),(4000,3000))); add(((8000,-3000),(8000,3000)))
d1, l1 = place((2000,0), 900); data = list(A.DB[d1])
r = wwe(face_line((3600,-75)), (3600,-75.2), face_line((7925,-1800)), (7924.8,-1800))
chk('H: WWE extends the wall through its old L corner (old span removed + replaced): door kept, hole kept',
    isinstance(r, list) and mk((0,0),(4000,0)) in master_set() and mk((4000,0),(8000,0)) in master_set()
    and hole_x(1550, 2450) and A.DB[d1] == data and alive(l1))
fresh(); add(((0,0),(6000,0))); add(((8000,-3000),(8000,3000)))
d1, _ = place((2000,0), 900); data = list(A.DB[d1])
r = wwe(face_line((5800,75)), (5800,75.2), face_line((7925,-1800)), (7924.8,-1800))
chk('AB: WWE changes the far end: opening and door unchanged', isinstance(r, list) and hole_x(1550, 2450) and A.DB[d1] == data)

fresh(); add(((0,0),(4000,0))); add(((4000,0),(4000,3000))); add(((2000,-3000),(2000,3000)))
d1, l1 = place((3000,0), 900); d2, _ = place((1000,0), 900)
before = snap()
r = wwe(face_line((3700,-75)), (3700,-75.2), face_line((1925,-1800)), (1924.8,-1800))
chk('AC: WWE removes the span holding a door -> that door and its label deleted, the other door kept',
    isinstance(r, list) and not alive(d1) and not alive(l1) and alive(d2) and hole_x(550, 1450)
    and jambs_x(2550) == 0 and jambs_x(3450) == 0)
undo_rec(r[0])
chk('AC: WWE record undo restores wall, door and label exactly', snap() == before)

# ---------------------------------------------------------------- I / L: TW
fresh(); add(((0,0),(6000,0))); add(((6000,100),(6000,3000)))   # gap at the corner for TW
d1, _ = place((2000,0), 900)
tw(-500, -500, 6500, 3500)
chk('I/L: TW repairs the corner and keeps the registered hole (not a broken face)',
    hole_x(1550, 2450) and alive(d1) and covered((6000+75, 0)))

# ---------------------------------------------------------------- J / K / M: WWR
fresh(); add(((0,0),(6000,0))); d1, _ = place((3000,0), 900); before = lines_geo()
wwr(-500, -500, 6500, 500)
chk('J: WWR on a healthy wall with a door: nothing changes, hole kept', lines_geo() == before)
fresh(); add(((0,0),(6000,0))); d1, _ = place((3000,0), 900)
A.entdel(masters_now()[0][0]); A.ev('(setq *wt:reg* nil)')
wwr(-500, -500, 6500, 500)
chk('K/M: WWR rebuilds a lost master across the door (jambs are not wall ends): one master, hole kept',
    master_set() == [mk((0,0),(6000,0))] and hole_x(2550, 3450))
fresh(); add(((0,0),(6000,0))); d1, _ = place((3000,0), 900)
A.entdel(face_line((1000,75)))                                  # accidental damage
wwr(-500, -500, 6500, 500)
chk('K: WWR repairs a deleted face piece and keeps the door hole', covered((1000,75)) and hole_x(2550, 3450))
fresh(); add(((0,0),(6000,0))); d1, _ = place((3000,0), 900)
A.entdel(face_line((1000,75))); A.ev('(setq *wt:reg* nil)')     # AF: new session
wwr(-500, -500, 6500, 500)
chk('K/AF: same repair with no session registry (reconstruction from the remaining face pieces)', covered((1000,75)) and hole_x(2550, 3450))
# unregistered hole (HH-style, no object) is damage for WWR
fresh(); add(((0,0),(6000,0)))
A.G[A.Sym('*T-E*')] = face_line((3000,75)); A.ev("(hole:do *t-e* (list 3000.0 75.0 0.0) nil)")
chk('HH on a WallTool wall: legacy cut (face split)', not covered((3000,75)))
wwr(-500, -500, 6500, 500)
chk('WWR closes an unregistered hole (no WinDoor object there)', solid_x(2550, 3450))

fresh(); add(((0,0),(6000,0))); add(((6000,0),(6000,3000))); d1, _ = place((2000,0), 900)
add(((6000,3000),(9000,3000)))
chk('context wall (touching a rebuilt wall, not redrawn) keeps its hole and jambs', hole_x(1550, 2450))

# ---------------------------------------------------------------- N / O
fresh(); add(((0,0),(6500,0)))
place((1000,0), 900); place((3000,0), 1200, kind='window'); place((5000,0), 800)
add(((4100,0),(4100,3000)))
chk('N: door + window + door on one wall all survive a rebuild', hole_x(550, 1450) and hole_x(2400, 3600) and hole_x(4600, 5400))
fresh(); add(((0,0),(6000,0)))
place((2000,0), 900); place((2600,0), 800, kind='window')
chk('O: overlapping openings -> one void, no internal jambs',
    hole_x(1550, 3000) and jambs_x(2450) == 0 and jambs_x(2200) == 0 and not covered((2300,75)))

# ---------------------------------------------------------------- P / Q / R: junctions
fresh(); add(((0,0),(6000,0))); add(((2700,0),(2700,3000)))
place((2000,0), 900)
chk('P: opening next to a T (bodies do not overlap) is kept, T intact', hole_x(1550, 2450) and covered((2625,1000)) and covered((2775,1000)))
fresh(); add(((0,0),(6000,0))); add(((2700,0),(2700,3000)))
dx, _ = opening((2700,0), 900); A.ev(f'(akd:wt-regen (list (list {P3((2700,0))} {P3((1,0))} 900.0)))')
chk('P: opening overlapping the T node is ambiguous: not cut, door kept (reported)', solid_x(2300, 3100) is False and covered((2500,-75)) and alive(dx))
chk('P: API reports AMBIG for it', A.ev(f'(wt:api-opening-status (list {P3((2700,0))} {P3((1,0))} 900.0))') == 'AMBIG')
fresh(); add(((0,0),(6000,0)), ((0,0),(0,3000)))
place((750,0), 900)
chk('Q: opening near an L corner kept, outer corner intact', hole_x(300, 1200) and covered((-75,-75)) and covered((-75,1500)))
fresh(); add(((0,0),(6000,0))); add(((3000,-3000),(3000,3000)))
place((1450,0), 900); place((4550,0), 900)
chk('R: openings on both sides of an X kept, X intact', hole_x(1000, 1900) and hole_x(4100, 5000) and covered((3075,1000)) and covered((2925,-1000)))
chk('free end: opening reaching past the wall end is ambiguous',
    A.ev(f'(wt:api-opening-status (list {P3((5800,0))} {P3((1,0))} 900.0))') == 'AMBIG')

# ---------------------------------------------------------------- S / T
fresh(); add(((0,0),(6000,0)))
opening((3000,500), 900); A.ev(f'(akd:wt-regen (list (list {P3((3000,500))} {P3((1,0))} 900.0)))')
add(((4500,0),(4500,3000)))
chk('S: opening outside the wall band ignored (wall stays solid)', solid_x(2550, 3450))
opening((3000,0), 900, d=(0.0, 1.0))
add(((1000,0),(1000,-3000)))
chk('S: opening not parallel to the wall ignored', solid_x(2550, 3450))
fresh(); add(((0,0),(6000,0))); add(((0,40),(6000,40)), th=200.0)
opening((3000,20), 900)
chk('T: opening inside two different overlapping walls -> AMBIG, no cut',
    A.ev(f'(wt:api-opening-status (list {P3((3000,20))} {P3((1,0))} 900.0))') == 'AMBIG')

# ---------------------------------------------------------------- U / V
fresh(); add(((0,0),(6000,0))); place((3000,0), 900)
wwf(face_line((1000,75)), (1000,75.2), 1500)
chk('U: WWF offset wall does not copy the door opening; source keeps it',
    hole_x(2550, 3450) and master_set() == [mk((0,0),(6000,0)), mk((0,1650),(6000,1650))] and solid_x(2550, 3450, 1650))
fresh(); add(((0,0),(6000,0))); d1, _ = place((3000,0), 900)
ids = [mkline((0,3000), (6000,3000), '0')]
xw(ids)
chk('V: XW converting an unrelated line does not steal the opening', hole_x(2550, 3450) and solid_x(2550, 3450, 3000) and alive(d1))

# ---------------------------------------------------------------- W / X / Y: WWD
fresh(); add(((0,0),(6000,0))); add(((0,3000),(6000,3000)))
d1, l1 = place((2000,0), 900); w1, wl = place((4500,0), 1200, kind='window')
before = snap(); lbl0 = list(A.DB[l1])
r = wwd(face_line((500,75)), (500,75.2), face_line((500,2925)), (500,2924.8), 1000)
dy = 1850.0
chk('W: WWD moves the door block, its XData midpoint and label with the wall',
    isinstance(r, list) and near(ins_pt(d1), (2000, dy)) and near(xd_mid(d1), (2000, dy))
    and near(ins_pt(l1), (2000, dy+450)))
chk('X: WWD moves the window too; both openings cut at the new position, none left behind',
    near(ins_pt(w1), (4500, dy)) and hole_x(1550, 2450, dy) and hole_x(3900, 5100, dy)
    and not any(abs(a[1]) < 100 or abs(b[1]) < 100 for _, a, b in walls_now()))
undo_rec(r)
chk('Y: WWD record undo restores wall, door, window, labels and holes exactly', snap() == before)

# ---------------------------------------------------------------- Z / AA / AG / AH: EW, splits, heal
fresh(); add(((0,0),(6000,0))); d1, l1 = place((2000,0), 900)
before = snap()
A.ev('(wt:pend-begin)'); A.G[A.Sym('*T-W*')] = A.ev(f'(wt:wall-from-master (car (car (wt:net-scan))) (cadr (wt:net-scan)))')
A.ev('(wt:rebuild nil (list *t-w*))'); rec = A.ev('(wt:pend-end)')
chk('Z: EW path deletes the wall together with its door and label', not masters_now() and not walls_now() and not alive(d1) and not alive(l1))
undo_rec(rec)
chk('AA: the same transaction undone restores wall + door + label + opening exactly', snap() == before)
fresh(); add(((0,0),(6000,0))); d1, l1 = place((2000,0), 900)
ew(masters_now()[0][0])
chk('Z: EW command path removes door with wall', not alive(d1) and not alive(l1) and not walls_now())

fresh(); add(((0,0),(6000,0))); d1, _ = place((1500,0), 900); d2, _ = place((4500,0), 900)
add(((3000,0),(3000,3000)))
chk('AG: master split at a T: both spans keep their openings', len(masters_now()) == 3 and hole_x(1050, 1950) and hole_x(4050, 4950))
stem = next(e for e, a, b in masters_now() if near(a, (3000,3000)) or near(b, (3000,3000)))
right = next(e for e, a, b in masters_now() if near(a, (6000,0)) or near(b, (6000,0)))
ew(right)
chk('AG: EW of one span deletes only the door on that span', alive(d1) and not alive(d2) and hole_x(1050, 1950))
fresh(); add(((0,0),(6000,0))); d1, _ = place((1500,0), 900); d2, _ = place((4500,0), 900)
add(((3000,0),(3000,3000)))
ew(next(e for e, a, b in masters_now() if near(a, (3000,3000)) or near(b, (3000,3000))))
A.ev('(wt:pend-begin)'); A.ev(f'(wt:axis-heal-local (list {P(3000,0)}))'); A.ev('(wt:pend-end)')
chk('AH: redundant node healed (two spans joined): both doors kept, both holes intact',
    master_set() == [mk((0,0),(6000,0))] and alive(d1) and alive(d2) and hole_x(1050, 1950) and hole_x(4050, 4950))

# ---------------------------------------------------------------- AD: EDW, CW, RH
fresh(); add(((0,0),(6000,0))); d1, l1 = place((2000,0), 900)
A.G[A.Sym('*T-E*')] = d1; A.ev('(ew:do-one (cw:read-xd *t-e*))')
chk('AD: EDW on a WallTool wall: door + label deleted, WallTool closes the wall, no jambs left',
    not alive(d1) and not alive(l1) and solid_x(1550, 2450) and len([1 for _, a, b in walls_now()]) == 4)
fresh(); add(((0,0),(6000,0))); d1, _ = place((2000,0), 900)
A.G[A.Sym('*T-E*')] = d1; A.ev(f'(akd:wt-regen (list (list {P3((2000,0))} {P3((1,0))} 900.0)))')
chk('re-regeneration with the door still registered keeps the hole (no duplicate jambs)', hole_x(1550, 2450))
fresh(); add(((0,0),(6000,0)))
A.G[A.Sym('*T-E*')] = face_line((3000,75)); A.ev("(hole:do *t-e* (list 3000.0 75.0 0.0) nil)")
A.ev(f'(akd:wt-regen (list (list {P3((3000,0))} {P3((1,0))} 900.0)))')   # RH on a WallTool wall
chk('RH on a WallTool wall: WallTool closes the unregistered hole, caps removed', solid_x(2550, 3450))

fresh(); add(((0,0),(6000,0)))
A.G[A.Sym('*T-SS*')] = [masters_now()[0][0], face_line((4000,75))]
chk('VX: the wall face is chosen over the WallTool centerline inside the wall', A.ev(f'(_vx-wall-ent *t-ss* {P3((4000,0))})') == face_line((4000,75)))
A.G[A.Sym('*T-E*')] = face_line((4000,75))
A.ev(f"(progn (setq *hole-force-ctr* {P3((4000,0))}) (hole:do *t-e* {P3((4000,0))} 't:post) (setq *hole-force-ctr* nil))")
chk('VX: re-placement is centred on the moved position, not the face middle', hole_x(3550, 4450) and solid_x(2550, 3450))

# ---------------------------------------------------------------- AF: reload / new session
fresh(); add(((0,0),(6000,0))); d1, _ = place((2000,0), 900)
A.load(WT); A.ev('(setq *wt:cfg* *wt:cfg-defaults* *wt:reg* nil)'); A.load(WD)
add(((4500,0),(4500,3000)))
chk('AF/AK: after reloading both files (new registry) the opening still survives a rebuild; one registration each',
    hole_x(1550, 2450) and reg_count() == [1, 1, 1])

# ---------------------------------------------------------------- AI / AJ: provider robustness
A.ev("(setq *t-saved* *wt:opening-fns*)")
A.ev("(defun t:junk () (list 1 \"x\" (cons 3 4) (list \"a\") (list (list 0 0) (list 0 0) 5.0) (list (list 0 0) (list 1 0) -5.0) (list (list 0 0) (list 1 0))))")
A.ev("(defun t:dotted () (cons (list (list 3000.0 0.0) (list 1.0 0.0) 900.0) 7))")
A.ev("(setq *wt:opening-fns* (list 't:missing 7 \"s\" 't:junk 't:dotted 't:junk))")
fresh(); add(((0,0),(6000,0))); add(((4500,0),(4500,3000)))
chk('AI/AJ: missing / non-symbol / malformed providers ignored; valid record of a dotted return still used', hole_x(2550, 3450))
A.ev("(setq *wt:opening-fns* (cons 7 (cons 't:junk 8)))")
fresh(); add(((0,0),(6000,0)))
chk('AJ: a dotted hook list is tolerated', solid_x(2550, 3450))
A.ev("(defun t:boom () (t:undefined-function))")
fresh(); add(((0,0),(6000,0))); before = lines_geo()
A.ev("(setq *wt:opening-fns* (list 't:boom))")
failed = False
try: add(((4500,0),(4500,3000)))
except A.LispError: failed = True
A.ev('(wt:error "provider failure")')
chk('provider error: command aborts, WallTool rolls the transaction back (drawing unchanged)', failed and lines_geo() == before)
A.ev("(setq *wt:opening-fns* *t-saved*)")

# ---------------------------------------------------------------- A / B / D: load combinations
out = sub(f"A.load({WT!r}); A.ev('(setq *wt:cfg* *wt:cfg-defaults*)')\n"
          "A.ev('(setq *wt:thk* 150.0 *wt:pos* \"CENTER\")'); A.ev('(wt:walls-add (list (list (list 0.0 0.0) (list 6000.0 0.0))))')\n"
          "print('LINES', len(A.db_lines('A-WALL')), 'WR', A.ev('c:WR') is None, 'FNS', A.ev('*wt:opening-fns*'))")
chk('A: WallTool alone: works, no hooks, no WR command', 'LINES 4 WR True FNS None' in out)
out = sub(f"A.load({WD!r})\n"
          "A.ev('(entmake (list (cons 0 \"LINE\") (cons 8 \"WALL\") (cons 10 (list 0.0 0.0 0.0)) (cons 11 (list 6000.0 0.0 0.0))))'); e = A.LAST[0]\n"
          "A.ev('(entmake (list (cons 0 \"LINE\") (cons 8 \"WALL\") (cons 10 (list 0.0 150.0 0.0)) (cons 11 (list 6000.0 150.0 0.0))))')\n"
          "A.G[A.Sym('*T-E*')] = e\n"
          "st = A.ev('(akd:wt-status (list 3000.0 75.0 0.0) (list 1.0 0.0 0.0) 900.0)')\n"
          "A.ev(\"(hole:do *t-e* (list 3000.0 0.0 0.0) nil)\")\n"
          "print('STATUS', st, 'PIECES', len(A.db_lines('WALL')), 'REGEN', A.ev('(akd:wt-regen nil)'), 'EW', A.ev('c:EW') is None)")
chk('B: WinDoor alone: status NONE, legacy hole cut splits both faces, no WallTool calls, no EW', 'STATUS NONE PIECES 4 REGEN None EW True' in out)
out = sub(f"A.load({WD!r}); A.load({WT!r}); A.ev('(setq *wt:cfg* *wt:cfg-defaults*)')\n"
          "A.ev('(setq *wt:thk* 150.0 *wt:pos* \"CENTER\")')\n"
          "A.ev('(wt:walls-add (list (list (list 0.0 0.0) (list 6000.0 0.0))))')\n"
          "A.ev('(entmake (list (cons 0 \"INSERT\") (cons 8 \"A-DOOR\") (cons 2 \"AKD-DS-900\") (cons 10 (list 3000.0 0.0 0.0))))'); A.G[A.Sym('*T-E*')] = A.LAST[0]\n"
          "A.ev('(_tagdoor *t-e* 900.0 \"S\" 1 1.0 (list 3000.0 0.0 0.0) (list 1.0 0.0 0.0) \"ADOOR-X\")')\n"
          "A.ev('(wt:walls-add (list (list (list 4500.0 0.0) (list 4500.0 3000.0))))')\n"
          "gap = not any(abs(a[1]-75) < 1e-6 and min(a[0],b[0]) < 3000 < max(a[0],b[0]) for _, a, b in A.db_lines('A-WALL'))\n"
          "print('GAP', gap, 'FNS', A.ev('*wt:opening-fns*'))")
chk('D: WinDoor then WallTool: hook survives WallTool load, opening kept', 'GAP True FNS [\'AKD:WT-OPENINGS\']' in out)

print(f'\n{fails} failure(s)')
sys.exit(1 if fails else 0)
