"""Source-backed AKDColumn/WallTool regression. Run: PYTHONDONTWRITEBYTECODE=1 python3 test/test_integration.py"""
import copy
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
WALL = ROOT.parent / 'AKD WallTool'
sys.path.insert(0, str(WALL / 'test'))
import alisp as A

A.OUTPUT = False
def entmake(data):
    A.entmake(data)
    en = A.LAST[0]
    A.DB[en].append(A.Pair(5, format(en.id, 'X')))
    return data
A.G[A.Sym('ENTMAKE')] = entmake
def entmakex(data):
    entmake(data)
    en = A.LAST[0]
    return en
A.G[A.Sym('ENTMAKEX')] = entmakex
A.G[A.Sym('ENTGET')] = lambda en, *apps: A.entget(en)
A.G[A.Sym('REGAPP')] = lambda name: name
A.G[A.Sym('ATOMS-FAMILY')] = lambda *args: list(A.G)
A.G[A.Sym('SSGETFIRST')] = lambda: [None, A.SSFIRST[0]]
nod = object()
dictionaries = {nod: {}}
A.G[A.Sym('NAMEDOBJDICT')] = lambda: nod
def dictsearch(dic, key):
    en = dictionaries.get(dic, {}).get(key)
    return [A.Pair(-1,en)] + A.DB[en] if en and A.entget(en) else None
def dictadd(dic, key, en):
    dictionaries.setdefault(dic, {})[key] = en
    if any(A.car(x) == 0 and A.cdr(x) == 'DICTIONARY' for x in A.DB[en]):
        dictionaries[en] = {}
    return en
A.G[A.Sym('DICTSEARCH')] = dictsearch
A.G[A.Sym('DICTADD')] = dictadd
A.G[A.Sym('DICTREMOVE')] = lambda dic,key: dictionaries.get(dic, {}).pop(key, None)
A.G[A.Sym('HANDENT')] = lambda h: next((e for e in A.DB if format(e.id,'X') == h), None)
A.load(str(WALL / 'WallTool.lsp'))
A.load(str(ROOT / 'AKDColumn.lsp'))
A.ev('(wt:cfg-load)')

undo = []
commands = []
def command_s(*args):
    commands.append(args)
    op = str(args[0]).upper() if args else ''
    if op == '_.UNDO' and len(args) > 1 and str(args[1]).upper().startswith('_BE'):
        undo.append(({e:copy.deepcopy(d) for e,d in A.DB.items()}, set(A.DELETED)))
    if op == '_.U' and undo:
        db, deleted = undo.pop(); A.DB.clear(); A.DB.update(db)
        A.DELETED.clear(); A.DELETED.update(deleted)
    A.SSFIRST[0] = None
    return A.T
A.G[A.Sym('COMMAND-S')] = command_s

def fresh(*walls):
    A.db_reset(); undo.clear(); commands.clear()
    dictionaries.clear(); dictionaries[nod] = {}
    A.ev('(setq *wt:reg* nil *wt:pending* nil *wt:thk* 150.0 *wt:pos* "CENTER")')
    if walls:
        A.G[A.Sym('*T-SEGS*')] = [[list(map(float, a)), list(map(float, b))] for a, b in walls]
        A.ev('(wt:walls-add *t-segs*)')

def lines(layer):
    return A.db_lines(layer)

def pairs(layer):
    return {tuple(sorted((tuple(round(v, 3) for v in a), tuple(round(v, 3) for v in b))))
            for _, a, b in lines(layer)}

def rect(bl=(-150,-150), tr=(150,150)):
    expr = f'(akc:wall-cut-plan (list {bl[0]}.0 {bl[1]}.0) (list {tr[0]}.0 {tr[1]}.0))'
    plan, err = A.ev(expr)
    assert not err, err
    A.G[A.Sym('*T-PLAN*')] = plan
    originals = A.ev('(akc:original-records *t-plan*)')
    stubs = A.ev('(akc:cut-masters *t-plan*)')
    p = [list(map(float, x)) for x in (bl, (tr[0],bl[1]), tr, (bl[0],tr[1]))]
    A.G[A.Sym('*T-PTS*')] = p
    poly = A.ev('(akc:mkpline *t-pts*)')
    A.G[A.Sym('*T-POLY*')] = poly
    A.G[A.Sym('*T-ORIGINALS*')] = originals
    A.G[A.Sym('*T-STUBS*')] = stubs
    A.ev('(akc:save-record *t-poly* *t-originals* *t-stubs*)')
    return poly

def group(poly):
    return ['AKCOL1', poly]

def restore(poly):
    A.G[A.Sym('*T-GRP*')] = group(poly)
    return A.ev('(akc:restore-plan *t-grp*)')

def ew(*enames, poly=None):
    grp = group(poly) if poly else None
    A.G[A.Sym('AKC:GROUP-OF')] = lambda en: grp if grp and en == poly and A.entget(poly) else None
    A.SSFIRST[0] = list(enames)
    A.ev('(c:EW)')

def check(name, condition):
    assert condition, name
    print('PASS', name)

# One crossing wall, caps, reload reconstruction, and geometric removal.
fresh(((-1000,0),(1000,0)))
original = pairs('X-AXIS'), pairs('A-WALL')
p = rect()
check('rectangular placement leaves two masters and two gap caps',
      len(lines('X-AXIS')) == 2 and len(lines('A-WALL')) == 8 and
      ((-150.0,-75.0),(-150.0,75.0)) in pairs('A-WALL') and
      ((150.0,-75.0),(150.0,75.0)) in pairs('A-WALL'))
A.ev('(setq *wt:reg* nil)')
check('reload reconstructs the two stubs', restore(p)[0][0] == 'RECORD')
A.G[A.Sym('*T-R*')] = restore(p)[0]
A.ev('(akc:restore-record-masters *t-r*)')
check('removal rejoins and rebuilds without duplicate masters or caps',
      (pairs('X-AXIS'),pairs('A-WALL')) == original)

# A nearby WallTool operation rebuilds locally without closing the gap.
fresh(((-1000,0),(1000,0)))
p = rect()
A.ev('(wt:walls-add (list (list (list -600.0 0.0) (list -600.0 500.0))))')
check('WallTool rebuild preserves the column gap',
      not any(a[0] < -150 and b[0] > 150 for _,a,b in lines('X-AXIS')) and
      ((150.0,-75.0),(150.0,75.0)) in pairs('A-WALL'))

# Wall-only EW uses WallTool's source resolver and one undo group.
fresh(((-1000,0),(1000,0)))
wall_master = lines('X-AXIS')[0][0]
original = pairs('X-AXIS'), pairs('A-WALL')
ew(wall_master)
check('EW on a wall removes its master and faces', not lines('X-AXIS') and not lines('A-WALL'))
A.ev('(command-s "_.U")')
check('Undo restores wall EW', (pairs('X-AXIS'),pairs('A-WALL')) == original)

# Column-only EW restores its gap and deletes its own outline.
fresh(((-1000,0),(1000,0)))
p = rect(); ew(p, poly=p)
check('EW on a column rejoins the wall',
      pairs('X-AXIS') == {((-1000.0,0.0),(1000.0,0.0))} and A.entget(p) is None)
old = pairs('X-AXIS'), pairs('A-WALL')
ew(p, poly=p)
check('repeated column removal makes no change', (pairs('X-AXIS'),pairs('A-WALL')) == old)

# Mixed PickFirst resolves everything before a single mutation.
fresh(((-1000,0),(1000,0)), ((2000,0),(3000,0)))
p = rect(); other = next(e for e,a,b in lines('X-AXIS') if a[0] >= 2000)
ew(p, other, poly=p)
check('mixed EW restores column gap and erases selected independent wall',
      pairs('X-AXIS') == {((-1000.0,0.0),(1000.0,0.0))} and A.entget(p) is None)

# Ambiguous restoration leaves both the column and every master unchanged.
fresh(((-1000,0),(1000,0)))
p = rect(); stub = next(e for e,a,b in lines('X-AXIS') if a[0] > 0)
A.entdel(stub); before = pairs('X-AXIS'),pairs('A-WALL')
ew(p, poly=p)
check('missing stub refuses erase without changes', A.entget(p) is not None and
      (pairs('X-AXIS'),pairs('A-WALL')) == before)

# Multiple independent crossings, split spans, one-ended wall, circle guard.
fresh(((-1000,-300),(1000,-300)), ((-1000,300),(1000,300)))
p = rect((-400,-500),(400,500))
check('independent crossing walls produce four capped stubs', len(lines('X-AXIS')) == 4)
fresh(((-1000,0),(1000,0)))
A.G[A.Sym('*T-W*')] = A.ev('(wt:wall-from-entity (car (car (car (wt:net-scan)))) (wt:net-scan))')
A.ev('(wt:pend-begin)')
A.ev('(wt:master-split-at *t-w* (list (list 0.0 0.0)))')
A.ev('(setq *wt:pending* nil)')
check('split fixture has two touching masters', len(lines('X-AXIS')) == 2)
p = rect()
check('existing split master spans cut without duplicates', len(lines('X-AXIS')) == 2 and restore(p)[0][0] == 'RECORD')
fresh(((-1000,0),(0,0)))
p = rect()
check('wall ending inside leaves one capped stub with restorable provenance',
      len(lines('X-AXIS')) == 1 and restore(p)[1] is None)
check('circle intersecting WallTool wall is refused',
      A.ev('(akc:circle-hits-wall (list -500.0 0.0) 150.0)') is not None)

# Actual placement assembly keeps each pair of AWALL tags in its own group.
fresh()
created_groups = []
A.G[A.Sym('AKC:PICK-AXIS')] = lambda bl,tr: [(bl[0]+tr[0])/2, (bl[1]+tr[1])/2, 0.0]
A.G[A.Sym('AKC:PICK-NUM')] = lambda: 1
A.G[A.Sym('AKC:LABEL')] = lambda *args: []
A.G[A.Sym('AKC:UNIQNAME')] = lambda prefix: f'{prefix}{len(created_groups)+1}'
A.G[A.Sym('AKC:MKGROUP')] = lambda name, ents: (created_groups.append((name, ents)), name)[1]
old_command = A.G[A.Sym('COMMAND-S')]
def hatch_command(*args):
    if args and str(args[0]).upper() == '_.-HATCH':
        A.entmake([A.Pair(0,'HATCH'), A.Pair(8,'S-COLUMN')])
    return old_command(*args)
A.G[A.Sym('COMMAND-S')] = hatch_command
A.ev('(akc:place-rect (list 0.0 0.0) 2700.0 0.0)')
A.ev('(akc:place-rect (list 1000.0 0.0) 2700.0 0.0)')
check('two placements group exactly their own projection tags',
      len(created_groups) == 2 and all(sum(any(A.car(x) == 0 and A.cdr(x) == 'POINT' for x in A.entget(e)) for e in ents) == 2
          for _,ents in created_groups))
first_tags = [e for e in created_groups[0][1] if any(A.car(x) == 0 and A.cdr(x) == 'POINT' for x in A.entget(e))]
second_tags = [e for e in created_groups[1][1] if any(A.car(x) == 0 and A.cdr(x) == 'POINT' for x in A.entget(e))]
A.G[A.Sym('*T-GRP*')] = ['AKCOL1'] + created_groups[0][1]
A.ev('(akc:erase-checked *t-grp* nil)')
check('removing one column leaves the neighboring projection tags',
      all(A.entget(e) is None for e in first_tags) and all(A.entget(e) for e in second_tags))
before = ({e.id:repr(d) for e,d in A.DB.items()}, {e.id for e in A.DELETED})
A.ev('(c:CCW)')
check('CCW refuses resize without touching a drawing',
      ({e.id:repr(d) for e,d in A.DB.items()}, {e.id for e in A.DELETED}) == before)

# Full command placement on a WallTool wall, then one native Undo step.
fresh(((-1000,0),(1000,0)))
created_groups.clear()
original = pairs('X-AXIS'), pairs('A-WALL')
A.ev('(akc:place-rect (list 0.0 0.0) 2700.0 0.0)')
check('full placement cuts masters and groups two tags',
      len(lines('X-AXIS')) == 2 and len(created_groups) == 1 and
      sum(any(A.car(x) == 0 and A.cdr(x) == 'POINT' for x in A.entget(e))
          for e in created_groups[0][1]) == 2)
A.ev('(command-s "_.U")')
check('Undo restores drawing after full placement',
      (pairs('X-AXIS'), pairs('A-WALL')) == original)

fresh(((-1000,0),(1000,0)), ((0,0),(0,700)))
created_groups.clear()
original = pairs('X-AXIS'), pairs('A-WALL')
A.ev('(akc:place-rect (list 0.0 0.0) 2700.0 0.0)')
check('full AC placement cuts T junction and writes persistent record',
      len(lines('X-AXIS')) == 3 and len(created_groups) == 1 and
      len(dictionaries.get(dictionaries[nod].get('AKCOL_MASTERS'), {})) == 1)
A.ev('(command-s "_.U")')
check('Undo restores original T after full placement',
      (pairs('X-AXIS'),pairs('A-WALL')) == original)

# An ambiguous column in a mixed selection blocks the independent wall too.
fresh(((-1000,0),(1000,0)), ((2000,0),(3000,0)))
p = rect(); other = next(e for e,a,b in lines('X-AXIS') if a[0] >= 2000)
stub = next(e for e,a,b in lines('X-AXIS') if a[0] > 0 and a[0] < 1000)
A.entdel(stub)
before = pairs('X-AXIS'), pairs('A-WALL')
ew(p, other, poly=p)
check('ambiguous mixed EW leaves column and wall selection unchanged',
      A.entget(p) is not None and (pairs('X-AXIS'),pairs('A-WALL')) == before)

# Junction planning is read-only; all touching masters join one cut plan.
fresh(((-1000,0),(1000,0)), ((0,0),(0,700)))
before = pairs('X-AXIS'), pairs('A-WALL')
plan = A.ev('(akc:wall-cut-plan (list -150.0 -150.0) (list 150.0 150.0))')
check('junction is included in one preflight plan without changes',
      plan[1] is None and len(plan[0]) >= 2 and (pairs('X-AXIS'),pairs('A-WALL')) == before)

def junction(name, walls, arms):
    fresh(*walls)
    original = pairs('X-AXIS'), pairs('A-WALL')
    p = rect()
    check(name + ' cuts every master arm and caps free ends',
          len(lines('X-AXIS')) == arms and
          all(not (-150 < a[0] < 150 and -150 < a[1] < 150)
                  and not (-150 < b[0] < 150 and -150 < b[1] < 150)
                  for _,a,b in lines('X-AXIS')) and
          len([q for q in pairs('A-WALL') if
               (abs(q[0][0]) == 150 and abs(q[1][0]) == 150) or
               (abs(q[0][1]) == 150 and abs(q[1][1]) == 150)]) >= arms)
    A.ev('(setq *wt:reg* nil)')
    ew(p, poly=p)
    check(name + ' EW restores original junction after registry reset',
          (pairs('X-AXIS'), pairs('A-WALL')) == original and A.entget(p) is None)

junction('T', (((-1000,0),(1000,0)), ((0,0),(0,700))), 3)
junction('L', (((-1000,0),(0,0)), ((0,0),(0,700))), 2)
junction('X', (((-1000,0),(1000,0)), ((0,-700),(0,700))), 4)
junction('reversed T', (((1000,0),(-1000,0)), ((0,700),(0,0))), 3)
junction('branch ending inside', (((-1000,0),(1000,0)), ((0,700),(0,40))), 3)
junction('wholly covered arm', (((-1000,0),(1000,0)), ((0,0),(0,100))), 2)

fresh(((-1000,0),(1000,0)), ((0,0),(0,700)))
p = rect()
A.ev('(wt:walls-add (list (list (list 0.0 -500.0) (list 0.0 500.0))))')
before = pairs('X-AXIS'), pairs('A-WALL')
ew(p, poly=p)
check('changed T gap refuses EW without touching column or walls',
      A.entget(p) is not None and (pairs('X-AXIS'),pairs('A-WALL')) == before)

fresh()
p = rect()
check('rectangle away from walls has a valid empty master record',
      restore(p)[1] is None)
ew(p, poly=p)
check('EW removes a no-wall rectangle', A.entget(p) is None)

fresh(((-1000,0),(1000,0)), ((0,0),(0,700)))
p = rect()
stub = next(e for e,a,b in lines('X-AXIS') if a[1] == 150)
A.entdel(stub)
A.G[A.Sym('*T-COPY*')] = A.ev('(wt:mk-line (list 0.0 150.0) (list 0.0 700.0) (wt:cfg "AXIS_LAYER"))')
before = pairs('X-AXIS'), pairs('A-WALL')
ew(p, poly=p)
check('replaced junction stub refuses despite matching geometry',
      A.entget(p) is not None and (pairs('X-AXIS'),pairs('A-WALL')) == before)

# EW must carry a face's pick location into WallTool's resolver.
fresh(((-1000,0),(1000,0)))
face = next(e for e,a,b in lines('A-WALL') if abs(a[1]-75) < 0.01 and abs(b[1]-75) < 0.01)
master = lines('X-AXIS')[0][0]
A.PICKPTS[face] = (0.0,75.0)
ew(face, master)
A.PICKPTS.clear()
check('EW face click and master PickFirst deduplicate through WallTool',
      not lines('X-AXIS') and not lines('A-WALL'))

# A master with no reconstructable faces is refused before touching any entity.
fresh()
A.ev('(wt:mk-line (list -1000.0 0.0) (list 1000.0 0.0) (wt:cfg "AXIS_LAYER"))')
before = pairs('X-AXIS'), pairs('A-WALL')
plan = A.ev('(akc:wall-cut-plan (list -150.0 -150.0) (list 150.0 150.0))')
check('ambiguous master ownership refuses placement without changes',
      plan[1] is not None and (pairs('X-AXIS'),pairs('A-WALL')) == before)

# Circles away from WallTool walls still assemble a column and own tags.
fresh(); created_groups.clear()
A.ev('(akc:place-circ (list 0.0 0.0 0.0) 2700.0 0.0)')
check('non-wall circular placement remains available',
      len(created_groups) == 1 and any(A.car(x) == 0 and A.cdr(x) == 'CIRCLE'
          for e in created_groups[0][1] for x in A.entget(e)))

# Integration follows configured layer names instead of literal defaults.
cfg = A.G[A.Sym('*WT:CFG*')]
A.ev('(setq *wt:cfg* (subst (cons "AXIS_LAYER" "ALT-AX") (assoc "AXIS_LAYER" *wt:cfg*) *wt:cfg*))')
A.ev('(setq *wt:cfg* (subst (cons "WALL_LAYER" "ALT-WALL") (assoc "WALL_LAYER" *wt:cfg*) *wt:cfg*))')
fresh(((-1000,0),(1000,0)))
A.G[A.Sym('*T-ALT-PLAN*')] = A.ev('(car (akc:wall-cut-plan (list -150.0 -150.0) (list 150.0 150.0)))')
A.ev('(akc:cut-masters *t-alt-plan*)')
check('configured master and face layers are used',
      len(lines('ALT-AX')) == 2 and len(lines('ALT-WALL')) == 8)
A.G[A.Sym('*WT:CFG*')] = cfg

fresh(((-1000,0),(1000,0)))
p = rect()
gap = pairs('X-AXIS'), pairs('A-WALL')
ew(p, poly=p)
A.ev('(command-s "_.U")')
check('Undo restores column-removal wall gap in one group',
      (pairs('X-AXIS'),pairs('A-WALL')) == gap)

fresh(((-150.0005,0),(1000,0)))
before = pairs('X-AXIS'), pairs('A-WALL')
plan = A.ev('(akc:wall-cut-plan (list -150.0 -150.0) (list 150.0 150.0))')
check('near-zero remnant refuses without mutation',
      plan[1] is not None and (pairs('X-AXIS'),pairs('A-WALL')) == before)

fresh(((-1000,0),(1000,0)))
p = rect()
A.ev('(wt:walls-add (list (list (list 0.0 -500.0) (list 0.0 500.0))))')
before = pairs('X-AXIS'), pairs('A-WALL')
check('new wall across gap blocks restoration without changes',
      restore(p)[1] is not None and (pairs('X-AXIS'),pairs('A-WALL')) == before)
print('All integration checks passed.')
