Red [title: "A learntris implementation in Red"]

; wrap a block in another block
wrap: func[xs][append/only copy [] xs]

ins: func['op xs][op: get op r: xs/1 foreach x next xs [r: r op x] r]
fold: func[f r0 xs][r: r0 foreach x xs [r: f r x] r]

foreach-ix: func['xsym 'isym xs block][
  set isym 0
  foreach xval xs [
    set xsym xval
    set isym ((get isym) + 1)
    do block]]

zip: func[xs ys][collect[foreach x xs [keep wrap (x ys/(index? xs))]]]
zipop: func['op xs ys][
  op: get op
  collect[foreach-ix x i xs [keep x op ys/(i)]]]

zipwith: func[f xs ys][
  collect[foreach-ix x i xs [keep f x ys/(i)]]]

slice: func[start len xs][
  take/part skip (copy xs) start - 1 len]

map: func[f xs][collect [foreach x copy xs [keep f x]]]
map2d: func[f xs][map func[row][wrap map :f row] copy xs]

upcase: func[s][uppercase copy s]
rev: func[s][reverse copy s]

; duplicate x n times
dup: func[n x][collect[loop n [keep copy x]]]

transpose: function[m][
  w: length? m/1
  tmp: copy/deep m
  collect[loop w [
    keep wrap collect[
      foreach row tmp [keep take row]]]]]

; -- actual learntris stuff -----------------------------
;
; TODO: can we make this non-global without complete rewrite?
mtx: none
score: 0
lines-cleared: 0
done: false

grid-new: func[w h c][dup h wrap dup w c]
line-new: func[w c][wrap dup w c]
p: put-grid: func[g][foreach row g [print row]]

mtx-new: func[][mtx: grid-new 10 22 "."]
get-row: func[s][wrap split trim s " "]
mtx-get: func[][mtx: collect[loop 22 [keep get-row input]]]

mtx: mtx-new
tet: grid-new 4 4 "."

new-tet: func[xs][collect[foreach x xs [keep get-row x]]]
I: new-tet [". . . ." "c c c c" ". . . ." ". . . ."]
O: new-tet ["y y" "y y"]
T: new-tet [". m ." "m m m" ". . ."]
S: new-tet [". g g" "g g ." ". . ."]
Z: new-tet ["r r ." ". r r" ". . ."]
J: new-tet ["b . ." "b b b" ". . ."]
L: new-tet [". . o" "o o o" ". . ."]

cw: func[{m rotated clockwise} m][transpose reverse copy m]
cc: func[{m rotated counterclockwise} m][reverse transpose m]

blit: func[{copy sprite s onto grid g at x y} s g x y][
  cy: 0  tmp: copy/deep g
  foreach s-row s [
    cy: cy + 1  gy: (y + cy - 1) cx: 0
    if gy > length? tmp [break]
    g-row: tmp/(gy)
    foreach c s-row [
      cx: cx + 1  gx: (x + cx - 1)
      if gx > length? g-row [break]
      if c <> "." [g-row/(gx): s/(cy)/(cx)]]]
  tmp]

step-game: func[][
  i: 0
  foreach row mtx [
    i: i + 1
    if not find row "." [
      score: score + 100
      lines-cleared: lines-cleared + 1
      mtx/(i): line-new 10 "."]]]

game: func[][blit (map2d :upcase tet) mtx tx ty]
pg: put-game: func[][ put-grid game]

rt-extent: func[{right edge of sprite s} s][
  res: 0
  foreach row s [
    loop (i: length? row) [
      if i == res [break]
      if row/(i) <> "." [res: i break]
      i: i - 1]]
  res]


row-last: func[{last non-empty cell in row r} r][
  res: length? r
  foreach c rev r [
    if c <> "." [return res]
    res: res - 1]
  res]

bottom-extent: func[{top edge of sprite s} s][
  res: length? s
  foreach row rev s [
    foreach c row [
      if c <> "." [return res ]]
    res: res - 1]
  res]

bottom-edge:  func[{bottom edge of sprite s} s][
  map :row-last transpose s]

dropzone: func[{slice of matrix including and below the current tetramino}][
  tw: length? tet
  map func[x][wrap slice tx tw x] skip mtx ty - 1]


inf: 99999999999 ; TODO: better "infinity"?
drop: func[][
  ldz: length? dz: dropzone
   ; dropzone heights (distance to next non-empty cell for each column)
  dzh: map func[x][ldz - x] bottom-edge rev dz
  bet: bottom-edge tet
  fold :min inf zipop - dzh bet
]

;
;


tx: 1 ty: 1

dn: func[][ty: min (23 - bottom-extent tet) ty + 1]
stamp: func[][mtx: blit tet mtx tx ty]

cmds: [
  q: [done: true]
  c: [mtx-new]
  p: [put-grid mtx]
  g: [mtx-get]
  s: [step-game]
  lf: [tx: max 1  tx - 1]
  rt: [tx: min (11 - rt-extent tet)  tx + 1]
  v: [dn]
  ^I: [tet: copy I tx: 4 ty: 1]
  ^O: [tet: copy O tx: 5 ty: 1]
  ^T: [tet: copy T tx: 4 ty: 1]
  ^S: [tet: copy S tx: 4 ty: 1]
  ^Z: [tet: copy Z tx: 4 ty: 1]
  ^J: [tet: copy J tx: 4 ty: 1]
  ^L: [tet: copy L tx: 4 ty: 1]
  ^V: [ty: ty + drop stamp]
  ^P: [put-game]
  cw: [tet: cw tet]
  cc: [tet: cc tet]
  nl: [print ""]
  t: [put-grid tet]
  ?s: [print score]
  ?n: [print lines-cleared]]

run: func[{run string s through game interpreter} s][
  state: 'normal
  foreach c s [
    cmd: rejoin [c]
    if state == 'inspect [
      state: 'normal  cmd: rejoin ["?" c]]
    if c == #" " [continue]
    if c == #"?" [state: 'inspect]
    if c == #")" [cmd: "cw"]
    if c == #"(" [cmd: "cc"]
    if c == #";" [cmd: "nl"]
    if c == #"<" [cmd: "lf"]
    if c == #">" [cmd: "rt"]
    if cmd == (upcase cmd) [cmd: rejoin [#^ c]]
    if state == 'normal [do cmds/(to-word cmd)]]]

main: func[][
  done: false
  mtx-new
  forever[
    if done [break]
    run trim input]]

rl: func [][do load %redtris.red]
main
