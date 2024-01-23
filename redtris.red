Red [title: "A learntris implementation in Red"]

; wrap a block in another block
wrap: func[xs][append/only copy [] xs]


map: func[f xs][collect [foreach x copy xs [keep f x]]]
map2d: func[f xs][map func[row][wrap map :f row] copy xs]

upcase: func[s][uppercase copy s]

; duplicate x n times
dup: func[n x][collect[loop n [keep copy x]]]

; -- actual learntris stuff -----------------------------
;
; TODO: can we make this non-global without complete rewrite?
mtx: none
score: 0
lines-cleared: 0
done: false

grid-new: func[w h c][dup h wrap dup w c]
line-new: func[w c][wrap dup w c]
grid-put: func[g][foreach row g [print row]]

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

transpose: function[m][
  tmp: copy/deep m
  collect[loop length? tmp [
    keep wrap collect[
      foreach row tmp [keep take row]]]]]

cw: func[{m rotated clockwise} m][transpose reverse copy m]
cc: func[{m rotated counterclockwise} m][reverse transpose m]

blit: func[{copy sprite s onto grid g at x y} s g x y][
  cy: 0  tmp: copy/deep g
  foreach s-row s [
    cy: cy + 1  cx: 0
    g-row: tmp/(y + cy - 1)
    foreach c s-row [
      cx: cx + 1
      if c <> "." [g-row/(x + cx - 1): s/(cy)/(cx)]]]
  tmp]


step-game: func[][
  i: 0
  foreach row mtx [
    i: i + 1
    if not find row "." [
      score: score + 100
      lines-cleared: lines-cleared + 1
      mtx/(i): line-new 10 "."]]]




tx: 5 ty: 1

cmds: [
  q: [done: true]
  c: [mtx-new]
  p: [grid-put mtx]
  g: [mtx-get]
  s: [step-game]
  ^I: [tet: copy I]
  ^O: [tet: copy O tx: 5 ty: 1]
  ^T: [tet: copy T]
  ^S: [tet: copy S]
  ^Z: [tet: copy Z]
  ^J: [tet: copy J]
  ^L: [tet: copy L  tx: 4 ty: 1]
  ^P: [grid-put blit (map2d :upcase tet) mtx tx ty]
  cw: [tet: cw tet]
  cc: [tet: cc tet]
  nl: [print ""]
  t: [grid-put tet]
  ?s: [print score]
  ?n: [print lines-cleared]]

main: func[/locals s cmd][
  done: false
  mtx-new
  forever[
    if done [break]
    s: trim input
    if s <> "" [
      foreach cmd (split s " ") [
        if cmd == ")" [cmd: "cw"]
        if cmd == ";" [cmd: "nl"]
        if cmd == (uppercase copy cmd) [cmd: rejoin [#^ cmd]]
        do cmds/(to-word cmd)]]]]

main
