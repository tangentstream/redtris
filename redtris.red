Red [title: "A learntris implementation in Red"]

; wrap a block in another block
wrap: func[xs][append/only copy [] xs]

; duplicate x n times
dup: func[n x][collect[loop n [keep copy x]]]

; -- actual learntris stuff -----------------------------
;
; TODO: can we make this non-global without complete rewrite?
mtx: none
score: 0
lines-cleared: 0

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

step-game: func[][
  i: 0
  foreach row mtx [
    i: i + 1
    if not find row "." [
      score: score + 100
      lines-cleared: lines-cleared + 1
      mtx/(i): line-new 10 "."]]]

cmds: [
  q: [break]
  c: [mtx-new]
  p: [grid-put mtx]
  g: [mtx-get]
  s: [step-game]
  I: [tet: copy I]
  t: [grid-put tet]
  ?s: [print score]
  ?n: [print lines-cleared]]

mtx-new
forever[
  s: trim input
  if s <> "" [do cmds/(to-word s)]]
