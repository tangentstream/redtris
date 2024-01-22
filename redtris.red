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

row-new: func[][wrap dup 10 "."]
mtx-new: func[][mtx: dup 22 row-new]
mtx-put: func[][foreach row mtx [print row]]
get-row: func[s][wrap split trim s " "]
mtx-get: func[][mtx: collect[loop 22 [keep get-row input]]]

step-game: func[][
  i: 0
  foreach row mtx [
    i: i + 1
    if not find row "." [
      score: score + 100
      lines-cleared: lines-cleared + 1
      mtx/(i): row-new]]]

cmds: [
  q: [break]
  c: [mtx-new]
  p: [mtx-put]
  g: [mtx-get]
  s: [step-game]
  ?s: [print score]
  ?n: [print lines-cleared]]

mtx-new
forever[
  s: trim input
  if s <> "" [do cmds/(to-word s)]]
