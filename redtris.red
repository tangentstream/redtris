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

mtx-new: func[][mtx: dup 22 (wrap dup 10 ".")]
mtx-put: func[][foreach row mtx [print row]]
mtx-get: func[][
  mtx: collect[loop 22 [keep wrap split trim input " "]]]

cmds: [
  q: [break]
  c: [mtx-new]
  p: [mtx-put]
  g: [mtx-get]
  ?s: [print score]
  ?n: [print lines-cleared]]

mtx-new
forever[
  s: trim input
  if s <> "" [do cmds/(to-word s)]]
