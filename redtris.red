Red [title: "A learntris implementation in Red"]

; wrap a block in another block
wrap: func[xs][append/only copy [] xs]

; duplicate x n times
dup: func[n x][collect[loop n [keep copy x]]]

; remove and add spaces to a row
; rows are just strings of characters
parse-row: func[s] [rejoin split trim s " "]
print-row: func[s][
  trim rejoin collect[foreach c s [keep c keep " "]]]

mtx: dup 22 (wrap dup 10 ".")
mtx-put: func[][foreach row mtx [print row]]
mtx-get: func[][
  mtx: collect[loop 22 [keep wrap split trim input " "]]]

cmds: [
  q: [break]
  p: [mtx-put]
  g: [mtx-get]]

forever[
  s: trim input
  if s <> "" [do cmds/(to-word s)]]
