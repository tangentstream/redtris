Red [title: "A learntris implementation in Red"]

mtx-line: func [][rejoin collect [loop 10 [keep ". "]]]
mtx-show: func [][loop 22 [print mtx-line]]

cmds: [
  q: [break]
  p: [mtx-show]]

forever[
  s: trim input
  if s <> "" [do cmds/(to-word s)]]
