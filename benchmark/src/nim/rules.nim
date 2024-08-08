import ./common

template send_more_money_assertion() {.dirty.} =
  if s * 1000 + e * 100 + n * 10 + d + m * 1000 + o * 100 + r * 10 + e ==
     m * 10000 + o * 1000 + n * 100 + e * 10 + y:
    doAssert (s, e, n, d, m, o, r, y) == (9, 5, 6, 7, 1, 0, 8, 2)
    return
proc send_more_money_imperative_inner() =
  for s in 0..9:
    if s != 0:
      for e in 0..9:
        if e != s:
          for n in 0..9:
            if n notin [s, e]:
              for d in 0..9:
                if d notin [s, e, n]:
                  for m in 0..9:
                    if m notin [s, e, n, d, 0]:
                      for o in 0..9:
                        if o notin [s, e, n, d, m]:
                          for r in 0..9:
                            if r notin [s, e, n, d, m, o]:
                              for y in 0..9:
                                if y notin [s, e, n, d, m, o, r]:
                                  send_more_money_assertion()
proc send_more_money_imperative*(tr: TaskResult, sz, n: int) =
  let Start = get_time()
  for i in 0..<n:
    send_more_money_imperative_inner()
  tr.add(get_time() - Start)
