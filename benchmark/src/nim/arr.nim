# import std/[math, algorithm, strutils, strformat, sequtils, tables]
import std/[sequtils]
import ../../../src/[values]
import ./common

proc setup_seq_of_arrs*(sz, it, offset: int): seq[ImValue] =
  var i_off, k: int
  var
    a: ImArray
    s: seq[ImValue]
  for i in 0..<it:
    i_off = i + offset
    s = newSeq[ImValue](sz)
    for j in 0..<sz:
      s[j] = (i_off + (j * 17)).v
    a = Arr(s)
    result.add(a.v)
template setup_seq_of_arrs*(sz, it: int): seq[ImValue] = setup_seq_of_arrs(sz, it, 0)

proc arr_create*(tr: TaskResult, sz, n: int) =
  var arrs: seq[ImValue] = @[]
  let Start = get_time()
  for i in 0..<n:
    arrs.add(V [i])
  tr.add(get_time() - Start)

proc arr_push*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_arrs(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    arrs[i] = arrs[i].push(i)
  tr.add(get_time() - Start)

proc arr_pop*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_arrs(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    arrs[i] = arrs[i].pop()[1]
  tr.add(get_time() - Start)

proc arr_slice*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_arrs(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    arrs[i] = arrs[i].slice(i, (arrs[i].size.as_f64 / 2.0).int)
  tr.add(get_time() - Start)

proc arr_get_existing*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_arrs(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    arrs[i] = arrs[i].get((arrs[i].size.as_f64 / 2.0).int)
  tr.add(get_time() - Start)

proc arr_get_non_existing*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_arrs(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    arrs[i] = arrs[i].get((arrs[i].size.as_f64 * 2.0).int)
  tr.add(get_time() - Start)

proc arr_set*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_arrs(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    arrs[i] = arrs[i].set((arrs[i].size.as_f64 / 2.0).int, -1.0)
  tr.add(get_time() - Start)

proc arr_iter*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_arrs(sz, n)
  var iters: seq[seq[ImValue]] = @[]
  var vals: seq[ImValue]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals = @[]
    for v in arrs[i].values: vals.add(v)
    iters.add(vals)
  tr.add(get_time() - Start)

proc arr_equal_true*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_arrs(sz, n)
  var copies = setup_seq_of_arrs(sz, n)
  var bools: seq[bool]
  # test
  let Start = get_time()
  for i in 0..<n:
    bools.add(arrs[i] == copies[i])
  tr.add(get_time() - Start)
  doAssert bools.all(proc (b: bool): bool = b)

proc arr_equal_false*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_arrs(sz, n)
  var arrs2 = setup_seq_of_arrs(sz, n, 3)
  var bools: seq[bool]
  # test
  let Start = get_time()
  for i in 0..<n:
    bools.add(arrs[i] == arrs2[i])
  tr.add(get_time() - Start)
  doAssert bools.all(proc (b: bool): bool = b.not)
