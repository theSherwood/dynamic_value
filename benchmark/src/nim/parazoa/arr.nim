# import std/[math, algorithm, strutils, strformat, sequtils, tables]
import std/[sequtils]
import ./parazoa
import ../common

proc setup_seq_of_parazoa_arrs*(sz, it, offset: int): seq[Vec[int]] =
  var i_off, k: int
  var a: Vec[int]
  for i in 0..<it:
    i_off = i + offset
    a = [i_off].toVec
    for j in 1..<sz:
      k = i_off + (j * 17)
      a = a.add(k)
    result.add(a)
template setup_seq_of_parazoa_arrs*(sz, it: int): seq[Vec[int]] = setup_seq_of_parazoa_arrs(sz, it, 0)

proc parazoa_arr_create*(tr: TaskResult, sz, n: int) =
  var arrs: seq[Vec[int]] = @[]
  let Start = get_time()
  for i in 0..<n:
    arrs.add([i].toVec)
  tr.add(get_time() - Start)

proc parazoa_arr_push*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_parazoa_arrs(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    arrs[i] = arrs[i].add(i)
  tr.add(get_time() - Start)

proc parazoa_arr_pop*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_parazoa_arrs(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    arrs[i] = arrs[i].setLen(arrs[i].len - 1)
  tr.add(get_time() - Start)

proc parazoa_arr_get_existing*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_parazoa_arrs(sz, n)
  var vals: seq[int] = @[]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals.add(arrs[i].get((arrs[i].len.float64 / 2.0).int))
  tr.add(get_time() - Start)

proc parazoa_arr_get_non_existing*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_parazoa_arrs(sz, n)
  var vals: seq[int] = @[]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals.add(arrs[i].getOrDefault(arrs[i].len * 2, 0))
  tr.add(get_time() - Start)

proc parazoa_arr_set*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_parazoa_arrs(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    arrs[i] = arrs[i].add((arrs[i].len.float64 / 2.0).int, -1)
  tr.add(get_time() - Start)

proc parazoa_arr_iter*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_parazoa_arrs(sz, n)
  var iters: seq[seq[int]] = @[]
  var vals: seq[int]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals = @[]
    for v in arrs[i].items: vals.add(v)
    iters.add(vals)
  tr.add(get_time() - Start)

proc parazoa_arr_equal_true*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_parazoa_arrs(sz, n)
  var copies = setup_seq_of_parazoa_arrs(sz, n)
  var bools: seq[bool]
  # test
  let Start = get_time()
  for i in 0..<n:
    bools.add(arrs[i] == copies[i])
  tr.add(get_time() - Start)
  doAssert bools.all(proc (b: bool): bool = b)

proc parazoa_arr_equal_false*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_parazoa_arrs(sz, n)
  var arrs2 = setup_seq_of_parazoa_arrs(sz, n, 3)
  var bools: seq[bool]
  # test
  let Start = get_time()
  for i in 0..<n:
    bools.add(arrs[i] == arrs2[i])
  tr.add(get_time() - Start)
  doAssert bools.all(proc (b: bool): bool = b.not)