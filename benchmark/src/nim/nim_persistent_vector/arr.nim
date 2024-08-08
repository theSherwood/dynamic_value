# import std/[math, algorithm, strutils, strformat, sequtils, tables]
import std/[sequtils]
import ./persvector
import ../common

proc setup_seq_of_persvector_arrs*(sz, it, offset: int): seq[PersistentVector[int]] =
  var i_off, k: int
  var a: PersistentVector[int]
  for i in 0..<it:
    i_off = i + offset
    a = [i_off].toPersistentVector
    for j in 1..<sz:
      k = i_off + (j * 17)
      a = a.add(k)
    result.add(a)
template setup_seq_of_persvector_arrs*(sz, it: int): seq[PersistentVector[int]] = setup_seq_of_persvector_arrs(sz, it, 0)

proc persvector_arr_create*(tr: TaskResult, sz, n: int) =
  var arrs: seq[PersistentVector[int]] = @[]
  let Start = get_time()
  for i in 0..<n:
    arrs.add([i].toPersistentVector)
  tr.add(get_time() - Start)

proc persvector_arr_push*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_persvector_arrs(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    arrs[i] = arrs[i].add(i)
  tr.add(get_time() - Start)

proc persvector_arr_pop*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_persvector_arrs(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    arrs[i] = arrs[i].delete()
  tr.add(get_time() - Start)

proc persvector_arr_get_existing*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_persvector_arrs(sz, n)
  var vals: seq[int] = @[]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals.add(arrs[i][(arrs[i].len.float64 / 2.0).int])
  tr.add(get_time() - Start)

proc persvector_arr_set*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_persvector_arrs(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    arrs[i] = arrs[i].update((arrs[i].len.float64 / 2.0).int, -1)
  tr.add(get_time() - Start)

proc persvector_arr_iter*(tr: TaskResult, sz, n: int) =
  # setup
  var arrs = setup_seq_of_persvector_arrs(sz, n)
  var iters: seq[seq[int]] = @[]
  var vals: seq[int]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals = @[]
    for v in arrs[i].items: vals.add(v)
    iters.add(vals)
  tr.add(get_time() - Start)

# proc persvector_arr_equal_true*(tr: TaskResult, sz, n: int) =
#   # setup
#   var arrs = setup_seq_of_persvector_arrs(sz, n)
#   var copies = setup_seq_of_persvector_arrs(sz, n)
#   var bools: seq[bool]
#   # test
#   let Start = get_time()
#   for i in 0..<n:
#     bools.add(arrs[i] == copies[i])
#   tr.add(get_time() - Start)
#   doAssert bools.all(proc (b: bool): bool = b)

# proc persvector_arr_equal_false*(tr: TaskResult, sz, n: int) =
#   # setup
#   var arrs = setup_seq_of_persvector_arrs(sz, n)
#   var arrs2 = setup_seq_of_persvector_arrs(sz, n, 3)
#   var bools: seq[bool]
#   # test
#   let Start = get_time()
#   for i in 0..<n:
#     bools.add(arrs[i] == arrs2[i])
#   tr.add(get_time() - Start)
#   doAssert bools.all(proc (b: bool): bool = b.not)