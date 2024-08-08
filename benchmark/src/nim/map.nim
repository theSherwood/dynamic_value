import std/[sequtils]
import ../../../src/[values]
import ./common

proc setup_seq_of_maps*(sz, it, offset: int): seq[ImValue] =
  var i_off, k: int
  var m: ImMap
  for i in 0..<it:
    i_off = i + offset
    m = Map {i_off: i_off}
    for j in 1..<sz:
      k = i_off + (j * 17)
      m = m.set(k, k)
    result.add(m.v)
template setup_seq_of_maps*(sz, it: int): seq[ImValue] = setup_seq_of_maps(sz, it, 0)

proc force_copy*(m: ImMap): ImMap =
  return m.set(-1, -1).del(-1)
proc copy_maps*(maps: seq[ImValue]): seq[ImValue] =
  return maps.map(proc (m: ImValue): ImValue = m.as_map.force_copy.v)

proc map_create*(tr: TaskResult, sz, n: int) =
  let Start = get_time()
  var maps: seq[ImValue] = @[]
  for i in 0..<n:
    maps.add(V {i:i})
  tr.add(get_time() - Start)

proc map_add_entry*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_maps(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    maps[i] = maps[i].set(i + 1, i)
  tr.add(get_time() - Start)

proc map_add_entry_multiple*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_maps(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    maps[i] = maps[i]
      .set(i + 1, i + 1)
      .set(i + 2, i + 2)
      .set(i + 3, i + 3)
      .set(i + 4, i + 4)
      .set(i + 5, i + 5)
  tr.add(get_time() - Start)

proc map_overwrite_entry*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_maps(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    maps[i] = maps[i].set(i, i + 1)
  tr.add(get_time() - Start)

proc map_del_entry*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_maps(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    maps[i] = maps[i].del(i)
  tr.add(get_time() - Start)

proc map_merge*(tr: TaskResult, sz, n: int) =
  # setup
  var maps1 = setup_seq_of_maps(sz, n)
  var maps2 = setup_seq_of_maps(sz, n, 3)
  var maps3: seq[ImValue] = @[]
  # test
  let Start = get_time()
  for i in 0..<n:
    maps3.add(maps1[i].merge(maps2[i]))
  tr.add(get_time() - Start)

proc map_has_key_true*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_maps(sz, n)
  var bools: seq[bool] = @[]
  # test
  let Start = get_time()
  for i in 0..<n:
    bools.add(i in maps[i])
  tr.add(get_time() - Start)
  doAssert bools.all(proc (b: bool): bool = b)

proc map_has_key_false*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_maps(sz, n)
  var bools: seq[bool] = @[]
  # test
  let Start = get_time()
  for i in 0..<n:
    bools.add((i + 1) in maps[i])
  tr.add(get_time() - Start)
  doAssert bools.all(proc (b: bool): bool = b.not)

proc map_get_existing*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_maps(sz, n)
  var vals: seq[ImValue] = @[]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals.add(maps[i][i])
  tr.add(get_time() - Start)
  doAssert vals.all(proc (v: ImValue): bool = v != Nil.v)

proc map_get_non_existing*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_maps(sz, n)
  var vals: seq[ImValue] = @[]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals.add(maps[i][i + 1])
  tr.add(get_time() - Start)
  doAssert vals.all(proc (v: ImValue): bool = v == Nil.v)

proc map_iter_keys*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_maps(sz, n)
  var iters: seq[seq[ImValue]] = @[]
  var vals: seq[ImValue]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals = @[]
    for v in maps[i].keys: vals.add(v)
    iters.add(vals)
  tr.add(get_time() - Start)

proc map_iter_values*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_maps(sz, n)
  var iters: seq[seq[ImValue]] = @[]
  var vals: seq[ImValue]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals = @[]
    for v in maps[i].values: vals.add(v)
    iters.add(vals)
  tr.add(get_time() - Start)

proc map_iter_entries*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_maps(sz, n)
  var iters: seq[seq[(ImValue, ImValue)]] = @[]
  var vals: seq[(ImValue, ImValue)]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals = @[]
    for e in maps[i].pairs: vals.add(e)
    iters.add(vals)
  tr.add(get_time() - Start)

proc map_equal_true*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_maps(sz, n)
  var copies = maps.copy_maps
  var bools: seq[bool]
  # test
  let Start = get_time()
  for i in 0..<n:
    bools.add(maps[i] == copies[i])
  tr.add(get_time() - Start)
  doAssert bools.all(proc (b: bool): bool = b)

proc map_equal_false*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_maps(sz, n)
  var maps2 = setup_seq_of_maps(sz, n, 3)
  var bools: seq[bool]
  # test
  let Start = get_time()
  for i in 0..<n:
    bools.add(maps[i] == maps2[i])
  tr.add(get_time() - Start)
  doAssert bools.all(proc (b: bool): bool = b.not)
