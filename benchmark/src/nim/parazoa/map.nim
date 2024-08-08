import std/[sequtils]
import ./parazoa
import ../common

proc setup_seq_of_parazoa_maps*(sz, it, offset: int): seq[Map[int, int]] =
  var i_off, k: int
  var m: Map[int, int]
  for i in 0..<it:
    i_off = i + offset
    m = [(i_off, i_off)].toMap
    for j in 1..<sz:
      k = i_off + (j * 17)
      m = m.add(k, k)
    result.add(m)
template setup_seq_of_parazoa_maps*(sz, it: int): seq[Map[int, int]] = setup_seq_of_parazoa_maps(sz, it, 0)

proc parazoa_map_create*(tr: TaskResult, sz, n: int) =
  let Start = get_time()
  var maps: seq[Map[int, int]] = @[]
  for i in 0..<n:
    maps.add([(i, i)].toMap)
  tr.add(get_time() - Start)

proc parazoa_map_add_entry*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_parazoa_maps(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    maps[i] = maps[i].add(i + 1, i)
  tr.add(get_time() - Start)

proc parazoa_map_add_entry_multiple*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_parazoa_maps(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    maps[i] = maps[i]
      .add(i + 1, i + 1)
      .add(i + 2, i + 2)
      .add(i + 3, i + 3)
      .add(i + 4, i + 4)
      .add(i + 5, i + 5)
  tr.add(get_time() - Start)

proc parazoa_map_overwrite_entry*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_parazoa_maps(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    maps[i] = maps[i].add(i, i + 1)
  tr.add(get_time() - Start)

proc parazoa_map_del_entry*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_parazoa_maps(sz, n)
  # test
  let Start = get_time()
  for i in 0..<n:
    maps[i] = maps[i].del(i)
  tr.add(get_time() - Start)

proc parazoa_map_merge*(tr: TaskResult, sz, n: int) =
  # setup
  var maps1 = setup_seq_of_parazoa_maps(sz, n)
  var maps2 = setup_seq_of_parazoa_maps(sz, n, 3)
  var maps3: seq[Map[int, int]] = @[]
  var m: Map[int, int]
  # test
  let Start = get_time()
  for i in 0..<n:
    m = maps1[i]
    m.add(maps2[i])
    maps3.add(m)
  tr.add(get_time() - Start)

proc parazoa_map_has_key_true*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_parazoa_maps(sz, n)
  var bools: seq[bool] = @[]
  # test
  let Start = get_time()
  for i in 0..<n:
    bools.add(i in maps[i])
  tr.add(get_time() - Start)
  doAssert bools.all(proc (b: bool): bool = b)

proc parazoa_map_has_key_false*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_parazoa_maps(sz, n)
  var bools: seq[bool] = @[]
  # test
  let Start = get_time()
  for i in 0..<n:
    bools.add((i + 1) in maps[i])
  tr.add(get_time() - Start)
  doAssert bools.all(proc (b: bool): bool = b.not)

proc parazoa_map_get_existing*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_parazoa_maps(sz, n)
  var vals: seq[int] = @[]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals.add(maps[i].get(i))
  tr.add(get_time() - Start)
  doAssert vals.all(proc (v: int): bool = v != -1)

proc parazoa_map_get_non_existing*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_parazoa_maps(sz, n)
  var vals: seq[int] = @[]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals.add(maps[i].getOrDefault(i + 1, -1))
  tr.add(get_time() - Start)
  doAssert vals.all(proc (v: int): bool = v == -1)

proc parazoa_map_iter_keys*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_parazoa_maps(sz, n)
  var iters: seq[seq[int]] = @[]
  var vals: seq[int]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals = @[]
    for v in maps[i].keys: vals.add(v)
    iters.add(vals)
  tr.add(get_time() - Start)

proc parazoa_map_iter_values*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_parazoa_maps(sz, n)
  var iters: seq[seq[int]] = @[]
  var vals: seq[int]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals = @[]
    for v in maps[i].values: vals.add(v)
    iters.add(vals)
  tr.add(get_time() - Start)

proc parazoa_map_iter_entries*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_parazoa_maps(sz, n)
  var iters: seq[seq[(int, int)]] = @[]
  var vals: seq[(int, int)]
  # test
  let Start = get_time()
  for i in 0..<n:
    vals = @[]
    for e in maps[i].pairs: vals.add(e)
    iters.add(vals)
  tr.add(get_time() - Start)

proc parazoa_map_equal_true*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_parazoa_maps(sz, n)
  var copies = setup_seq_of_parazoa_maps(sz, n)
  var bools: seq[bool]
  # test
  let Start = get_time()
  for i in 0..<n:
    bools.add(maps[i] == copies[i])
  tr.add(get_time() - Start)
  doAssert bools.all(proc (b: bool): bool = b)

proc parazoa_map_equal_false*(tr: TaskResult, sz, n: int) =
  # setup
  var maps = setup_seq_of_parazoa_maps(sz, n)
  var maps2 = setup_seq_of_parazoa_maps(sz, n, 3)
  var bools: seq[bool]
  # test
  let Start = get_time()
  for i in 0..<n:
    bools.add(maps[i] == maps2[i])
  tr.add(get_time() - Start)
  doAssert bools.all(proc (b: bool): bool = b.not)
