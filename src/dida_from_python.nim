import std/[tables, sets, bitops, strutils, sequtils, sugar, algorithm, strformat]
import hashes
import values

# Collections #
# ---------------------------------------------------------------------

type
  Value* = ImValue
  Entry* = Value

  Row* = (Entry, int)

  Collection* = ref object
    rows*: seq[Row]

  MapFn* = proc (e: Entry): Entry {.closure.}
  FilterFn* = proc (e: Entry): bool {.closure.}
  ReduceFn* = proc (rows: seq[Row]): seq[Row] {.closure.}
  CollIterateFn* = proc (c: Collection): Collection {.closure.}
  # TODO - figure out some stream or iterator concept so we don't have a bunch
  # of different versions of this.
  FlatMapFn* = proc (e: Entry): iterator(): Entry {.closure.}

func size*(c: Collection): int = return c.rows.len

proc `$`*(c: Collection): string =
  $(c[])

proc key*(e: Entry): Value =
  doAssert e.is_array
  return e.as_arr[0]
proc value*(e: Entry): Value =
  doAssert e.is_array
  return e.as_arr[1]
template entry*(r: Row): Entry = r[0]
template key*(r: Row): Value = r.entry.key
template value*(r: Row): Value = r.entry.value
template multiplicity*(r: Row): int = r[1]

template `[]`*(c: Collection, i: int): untyped = c.rows[i]
template `[]=`*(c: Collection, i: int, r: Row) = c.rows[i] = r
template add*(c: Collection, r: Row) = c.rows.add(r)

iterator items*(c: Collection): Row =
  for r in c.rows:
    yield r

## This is quite an expensive operation. It would be good to find a faster way
## to compute this.
## Using an xor-based hash for entries could help a lot.
func `==`*(c1, c2: Collection): bool =
  var
    t1 = initTable[Entry, int]()
    t2 = initTable[Entry, int]()
  for (e, m) in c1:
    t1[e] = m + t1.getOrDefault(e, 0)
  for (e, m) in c2:
    t2[e] = m + t2.getOrDefault(e, 0)
  return t1 == t2

func map*(c: Collection, f: MapFn): Collection =
  new result
  result.rows.setLen(c.size)
  for i in 0..<c.size:
    result[i] = (f(c[i].entry), c[i].multiplicity)

func filter*(c: Collection, f: FilterFn): Collection =
  new result
  for r in c:
    if f(r.entry): result.add(r)

func flat_map*(c: Collection, f: FlatMapFn): Collection =
  new result
  for r in c:
    for e in f(r.entry):
      result.add((e, r.multiplicity))

func negate*(c: Collection): Collection =
  new result
  result.rows.setLen(c.size)
  for i in 0..<c.size:
    result[i] = (c[i].entry, 0 - c[i].multiplicity)

func concat*(c1, c2: Collection): Collection =
  new result
  for r in c1: result.add(r)
  for r in c2: result.add(r)

proc mut_concat(c1: var Collection, c2: Collection) =
  for r in c2: c1.add(r)

func consolidate*(rows: seq[Row]): seq[Row] =
  var t = initTable[Entry, int]()
  for (e, m) in rows:
    t[e] = m + t.getOrDefault(e, 0)
  for e, m in t.pairs:
    if m != 0: result.add((e, m))

func consolidate*(c: Collection): Collection =
  new result
  var t = initTable[Entry, int]()
  for (e, m) in c:
    t[e] = m + t.getOrDefault(e, 0)
  for e, m in t.pairs:
    if m != 0: result.add((e, m))

proc print*(c: Collection, label: string): Collection =
  echo label, ": ", c
  return c

proc to_row_table_by_key(t: var Table[Value, seq[Row]], c: Collection) =
  for r in c:
    if t.hasKey(r.key):
      t[r.key].add(r)
    else:
      t[r.key] = @[r]

proc join*(c1, c2: Collection): Collection =
  new result
  let empty_seq = newSeq[Row]()
  var t = initTable[Value, seq[Row]]()
  t.to_row_table_by_key(c1)
  for r in c2:
    for r2 in t.getOrDefault(r.key, empty_seq):
      result.add((V [r.key, [r2.value, r.value]], r.multiplicity * r2.multiplicity))

## Keys must not be changed by the reduce fn
proc reduce*(c: Collection, f: ReduceFn): Collection =
  new result
  var t = initTable[Value, seq[Row]]()
  t.to_row_table_by_key(c)
  for r in t.values:
    for r2 in f(r):
      result.add(r2)

proc count_inner(rows: seq[Row]): seq[Row] =
  let k = rows[0].key
  var cnt = 0
  for r in rows: cnt += r.multiplicity
  return @[(V [k, cnt.float64], 1)]

proc count*(c: Collection): Collection =
  return c.reduce(count_inner)

proc sum_inner(rows: seq[Row]): seq[Row] =
  let k = rows[0].key
  var cnt = 0.float64
  for r in rows: cnt += r.value.as_f64 * r.multiplicity.float64
  return @[(V [k, cnt], 1)]

proc sum*(c: Collection): Collection =
  return c.reduce(sum_inner)

proc distinct_inner(rows: seq[Row]): seq[Row] =
  var t = initTable[Entry, int]()
  for r in rows:
    t[r.entry] = r.multiplicity + t.getOrDefault(r.entry, 0)
  result = @[]
  for e, i in t.pairs:
    doAssert i >= 0
    if i != 0:
      result.add((e, 1))

## Reduce a collection to a set
proc `distinct`*(c: Collection): Collection =
  return c.reduce(distinct_inner)

proc min_inner(rows: seq[Row]): seq[Row] =
  var t = initTable[Entry, int]()
  var k = rows[0].key
  for r in rows:
    t[r.entry] = r.multiplicity + t.getOrDefault(r.entry, 0)
  result = @[]
  var value_seen = false
  var min_val: ImValue
  for e, i in t.pairs:
    doAssert i >= 0
    if i != 0:
      if not(value_seen):
        value_seen = true
        min_val = e.value
      elif e.value < min_val:
        min_val = e.value
  if value_seen:
    return @[(V [k, min_val], 1)]
  else:
    return @[]

proc min*(c: Collection): Collection =
  try:
    return c.reduce(min_inner)
  except TypeException as e:
    raise newException(TypeException, "Incomparable types")

proc max_inner(rows: seq[Row]): seq[Row] =
  var t = initTable[Entry, int]()
  var k = rows[0].key
  for r in rows:
    t[r.entry] = r.multiplicity + t.getOrDefault(r.entry, 0)
  result = @[]
  var value_seen = false
  var max_val: ImValue
  for e, i in t.pairs:
    doAssert i >= 0
    if i != 0:
      if not(value_seen):
        value_seen = true
        max_val = e.value
      elif e.value > max_val:
        max_val = e.value
  if value_seen:
    return @[(V [k, max_val], 1)]
  else:
    return @[]

proc max*(c: Collection): Collection =
  try:
    return c.reduce(max_inner)
  except TypeException as e:
    raise newException(TypeException, "Incomparable types")

proc iterate*(c: Collection, f: CollIterateFn): Collection =
  var curr = c
  while true:
    result = f(curr)
    if curr == result: break
    curr = result

proc init_collection*(rows: openArray[Row]): Collection =
  new result
  for r in rows:
    result.add(r)

# Versions and Frontiers #
# ---------------------------------------------------------------------

type
  Version* = object
    hash*: Hash
    timestamps*: seq[int]

  Frontier* = ref object
    hash*: Hash
    versions*: seq[Version]

proc to_version(timestamps: seq[int]): Version =
  result.timestamps = timestamps
  result.hash = hash(timestamps)

template init_version*(): Version = to_version(@[0])
template init_version*(timestamps: openArray[int]): Version = to_version(toSeq[timestamps])

template `==`*(v1, v2: Version): bool = v1.hash == v2.hash and v1.timestamps == v2.timestamps
template size*(v: Version): int = v.timestamps.len
template `[]`*(v: Version, i: int): int = v.timestamps[i]

proc validate(v: Version) =
  doAssert v.size > 0
proc validate(v1, v2: Version) =
  doAssert v1.size > 0
  doAssert v1.size == v2.size

proc le*(v1, v2: Version): bool =
  validate(v1, v2)
  for i in 0..<v1.size:
    if v1[i] > v2[i]: return false
  return true
template lt*(v1, v2: Version): bool = v1.le(v2) and v1 != v2

proc join*(v1, v2: Version): Version =
  validate(v1, v2)
  var timestamps: seq[int] = @[]
  for i in 0..<v1.size:
    timestamps.add(max(v1[i], v2[i]))
  return to_version(timestamps)

proc meet*(v1, v2: Version): Version =
  validate(v1, v2)
  var timestamps: seq[int] = @[]
  for i in 0..<v1.size:
    timestamps.add(min(v1[i], v2[i]))
  return to_version(timestamps)

proc advance_by*(v: Version, f: Frontier): Version =
  if f.versions.len == 0: return v
  var curr = v.join(f.versions[0])
  for v2 in f.versions:
    curr = curr.meet(v.join(v2))
  return curr

proc extend*(v: Version): Version =
  var timestamps = toSeq(v.timestamps)
  timestamps.add(0)
  return to_version(timestamps)

proc truncate*(v: Version): Version =
  var timestamps = toSeq(v.timestamps[0..^2])
  return to_version(timestamps)

proc step*(v: Version, delta: int): Version =
  doAssert delta > 0
  var timestamps = toSeq(v.timestamps)
  timestamps[^1] += 1
  return to_version(timestamps)

proc sort(vs: var seq[Version]) =
  vs.sort(
    proc (a, b: Version): int =
      let
        aa = a.timestamps
        bb = b.timestamps
        l = min(aa.len, bb.len)
      for i in 0..<l:
        if aa[i] < bb[i]: return -1
        if aa[i] > bb[i]: return 1
      return aa.len - bb.len
  )

iterator items*(f: Frontier): Version =
  for v in f.versions:
    yield v

proc add(f: Frontier, v: Version) =
  var new_versions: seq[Version] = @[]
  for v2 in f:
    if v2.le(v): return
    if not(v.le(v2)):
      new_versions.add(v2)
  new_versions.add(v)
  f.versions = new_versions

## Must call after `add` or any other mutation is called
proc update_hash(f: Frontier) =
  var new_hash: Hash = 0
  for v in f:
    new_hash = new_hash xor v.hash
  f.hash = new_hash

proc init_frontier*(versions: openArray[Version]): Frontier =
  var new_f = Frontier()
  for v in versions:
    new_f.add(v)
  new_f.update_hash
  return new_f

proc meet*(f1, f2: Frontier): Frontier =
  var new_f = Frontier()
  for v in f1: new_f.add(v)
  for v in f2: new_f.add(v)
  new_f.update_hash
  return new_f

template sort(f: Frontier) = f.versions.sort()

proc `==`*(f1, f2: Frontier): bool =
  result = false
  if f1.hash == f2.hash:
    f1.sort
    f2.sort
    result = f1.versions == f2.versions

proc `$`*(v: Version): string =
  return "v" & v.timestamps.map(proc (t: int): string = $t).join(".")
proc `$`*(f: Frontier): string =
  return "F(" & f.versions.map(proc (v: Version): string = $v).join(" ") & ")" 

proc le*(f: Frontier, v: Version): bool =
  for v2 in f:
    if v2.le(v): return true
  return false

proc le*(f1, f2: Frontier): bool =
  var less_equal = false
  for v2 in f2:
    less_equal = false
    for v1 in f1:
      if v1.le(v2):
        less_equal = true
    if not(less_equal): return false
  return true
template lt*(f1, f2: Frontier): bool = f1.le(f2) and f1 != f2

proc extend*(f: Frontier): Frontier =
  var new_f = Frontier()
  for v in f:
    new_f.add(v.extend)
  new_f.update_hash
  return new_f

proc truncate*(f: Frontier): Frontier =
  var new_f = Frontier()
  for v in f:
    new_f.add(v.truncate)
  new_f.update_hash
  return new_f

proc step*(f: Frontier, delta: int): Frontier = 
  var new_f = Frontier()
  for v in f:
    new_f.add(v.step(delta))
  new_f.update_hash
  return new_f

# Index #
# ---------------------------------------------------------------------

type
  Index* = ref object
    compaction_frontier*: Frontier
    # Might be better as a tuple tree
    key_to_versions*: Table[Value, seq[Version]] 
    key_version_to_rows*: Table[(Value, Version), seq[Row]]

proc is_empty(i: Index): bool =
  return i.key_to_versions.len == 0

proc validate(i: Index, v: Version) =
  if i.compaction_frontier.isNil: return
  doAssert i.compaction_frontier.le(v)
proc validate(i: Index, f: Frontier) =
  if i.compaction_frontier.isNil: return
  doAssert i.compaction_frontier.le(f)

proc reconstruct_at(i: Index, key: Value, v: Version): seq[Row] =
  i.validate(v)
  for vers in i.key_to_versions.getOrDefault(key):
    if vers.le(v):
      result.add(i.key_version_to_rows.getOrDefault((key, vers)))

proc versions(i: Index, key: Value): seq[Version] =
  return i.key_to_versions.getOrDefault(key)

proc add(i: var Index, key: Value, version: Version, row: Row) =
  if key in i.key_to_versions:
    var s = i.key_to_versions[key]
    if s.find(version) == -1:
      s.add(version)
      i.key_to_versions[key] = s
    if (key, version) in i.key_version_to_rows:
      i.key_version_to_rows[(key, version)].add(row)
    else:
      i.key_version_to_rows[(key, version)] = @[row]
  else:
    i.key_to_versions[key] = @[version]
    i.key_version_to_rows[(key, version)] = @[row]
proc add(i: var Index, key: Value, version: Version, rows: seq[Row]) =
  if key in i.key_to_versions:
    var s = i.key_to_versions[key]
    if s.find(version) == -1:
      s.add(version)
      i.key_to_versions[key] = s
    if (key, version) in i.key_version_to_rows:
      i.key_version_to_rows[(key, version)].add(rows)
    else:
      i.key_version_to_rows[(key, version)] = rows
  else:
    i.key_to_versions[key] = @[version]
    i.key_version_to_rows[(key, version)] = rows

proc pop(i: var Index, key: Value, version: Version, rows: var seq[Row]) =
  var versions = i.key_to_versions[key]
  let idx = versions.find(version)
  if idx == -1: return
  versions.delete(idx)
  i.key_to_versions[key] = versions
  discard i.key_version_to_rows.pop((key, version), rows)

proc mut_concat(i1: var Index, i2: Index) =
  if i2.is_empty: return
  for (kv, rows) in i2.key_version_to_rows.pairs:
    let (key, version) = kv
    if key in i1.key_to_versions:
      var s = i1.key_to_versions[key]
      if s.find(version) == -1:
        s.add(version)
        i1.key_to_versions[key] = s
      if kv in i1.key_version_to_rows:
        i1.key_version_to_rows[kv].add(rows)
      else:
        i1.key_version_to_rows[kv] = rows
    else:
      i1.key_to_versions[key] = @[version]
      i1.key_version_to_rows[kv] = rows

proc join_inner(i1, i2: Index, fn: proc (r1, r2: Row): Row): seq[(Version, Collection)] =
  if i1.is_empty or i2.is_empty: return
  var join_version: Version
  var version_to_rows: Table[Version, seq[Row]]
  var rows, rows1, rows2: seq[Row]
  for (key, versions) in i1.key_to_versions.pairs:
    if key notin i2.key_to_versions: continue
    let versions2 = i2.key_to_versions[key]
    for v1 in versions:
      rows1 = i1.key_version_to_rows.getOrDefault((key, v1))
      for v2 in versions2:
        rows2 = i2.key_version_to_rows.getOrDefault((key, v2))
        join_version = v1.join(v2)
        if join_version in version_to_rows:
          rows = version_to_rows[join_version]
        else:
          rows = @[]
        for r1 in rows1:
          for r2 in rows2:
            rows.add(fn(r1, r2))
        version_to_rows[join_version] = rows
  for (v, rows) in version_to_rows.pairs:
    if rows.len > 0: result.add((v, Collection(rows: rows)))

template product_join(i1, i2: Index): seq[(Version, Collection)] =
  join_inner(i1, i2, (r1, r2) => (V([r1.entry, r2.entry]), r1.multiplicity * r2.multiplicity))
template join(i1, i2: Index): seq[(Version, Collection)] =
  join_inner(i1, i2, (r1, r2) => (V([r1.key, [r1.value, r2.value]]), r1.multiplicity * r2.multiplicity))

proc semijoin(i1, i2: Index): seq[(Version, Collection)] =
  # if i1.is_empty or i2.is_empty: return
  var join_version: Version
  var version_to_rows: Table[Version, seq[Row]]
  var rows, rows1: seq[Row]
  for (key, versions) in i1.key_to_versions.pairs:
    if key notin i2.key_to_versions: continue
    let versions2 = i2.key_to_versions[key]
    for v1 in versions:
      rows1 = i1.key_version_to_rows.getOrDefault((key, v1))
      for v2 in versions2:
        join_version = v1.join(v2)
        if join_version in version_to_rows:
          rows = version_to_rows[join_version]
        else:
          rows = @[]
        for r1 in rows1:
          rows.add(r1)
        version_to_rows[join_version] = rows
  for (v, rows) in version_to_rows.pairs:
    if rows.len > 0: result.add((v, Collection(rows: rows)))

## TODO - make this faster
## It is currently so slow as to be completely unusable.
proc compact(i: var Index, compaction_frontier: Frontier) =
  i.validate(compaction_frontier)
  for (key, versions) in i.key_to_versions.pairs:
    var to_consolidate: seq[Version] = @[]
    var to_compact: seq[Version]
    for version in versions:
      if compaction_frontier.le(version).not:
        to_compact.add(version)
    for version in to_compact:
      var rows: seq[Row] 
      i.pop(key, version, rows)
      let new_version = version.advance_by(compaction_frontier)
      i.add(key, new_version, rows)
      to_consolidate.add(new_version)
    for version in to_consolidate:
      let consolidated = i.key_version_to_rows[(key, version)].consolidate
      i.key_version_to_rows[(key, version)] = consolidated
  doAssert i.compaction_frontier.isNil or i.compaction_frontier.le(compaction_frontier)
  i.compaction_frontier = compaction_frontier

# Nodes #
# ---------------------------------------------------------------------

type
  BuilderIterateFn* = proc (b: Builder): Builder
  OnRowFn* = proc (r: Row): void
  OnCollectionFn* = proc (v: Version, c: Collection): void
  OnMessageFn* = proc (m: Message): void

  MessageTag* = enum
    tData
    tFrontier
  
  Message* = object
    case tag*: MessageTag:
      of tData:
        version*: Version
        collection*: Collection
      of tFrontier:
        frontier*: Frontier

  Edge* = ref object
    id*: Hash
    input*: Node
    output*: Node
    queue*: seq[Message]
    frontier*: Frontier
  
  NodeTag* = enum
    tPassThrough
    tIterate
    tIngress
    tEgress
    tFeedback
    tInput
    tOutput
    tIndex
    tMap
    tFilter
    tFlatMap
    tReduce
    tDistinct
    tCount
    tMin
    tMax
    tSum
    tPrint
    tNegate
    tConsolidate
    tSink
    # binary
    tProduct
    tConcat
    tJoinColumns
    tJoin
    tSemijoin
    # freeform - useful for debugging and tests
    tOnRow
    tOnCollection
    tOnMessage
    tAccumulateResults
    tAccumulateMessages
    # version manipulation - used in iteration
    tVersionPush
    tVersionIncrement
    tVersionPop

  Node* = ref object
    id*: int
    inputs*: seq[Edge]
    outputs*: seq[Edge]
    input_frontiers*: seq[Frontier]
    output_frontier*: Frontier
    case tag*: NodeTag:
      of tPrint:
        label*: string
      of tSink:
        sink_fn*: OnMessageFn
      of tConsolidate:
        collections*: Table[Version, Collection]
      of tSemijoin, tJoin, tProduct:
        indexes*: (Index, Index)
      of tMap:
        map_fn*: MapFn
      of tFilter:
        filter_fn*: FilterFn
      of tFlatMap:
        flat_map_fn*: FlatMapFn
      of tReduce:
        index*: Index
        index_out*: Index
        keys_todo*: Table[Version, HashSet[Value]]
        reduce_fn*: ReduceFn
      of tFeedback:
        step_size*: int
        in_flight_data*: Table[Version, HashSet[Version]]
        empty_versions*: Table[Version, HashSet[Version]]
      of tOnRow:
        on_row*: OnRowFn
      of tOnCollection:
        on_collection*: OnCollectionFn
      of tOnMessage:
        on_message*: OnMessageFn
      of tAccumulateResults:
        results*: seq[(Version, Collection)]
      of tAccumulateMessages:
        messages*: seq[Message]
      else:
        discard
  
  Graph* = ref object
    top_node*: Node
    nodes*: HashSet[Node]
    edges*: HashSet[Edge]
  
  Builder* = object
    frontier_stack*: seq[Frontier]
    graph*: Graph
    node*: Node

template hash(e: Edge): Hash = e.id
template hash(n: Node): Hash = n.id

var edge_id: int = 0
var node_id: int = 0

proc connect*(g: Graph, n1, n2: Node) =
  var n = n1
  if n == nil:
    n = g.top_node
  else:
    doAssert n in g.nodes or n == g.top_node
  var e = Edge()
  e.id = edge_id
  edge_id += 1
  e.input = n
  e.output = n2
  e.queue = @[]
  n.outputs.add(e)
  n2.inputs.add(e)
  n2.input_frontiers.add(n.output_frontier)
  if n2.output_frontier.isNil:
    n2.output_frontier = n.output_frontier
  g.edges.incl(e)
  g.nodes.incl(n2)

proc disconnect*(g: Graph, n: Node) =
  var i: int
  for e in n.inputs:
    i = e.input.outputs.find(e)
    e.input.outputs.del(i)
    g.edges.excl(e)
  for e in n.outputs:
    disconnect(g, e.output)
  g.nodes.excl(n)

proc init_node(t: NodeTag, f: Frontier): Node =
  var n = Node(
    tag: t,
    id: node_id,
    input_frontiers: @[],
    output_frontier: f,
    inputs: @[],
    outputs: @[],
  )
  node_id += 1
  return n

proc init_graph*(n: Node): Graph =
  return Graph(
    top_node: n,
    nodes: initHashSet[Node](),
    edges: initHashSet[Edge](),
  )
template is_empty*(e: Edge): bool = (e.queue.len == 0)

template clear(e: Edge) = e.queue.setLen(0)

proc pending_data*(n: Node): bool =
  for e in n.inputs:
    if not(e.is_empty): return true
  return false

proc probe_frontier_less_than*(n: Node, f: Frontier): bool =
  return n.output_frontier.lt(f)

proc `==`*(m1, m2: Message): bool =
  if m1.tag != m2.tag: return false
  case m1.tag:
    of tData:
      return m1.version == m2.version and m1.collection == m2.collection
    of tFrontier:
      return m1.frontier == m2.frontier

template to_message*(v: Version, c: Collection): Message =
  Message(tag: tData, version: v, collection: c)
template to_message*(f: Frontier): Message =
  Message(tag: tFrontier, frontier: f)

# Builder #
# ---------------------------------------------------------------------

proc init_builder*(g: Graph, f: Frontier): Builder =
  var b = Builder()
  b.graph = g
  b.frontier_stack = @[f]
  b.node = g.top_node
  return b
template init_builder*(f: Frontier): Builder =
  init_builder(init_graph(init_node(tPassThrough, f)), f)
template init_builder*(g: Graph): Builder =
  init_builder(g, init_frontier([init_version()]))
template init_builder*(): Builder =
  let f = init_frontier([init_version()])
  init_builder(init_graph(init_node(tPassThrough, f)), f)

template frontier*(b: Builder): Frontier = b.frontier_stack[^1]

proc start_scope(b: Builder): Builder =
  result.frontier_stack = b.frontier_stack
  result.frontier_stack.add(result.frontier.extend)
  result.graph = b.graph
  result.node = b.node
proc end_scope(b: Builder): Builder =
  result.frontier_stack = b.frontier_stack
  discard result.frontier_stack.pop()
  result.graph = b.graph
  result.node = b.node

template build_unary(b: Builder, t: NodeTag) {.dirty.} =
  var n = init_node(t, b.frontier)
  connect(b.graph, b.node, n)
  result.frontier_stack = b.frontier_stack
  result.graph = b.graph
  result.node = n
proc build_unary_proc(b: Builder, t: NodeTag): Builder =
  build_unary(b, t)

template build_binary(b: Builder, t: NodeTag, other: Node) {.dirty.} =
  var n = init_node(t, b.frontier)
  doAssert b.node != nil
  connect(b.graph, b.node, n)
  connect(b.graph, other, n)
  result.frontier_stack = b.frontier_stack
  result.graph = b.graph
  result.node = n

proc print*(b: Builder, label: string): Builder =
  build_unary(b, tPrint)
  n.label = label

proc negate*(b: Builder): Builder =
  build_unary(b, tNegate)

proc concat*(b: Builder, other: Node): Builder =
  build_binary(b, tConcat, other)
template concat*(b1, b2: Builder): Builder = b1.concat(b2.node)

proc product*(b: Builder, other: Node): Builder =
  build_binary(b, tProduct, other)
  n.indexes = (Index(), Index())
template product*(b1, b2: Builder): Builder = b1.product(b2.node)

proc join*(b: Builder, other: Node): Builder =
  build_binary(b, tJoin, other)
  n.indexes = (Index(), Index())
template join*(b1, b2: Builder): Builder = b1.join(b2.node)

proc semijoin*(b: Builder, other: Node): Builder =
  build_binary(b, tSemijoin, other)
  n.indexes = (Index(), Index())
template semijoin*(b1, b2: Builder): Builder = b1.semijoin(b2.node)

proc consolidate*(b: Builder): Builder =
  build_unary(b, tConsolidate)

proc map*(b: Builder, fn: MapFn): Builder =
  build_unary(b, tMap)
  n.map_fn = fn

proc filter*(b: Builder, fn: FilterFn): Builder =
  build_unary(b, tFilter)
  n.filter_fn = fn

proc flat_map*(b: Builder, fn: FlatMapFn): Builder =
  build_unary(b, tFlatMap)
  n.flat_map_fn = fn

proc reduce*(b: Builder, fn: ReduceFn): Builder =
  build_unary(b, tReduce)
  n.index = Index()
  n.index_out = Index()
  n.reduce_fn = fn

proc `distinct`*(b: Builder): Builder =
  return b.reduce(distinct_inner)

proc ingress(b: Builder): Builder =
  build_unary(b, tIngress)

proc egress(b: Builder): Builder =
  build_unary(b, tEgress)

proc feedback(b: Builder, step: int): Builder =
  build_unary(b, tFeedback)
  n.step_size = step

proc iterate*(b: Builder, fn: BuilderIterateFn): Builder =
  var scope = b.start_scope
  var ingress_b = scope.ingress
  # we only connect one stream to our concat node for now
  var concat_b = build_unary_proc(ingress_b, tConcat)
  var output = fn(concat_b)
  var feedback_b = output.feedback(1)
  # now we connect the other stream to our concat node
  connect(feedback_b.graph, feedback_b.node, concat_b.node)
  return output.end_scope.egress

proc count*(b: Builder): Builder =
  return b.reduce(count_inner)
  # build_unary(b, tCount)

proc on_row*(b: Builder, fn: OnRowFn): Builder =
  build_unary(b, tOnRow)
  n.on_row = fn

proc on_collection*(b: Builder, fn: OnCollectionFn): Builder =
  build_unary(b, tOnCollection)
  n.on_collection = fn

proc on_message*(b: Builder, fn: OnMessageFn): Builder =
  build_unary(b, tOnMessage)
  n.on_message = fn

proc accumulate_results*(b: Builder): Builder =
  build_unary(b, tAccumulateResults)
  n.results = @[]

proc accumulate_messages*(b: Builder): Builder =
  build_unary(b, tAccumulateMessages)
  n.messages = @[]

# Sinks #
# ---------------------------------------------------------------------

type
  CompletableMultisetValue = ref object
    done*: bool
    table*: Table[Value, int]

  VersionedMultiset* = ref object
    data*: Table[Version, CompletableMultisetValue]
  ## TODO - figure out how to accumulate a multiset as of some version
  ## This turns out to be trickier than I would have thought on account of
  ## the multidimensional nature of differential dataflow timestamps.
  ## The problem happens when we add data at a new version. We need to
  ## accumulate all the multisets from previous values but without double
  ## counting. If v1 < v2 < v3, and we create v3, we need to add the multiset
  ## from v2 but NOT v1. As the quantity of versions increases, all these
  ## version comparisons explode without some means of indexing versions.
  CumulativeVersionedMultiset* = ref object
    data*: Table[Version, CompletableMultisetValue]

proc mut_concat(cmv1, cmv2: CompletableMultisetValue) = 
  for value, multiplicity in cmv2.table.pairs:
    let m = multiplicity + cmv1.table.getOrDefault(value, 0)
    if m == 0:
      cmv1.table.del(value)
    else:
      cmv1.table[value] = m

proc init_versioned_multiset*(): VersionedMultiset =
  return VersionedMultiset()

proc init_cumulative_versioned_multiset*(): CumulativeVersionedMultiset =
  return CumulativeVersionedMultiset()

proc sink*(b: Builder, s: VersionedMultiset): Builder = 
  build_unary(b, tSink)
  n.sink_fn = proc (m: Message): void =
    case m.tag:
      of tData:
        var v: CompletableMultisetValue
        if m.version in s.data:
          v = s.data[m.version]
          doAssert not(v.done)
        else:
          v = CompletableMultisetValue()
          v.done = false
          s.data[m.version] = v
        for r in m.collection:
          let multiplicity = r.multiplicity + v.table.getOrDefault(r.entry, 0)
          if multiplicity == 0:
            v.table.del(r.entry)
          else:
            v.table[r.entry] = multiplicity
      of tFrontier:
        for version, v in s.data.mpairs:
          if not(m.frontier.le(version)):
            v.done = true

# proc sink*(b: Builder, s: CumulativeVersionedMultiset): Builder = 
#   build_unary(b, tSink)
#   n.sink_fn = proc (m: Message): void =
#     case m.tag:
#       of tData:
#         if m.version notin s.data:
#           var new_v = CompletableMultisetValue()
#           new_v.done = false
#           for version, v in s.data.mpairs:
#             echo "m.version: ", m.version, " ", version
#             if version.lt(m.version):
#               new_v.mut_concat(v)
#           s.data[m.version] = new_v
#         for version, v in s.data.mpairs:
#           if m.version.le(version):
#             doAssert not(v.done)
#             for r in m.collection:
#               let multiplicity = r.multiplicity + v.table.getOrDefault(r.entry, 0)
#               if multiplicity == 0:
#                 v.table.del(r.entry)
#               else:
#                 v.table[r.entry] = multiplicity
#       of tFrontier:
#         for version, v in s.data.mpairs:
#           if not(m.frontier.le(version)):
#             v.done = true

proc to_collection*(s: VersionedMultiset, v: Version): Collection =
  if v in s.data:
    var rows: seq[Row] = @[]
    for entry, multiplicity in s.data[v].table.pairs:
      rows.add((entry, multiplicity))
    return init_collection(rows)
  else:
    return init_collection([])

# proc to_collection*(s: CumulativeVersionedMultiset, v: Version): Collection =
#   if v in s.data:
#     var rows: seq[Row] = @[]
#     for entry, multiplicity in s.data[v].table.pairs:
#       rows.add((entry, multiplicity))
#     return init_collection(rows)
#   else:
#     return init_collection([])

# Pretty Print #
# ---------------------------------------------------------------------

proc `$`*(t: NodeTag): string =
  result = case t:
    of tInput:              "Input"
    of tPassThrough:        "PassThrough"
    of tPrint:              "Print"
    of tNegate:             "Negate"
    of tConcat:             "Concat"
    of tProduct:            "Product"
    of tSemijoin:           "Semijoin"
    of tJoinColumns:        "JoinColumns"
    of tJoin:               "Join"
    of tMap:                "Map"
    of tFilter:             "Filter"
    of tOnRow:              "OnRow"
    of tOnCollection:       "OnCollection"
    of tOnMessage:          "OnMessage"
    of tIngress:            "Ingress"
    of tEgress:             "Egress"
    of tReduce:             "Reduce"
    of tCount:              "Count"
    of tConsolidate:        "Consolidate"
    of tDistinct:           "Distinct"
    of tAccumulateResults:  "AccumulateResults"
    of tAccumulateMessages: "AccumulateMessages"
    of tFeedback:           "Feedback"
    else:                   "TODO"

proc string_from_pprint_seq(s: seq[(int, string)]): string =
  for (count, str) in s:
    result.add(str.indent(count))
    result.add('\n')

template pprint*(v: Version): string = "(" & v.timestamps.join(" ") & ")"
template pprint*(f: Frontier): string = "[" & f.versions.map(proc (v: Version): string = v.pprint).join(" ") & "]"

proc pprint_inner(m: Message, indent = 0): seq[(int, string)] =
  case m.tag:
    of tData:
      var s = &"DATA:     {m.version.pprint} {$m.collection.rows}"
      result.add((indent, s))
    of tFrontier:
      var s = &"FRONTIER: {m.frontier.pprint}"
      result.add((indent, s))

proc pprint_inner(e: Edge, indent = 0): seq[(int, string)] =
  var s = &"q:@{e.id} {e.queue.len}"
  result.add((indent, s))
  for m in e.queue:
    result.add(m.pprint_inner(indent + 2))

proc pprint_inner(n: Node, indent = 0): seq[(int, string)] =
  var s = &"[{$(n.tag)}] @{n.id} F:{n.output_frontier.pprint}"
  result.add((indent, s))
  for e in n.outputs:
    result.add(pprint_inner(e, indent + 1))
    discard

proc pprint_recursive_inner*(n: Node, indent = 0): seq[(int, string)] =
  result = n.pprint_inner(indent)
  if n.outputs.len > 0:
    result.add((indent + 1, &"children: {n.outputs.len}"))
    for e in n.outputs:
      result.add(pprint_recursive_inner(e.output, indent + 3))

template pprint*(n: Node, indent = 0): string =
  n.pprint_inner(indent).string_from_pprint_seq

template pprint_recursive*(n: Node, indent = 0): string =
  n.pprint_recursive_inner(indent).string_from_pprint_seq

proc pprint*(g: Graph): string =
  return g.top_node.pprint_recursive(0)

# Send #
# ---------------------------------------------------------------------

template send*(e: Edge, v: Version, c: Collection) =
  doAssert e.frontier.isNil or e.frontier.le(v)
  e.queue.add(Message(tag: tData, version: v, collection: c))
template send*(e: Edge, f: Frontier) =
  doAssert e.frontier.isNil or e.frontier.le(f)
  e.frontier = f
  e.queue.add(Message(tag: tFrontier, frontier: f))
template send*(e: Edge, m: Message) =
  if m.tag == tData:
    doAssert e.frontier.isNil or e.frontier.le(m.version)
  else:
    doAssert e.frontier.isNil or e.frontier.le(m.frontier)
    e.frontier = m.frontier
  e.queue.add(m)

template send(n: Node, v: Version, c: Collection) {.dirty.} =
  for e in n.outputs:
    e.send(v, c)
template send(n: Node, f: Frontier) {.dirty.} =
  for e in n.outputs:
    e.send(f)
template send(n: Node, m: Message) {.dirty.} =
  for e in n.outputs:
    e.send(m)

proc send*(g: Graph, v: Version, c: Collection) = g.top_node.send(v, c)
proc send*(g: Graph, f: Frontier) = g.top_node.send(f)
proc send*(g: Graph, m: Message) = g.top_node.send(m)

# Step #
# ---------------------------------------------------------------------

proc handle_frontier_message(n: Node, f: Frontier, idx: int) =
  doAssert n.input_frontiers[idx].le(f)
  n.input_frontiers[idx] = f
template handle_frontier_message(n: Node, m: Message, idx: int) =
  handle_frontier_message(n, m.frontier, idx)
template handle_frontier_message_unary(n: Node, f: Frontier) =
  handle_frontier_message(n, f, 0)
template handle_frontier_message_unary(n: Node, m: Message) =
  handle_frontier_message(n, m.frontier, 0)

proc output_frontier_message(n: Node) =
  var input_frontier: Frontier
  let l = n.input_frontiers.len
  if l == 1:
    input_frontier = n.input_frontiers[0]
  elif l == 2:
    input_frontier = n.input_frontiers[0].meet(n.input_frontiers[1])
  doAssert n.output_frontier.le(input_frontier)
  if n.output_frontier.lt(input_frontier):
    n.output_frontier = input_frontier
    n.send(input_frontier)

proc step(n: Node) =
  var frontier_change = false
  case n.tag:
    of tInput:
      discard "TODO"
    of tPassThrough:
      for m in n.inputs[0].queue:
        n.send(m)
      n.inputs[0].clear
    of tPrint:
      for m in n.inputs[0].queue:
        case m.tag:
          of tData:
            echo &"{n.label}: [data] {m.version} {m.collection.rows}"
            n.send(m)
          of tFrontier:
            echo &"{n.label}: [frontier] {m.frontier}"
            frontier_change = true
            n.handle_frontier_message(m, 0)
      n.inputs[0].clear
    of tSink:
      for m in n.inputs[0].queue:
        n.sink_fn(m)
        case m.tag:
          of tData:
            n.send(m)
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message(m, 0)
      n.inputs[0].clear
    of tNegate:
      for m in n.inputs[0].queue:
        case m.tag:
          of tData:
            n.send(m.version, m.collection.negate)
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message_unary(m)
      n.inputs[0].clear
    of tMap:
      for m in n.inputs[0].queue:
        case m.tag:
          of tData:
            n.send(m.version, m.collection.map(n.map_fn))
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message_unary(m)
      n.inputs[0].clear
    of tFilter:
      for m in n.inputs[0].queue:
        case m.tag:
          of tData:
            let new_coll = m.collection.filter(n.filter_fn)
            if new_coll.size > 0: n.send(m.version, new_coll)
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message_unary(m)
      n.inputs[0].clear
    of tFlatMap:
      for m in n.inputs[0].queue:
        case m.tag:
          of tData:
            let new_coll = m.collection.flat_map(n.flat_map_fn)
            if new_coll.size > 0: n.send(m.version, new_coll)
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message_unary(m)
      n.inputs[0].clear
    of tIngress:
      for m in n.inputs[0].queue:
        case m.tag:
          of tData:
            let new_version = m.version.extend
            n.send(new_version, m.collection)
            n.send(new_version.step(1), m.collection.negate)
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message_unary(m.frontier)
      n.inputs[0].clear
      if frontier_change:
        var input_f_ext = n.input_frontiers[0].extend
        doAssert n.output_frontier.le(input_f_ext)
        if n.output_frontier.lt(input_f_ext):
          n.output_frontier = input_f_ext
          n.send(input_f_ext)
      frontier_change = false
    of tEgress:
      var input_frontier: Frontier
      for m in n.inputs[0].queue:
        case m.tag:
          of tData:
            let new_version = m.version.truncate
            n.send(new_version, m.collection)
          of tFrontier:
            frontier_change = true
            let new_frontier = m.frontier.truncate
            let truncated_input_frontier = n.input_frontiers[0].truncate
            doAssert truncated_input_frontier.le(new_frontier)
            n.input_frontiers[0] = m.frontier
            input_frontier = new_frontier
      if frontier_change:
        doAssert n.output_frontier.le(input_frontier)
        if n.output_frontier.lt(input_frontier):
          n.output_frontier = input_frontier
          n.send(input_frontier)
      frontier_change = false
      n.inputs[0].clear
    of tFeedback:
      for m in n.inputs[0].queue:
        case m.tag:
          of tData:
            let new_version = m.version.step(n.step_size)
            n.send(new_version, m.collection)
            let truncated = new_version.truncate
            if truncated in n.in_flight_data:
              n.in_flight_data[truncated].incl(new_version)
            else:
              n.in_flight_data[truncated] = toHashSet([new_version])
            if truncated notin n.empty_versions:
              n.empty_versions[truncated] = initHashSet[Version]()
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message_unary(m)
      let incremented_input_frontier = n.input_frontiers[0].step(n.step_size)
      var versions = incremented_input_frontier.versions
      var candidate_versions: seq[Version] = @[]
      var rejected: seq[Version] = @[]
      for v in versions:
        var truncated = v.truncate
        if truncated in n.in_flight_data and n.in_flight_data[truncated].len != 0:
          candidate_versions.add(v)
          var to_excl: seq[Version] = @[]
          for x in n.in_flight_data[truncated]:
            if x.lt(v): to_excl.add(x)
          for x in to_excl:
            n.in_flight_data[truncated].excl(x)
        else:
          if truncated in n.empty_versions:
            n.empty_versions[truncated].incl(v)
          else:
            n.empty_versions[truncated] = toHashSet([v])
          if truncated notin n.empty_versions or n.empty_versions[truncated].len <= 3:
            candidate_versions.add(v)
          else:
            n.in_flight_data.del(truncated)
            n.empty_versions.del(truncated)
            rejected.add(v)
      for v in rejected:
        for truncated in n.in_flight_data.keys:
          candidate_versions.add(v.join(truncated.extend))
      let candidate_frontier = init_frontier(candidate_versions)
      doAssert n.output_frontier.le(candidate_frontier)
      if n.output_frontier.lt(candidate_frontier):
        n.output_frontier = candidate_frontier
        n.send(candidate_frontier)
      frontier_change = false
      n.inputs[0].clear
    of tReduce:
      proc subtract_values(fst, snd: seq[Row]): seq[Row] =
        var res: Table[Value, int]
        for (e, m) in fst:
          res[e] = res.getOrDefault(e, 0) + m
        for (e, m) in snd:
          res[e] = res.getOrDefault(e, 0) - m
        for (e, m) in res.pairs:
          if m != 0: result.add((e, m))
      for m in n.inputs[0].queue:
        case m.tag:
          of tData:
            for r in m.collection:
              n.index.add(r.key, m.version, r)
              if m.version in n.keys_todo:
                n.keys_todo[m.version].incl(r.key)
              else:
                n.keys_todo[m.version] = toHashSet([r.key])
              for v2 in n.index.key_to_versions[r.key]:
                let v3 = m.version.join(v2)
                if v3 in n.keys_todo:
                  n.keys_todo[v3].incl(r.key)
                else:
                  n.keys_todo[v3] = toHashSet([r.key])
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message_unary(m)
      let input_frontier = n.input_frontiers[0]
      var finished_versions: seq[Version] = @[]
      for version in n.keys_todo.keys:
        if not(input_frontier.le(version)): finished_versions.add(version)
      finished_versions.sort()
      for version in finished_versions:
        let keys = n.keys_todo[version]
        n.keys_todo.del(version)
        var res: seq[Row] = @[]
        for key in keys:
          var curr = n.index.reconstruct_at(key, version)
          var curr_out = n.index_out.reconstruct_at(key, version)
          var Out = n.reduce_fn(curr)
          var delta = subtract_values(Out, curr_out)
          for r in delta:
            res.add(r)
            n.index_out.add(key, version, r)
        if res.len > 0:
          n.send(version, Collection(rows: res))
      n.inputs[0].clear
    of tConcat:
      for m in n.inputs[0].queue:
        case m.tag:
          of tData: n.send(m)
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message(m, 0)
      n.inputs[0].clear
      for m in n.inputs[1].queue:
        case m.tag:
          of tData: n.send(m)
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message(m, 1)
      n.inputs[1].clear
    of tProduct:
      var deltas = [Index(), Index()]
      for idx in 0..<2:
        for m in n.inputs[idx].queue:
          case m.tag:
            of tData:
              for row in m.collection:
                deltas[idx].add(Nil.v, m.version, row)
            of tFrontier:
              frontier_change = true
              n.handle_frontier_message(m, idx)
        n.inputs[idx].clear
      for (v, c) in deltas[0].product_join(n.indexes[1]): n.send(v, c)
      n.indexes[0].mut_concat(deltas[0])
      for (v, c) in n.indexes[0].product_join(deltas[1]): n.send(v, c)
      n.indexes[1].mut_concat(deltas[1])
      if frontier_change:
        n.output_frontier_message
        # n.indexes[0].compact(n.output_frontier)
        # n.indexes[1].compact(n.output_frontier)
      frontier_change = false
    of tJoin:
      var deltas = [Index(), Index()]
      for idx in 0..<2:
        for m in n.inputs[idx].queue:
          case m.tag:
            of tData:
              for row in m.collection:
                deltas[idx].add(row.key, m.version, row)
            of tFrontier:
              frontier_change = true
              n.handle_frontier_message(m, idx)
        n.inputs[idx].clear
      for (v, c) in deltas[0].join(n.indexes[1]):
        n.send(v, c)
      n.indexes[0].mut_concat(deltas[0])
      for (v, c) in n.indexes[0].join(deltas[1]):
        n.send(v, c)
      n.indexes[1].mut_concat(deltas[1])
      if frontier_change:
        n.output_frontier_message
        # n.indexes[0].compact(n.output_frontier)
        # n.indexes[1].compact(n.output_frontier)
      frontier_change = false
    of tSemijoin:
      var deltas = [Index(), Index()]
      for idx in 0..<2:
        for m in n.inputs[idx].queue:
          case m.tag:
            of tData:
              for row in m.collection:
                deltas[idx].add(row.key, m.version, row)
            of tFrontier:
              frontier_change = true
              n.handle_frontier_message(m, idx)
        n.inputs[idx].clear
      for (v, c) in deltas[0].semijoin(n.indexes[1]): n.send(v, c)
      n.indexes[0].mut_concat(deltas[0])
      for (v, c) in n.indexes[0].semijoin(deltas[1]): n.send(v, c)
      n.indexes[1].mut_concat(deltas[1])
      if frontier_change:
        n.output_frontier_message
        # n.indexes[0].compact(n.output_frontier)
        # n.indexes[1].compact(n.output_frontier)
      frontier_change = false
    of tConsolidate:
      for m in n.inputs[0].queue:
        case m.tag:
          of tData:
            if n.collections.hasKey(m.version):
              n.collections[m.version] = n.collections[m.version].concat(m.collection)
            else:
              n.collections[m.version] = m.collection
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message(m, 0)
      let input_frontier = n.input_frontiers[0]
      var deletions: seq[Version]
      if frontier_change:
        for (v, c) in n.collections.pairs:
          if not(input_frontier.le(v)):
            let new_c = c.consolidate()
            if new_c.size > 0: n.send(v, new_c)
            deletions.add(v)
      for v in deletions:
        n.collections.del(v)
      n.inputs[0].clear
    of tOnRow:
      for m in n.inputs[0].queue:
        case m.tag:
          of tData:
            for r in m.collection:
              n.on_row(r)
            n.send(m)
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message(m, 0)
      n.inputs[0].clear
    of tOnCollection:
      for m in n.inputs[0].queue:
        case m.tag:
          of tData:
            n.on_collection(m.version, m.collection)
            n.send(m)
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message(m, 0)
      n.inputs[0].clear
    of tOnMessage:
      for m in n.inputs[0].queue:
        n.on_message(m)
        case m.tag:
          of tData:
            n.send(m)
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message(m, 0)
      n.inputs[0].clear
    of tAccumulateResults:
      for m in n.inputs[0].queue:
        case m.tag:
          of tData:
            n.results.add((m.version, m.collection))
            n.send(m)
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message(m, 0)
      n.inputs[0].clear
    of tAccumulateMessages:
      for m in n.inputs[0].queue:
        n.messages.add(m)
        case m.tag:
          of tData:
            n.send(m)
          of tFrontier:
            frontier_change = true
            n.handle_frontier_message(m, 0)
      n.inputs[0].clear
    else:
      discard
  if frontier_change: n.output_frontier_message
proc step*(g: Graph) =
  for n in g.nodes: n.step()
