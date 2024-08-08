## TODO
## - [x] simplify mutations
## - [x] put `size` in summary
## - [x] put summary in the parent
## - [ ] more efficient splice for small insertions/deletions
## - [x] splice that uses slices
## - [-] `[]=`
## - [-] `[]=` that uses a slice and splice

import std/[strformat, sequtils, strutils, sugar]
import hashes
import chunk
export chunk

const
  BRANCH_WIDTH = 32
  BUFFER_WIDTH = 64

type
  KeyError* = object of CatchableError
  IndexError* = object of CatchableError

  NodeKind* = enum
    kLeaf
    kInterior

  PVecSummary[T] = object
    size*: Natural
    hash*: Hash

  PVec*[T] = object
    summary*: PVecSummary[T]
    case kind*: NodeKind
      of kInterior:
        depth*: uint8
        nodes: Chunk[BRANCH_WIDTH, PVecRef[T]]
        node_summaries: Chunk[BRANCH_WIDTH, PVecSummary[T]]
      of kLeaf:
        data*: Chunk[BUFFER_WIDTH, T]
  PVecRef*[T] = ref PVec[T]

  PathStackItem*[T] = tuple[node: PVecRef[T], len: int, index: int]
  PathStack*[T] = seq[PathStackItem[T]]

func debug_json*[T](s: PVecRef[T]): string =
  result.add("{\n")
  result.add(&"  \"size\": {s.summary.size},\n")
  result.add(&"  \"kind\": \"{s.kind}\",\n")
  if s.kind == kLeaf:
    result.add(&"  \"data_len\": {s.data.len}")
  else:
    var inner = ""
    for (i, it) in s.nodes.pairs:
      if i == s.nodes.len - 1:
        inner = inner & it.debug_json
      else:
        inner = inner & it.debug_json & ","
    result.add(&"  \"depth\": {s.depth},\n")
    result.add(&"  \"nodes_len\": {s.nodes.len},\n")
    result.add(&"  \"nodes\": [{inner}]\n")
  result.add("}")

func `$`*[T](s: PVecRef[T]): string =
  var res = "["
  for it in s.items:
    res.add(&"{it}, ")
  if s.len > 0: res.delete((res.len - 2)..<(res.len.int))
  res.add("]")
  return res

# ===================================================================
# FORWARD DECLARATIONS
# ===================================================================

func init_sumtree*[T](d: T): PVecRef[T]
func init_sumtree*[T](kind: NodeKind): PVecRef[T]

func clone*[T](s: PVecRef[T]): PVecRef[T]

func im_delete_before*[T](s: PVecRef[T], idx: int): PVecRef[T]
func im_delete_after*[T](s: PVecRef[T], idx: int): PVecRef[T]

func pairs_closure[T](s: PVecRef[T]): iterator(): (int, T)


# ===================================================================
# SUMMARY OPERATIONS
# ===================================================================

func zero*[T](t: typedesc[PVecSummary[T]]): PVecSummary[T] =
  result.size = 0
  result.hash = 0

func `+`*[T](s1, s2: PVecSummary[T]): PVecSummary[T] =
  result.size = s1.size + s2.size
  result.hash = s1.hash xor s2.hash
func `+`*[T](s: PVecSummary[T], it: T): PVecSummary[T] =
  result.size = s.size + 1
  result.hash = s.hash xor it.hash

func `-`*[T](s1, s2: PVecSummary[T]): PVecSummary[T] =
  result.size = s1.size - s2.size
  result.hash = s1.hash xor s2.hash
func `-`*[T](s: PVecSummary[T], it: T): PVecSummary[T] =
  result.size = s.size - 1
  result.hash = s.hash xor it.hash

func from_buf*[T](t: typedesc[PVecSummary[T]], buf: openArray[T], l: Natural): PVecSummary[T] =
  result.size = l.uint
  result.hash = 0
  for i in 0..<l:
    result.hash = result.hash xor buf[i].hash

func from_item*[T](t: typedesc[PVecSummary[T]], it: T): PVecSummary[T] =
  result.size = 1
  result.hash = it.hash

# ===================================================================
# HELPERS
# ===================================================================

func depth_safe*[T](s: PVecRef[T]): uint8 =
  if s.kind == kLeaf:
    return 0
  return s.depth

template find_local_node_index_by_total_idx_template*(s, idx: untyped) {.dirty.} =
  ## Assumes s is an interior node
  var 
    node_idx: int
    adj_idx = idx
  block:
    var candidate: PVecSummary[T]
    for i in 0..<s.node_summaries.len:
      candidate = s.node_summaries[i]
      if adj_idx >= candidate.size:
        adj_idx -= candidate.size
      else:
        node_idx = i
        break;
func find_local_node_index_by_total_idx*[T](s: PVecRef[T], idx: int): (int, int) =
  find_local_node_index_by_total_idx_template(s, idx)
  return (node_idx, adj_idx)

template find_leaf_node_at_index_template*(s, idx: untyped) {.dirty.} =
  var
    n = s
    adj_idx = idx
  block:
    var candidate: PVecSummary[T]
    while n.kind == kInterior:
      block inner:
        for i in 0..<n.nodes.len:
          candidate = n.node_summaries[i]
          if adj_idx >= candidate.size:
            adj_idx = adj_idx - candidate.size
          else:
            n = n.nodes[i]
            break inner

func find_leaf_node_at_index*[T](s: PVecRef[T], idx: int): (PVecRef[T], int) =
  find_leaf_node_at_index_template(s, idx)
  return (n, adj_idx)

func get_stack_to_leaf_at_index*[T](s: PVecRef[T], idx: int): PathStack[T] =
  var stack: PathStack[T]
  if s.kind == kLeaf:
    stack.add((s, s.data.len, idx))
  else:
    var
      n = s
      adj_idx = idx
      candidate: PVecSummary[T]
    while n.kind == kInterior:
      block inner:
        for i in 0..<n.nodes.len:
          candidate = n.node_summaries[i]
          if adj_idx >= candidate.size:
            adj_idx -= candidate.size
          else:
            stack.add((n, n.nodes.len, i))
            n = n.nodes[i]
            break inner
    stack.add((n, n.data.len, adj_idx))
  return stack

func shadow*[T](stack: var PathStack[T], child: PVecRef[T]): PVecRef[T] =
  var 
    ch = child
    n_clone = child
    n: PVecRef[T]
    l: int
    i: int
  while stack.len > 0:
    (n, l, i) = stack.pop()
    n_clone = n.clone()
    n_clone.summary = (n_clone.summary - n_clone.nodes[i].summary) + ch.summary
    n_clone.depth = max(n_clone.depth, ch.depth_safe)
    n_clone.nodes[i] = ch
    n_clone.node_summaries[i] = ch.summary
    ch = n_clone
  return n_clone

func get_minimum_root*[T](s: PVecRef[T]): PVecRef[T] =
  var n = s
  while n.kind == kInterior and n.nodes.len == 1:
    n = n.nodes[0]
  return n

## Does not change the Node kind
proc reset*[T](s: PVecRef[T]) =
  s.summary = PVecSummary[T].zero()
  if s.kind == kInterior:
    s.depth = 1
    s.nodes.len = 0
    s.node_summaries.len = 0
  else:
    s.data.len = 0

func resummarize*[T](s: PVecRef[T]) =
  if s.kind == kInterior:
    s.summary = PVecSummary[T].zero()
    for i in 0..<s.nodes.len:
      s.summary = s.summary + s.nodes[i].summary
      s.node_summaries[i] = s.nodes[i].summary
  else:
    s.summary = PVecSummary[T].from_buf(s.data.buf, s.data.len)

func compute_local_summary*[T](s: PVecRef[T]): PVecSummary[T] =
  if s.kind == kLeaf:
    result = PVecSummary[T].from_buf(s.data.buf, s.data.len)
  else:
    result = PVecSummary[T].zero()
    for i in 0..<s.nodes.len:
      result = result + s.nodes[i].summary

func compute_local_size[T](s: PVecRef[T]): int =
  if s.kind == kLeaf:
    return s.data.len.int
  else:
    var computed_size = 0
    for i in 0..<s.nodes.len:
      computed_size += s.node_summaries[i].size
    return computed_size

func compute_local_depth[T](s: PVecRef[T]): uint8 =
  if s.kind == kLeaf:
    return 0
  else:
    var computed_depth: uint8 = 0
    var n: PVecRef[T]
    for i in 0..<s.nodes.len:
      n = s.nodes[i]
      if n.kind == kInterior:
        computed_depth = max(computed_depth, n.depth)
    return computed_depth + 1

func tree_from_leaves[T](leaves: seq[PVecRef[T]]): PVecRef[T] =
  var
    layer = leaves
    interiors: type(layer)
    idx: int
  while layer.len > 1:
    interiors.setLen(0)
    idx = 0
    while idx < layer.len:
      var n = init_sumtree[T](kInterior)
      n.mut_append_to_interior_with_room(idx, min(idx + BRANCH_WIDTH, layer.len), layer)
      interiors.add(n)
      idx += BRANCH_WIDTH
    layer = interiors
  return layer[0]

# ===================================================================
# MUTABLE HELPERS
# ===================================================================

template mut_append_to_leaf_with_room*[T](s: PVecRef[T], d: T) =
  s.data.add(d)
  s.summary = s.summary + d
proc mut_append_to_leaf_with_room*[T](s: PVecRef[T], start_idx, end_idx: int, data: openArray[T]) =
  var datum: T
  for i in start_idx..<end_idx:
    datum = data[i]
    s.data.add(datum)
    s.summary = s.summary + datum
template mut_append_to_leaf_with_room*[T](s: PVecRef[T], len: int, data: openArray[T]) =
  mut_append_to_leaf_with_room(s, 0, len, data)
template mut_append_to_leaf_with_room*[T](s: PVecRef[T], data: openArray[T]) =
  mut_append_to_leaf_with_room(s, 0, data.len, data)

template mut_insert_in_leaf_with_room*[T](s: PVecRef[T], idx: int, d: T) =
  s.data.insert(idx, d)
  s.summary = s.summary + d
proc mut_insert_in_leaf_with_room*[T](s: PVecRef[T], idx: int, data: openArray[T]) =
  s.data.insert(idx, data)
  for d in data:
    s.summary = s.summary + d

template mut_prepend_to_leaf_with_room*[T](s: PVecRef[T], d: T) =
  mut_insert_in_leaf_with_room(s, 0, d)

proc mut_append_to_interior_with_room*[T](s, child: PVecRef[T]) =
  s.nodes.add(child)
  s.node_summaries.add(child.summary)
  s.summary = s.summary + child.summary
  s.depth = max(s.depth, child.depth_safe + 1)
proc mut_append_to_interior_with_room*[T](s: PVecRef[T], start_idx, end_idx: int, children: openArray[PVecRef[T]]) =
  var child: PVecRef[T]
  for i in start_idx..<end_idx:
    child = children[i]
    s.nodes.add(child)
    s.node_summaries.add(child.summary)
    s.summary = s.summary + child.summary
    s.depth = max(s.depth, child.depth_safe + 1)
template mut_append_to_interior_with_room*[T](s: PVecRef[T], len: int, children: openArray[PVecRef[T]]) =
  mut_append_to_interior_with_room(s, 0, len, children)
template mut_append_to_interior_with_room*[T](s: PVecRef[T], children: openArray[PVecRef[T]]) =
  mut_append_to_interior_with_room(s, 0, children.len, children)

proc mut_insert_in_interior_with_room*[T](s: PVecRef[T], idx: int, child: PVecRef[T]) =
  s.nodes.insert(idx, child)
  s.node_summaries.insert(idx, child.summary)
  s.summary = s.summary + child.summary
  s.depth = max(s.depth, child.depth_safe + 1)
proc mut_insert_in_interior_with_room*[T](s: PVecRef[T], idx: int, children: openArray[PVecRef[T]]) =
  s.nodes.insert(idx, children)
  var summaries: seq[PVecSummary[T]]
  for child in children:
    summaries.add(child.summary)
    s.depth = max(s.depth, child.depth_safe + 1)
    s.summary = s.summary + child.summary
  s.node_summaries.insert(idx, summaries)

template mut_prepend_to_interior_with_room*[T](s, child: PVecRef[T]) =
  mut_insert_in_interior_with_room(s, 0, child)

proc mut_pop_leaf*[T](s: PVecRef[T]): T =
  var d = s.data.pop()
  s.summary = s.summary - d
  return d

proc mut_pop_interior*[T](s: PVecRef[T]): PVecRef[T] =
  let child = s.nodes.pop()
  discard s.node_summaries.pop()
  s.summary = s.summary - child.summary
  if s.depth == child.depth_safe + 1:
    s.depth = s.compute_local_depth
  return child

# ===================================================================
# IMMUTABLE HELPERS
# ===================================================================

func im_append_to_leaf_with_room*[T](s: PVecRef[T], d: T): PVecRef[T] =
  result = s.clone
  result.mut_append_to_leaf_with_room(d)

func im_append_to_leaf_no_room*[T](s: PVecRef[T], d: T): PVecRef[T] =
  var new_leaf = init_sumtree[T](d)
  result = init_sumtree[T](kInterior)
  result.mut_append_to_interior_with_room(s)
  result.mut_append_to_interior_with_room(new_leaf)

func im_prepend_to_leaf_with_room*[T](s: PVecRef[T], d: T): PVecRef[T] =
  result = s.clone
  result.mut_prepend_to_leaf_with_room(d)

func im_prepend_to_leaf_no_room*[T](s: PVecRef[T], d: T): PVecRef[T] =
  var new_leaf = init_sumtree[T](d)
  result = init_sumtree[T](kInterior)
  result.mut_prepend_to_interior_with_room(s)
  result.mut_prepend_to_interior_with_room(new_leaf)

# ===================================================================
# INITIALIZERS
# ===================================================================

func init_sumtree*[T](d: T): PVecRef[T] =
  var s = PVecRef[T](kind: kLeaf)
  s.mut_append_to_leaf_with_room(d)
  return s
func init_sumtree*[T](kind: NodeKind): PVecRef[T] =
  var s = PVecRef[T](kind: kind)
  s.summary = PVecSummary[T].zero()
  return s

func clone*[T](s: PVecRef[T]): PVecRef[T] =
  result = PVecRef[T]()
  result.kind = s.kind
  result.summary = s.summary
  if result.kind == kLeaf:
    result.data = s.data
  else:
    result.depth = s.depth
    result.nodes = s.nodes
    result.node_summaries = s.node_summaries

proc fill_sumtree_of_len*[T](len: int, filler: T): PVecRef[T] =
  if len == 0:
    return init_sumtree[T](kLeaf)
  var
    i = 0
    adj_size = len
    n: PVecRef[T]
    leaves: seq[PVecRef[T]]
  # build the leaves
  while adj_size >= 0:
    n = init_sumtree[T](kLeaf)
    for idx in 0..<min(adj_size, BUFFER_WIDTH):
      n.mut_append_to_leaf_with_room(filler)
    leaves.add(n)
    i += BUFFER_WIDTH
    adj_size -= BUFFER_WIDTH
  return tree_from_leaves(leaves)
template init_empty_sumtree_of_len*[T](len: int): PVecRef[T] =
  fill_sumtree_of_len[T](len, default(T))

func to_sumtree*[T](its: openArray[T]): PVecRef[T] =
  if its.len == 0:
    return init_sumtree[T](kLeaf)
  if its.len <= BUFFER_WIDTH:
    var leaf = init_sumtree[T](kLeaf)
    leaf.mut_append_to_leaf_with_room(its)
    return leaf
  var
    i = 0
    n: PVecRef[T]
    leaves: seq[PVecRef[T]]
  # build the leaves
  while i < its.len:
    n = init_sumtree[T](kLeaf)
    n.mut_append_to_leaf_with_room(i, min(i + BUFFER_WIDTH, its.len), its)
    leaves.add(n)
    i += BUFFER_WIDTH
  return tree_from_leaves(leaves)

## We use this for getting a sumtree from an iterator
template to_sumtree*(T: typedesc, iter: untyped): untyped =
  var
    i = 0
    n = init_sumtree[T](kLeaf)
    leaves: seq[PVecRef[T]]
  # build the leaves
  for it in iter:
    if i == BUFFER_WIDTH:
      leaves.add(n)
      n = init_sumtree[T](kLeaf)
      i = 0
    n.data.add(it)
    n.summary = n.summary + it
    i += 1
  leaves.add(n)
  result = tree_from_leaves(leaves)

# ===================================================================
# GETTER API
# ===================================================================

func get*[T](s: PVecRef[T], idx: int): T =
  find_leaf_node_at_index_template(s, idx)
  return n.data[adj_idx]
func get*[T](s: PVecRef[T], slice: Slice[int]): PVecRef[T] =
  if slice.a > s.summary.size:
    result = init_sumtree[T](kLeaf)
  elif s.kind == kLeaf:
    result = init_sumtree[T](kLeaf)
    result.data = s.data[slice]
    result.resummarize
  else:
    result = s.im_delete_before(slice.a).im_delete_after(slice.b - slice.a)

template `[]`*[T](s: PVecRef[T], idx: int): T = s.get(idx)
template `[]`*[T](s: PVecRef[T], slice: Slice[int]): PVecRef[T] = s.get(slice)

func get_or_default*[T](s: PVecRef[T], idx: int, d: T): T =
  if idx < 0 or idx >= s.len: return d
  find_leaf_node_at_index_template(s, idx)
  return n.data[adj_idx]
template get_or_default*[T](s: PVecRef[T], idx: int): T = get_or_default[T](s, idx, default(T))

func valid*[T](s: PVecRef[T]): bool =
  for n in s.nodes_post_order:
    if n.summary.size != n.compute_local_size:
      debugEcho "size"
      return false
    if n.summary != n.compute_local_summary:
      debugEcho "summary"
      return false
    if n.kind == kInterior:
      if n.nodes.len != n.node_summaries.len:
        debugEcho "inline summaries length"
        return false
      for i in 0..<n.nodes.len:
        if n.nodes[i].summary != n.node_summaries[i]:
          debugEcho "inline summaries do not match"
          return false
      if n.depth == 0:
        debugEcho "depth == 0"
        return false
      if n.depth != n.compute_local_depth:
        debugEcho "depth was ", n.depth, " but should have been ", n.compute_local_depth 
        return false
      if n.nodes.len == 1 and n.nodes[0].kind == kInterior:
        debugEcho "not minimum root"
        return false
  return true

template len*[T](s: PVecRef[T]): Natural = s.summary.size
template size*[T](s: PVecRef[T]): Natural = s.summary.size
template low*[T](s: PVecRef[T]): Natural = 0
template high*[T](s: PVecRef[T]): Natural = s.summary.size - 1

func `==`*[T](v1, v2: PVecRef[T]): bool  =
  if v1.isNil:
    if v2.isNil: return true
    return false
  if v2.isNil: return false
  if v1.summary != v2.summary: return false
  # TODO - figure out how to get rid of this
  {.cast(noSideEffect).}:
    var
      t1 = v1.pairs_closure()
      t2 = v2.pairs_closure()
      fin: bool
    while true:
      fin = finished(t1)
      if fin != finished(t2): return false
      if t1() != t2(): return false
      if fin: return true

# ===================================================================
# ITERATORS
# ===================================================================

iterator nodes_pre_order*[T](s: PVecRef[T]): PVecRef[T] =
  # yield after we push onto the stack
  var
    n = s
    idx: Natural
    n_stack: seq[PVecRef[T]]
    idx_stack: seq[Natural]
  if s.kind == kLeaf:
    yield s
  else:
    n_stack.add(s)
    yield n_stack[^1]
    # We push an extra idx onto the stack because we are going to be fiddling
    # with the top of the idx_stack after popping. This gives us a little 
    # cushion when the n_stack is empty before the while loop ends.
    idx_stack.add(0)
    idx_stack.add(0)
    while n_stack.len > 0:
      n = n_stack[^1]
      idx = idx_stack[^1]
      if n.kind == kLeaf:
        discard n_stack.pop()
        discard idx_stack.pop()
        idx_stack[^1] += 1
      else:
        if idx < n.nodes.len:
          # We haven't reached the end of the node's children
          n_stack.add(n.nodes[idx])
          yield n_stack[^1]
          idx_stack.add(0)
        else:
          # We reached the end of the node's children
          discard n_stack.pop()
          discard idx_stack.pop()
          idx_stack[^1] += 1

iterator nodes_post_order*[T](s: PVecRef[T]): PVecRef[T] =
  # yield before we pop off the stack
  var
    n = s
    idx: Natural
    n_stack: seq[PVecRef[T]]
    idx_stack: seq[Natural]
  if s.kind == kLeaf:
    yield s
  else:
    n_stack.add(s)
    # We push an extra idx onto the stack because we are going to be fiddling
    # with the top of the idx_stack after popping. This gives us a little 
    # cushion when the n_stack is empty before the while loop ends.
    idx_stack.add(0)
    idx_stack.add(0)
    while n_stack.len > 0:
      n = n_stack[^1]
      idx = idx_stack[^1]
      if n.kind == kLeaf:
        yield n
        discard n_stack.pop()
        discard idx_stack.pop()
        idx_stack[^1] += 1
      else:
        if idx < n.nodes.len:
          # We haven't reached the end of the node's children
          n_stack.add(n.nodes[idx])
          idx_stack.add(0)
        else:
          # We reached the end of the node's children
          yield n
          discard n_stack.pop()
          discard idx_stack.pop()
          idx_stack[^1] += 1

iterator leaves*[T](s: PVecRef[T]): PVecRef[T] =
  var
    n = s
    sz = 0
    idx = 0
    stack: PathStack[T]
  if n.kind == kLeaf:
    yield n
  else:
    stack.add((n, 0, 0))
    stack.add((n.nodes[0], 0, 0))
    while stack.len > 0:
      (n, sz, idx) = stack[^1]
      if n.kind == kLeaf:
        yield n
        discard stack.pop()
        if stack.len > 0: stack[^1][2] += 1
      elif idx < n.nodes.len:
        n = n.nodes[idx]
        stack.add((n, 0, 0))
      else:
        discard stack.pop()
        if stack.len > 0: stack[^1][2] += 1

iterator leaves_reverse*[T](s: PVecRef[T]): PVecRef[T] =
  var
    n = s
    sz = 0
    idx = 0
    stack: PathStack[T]
  if s.kind == kLeaf:
    yield s
  else:
    idx = n.nodes.len - 1
    stack.add((n, 0, idx))
    n = n.nodes[idx]
    if n.kind == kLeaf: stack.add((n, 0, n.data.len - 1))
    else:               stack.add((n, 0, n.nodes.len - 1))
    while stack.len > 0:
      (n, sz, idx) = stack[^1]
      if n.kind == kLeaf:
        yield n
        discard stack.pop()
        if stack.len > 0: stack[^1][2] -= 1
      elif idx > -1:
        n = n.nodes[idx]
        if n.kind == kLeaf: stack.add((n, 0, n.data.len - 1))
        else:               stack.add((n, 0, n.nodes.len - 1))
      else:
        discard stack.pop()
        if stack.len > 0: stack[^1][2] -= 1

template iterate_pairs*[T](s: PVecRef[T]) {.dirty.} =
  var total_idx = 0
  for n in s.leaves:
    for it in n.data.items:
      yield (total_idx, it)
      total_idx += 1

template iterate_pairs_reverse*[T](s: PVecRef[T]) {.dirty.} =
  var total_idx = s.len
  for n in s.leaves_reverse:
    for i in countdown(n.data.len - 1, 0):
      total_idx -= 1
      yield (total_idx, n.data[i])

iterator pairs*[T](s: PVecRef[T]): (int, T) =
  iterate_pairs(s)
iterator pairs_reverse*[T](s: PVecRef[T]): (int, T) =
  iterate_pairs_reverse(s)
iterator items*[T](s: PVecRef[T]): T =
  for (idx, d) in s.pairs:
    yield d
iterator items_reverse*[T](s: PVecRef[T]): T =
  for (idx, d) in s.pairs_reverse:
    yield d
func pairs_closure[T](s: PVecRef[T]): iterator(): (int, T) =
  return iterator(): (int, T) =
    iterate_pairs(s)

iterator map_iter*[T, U](s: PVecRef[T], op: proc (x: T, idx: int): U {.closure.}): U =
  for (idx, d) in s.pairs:
    yield op(d, idx)
iterator map_iter*[T, U](s: PVecRef[T], op: proc (x: T): U {.closure.}): U =
  for (idx, d) in s.pairs:
    yield op(d)
iterator filter_iter*[T](s: PVecRef[T], pred: proc (x: T, idx: int): bool {.closure.}): T =
  for (idx, d) in s.pairs:
    if pred(d, idx): yield d
iterator filter_iter*[T](s: PVecRef[T], pred: proc (x: T): bool {.closure.}): T =
  for (idx, d) in s.pairs:
    if pred(d): yield d
iterator zip_iter*[T, U](s1: PVecRef[T], s2: PVecRef[U]): (T, U) =
  var
    t1 = s1.pairs_closure()
    t2 = s2.pairs_closure()
  for i in 0..<min(s1.len, s2.len):
    yield (t1()[1], t2()[1])

# TODO - figure out how to deal with iterables for flat_map
# iterator flat_map*[T, U](s: PVecRef[T], op: proc (x: T, idx: int): iterable[U] {.closure.}): U =
#   for (idx, d) in s.pairs:
#     for item in op(d, idx):
#       yield item
# iterator flat_map*[T, U](s: PVecRef[T], op: proc (x: T): iterable[U] {.closure.}): U =
#   for (idx, d) in s.pairs:
#     for item in op(d):
#       yield item

func map*[T, U](s: PVecRef[T], op: proc (x: T, idx: int): U {.closure.}): PVecRef[U] =
  to_sumtree(U, map_iter[T, U](s, op))
func map*[T, U](s: PVecRef[T], op: proc (x: T): U {.closure.}): PVecRef[U] =
  to_sumtree(U, map_iter[T, U](s, op))
func filter*[T](s: PVecRef[T], pred: proc (x: T, idx: int): bool {.closure.}): PVecRef[T] =
  to_sumtree(T, filter_iter[T](s, pred))
func filter*[T](s: PVecRef[T], pred: proc (x: T): bool {.closure.}): PVecRef[T] =
  to_sumtree(T, filter_iter[T](s, pred))
proc zip*[T, U](s1: PVecRef[T], s2: PVecRef[U]): PVecRef[(T, U)] =
  to_sumtree((T, U), zip_iter[T, U](s1, s2))

func reverse*[T](s: PVecRef[T]): PVecRef[T] =
  to_sumtree(T, items_reverse[T](s))

# ===================================================================
# MUTABLE API
# ===================================================================

proc mut_append*[T](s: PVecRef[T], d: T) =
  var n = s
  var stack: seq[PVecRef[T]]
  while n.kind == kInterior:
    stack.add(n)
    n = n.nodes[n.nodes.len - 1]
  if n.data.len < BUFFER_WIDTH:
    n.mut_append_to_leaf_with_room(d)
  else:
    let s_clone = s.clone()
    var new_st = init_sumtree[T](d)
    s.reset()
    s.kind = kInterior
    s.mut_append_to_interior_with_room(s_clone)
    s.mut_append_to_interior_with_room(new_st)

# ===================================================================
# IMMUTABLE API
# ===================================================================

func im_delete_before*[T](s: PVecRef[T], idx: int): PVecRef[T] =
  if idx <= 0: return s
  if idx >= s.len: return init_sumtree[T](kLeaf)
  var stack = get_stack_to_leaf_at_index(s, idx)
  var (n, l, i) = stack.pop()
  var n_clone: PVecRef[T]
  if i == 0:
    result = n
  else:
    result = init_sumtree[T](kLeaf)
    result.mut_append_to_leaf_with_room(i, n.data.len, n.data.buf)
  while stack.len > 0:
    (n, l, i) = stack.pop()
    n_clone = init_sumtree[T](kInterior)
    n_clone.mut_append_to_interior_with_room(result)
    n_clone.mut_append_to_interior_with_room(i + 1, n.nodes.len, n.nodes.buf)
    result = n_clone
  result = result.get_minimum_root
template im_drop*[T](s: PVecRef[T], idx: int): PVecRef[T] = s.im_delete_before(idx)

func im_delete_after*[T](s: PVecRef[T], idx: int): PVecRef[T] =
  if idx < 0: return init_sumtree[T](kLeaf)
  if idx >= s.len: return s
  var stack = get_stack_to_leaf_at_index(s, idx)
  var (n, l, i) = stack.pop()
  var n_clone: PVecRef[T]
  if i == l - 1:
    result = n
  else:
    result = init_sumtree[T](kLeaf)
    result.mut_append_to_leaf_with_room(i + 1, n.data.buf)
  while stack.len > 0:
    (n, l, i) = stack.pop()
    n_clone = init_sumtree[T](kInterior)
    n_clone.mut_append_to_interior_with_room(i, n.nodes.buf)
    n_clone.mut_append_to_interior_with_room(result)
    result = n_clone
  result = result.get_minimum_root
template im_take*[T](s: PVecRef[T], idx: int): PVecRef[T] = s.im_delete_after(idx - 1)

func im_concat*[T](s1, s2: PVecRef[T]): PVecRef[T] =
  if s2.len == 0: return s1
  if s1.len == 0: return s2
  # TODO - take depth into account to try not to be too imbalanced
  let kinds = (s1.kind, s2.kind)
  if kinds == (kLeaf, kLeaf):
    if s1.data.len + s2.data.len <= BUFFER_WIDTH:
      result = init_sumtree[T](kLeaf)
      result.mut_append_to_leaf_with_room(s1.data.len, s1.data.buf)
      result.mut_append_to_leaf_with_room(s2.data.len, s2.data.buf)
    else:
      result = init_sumtree[T](kInterior)
      result.mut_append_to_interior_with_room([s1, s2])
  elif kinds == (kLeaf, kInterior):
    var
      stack = get_stack_to_leaf_at_index[T](s2, 0)
      child: PVecRef[T] 
      (n, l, i) = stack.pop()
    if s1.data.len + l <= BUFFER_WIDTH:
      child = init_sumtree[T](kLeaf)
      child.mut_append_to_leaf_with_room(s1.data.len, s1.data.buf)
      child.mut_append_to_leaf_with_room(n.data.len, n.data.buf)
      return shadow(stack, child)
    (n, l, i) = stack.pop()
    while true:
      if l < BRANCH_WIDTH:
        child = n.clone()
        child.mut_insert_in_interior_with_room(0, s1)
        return shadow(stack, child)
      elif stack.len == 0:
        result = init_sumtree[T](kInterior)
        result.mut_append_to_interior_with_room([s1, s2])
        break
      else:
        (n, l, i) = stack.pop()
  elif kinds == (kInterior, kLeaf):
    var
      stack = get_stack_to_leaf_at_index[T](s1, s1.len - 1)
      child: PVecRef[T] 
      (n, l, i) = stack.pop()
    if n.data.len + s2.data.len <= BUFFER_WIDTH:
      child = init_sumtree[T](kLeaf)
      child.mut_append_to_leaf_with_room(n.data.len, n.data.buf)
      child.mut_append_to_leaf_with_room(s2.data.len, s2.data.buf)
      return shadow(stack, child)
    (n, l, i) = stack.pop()
    while true:
      if l < BRANCH_WIDTH:
        child = n.clone()
        child.mut_append_to_interior_with_room(s2)
        return shadow(stack, child)
      elif stack.len == 0:
        result = init_sumtree[T](kInterior)
        result.mut_append_to_interior_with_room([s1, s2])
        break
      else:
        (n, l, i) = stack.pop()
  elif kinds == (kInterior, kInterior):
    result = init_sumtree[T](kInterior)
    if s1.nodes.len + s2.nodes.len <= BRANCH_WIDTH:
      result.mut_append_to_interior_with_room(s1.nodes.len, s1.nodes.buf)
      result.mut_append_to_interior_with_room(s2.nodes.len, s2.nodes.buf)
    else:
      result.mut_append_to_interior_with_room([s1, s2])

func im_append*[T](s: PVecRef[T], d: T): PVecRef[T] =
  var stack = get_stack_to_leaf_at_index[T](s, s.len - 1)
  var stack_len = stack.len
  var (n, l, i) = stack.pop()
  if i < BUFFER_WIDTH - 1:
    var n_clone = n.clone()
    n_clone.mut_append_to_leaf_with_room(d)
    return shadow[T](stack, n_clone)
  else:
    while stack.len > 0:
      (n, l, i) = stack.pop()
      if i < BRANCH_WIDTH - 1:
        var new_child = init_sumtree[T](d)
        # Try to keep things balanced by filling out to approximately the same
        # depth as other leaves? There is probably a better way to do this for
        # more random access and write patterns. This approach works well
        # for many successive pushes. But if some user action causes the depth
        # to get uncharacteristically large in some node, this approach to
        # appending could cause that increased depth to be maintained for other
        # nodes unnecessarily.
        for j in 0..<min(n.depth.int - 1, 2):
          var s = PVecRef[T](kind: kInterior)
          s.mut_append_to_interior_with_room(new_child)
          new_child = s
        var n_clone = n.clone()
        n_clone.mut_append_to_interior_with_room(new_child)
        return shadow[T](stack, n_clone)
  return n.im_append_to_leaf_no_room(d)

func im_prepend*[T](s: PVecRef[T], d: T): PVecRef[T] =
  var stack = get_stack_to_leaf_at_index[T](s, 0)
  var stack_len = stack.len
  var (n, l, i) = stack.pop()
  if l < BUFFER_WIDTH:
    var n_clone = n.clone()
    n_clone.mut_prepend_to_leaf_with_room(d)
    return shadow[T](stack, n_clone)
  else:
    while stack.len > 0:
      (n, l, i) = stack.pop()
      if l < BRANCH_WIDTH:
        var new_child = init_sumtree[T](d)
        # Try to keep things balanced by filling out to approximately the same
        # depth as other leaves? There is probably a better way to do this for
        # more random access and write patterns. This approach works well
        # for many successive pushes. But if some user action causes the depth
        # to get uncharacteristically large in some node, this approach to
        # prepending could cause that increased depth to be maintained for other
        # nodes unnecessarily.
        for j in 0..<min(n.depth.int - 1, 2):
          var s = PVecRef[T](kind: kInterior)
          s.mut_prepend_to_interior_with_room(new_child)
          new_child = s
        var n_clone = n.clone()
        n_clone.mut_prepend_to_interior_with_room(new_child)
        return shadow[T](stack, n_clone)
  return n.im_prepend_to_leaf_no_room(d)

func im_splice*[T](s: PVecRef[T], idx, length: int, items: openArray[T]): PVecRef[T] =
  var l = max(0, length)
  doAssert idx >= 0 and idx < s.len
  return im_concat(
    im_concat(s.take(idx), to_sumtree[T](items)),
    s.drop(idx + l)
  )
func im_splice*[T](s: PVecRef[T], idx, length: int, vec: PVecRef[T]): PVecRef[T] =
  var l = max(0, length)
  doAssert idx >= 0 and idx < s.len
  return im_concat(
    im_concat(s.take(idx), vec),
    s.drop(idx + l)
  )
func im_splice*[T](s: PVecRef[T], idx, length: int): PVecRef[T] =
  if length < 1: return s
  doAssert idx >= 0 and idx < s.len
  return im_concat(s.take(idx), s.drop(idx + length))

template im_splice*[T](s: PVecRef[T], slice: Slice[int], items: openArray[T]): PVecRef[T] =
  s.im_splice(slice.a, slice.b + 1 - slice.a, items)
template im_splice*[T](s: PVecRef[T], slice: Slice[int], vec: PVecRef[T]): PVecRef[T] =
  s.im_splice(slice.a, slice.b + 1 - slice.a, vec)
template im_splice*[T](s: PVecRef[T], slice: Slice[int]): PVecRef[T] =
  s.im_splice(slice.a, slice.b + 1 - slice.a)

template im_delete*[T](s: PVecRef[T], slice: Slice[int]): PVecRef[T] =
  s.im_splice(slice)
template im_insert*[T](s: PVecRef[T], items: openArray[T], idx: int): PVecRef[T] =
  s.im_splice(idx, 0, items)
template im_insert*[T](s: PVecRef[T], vec: PVecRef[T], idx: int): PVecRef[T] =
  s.im_splice(idx, 0, vec)

proc im_set*[T](s: PVecRef[T], idx: int, d: T): PVecRef[T] =
  ## TODO - handle indices that don't yet exist.
  if idx < 0 or idx > s.len:
    raise newException(IndexError, "Index is out of bounds")
  var stack = get_stack_to_leaf_at_index[T](s, idx)
  var (n, l, i) = stack.pop()
  var n_clone = n.clone()
  n_clone.summary = n_clone.summary - n_clone.data[i] + d
  n_clone.data[i] = d
  return shadow[T](stack, n_clone)
template im_set*[T](s: PVecRef[T], slice: Slice[int], data: openArray[T]): PVecRef[T] =
  s.im_splice(slice, data)
template im_set*[T](s: PVecRef[T], slice: Slice[int], vec: PVecRef[T]): PVecRef[T] =
  s.im_splice(slice, vec)

func im_set_len*[T](s: PVecRef[T], len: int): PVecRef[T] =
  if len == s.len: return s
  if len < s.len: return s.take(len)
  return im_concat(s, fill_sumtree_of_len[T](len - s.len, default(T)))

func im_pop*[T](s: PVecRef[T]): (PVecRef[T], T) =
  var stack = get_stack_to_leaf_at_index[T](s, s.len - 1)
  var (n, l, i) = stack.pop()
  var datum: T
  if l == 1:
    datum = n.data[0]
    while l == 1:
      if stack.len > 0:
        (n, l, i) = stack.pop()
      else:
        return (init_sumtree[T](kLeaf), datum)
    var n_clone = n.clone()
    discard n_clone.mut_pop_interior()
    return (shadow[T](stack, n_clone).get_minimum_root, datum)
  else:
    var n_clone = n.clone()
    var datum = n_clone.mut_pop_leaf()
    return (shadow[T](stack, n_clone), datum)

# ===================================================================
# VEC API
# ===================================================================

template init_vec*[T](): PVecRef[T] = init_sumtree[T](kLeaf)
template to_vec*[T](items: openArray[T]): PVecRef[T] = to_sumtree[T](items)
template to_vec*[T](iter: iterator): PVecRef[T] = to_sumtree[T](iter)

template append*[T](vec: PVecRef[T], item: T): PVecRef[T] = vec.im_append(item)
template push*[T](vec: PVecRef[T], item: T): PVecRef[T] = vec.im_append(item)
template add*[T](vec: PVecRef[T], item: T): PVecRef[T] = vec.im_append(item)

template prepend*[T](vec: PVecRef[T], item: T): PVecRef[T] = vec.im_prepend(item)
template push_front*[T](vec: PVecRef[T], item: T): PVecRef[T] = vec.im_prepend(item)

template pop*[T](vec: PVecRef[T]): (PVecRef[T], T) = vec.im_pop()

template set*[T](vec: PVecRef[T], idx: int, item: T): PVecRef[T] = vec.im_set(idx, item)
template set*[T](vec: PVecRef[T], slice: Slice[int], data: openArray[T]): PVecRef[T] = vec.im_set(slice, data)
template set*[T](vec: PVecRef[T], slice: Slice[int], data: PVecRef[T]): PVecRef[T] = vec.im_set(slice, data)

template set_len*[T](vec: PVecRef[T], len: int): PVecRef[T] = vec.im_set_len(len)

template delete*[T](s: PVecRef[T], slice: Slice[int]): PVecRef[T] = s.im_delete(slice)

template insert*[T](s: PVecRef[T], items: openArray[T], idx: int): PVecRef[T] = s.im_insert(items, idx)
template insert*[T](s: PVecRef[T], vec: PVecRef[T], idx: int): PVecRef[T] = s.im_insert(vec, idx)

template concat*[T](s1, s2: PVecRef[T]): PVecRef[T] = im_concat(s1, s2)
template `&`*[T](s1, s2: PVecRef[T]): PVecRef[T] = im_concat(s1, s2)

template drop*[T](s: PVecRef[T], idx: int): PVecRef[T] = s.im_drop(idx)
template take*[T](s: PVecRef[T], idx: int): PVecRef[T] = s.im_take(idx)