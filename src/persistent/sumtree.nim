## 
## DESIGN
## 
## Zed
##
##  struct SumTree<T: Item>(pub Arc<Node<T>>);
##  enum Node<T: Item> {
##      Internal {
##          height: u8,
##          summary: T::Summary,
##          child_summaries: ArrayVec<T::Summary, { 2 * TREE_BASE }>,
##          child_trees: ArrayVec<SumTree<T>, { 2 * TREE_BASE }>,
##      },
##      Leaf {
##          summary: T::Summary,
##          items: ArrayVec<T, { 2 * TREE_BASE }>,
##          item_summaries: ArrayVec<T::Summary, { 2 * TREE_BASE }>,
##      },
##  }
## 
## 
#[

type 
  Chunk[Count: static int, T] = object
    len: uint32
    buf: array[Count, T]
  
  ZedSumTree*[Data, Summary] = object
    summary*: Summary
    case kind*: STNodeKind
      of STInterior:
        height*: uint8
        child_trees*: Chunk[32, ZedSumTreeRef[Data, Summary]]
        child_summaries: Chunk[32, Summary]
      of STLeaf:
        items: Chunk64[Data]
        item_summaries: Chunk64[Summary]
  ZedSumTreeRef*[Data, Summary] = ref ZedSumTree[Data, Summary]

  ImArraySummary = object
    hash: Hash
    len: uint32
    # we could accumulate additional stats here, like `max`, `min`, etc, but
    # probably no point.

  Array*[Value, Summary] = object
    summary*: Summary
    case kind*: STNodeKind
      of STInterior:
        height*: uint8
        children*: Chunk[32, ArrayRef[Value, Summary]]
        children_summaries*: Chunk[32, Summary]
      of STLeaf:
        data: Chunk[64, Value]
  ArrayRef*[Value, Summary] = ref Array[Value, Summary]

  ## Additional Array considerations
  ## - do we want to have cumulative summaries of children on interior nodes?
  ##   - this is also a consideration for strings
  ##   - that lets us do binary search on children summaries when we `get`
  ##   - but comes at the cost of reaccumulating summaries on every `set`
  ##   - probably not worth doing initially
  ## - do we want to support sparse arrays?
  ##   - this is probably not worth doing
  ##   - probably better to just have ops like `setLen` fill with `default(Value)`
  ##   - probably not worth doing initially
  ##   

  TextSummary = object
    hash: Hash
    # utf8 string
    bytes: uint32
    # What the user would consider a character/grapheme
    # characters/graphemes must not cross chunk boundaries
    characters: uint32
    # Just count the number of newlines
    lines: uint32

  String*[Summary] = object
    summary*: Summary
    case kind*: STNodeKind
      of STInterior:
        height*: uint8
        children*: Chunk[32, StringRef[Summary]]
        children_summaries*: Chunk[32, Summary]
      of STLeaf:
        data: Chunk[64, uint8]
  StringRef*[Summary] = ref String[Summary]

  Entry[Key, Value] = object
    key: Key
    value: Value
  ImSortedMapSummary = object
    hash: Hash
    len: uint32
    max: Key
    high: Key

  # TODO - support custom sort orders?
  # This would also need a sort fn or something if we want to support custom
  # sort orders.
  SortedMap*[Entry[Key, Value], Summary] = object
    summary*: Summary
    case kind*: STNodeKind
      of STInterior:
        height*: uint8
        children*: Chunk[32, SortedMapRef[Summary]]
        children_summaries*: Chunk[32, Summary]
      of STLeaf:
        data: Chunk[64, Entry[Key, Value]]
  SortedMapRef*[Summary] = ref SortedMap[Summary]

  ## Additional SortedMap considerations
  ## - unlike Arrays, we may profit from doing binary search to find elements

  ImSortedSetSummary = object
    hash: Hash
    len: uint32
    max: Key
    high: Key

  SortedSet*[Value, Summary] = object
    summary*: Summary
    case kind*: STNodeKind
      of STInterior:
        height*: uint8
        children*: Chunk[32, SortedSetRef[Summary]]
        children_summaries*: Chunk[32, Summary]
      of STLeaf:
        data: Chunk[64, Value]
  SortedSetRef*[Summary] = ref SortedSet[Summary]

  ## 
  ## - Orderless versions of Map and Set should use HAMT or similar
  ## - How do we want to handle Rich text and other widget trees?
  ## - How about refs? Do we want to use SumTrees for refs?
  ##   - They're still useful for any kind of sorted data where
  ##     insertions/deletions can happen at any key
  ##     - SortedMap, SortedSet, Text, possibly even Array
  ## - Transients?
  ##   - It might be good to have an additional Summary operation for removing
  ##     or adding a single element rather than rehashing up to 64 data items.
  ##     - for ImValues, the hash should just be the 64bit float cast to a hash
  ##     - probably good even without transients
  ## - Tables?
  ## 
  ## USE CASES???
  ## (UNI = our dynamic Value)
  ## (UNI_IM = our immutable dynamic values)
  ## (UNI_REF = our reference dynamic values)
  ## 
  ## # UNI_IM
  ## - ImString
  ## 
  ## # Specific to UNI_IMs (this affects the shape of summaries)
  ## # The box around these things will probably get interned for fast comparisons.
  ## - ImArray[UNI_IM]
  ## - ImSortedSet[UNI_IM]
  ## - ImSortedMap[UNI_IM, UNI_IM]
  ## 
  ## # Generic (level 1)
  ## # Use the immutable collections in a typed fashion?
  ## # We can have basic summary and summary ops prepped for each type
  ## # Do we still hash in the summaries?
  ## - ImArray[V]
  ## - ImSortedSet[V]
  ## - ImSortedMap[K, V]
  ## 
  ## # Generic (level 2)
  ## # Expose the summary interface? (this is almost definitely a bad idea)
  ## - ImArray[V, Summary]
  ## - ImSortedSet[V, Summary]
  ## - ImSortedMap[K, V, Summary]
  ## 
  ## # Mutable UNI_REFs
  ## - Text (mutable string)
  ## 
  ## # Still dedicated to our UNI-type. Is there any particular restriction
  ## # on Summaries? If not, then we can just have this all be Generic (level 1)
  ## - Array[UNI]
  ## - SortedSet[UNI]
  ## - SortedMap[UNI, UNI]
  ## 
  ## # Generic (level 1)
  ## - Array[V]
  ## - SortedSet[V]
  ## - SortedMap[K, V]
  ## 
  ## # Generic (level 2)
  ## # Again, bad idea?
  ## - Array[V, Summary]
  ## - SortedSet[V, Summary]
  ## - SortedMap[K, V, Summary]
  ## 
  ## # With all this mutable stuff, how do we actually use that within Jackal?
  ## # How does any of that work with our logic programming?
  ## # Obviously we need something that is mutable at the top level, but
  ## # other than that, mutability is going to be a problem for tracking state
  ## # and ensuring determinism.
  ## 
  ## # With all the generic stuff, do we want to allow for changes to chunk
  ## # sizes? Probably not.
  ## 
  ## RichText, Widget Trees (like vDOM), R trees?
  ## - It's conceptually straightforward to use SumTrees for something linear,
  ##   but how does it work for something tree-like?
  ## - I think R-trees might be possible just with summaries as interior nodes
  ##   and the data items in the chunk in the leaf.
  ##   - Insertion is going to have to be a bit different because sometimes
  ##     it might make more sense to expand a slice of node data into its own
  ##     node rather than wrapping it.
  ##   - maintaining balance may be irritating
  ##   - ideally we could parameterize the number of dimensions, the types for
  ##     each dimension, a comparison function for each dimension, and what
  ##     indices we want to maintain for traversal in different orders.
  ##     - maintaining different indices may be overkill
  ## - I don't think widget trees make sense to model with SumTrees because
  ##   each node in a widget tree needs to represent some widget. If the
  ##   thought is to put a widget in the summary, there's a problem with that.
  ##   Namely that widgets aren't summable.
  ##   - RichText is probably not going to work for similar reasons
  ## 
  
  RangeTree1D[Key, Value, Summary] = object
    summary*: Summary
    case kind*: STNodeKind
      of STInterior:
        height*: uint8
        children*: Chunk[32, RangeTreeRef[Summary]]
        children_summaries*: Chunk[32, Summary]
      of STLeaf:
        data: Chunk[64, Value]
  RangeTreeRef*[Summary] = ref RangeTree[Summary]

  # Ideal API for range trees requires macros
  RangeTree:
    name:            Super3DRangeTree3000        # the name to be used for the generated types
    value:           ValueType
    dimension_types: [ Key1,  Key2,  Key3]       # 3D range tree
    dimension_index: [  Asc,  None,  Both]       # tells the number and types of sorted
                                                 # buffers we want to maintain on each
                                                 # node
    dimension_cmps:  [func1, func2, func3]       # comparison functions for the key types
    branch_width:    16                          # defaults to 32
    buffer_width:    64                          # defaults to 2 * branch_width
    summary:         SummaryType
    summary_zero:    func4           # get an empty SummaryType
    summary_from:    func7           # get a SummaryType from a buffer of ValueType
    summary_sum:     func5           # sum two SummaryTypes together
    summary_minus:   func6           # subtract a SummaryType from a SummaryType
    partial_sum:     func8           # add a ValueType to a SummaryType
    partial_minus:   func9           # subtract a ValueType from a SummaryType
  
  # Ideal API for SumTrees requires macros?
  SumTree:
    name:            SomeSumTree     # the name to be used fro the generated types
    value:           ValueType
    summary:         SummaryType
    branch_width:    16              # defaults to 32
    buffer_width:    64              # defaults to 2 * branch_width
    summary_zero:    func4           # get an empty SummaryType
    summary_from:    func7           # get a SummaryType from a buffer of ValueType
    summary_sum:     func5           # sum two SummaryTypes together
    summary_minus:   func6           # subtract a SummaryType from a SummaryType
    partial_sum:     func8           # add a ValueType to a SummaryType
    partial_minus:   func9           # subtract a ValueType from a SummaryType

]#
## 
## 

import std/[strformat, sequtils]
import hashes
import chunk

func copyRef[T](node: T): T =
  new result
  if node != nil:
    result[] = node[]

const
  BRANCH_WIDTH = 32
  BUFFER_WIDTH = 64

type
  KeyError* = object of CatchableError
  # IndexError* = object of CatchableError

  Chunk[Count: static int, T] = object
    len: uint32
    buf: array[Count, T]


  Summary[Data] = concept x, y, type T
    x + y is T
    x - y is T
    T.zero is T
    T.from_buf(buf: openArray[Data], i: Natural) is T
  STBuffer[Data] = array[BUFFER_WIDTH, Data]
  STBufferRef[Data] = ref STBuffer[Data]
  STNodeKind* = enum
    STInterior
    STLeaf
  SumTree*[Data, Summ] = object
    # total count of Data items in the tree
    size*: Natural
    summary*: Summ
    case kind*: STNodeKind
    of STInterior:
      depth*: uint8
      nodes_count*: Natural
      nodes: array[BRANCH_WIDTH, SumTreeRef[Data, Summ]]
    of STLeaf:
      data_count*: Natural
      data: STBuffer[Data]
  SumTreeRef*[Data, Summ] = ref SumTree[Data, Summ]

proc `$`*[D, S](s: SumTreeRef[D, S]): string =
  result.add(&"ST(\n")
  result.add(&"  size: {s.size}\n")
  # result.add(&"  summary: TODO\n")
  result.add(&"  kind: {s.kind}\n")
  if s.kind == STLeaf:
    discard
    # result.add(&"  data_count: {s.data_count}")
  else:
    result.add(&"  depth: {s.depth}\n")
    result.add(&"  nodes_count: {s.nodes_count}\n")
    # result.add(&"  nodes: {s.nodes}\n")
  result.add(&")")

proc clone*[D, S](s: SumTreeRef[D, S]): SumTreeRef[D, S] =
  result = SumTreeRef[D, S]()
  result.size = s.size
  result.kind = s.kind
  result.summary = s.summary
  if result.kind == STLeaf:
    result.data_count = s.data_count
    result.data = s.data
  else:
    result.depth = s.depth
    result.nodes_count = s.nodes_count
    result.nodes = s.nodes

template find_leaf_node_at_index_template*(s, idx: untyped) {.dirty.} =
  var
    n = s
    adj_idx = idx
  block:
    var candidate: SumTreeRef[D, S]
    while n.kind == STInterior:
      block inner:
        for i in 0..<n.nodes_count:
          candidate = n.nodes[i]
          if adj_idx > candidate.size:
            adj_idx = adj_idx - candidate.size
          else:
            n = candidate
            break inner

proc find_leaf_node_at_index*[D, S](s: SumTreeRef[D, S], idx: int): (SumTreeRef[D, S], int) =
  find_leaf_node_at_index_template(s, idx)
  return (n, adj_idx)

# TODO - handle spare nodes
template get_stack_to_leaf_at_index_template*(s, idx: untyped) {.dirty.} =
  var stack: seq[(SumTreeRef[D, S], int)]
  if s.kind == STLeaf:
    stack.add((s, idx))
  else:
    var
      n = s
      adj_idx = idx
      candidate: SumTreeRef[D, S]
    while n.kind == STInterior:
      block inner:
        for i in 0..<n.nodes_count:
          candidate = n.nodes[i]
          if adj_idx > candidate.size:
            adj_idx = adj_idx - candidate.size
          else:
            stack.add((n, i))
            n = candidate
            break inner
    stack.add((n, adj_idx))

proc get_stack_to_leaf_at_index*[D, S](s: SumTreeRef[D, S], idx: int): seq[(SumTreeRef[D, S], int)] =
  get_stack_to_leaf_at_index_template(s, idx)
  return stack

proc get*[D, S](s: SumTreeRef[D, S], idx: int): D =
  ## TODO - handle negative indices?
  ## TODO - handle sparse arrays
  if idx < 0 or idx > s.size:
    raise newException(IndexError, "Index is out of bounds")
  find_leaf_node_at_index_template(s, idx)
  return n.data[adj_idx]

proc shadow*[D, S](stack: var seq[(SumTreeRef[D, S], int)], child: var SumTreeRef[D, S]): SumTreeRef[D, S] =
  var 
    ch = child
    n_clone = child
    n: SumTreeRef[D, S]
    i: int
  while stack.len > 0:
    (n, i) = stack.pop()
    n_clone = n.clone()
    n_clone.summary = (n_clone.summary - n_clone.nodes[i].summary) + ch.summary
    n_clone.nodes[i] = ch
    ch = n_clone
  return n_clone

proc im_set*[D, S](s: SumTreeRef[D, S], idx: int, d: D): SumTreeRef[D, S] =
  ## TODO - handle indices that don't yet exist.
  ## TODO - handle sparse arrays
  if idx < 0 or idx > s.size:
    raise newException(IndexError, "Index is out of bounds")
  get_stack_to_leaf_at_index_template(s, idx)
  var (n, i) = stack.pop()
  var n_clone = n.clone()
  n_clone.data[i] = d
  n_clone.summary = S.from_buf(n_clone.data, n_clone.data_count)
  return shadow[D, S](stack, n_clone)

proc im_pop*[D, S](s: SumTreeRef[D, S]): (SumTreeRef[D, S], D) =
  ## TODO - handle indices that don't yet exist.
  ## TODO - handle sparse arrays
  get_stack_to_leaf_at_index_template(s, s.size - 1)
  var (n, i) = stack.pop()
  var n_clone = n.clone()
  var item = n_clone.data[i]
  n_clone.data[i] = default(D)
  n_clone.data_count -= 1
  n_clone.size -= 1
  n_clone.summary = S.from_buf(n_clone.data, n_clone.data_count)
  return (shadow[D, S](stack, n_clone), item)

const
  VEC_BITS = 5
  VEC_BRANCHING_FACTOR = 1 shl VEC_BITS

type
  PVecSummary[T] = object
    hash*: Hash
    size*: uint

proc `+`*[T](s1, s2: PVecSummary[T]): PVecSummary[T] =
  result.size = s1.size + s2.size
  result.hash = s1.hash xor s2.hash
proc `-`*[T](s1, s2: PVecSummary[T]): PVecSummary[T] =
  result.size = s1.size - s2.size
  result.hash = s1.hash xor s2.hash
proc zero*[T](t: typedesc[PVecSummary[T]]): PVecSummary[T] =
  result.size = 0
  result.hash = 0
proc from_buf*[T](t: typedesc[PVecSummary[T]], buf: openArray[T], l: Natural): PVecSummary[T] =
  result.size = l.uint
  result.hash = 0
  for i in 0..<l:
    result.hash = result.hash xor buf[i].hash

type
  PVec*[T] = SumTree[T, PVecSummary[T]]
  PVecRef*[T] = SumTreeRef[T, PVecSummary[T]]

## Does not change the Node kind
proc reset*[D, S](s: SumTreeRef[D, S]) =
  s.summary = S.zero()
  s.size = 0
  if s.kind == STInterior:
    s.depth = 0
    s.nodes_count = 0
    s.nodes = array[BRANCH_WIDTH, D]
  else:
    s.data_count = 0
    s.data = array[BUFFER_WIDTH, D]

proc resummarize*[D, S](s: SumTreeRef[D, S]) =
  if s.kind == STInterior:
    s.summary = S.zero()
    for i in 0..<s.nodes_count:
      s.summary = s.summary + s.nodes[i].summary
  else:
    s.summary = S.from_buf(s.data, s.data_count)

template mut_append_case_1*[D, S](s: SumTreeRef[D, S], d: D) =
  ## The node is a leaf and there's room in the data
  s.data[s.data_count] = d
  s.data_count += 1
  s.size += 1
  s.summary = S.from_buf(s.data, s.data_count)

proc init_sumtree*[D, S](d: D): SumTreeRef[D, S] =
  var s = SumTreeRef[D, S](kind: STLeaf)
  s.mut_append_case_1(d)
  return s
proc init_sumtree*[D, S](kind: STNodeKind): SumTreeRef[D, S] =
  var s = SumTreeRef[D, S](kind: kind)
  s.summary = S.zero()
  return s
template init_sumtree*[D, S](): SumTreeRef[D, S] = init_sumtree(STLeaf)

proc mut_append_case_2*[D, S](s, child: SumTreeRef[D, S]) =
  ## The node is an interior with room for a new child
  s.nodes[s.nodes_count] = child
  s.nodes_count += 1
  s.size += 1
  s.summary = s.summary + child.summary
  if s.depth <= child.depth:
    s.depth = child.depth + 1

template create_interior_dirty_template() {.dirty.} =
  var interior = init_sumtree[D, S](STInterior)
  interior.nodes = leaves
  interior.nodes_count = leaves_count
  interior.size = BUFFER_WIDTH * (leaves_count - 1)
  interior.size += interior.nodes[leaves_count - 1].size
  interior.depth = 1
  interior.resummarize()

template add_leaf_dirty_template() {.dirty.} =
  if leaves_count == BRANCH_WIDTH:
    create_interior_dirty_template()
    interiors.add(interior)
    leaves[0] = n
    leaves_count = 1
  else:
    leaves[leaves_count] = n
    leaves_count += 1

proc to_sumtree*[D, S](its: openArray[D]): SumTreeRef[D, S] =
  if its.len == 0:
    return init_sumtree[D, S](STLeaf)
  if its.len <= BUFFER_WIDTH:
    var n = init_sumtree[D, S](STLeaf)
    for idx in 0..<its.len:
      n.data[idx] = its[idx]
    n.data_count = its.len
    n.size = its.len
    n.resummarize()
    return n
  var
    i = 0
    adj_size = its.len
    n: SumTreeRef[D, S]
    leaves_count = 0
    leaves: array[BRANCH_WIDTH, SumTreeRef[D, S]]
    interiors: seq[SumTreeRef[D, S]]
  while adj_size > BUFFER_WIDTH:
    adj_size -= BUFFER_WIDTH
    n = init_sumtree[D, S](STLeaf)
    for idx in 0..<BUFFER_WIDTH:
      n.data[idx] = its[i + idx]
    n.data_count = BUFFER_WIDTH
    n.resummarize()
    n.size = BUFFER_WIDTH
    i += BUFFER_WIDTH
    add_leaf_dirty_template()
  if adj_size > 0:
    n = init_sumtree[D, S](STLeaf)
    for idx in 0..<adj_size:
      n.data[idx] = its[i + idx]
    n.data_count = adj_size
    n.resummarize()
    n.size = n.data_count
    add_leaf_dirty_template()
  if leaves_count > 0:
    create_interior_dirty_template()
    interiors.add(interior)
  if interiors.len == 0:
    case leaves_count:
      of 0:
        return init_sumtree[D, S](STLeaf)
      of 1:
        return leaves[0]
      else:
        create_interior_dirty_template()
        return interior
  if interiors.len == 1: return interiors[0]
  while interiors.len > 0:
    i = 0
    adj_size = interiors.len
    var
      interior: SumTreeRef[D, S]
      interiors2: seq[SumTreeRef[D, S]]
    while adj_size > BRANCH_WIDTH:
      adj_size -= BRANCH_WIDTH
      n = init_sumtree[D, S](STInterior)
      n.size = 0
      n.depth = 0
      for idx in 0..<BRANCH_WIDTH:
        interior = interiors[i + idx]
        n.nodes[idx] = interior
        n.size += interior.size
        n.depth = max(n.depth, interior.depth)
      n.depth += 1
      n.nodes_count = BRANCH_WIDTH
      n.resummarize()
      i += BRANCH_WIDTH
      interiors2.add(n)
    if adj_size > 0:
      n = init_sumtree[D, S](STInterior)
      n.depth = 0
      for idx in 0..<adj_size:
        interior = interiors[i + idx]
        n.nodes[idx] = interior
        n.size += interior.size
        n.depth = max(n.depth, interior.depth)
      n.depth += 1
      n.nodes_count = adj_size
      n.resummarize()
      interiors2.add(n)
    if interiors2.len == 1:
      return interiors2[0]
    else:
      interiors = interiors2


## Assumes that bounds checks have already been performed
proc shift_nodes*[D, S](s: SumTreeRef[D, S]) =
  for i in countdown(s.nodes_count, 1):
    s.nodes[i] = s.nodes[i - 1]

## Assumes that bounds checks have already been performed
proc shift_data*[D, S](s: SumTreeRef[D, S]) =
  for i in countdown(s.data_count, 1):
    s.data[i] = s.data[i - 1]

proc depth_safe*[D, S](s: SumTreeRef[D, S]): uint8 =
  if s.kind == STLeaf:
    return 0
  return s.depth

proc concat*[D, S](s1, s2: SumTreeRef[D, S]): SumTreeRef[D, S] =
  if s2.size == 0: return s1
  if s1.size == 0: return s2
  # TODO - take depth into account to try not to be too imbalanced
  var root: SumTreeRef[D, S]
  let kinds = (s1.kind, s2.kind)
  if kinds == (STLeaf, STLeaf):
    if s1.data_count + s2.data_count <= BUFFER_WIDTH:
      # pack the contents of both nodes into a new one
      root = init_sumtree[D, S](STLeaf)
      for i in 0..<s1.data_count:
        root.data[i] = s1.data[i] 
      for i in 0..<s2.data_count:
        root.data[i + s1.data_count] = s2.data[i]
      root.data_count = s1.data_count + s2.data_count
    else:
      # make the nodes children of a new one
      root = init_sumtree[D, S](STInterior)
      root.nodes[0] = s1
      root.nodes[1] = s2
      root.nodes_count = 2
      root.depth = 1
  elif kinds == (STLeaf, STInterior):
    var
      stack = s2.get_stack_to_leaf_at_index(0)
      child: SumTreeRef[D, S] 
      n_clone: SumTreeRef[D, S] 
      (n, i) = stack.pop()
    if s1.data_count + n.data_count <= BUFFER_WIDTH:
      child = init_sumtree[D, S](STLeaf)
      for i in 0..<s1.data_count:
        child.data[i] = s1.data[i] 
      for i in 0..<n.data_count:
        child.data[i + s1.data_count] = n.data[i]
      child.data_count = s1.data_count + n.data_count
      return shadow(stack, child)
    (n, i) = stack.pop()
    while true:
      if n.nodes_count < BRANCH_WIDTH:
        n_clone = n.clone()
        n_clone.shift_nodes
        n_clone.nodes[0] = s1
        n_clone.nodes_count += 1
        return shadow(stack, n_clone)
      elif stack.len == 0:
        root = init_sumtree[D, S](STInterior)
        root.nodes[0] = s1
        root.nodes[1] = s2
        root.nodes_count = 2
        root.depth = max(s1.depth_safe, s2.depth_safe) + 1
        break
      else:
        (n, i) = stack.pop()
  elif kinds == (STInterior, STLeaf):
    var
      stack = s1.get_stack_to_leaf_at_index(s1.size - 1)
      child: SumTreeRef[D, S] 
      n_clone: SumTreeRef[D, S] 
      (n, i) = stack.pop()
    if n.data_count + s2.data_count <= BUFFER_WIDTH:
      child = init_sumtree[D, S](STLeaf)
      for i in 0..<n.data_count:
        child.data[i] = n.data[i] 
      for i in 0..<s2.data_count:
        child.data[i + n.data_count] = s2.data[i]
      child.data_count = n.data_count + s2.data_count
      return shadow(stack, child)
    (n, i) = stack.pop()
    while true:
      if n.nodes_count < BRANCH_WIDTH:
        n_clone = n.clone()
        n_clone.nodes[n_clone.nodes_count] = s2
        n_clone.nodes_count += 1
        return shadow(stack, n_clone)
      elif stack.len == 0:
        root = init_sumtree[D, S](STInterior)
        root.nodes[0] = s1
        root.nodes[1] = s2
        root.nodes_count = 2
        root.depth = max(s1.depth_safe, s2.depth_safe) + 1
        break
      else:
        (n, i) = stack.pop()
  elif kinds == (STInterior, STInterior):
    root = init_sumtree[D, S](STInterior)
    if s1.nodes_count + s2.nodes_count <= BRANCH_WIDTH:
      # pack the contents of both nodes into this one
      root = init_sumtree[D, S](STInterior)
      var n: SumTreeRef[D, S]
      for i in 0..<s1.nodes_count:
        n = s1.nodes[i]
        root.nodes[i] = n
        root.depth = max(root.depth, n.depth_safe)
      for i in 0..<s2.nodes_count:
        n = s2.nodes[i]
        root.nodes[i + s1.nodes_count] = n
        root.depth = max(root.depth, n.depth_safe)
      root.nodes_count = s1.nodes_count + s2.nodes_count
      root.depth += 1
    else:
      # add the nodes as children of this one
      root.nodes[0] = s1
      root.nodes[1] = s2
      root.nodes_count = 2
      root.depth = max(s1.depth, s2.depth) + 1
  root.summary = s1.summary + s2.summary
  root.size = s1.size + s2.size
  return root
template `&`*[D, S](s1, s2: SumTreeRef[D, S]): SumTreeRef[D, S] = s1.concat(s2)

## TODO - Mutates in place where safe to do so.
proc normalize*[D, S](s: SumTreeRef[D, S]) =
  if s.kind == STLeaf: return
  # TODO - normalize based on counts and depth
  discard

template mut_pop_case_1*[D, S](s: SumTreeRef[D, S]) =
  ## The node is a leaf
  s.data_count -= 1
  s.data[s.data_count] = default(D)
  s.size -= 1
  s.summary = S.from_buf(s.data, s.data_count)

proc mut_pop_case_2*[D, S](s, child: SumTreeRef[D, S]) =
  ## The node is an interior
  s.nodes_count -= 1
  let child = s.nodes[s.nodes_count]
  s.nodes[s.nodes_count] = default(D)
  s.size -= 1
  s.summary = s.summary - child.summary
  var depth = 0
  for i in 0..<s.nodes_count:
    depth = max(s.nodes[i].depth, depth)
  s.depth = depth + 1

proc mut_append*[D, S](s: SumTreeRef[D, S], d: D) =
  var n = s
  var stack: seq[SumTreeRef]
  while n.kind == STInterior:
    if n.nodes_count == 0:
      # The node is being used to express a gap (sparse arr).
      # So we have to backtrack and add a child to the parent.
      n = stack.pop()
      while n.nodes_count == BRANCH_WIDTH:
        n = stack.pop()
        if n.isNil:
          # There is no more room at the end of any of the SumTrees
          let s_clone = s.clone()
          var new_st = init_sumtree(d)
          s.reset()
          s.mut_append_case_2(s_clone)
          s.mut_append_case_2(new_st)
          return
      # Add a child
      var new_st = init_sumtree(d)
      n.nodes[n.nodes_count] = new_st
      n.nodes_count += 1
      # Walk up what's left of the stack to increase the size and fix summaries
      while n.isNil.not:
        n.size += 1
        n.resummarize()
        n = stack.pop()
      return
    stack.add(n)
    n = n.nodes[n.nodes_count - 1]
  if n.data_count < BUFFER_WIDTH:
    n.mut_append_case_1(d)
  else:
    let s_clone = s.clone()
    var new_st = init_sumtree(d)
    s.reset()
    s.kind = STInterior
    s.mut_append_case_2(s_clone)
    s.mut_append_case_2(new_st)

template mut_prepend_case_1*[D, S](s: SumTreeRef[D, S], d: D) =
  ## The node is a leaf and there's room in the data
  s.shift_data
  s.data[0] = d
  s.data_count += 1
  s.size += 1
  s.summary = S.from_buf(s.data, s.data_count)

proc mut_prepend_case_2*[D, S](s, child: SumTreeRef[D, S]) =
  ## The node is an interior with room for a new child
  s.shift_nodes
  s.nodes_count += 1
  s.size += 1
  s.summary = s.summary + child.summary
  if s.depth <= child.depth:
    s.depth = child.depth + 1

proc im_append_case_1*[D, S](s: SumTreeRef[D, S], d: D): SumTreeRef[D, S] =
  ## The node is a leaf and there's room in the data
  var new_st = s.clone
  new_st.mut_append_case_1(d)
  return new_st

proc im_append_case_2*[D, S](s: SumTreeRef[D, S], d: D): SumTreeRef[D, S] =
  ## The node is a leaf and there's room in the data
  var new_st = init_sumtree[D, S](STInterior)
  var new_leaf = init_sumtree[D, S](d)
  new_st.mut_append_case_2(s)
  new_st.mut_append_case_2(new_leaf)
  return new_st

proc im_append*[D, S](s: SumTreeRef[D, S], d: D): SumTreeRef[D, S] =
  var
    n = s
    stack: seq[SumTreeRef[D, S]]
  while n.kind == STInterior:
    if n.nodes_count == 0:
      # The node is being used to express a gap (sparse arr).
      # So we have to backtrack and add a child to the parent.
      n = stack.pop()
      while n.nodes_count == BRANCH_WIDTH:
        n = stack.pop()
        if n.isNil:
          # There is no more room at the end of any of the SumTrees
          var new_st = init_sumtree[D, S](STInterior)
          var new_leaf = init_sumtree[D, S](d)
          new_st.mut_append_case_2(s)
          new_st.mut_append_case_2(new_leaf)
          return new_st
      # Add a child
      var new_child = init_sumtree[D, S](d)
      # Walk up what's left of the stack to increase the size and fix summaries
      while n.isNil.not:
        var n_clone = n.clone()
        n_clone.nodes[n.nodes_count - 1] = new_child
        n_clone.size += 1
        n_clone.resummarize()
        new_child = n_clone
        n = stack.pop()
      return new_child
    stack.add(n)
    n = n.nodes[n.nodes_count - 1]
  var new_child: SumTreeRef[D, S]
  if n.data_count < BUFFER_WIDTH:
    new_child = n.im_append_case_1(d)
  else:
    new_child = n.im_append_case_2(d)
  if stack.len > 0:
    n = stack.pop()
  # Walk up what's left of the stack to increase the size and fix summaries
  while stack.len > 0:
    var n_clone = n.clone()
    n_clone.nodes[n.nodes_count - 1] = new_child
    n_clone.size += 1
    n_clone.resummarize()
    new_child = n_clone
    n = stack.pop()
  return new_child

proc im_prepend_case_1*[D, S](s: SumTreeRef[D, S], d: D): SumTreeRef[D, S] =
  ## The node is a leaf and there's room in the data
  var new_st = s.clone
  new_st.mut_prepend_case_1(d)
  return new_st

proc im_prepend_case_2*[D, S](s: SumTreeRef[D, S], d: D): SumTreeRef[D, S] =
  ## The node is a leaf and there's room in the data
  var new_st = init_sumtree[D, S](STInterior)
  var new_leaf = init_sumtree[D, S](d)
  new_st.mut_prepend_case_2(s)
  new_st.mut_prepend_case_2(new_leaf)
  return new_st

proc im_prepend*[D, S](s: SumTreeRef[D, S], d: D): SumTreeRef[D, S] =
  var
    n = s
    stack: seq[SumTreeRef[D, S]]
  while n.kind == STInterior:
    if n.nodes_count == 0:
      # The node is being used to express a gap (sparse arr).
      # So we have to backtrack and add a child to the parent.
      n = stack.pop()
      while n.nodes_count == BRANCH_WIDTH:
        n = stack.pop()
        if n.isNil:
          # There is no more room at the end of any of the SumTrees
          var new_st = init_sumtree[D, S](STInterior)
          var new_leaf = init_sumtree[D, S](d)
          new_st.mut_prepend_case_2(s)
          new_st.mut_prepend_case_2(new_leaf)
          return new_st
      # Add a child
      var new_child = init_sumtree[D, S](d)
      # Walk up what's left of the stack to increase the size and fix summaries
      while n.isNil.not:
        var n_clone = n.clone()
        n_clone.nodes[n.nodes_count - 1] = new_child
        n_clone.size += 1
        n_clone.resummarize()
        new_child = n_clone
        n = stack.pop()
      return new_child
    stack.add(n)
    n = n.nodes[n.nodes_count - 1]
  var new_child: SumTreeRef[D, S]
  if n.data_count < BUFFER_WIDTH:
    new_child = n.im_prepend_case_1(d)
  else:
    new_child = n.im_prepend_case_2(d)
  if stack.len > 0:
    n = stack.pop()
  # Walk up what's left of the stack to increase the size and fix summaries
  while stack.len > 0:
    var n_clone = n.clone()
    n_clone.nodes[n.nodes_count - 1] = new_child
    n_clone.size += 1
    n_clone.resummarize()
    new_child = n_clone
    n = stack.pop()
  return new_child

iterator nodes_pre_order*[D, S](s: SumTreeRef[D, S]): SumTreeRef[D, S] =
  # yield after we push onto the stack
  var
    n = s
    idx: Natural
    n_stack: seq[SumTreeRef[D, S]]
    idx_stack: seq[Natural]
  if s.kind == STLeaf:
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
      if n.kind == STLeaf:
        discard n_stack.pop()
        discard idx_stack.pop()
        idx_stack[^1] += 1
      else:
        if n.nodes_count == 0:
          # The node is empty but is being used to indicate a sparse section in the arr
          discard n_stack.pop()
          discard idx_stack.pop()
          idx_stack[^1] += 1
        elif idx < n.nodes_count:
          # We haven't reached the end of the node's children
          n_stack.add(n.nodes[idx])
          yield n_stack[^1]
          idx_stack.add(0)
        else:
          # We reached the end of the node's children
          discard n_stack.pop()
          discard idx_stack.pop()
          idx_stack[^1] += 1

iterator nodes_post_order*[D, S](s: SumTreeRef[D, S]): SumTreeRef[D, S] =
  # yield before we pop off the stack
  var
    n = s
    idx: Natural
    n_stack: seq[SumTreeRef[D, S]]
    idx_stack: seq[Natural]
  if s.kind == STLeaf:
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
      if n.kind == STLeaf:
        yield n
        discard n_stack.pop()
        discard idx_stack.pop()
        idx_stack[^1] += 1
      else:
        if n.nodes_count == 0:
          # The node is empty but is being used to indicate a sparse section in the arr
          yield n
          discard n_stack.pop()
          discard idx_stack.pop()
          idx_stack[^1] += 1
        elif idx < n.nodes_count:
          # We haven't reached the end of the node's children
          n_stack.add(n.nodes[idx])
          idx_stack.add(0)
        else:
          # We reached the end of the node's children
          yield n
          discard n_stack.pop()
          discard idx_stack.pop()
          idx_stack[^1] += 1

iterator leaves_and_sparse_nodes_in_order*[D, S](s: SumTreeRef[D, S]): SumTreeRef[D, S] =
  var
    n = s
    idx: Natural
    n_stack: seq[SumTreeRef[D, S]]
    idx_stack: seq[Natural]
  if s.kind == STLeaf:
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
      if n.kind == STLeaf:
        yield n
        discard n_stack.pop()
        discard idx_stack.pop()
        idx_stack[^1] += 1
      else:
        if n.nodes_count == 0:
          # The node is empty but is being used to indicate a sparse section in the arr
          yield n
          discard n_stack.pop()
          discard idx_stack.pop()
          idx_stack[^1] += 1
        elif idx < n.nodes_count:
          # We haven't reached the end of the node's children
          n_stack.add(n.nodes[idx])
          idx_stack.add(0)
        else:
          # We reached the end of the node's children
          discard n_stack.pop()
          discard idx_stack.pop()
          idx_stack[^1] += 1

template iterate_pairs*[D, S](s: SumTreeRef[D, S]) {.dirty.} =
  var total_idx = 0
  for n in s.leaves_and_sparse_nodes_in_order:
    if n.kind == STLeaf:
      for i in 0..<n.data_count:
        yield (total_idx, n.data[i])
        total_idx += 1
    else:
      # sparse node
      for i in 0..<n.size:
        yield (total_idx, default(D))
        total_idx += 1

iterator pairs*[D, S](s: SumTreeRef[D, S]): (int, D) =
  iterate_pairs(s)
iterator items*[D, S](s: SumTreeRef[D, S]): D =
  for (idx, d) in s.pairs:
    yield d
proc pairs_closure[D, S](s: SumTreeRef[D, S]): iterator(): (int, D) =
  return iterator(): (int, D) =
    iterate_pairs(s)

proc compute_local_summary*[D, S](s: SumTreeRef[D, S]): S =
  if s.kind == STInterior:
    result = S.zero()
    for i in 0..<s.nodes_count:
      result = result + s.nodes[i].summary
  else:
    result = S.from_buf(s.data, s.data_count)

proc compute_local_size[D, S](s: SumTreeRef[D, S]): int =
  if s.kind == STLeaf:
    return s.data_count
  else:
    var computed_size = 0
    for i in 0..<s.nodes_count:
      computed_size += s.nodes[i].size
    return computed_size

proc compute_local_depth[D, S](s: SumTreeRef[D, S]): uint8 =
  if s.kind == STLeaf:
    return 0
  else:
    var computed_depth: uint8 = 0
    var n: SumTreeRef[D, S]
    for i in 0..<s.nodes_count:
      n = s.nodes[i]
      if n.kind == STInterior:
        computed_depth = max(computed_depth, n.depth)
    return computed_depth + 1

proc valid*[D, S](s: SumTreeRef[D, S]): bool =
  for n in s.nodes_post_order:
    if n.size != n.compute_local_size: return false
    if n.summary != n.compute_local_summary: return false
    if n.kind == STInterior:
      if n.depth == 0: return false
      if n.depth != n.compute_local_depth: return false
      if n.nodes_count == 1 and n.nodes[0].kind == STInterior: return false
  return true

template len*[D, S](s: SumTreeRef[D, S]): Natural = s.size

proc `==`*[D](v1, v2: PVecRef[D]): bool =
  if v1.size != v2.size: return false
  if v1.summary.hash != v2.summary.hash: return false
  var
    t1 = v1.pairs_closure()
    t2 = v2.pairs_closure()
    fin: bool
  while true:
    fin = finished(t1)
    if fin != finished(t2): return false
    if t1() != t2(): return false
    if fin: return true
  
template init_vec*[T](): PVecRef[T] = init_sumtree[T, PVecSummary[T]](STLeaf)
template to_vec*[T](items: openArray[T]): PVecRef[T] = to_sumtree[T, PVecSummary[T]](items)

template append*[T](vec: PVecRef[T], item: T): PVecRef[T] = vec.im_append(item)
template push*[T](vec: PVecRef[T], item: T): PVecRef[T] = vec.im_append(item)

template prepend*[T](vec: PVecRef[T], item: T): PVecRef[T] = vec.im_prepend(item)
template push_front*[T](vec: PVecRef[T], item: T): PVecRef[T] = vec.im_prepend(item)

template pop*[T](vec: PVecRef[T]): (PVecRef[T], T) = vec.im_pop()

template set*[T](vec: PVecRef[T], idx: int, item: T): PVecRef[T] = vec.im_set(idx, item)

