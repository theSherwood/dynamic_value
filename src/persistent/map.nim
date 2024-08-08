##[

https://github.com/paranim/parazoa/blob/master/src/parazoa.nim

I had to fix some bugs (when using int keys).
Though I'm pretty sure it's still broken as I don't think there's any handling
of hash collisions.

]##

import std/[tables, sets, bitops, strutils, sequtils, strformat, macros]
import hashes
from strutils import nil
import chunk
export chunk

const c32 = defined(cpu32)

const
  INDEX_BITS* {.intdefine.} = 5
  BRANCH_WIDTH = 1 shl INDEX_BITS
  MASK = BRANCH_WIDTH - 1
  ARRAY_WIDTH = BRANCH_WIDTH shr 1

type
  KeyError* = object of CatchableError
  IndexError* = object of CatchableError

  NodeKind = enum
    Array,
    Interior,

  HashedEntry[K, V] = object
    hash: Hash
    entry: tuple[key: K, value: V]
  HashedEntryBox[K, V] = ref object
    hashed_entry: HashedEntry[K, V]
  
  NodeListEntryKind = enum
    kEmpty    # If we implement HAMT, get rid of this
    kInterior
    kCollision
    kLeaf

  NodeListEntry[K, V] = object
    case kind: NodeListEntryKind:
      of kEmpty:
        discard
      of kInterior:
        node: MapNodeRef[K, V]
      of kCollision:
        hashed_entries: seq[HashedEntry[K, V]]
      of kLeaf:
        hashed_entry: HashedEntryBox[K, V]

  MapNode*[K, V] = object
    case kind: NodeKind:
      of Array:
        entries: Chunk[ARRAY_WIDTH, HashedEntry[K, V]]
      of Interior:
        count: uint8
        nodes: array[BRANCH_WIDTH, NodeListEntry[K, V]]
  MapNodeRef*[K, V] = ref MapNode[K, V]

  PMap*[K, V] = object
    node*: MapNode[K, V]
    hash*: Hash
    size*: Natural
  PMapRef*[K, V] = ref PMap[K, V]

  PathStack[K, V] = Chunk[32, tuple[parent: MapNodeRef[K, V], index: int]]
  
  UpdateFn[V] = proc(v: V, exists: bool): (V, bool)

func copy_interior_node*[K, V](m: MapNodeRef[K, V]): MapNodeRef[K, V] =
  result = MapNodeRef[K, V](kind: Interior)
  result.count = m.count
  result.nodes = m.nodes

func `$`[K, V](n: MapNodeRef[K, V]): string =
  return $(n[])
func `$`[K, V](s: PathStack[K, V]): string =
  var x = newSeq[string]()
  for t in s:
    x.add($t.parent & "\n||| " & $t.index)
  "{" & strutils.join(x, " \n\n ") & "}"

template hash*[K, V](h_entry: HashedEntryBox[K, V]): Hash =
  h_entry.hashed_entry.hash
template unbox*[K, V](h_entry: HashedEntryBox[K, V]): HashedEntry[K, V] =
  h_entry.hashed_entry

template key*[K, V](h_entry: HashedEntryBox[K, V]): K =
  h_entry.hashed_entry.entry.key
template value*[K, V](h_entry: HashedEntryBox[K, V]): V =
  h_entry.hashed_entry.entry.value
template key*[K, V](h_entry: HashedEntry[K, V]): K =
  h_entry.entry.key
template value*[K, V](h_entry: HashedEntry[K, V]): V =
  h_entry.entry.value

when c32:
  const TOP_BIT_MASK = cast[Hash](0x3fffffff'u32)
else:
  const TOP_BIT_MASK = cast[Hash](0x3fffffffffffffff'u64)

template entry_hash*[K, V](h_entry: HashedEntryBox[K, V]): Hash =
  (h_entry.hash and TOP_BIT_MASK) + (hash(h_entry.value) and TOP_BIT_MASK)
template entry_hash*[K, V](h_entry: HashedEntry[K, V]): Hash =
  (h_entry.hash and TOP_BIT_MASK) + (hash(h_entry.value) and TOP_BIT_MASK)

func init_map*[K, V](): PMapRef[K, V]  =
  ## Returns a new `PMap`
  result = PMapRef[K, V]()
  result.node = MapNode[K, V](kind: Array)

func len*[K, V](m: PMapRef[K, V]): Natural =
  ## Returns the number of key-value pairs in the `PMap`
  m.size

iterator interior_nodes*[K, V](m: PMapRef[K, V]): MapNodeRef[K, V] =
  if m.node.kind == Interior:
    var
      node = cast[MapNodeRef[K, V]](m.node.addr)
      node_list_entry: NodeListEntry[K, V]
      stack: PathStack[K, V]
    stack.add((node, 0))
    yield node
    while stack.len > 0:
      let (parent, index) = stack[stack.len-1]
      if index == parent.nodes.len:
        discard stack.pop()
        if stack.len > 0:
          stack[stack.len-1].index += 1
      else:
        node_list_entry = parent.nodes[index]
        if node_list_entry.kind == kInterior:
          yield node_list_entry.node
          stack.add((node_list_entry.node, 0))
        else:
          stack[stack.len-1].index += 1

iterator hashed_entries*[K, V](m: PMapRef[K, V]): HashedEntry[K, V] =
  ## Iterates over the hash-key-value triples in the `PMap`
  if m.node.kind == Array:
    for h_entry in m.node.entries:
      yield h_entry
  else:
    var
      node = cast[MapNodeRef[K, V]](m.node.addr)
      node_list_entry: NodeListEntry[K, V]
      stack: PathStack[K, V]
    stack.add((node, 0))
    while stack.len > 0:
      let (parent, index) = stack[stack.len-1]
      if index == parent.nodes.len:
        discard stack.pop()
        if stack.len > 0:
          stack[stack.len-1].index += 1
      else:
        node_list_entry = parent.nodes[index]
        case node_list_entry.kind:
          of kEmpty:
            stack[stack.len-1].index += 1
          of kLeaf:
            yield node_list_entry.hashed_entry.hashed_entry
            stack[stack.len-1].index += 1
          of kCollision:
            for h_entry in node_list_entry.hashed_entries:
              yield h_entry
            stack[stack.len-1].index += 1
          of kInterior:
            stack.add((node_list_entry.node, 0))

iterator pairs*[K, V](m: PMapRef[K, V]): (K, V) =
  ## Iterates over the key-value entries in the `PMap`
  for h_entry in m.hashed_entries:
    yield h_entry.entry
iterator keys*[K, V](m: PMapRef[K, V]): K =
  ## Iterates over the keys in the `PMap`
  for h_entry in m.hashed_entries:
    yield h_entry.key
iterator values*[K, V](m: PMapRef[K, V]): V =
  ## Iterates over the values in the `PMap`
  for h_entry in m.hashed_entries:
    yield h_entry.value
iterator items*[K, V](m: PMapRef[K, V]): V =
  ## Iterates over the values in the `PMap`
  for h_entry in m.hashed_entries:
    yield h_entry.value

template mut_add_to_interior_map[K, V](m: PMapRef[K, V], h_entry: HashedEntry[K, V]): untyped =
  var
    h = h_entry.hash
    bits = 0
    parent = cast[MapNodeRef[K, V]](m.node.addr)
    index = (h shr bits) and MASK
    node_list_entry: NodeListEntry[K, V]
  m.size += 1
  m.hash = m.hash xor entry_hash(h_entry)
  block outer:
    while true:
      node_list_entry = parent.nodes[index]
      case node_list_entry.kind:
        of kInterior:
          parent = node_list_entry.node
          bits += INDEX_BITS
          index = (h shr bits) and MASK
        of kEmpty:
          parent.nodes[index] = NodeListEntry[K, V](
            kind: kLeaf,
            hashed_entry: HashedEntryBox[K, V](hashed_entry: h_entry)
          )
          parent.count += 1
          break outer
        of kLeaf:
          let existing_entry = node_list_entry.hashed_entry
          if h_entry.hash == existing_entry.hash:
            # we have a hash collision
            if h_entry.key == existing_entry.key:
              m.hash = m.hash xor entry_hash(h_entry)
              m.size -= 1
              if h_entry.value == existing_entry.value:
                # bail early because we have an exact match
                break outer
              else:
                # overwrite the existing entry
                parent.nodes[index] = NodeListEntry[K, V](
                  kind: kLeaf,
                  hashed_entry: HashedEntryBox[K, V](hashed_entry: h_entry)
                )
                break outer
            else:
              parent.nodes[index] = NodeListEntry[K, V](
                kind: kCollision,
                hashed_entries: @[h_entry, existing_entry.unbox],
              )
              break outer
          else:
            # we have to expand to an Interior node because our leaf was a shortcut
            # and we don't create collisions at shortcuts
            bits += INDEX_BITS
            var
              new_node = MapNodeRef[K, V](kind: Interior)
              curr_node = new_node
              new_idx_for_h_entry = (h shr bits) and MASK
              new_idx_for_existing_entry = (existing_entry.hash shr bits) and MASK
            block inner:
              while true:
                if new_idx_for_h_entry == new_idx_for_existing_entry:
                  if bits < BRANCH_WIDTH:
                    # keep building deeper
                    var new_node = MapNodeRef[K, V](kind: Interior)
                    curr_node.nodes[new_idx_for_h_entry] = NodeListEntry[K, V](kind: kInterior, node: new_node)
                    curr_node = new_node
                    bits += INDEX_BITS
                    new_idx_for_h_entry = (h shr bits) and MASK
                    new_idx_for_existing_entry = (existing_entry.hash shr bits) and MASK
                  else:
                    # build collision
                    curr_node.nodes[new_idx_for_h_entry] = NodeListEntry[K, V](
                      kind: kCollision,
                      hashed_entries: @[h_entry, existing_entry.unbox]
                    )
                    curr_node.count = 1
                    break inner
                else:
                  curr_node.nodes[new_idx_for_h_entry] = NodeListEntry[K, V](
                    kind: kLeaf,
                    hashed_entry: HashedEntryBox[K, V](hashed_entry: h_entry)
                  )
                  curr_node.nodes[new_idx_for_existing_entry] = NodeListEntry[K, V](
                    kind: kLeaf,
                    hashed_entry: HashedEntryBox[K, V](hashed_entry: existing_entry.unbox)
                  )
                  curr_node.count = 2
                  break inner
            parent.nodes[index] = NodeListEntry[K, V](kind: kInterior, node: new_node)
            break outer
        of kCollision:
          var new_entries = @[h_entry]
          for e in node_list_entry.hashed_entries:
            if e.key == h_entry.key:
              m.hash = m.hash xor entry_hash(e)
              m.size -= 1
              if e.value == h_entry.value:
                break outer
            else:
              new_entries.add(e)
          parent.nodes[index] = NodeListEntry[K, V](
            kind: kCollision,
            hashed_entries: new_entries
          )
          break outer

func to_interior_map[K, V](pairs: openArray[(K, V)]): PMapRef[K, V] =
  result = PMapRef[K, V]()
  result.node = MapNode[K, V](kind: Interior)
  for p in pairs:
    mut_add_to_interior_map(result, HashedEntry[K, V](hash: hash(p[0]), entry: p))
func to_interior_map[K, V](entries: openArray[HashedEntry[K, V]]): PMapRef[K, V] =
  result = PMapRef[K, V]()
  result.node = MapNode[K, V](kind: Interior)
  for e in entries:
    mut_add_to_interior_map(result, e)
func to_interior_map[K, V](map: PMapRef[K, V]): PMapRef[K, V] =
  result = PMapRef[K, V]()
  result.node = MapNode[K, V](kind: Interior)
  for e in map.hashed_entries:
    mut_add_to_interior_map(result, e)

template add_to_array_map*[K, V](m: PMapRef[K, V], h_entry: HashedEntry[K, V]): untyped  =
  result.node = MapNode[K, V](kind: Array)
  result.hash = m.hash xor entry_hash(h_entry)
  result.node.entries.add(h_entry)
  for e in m.hashed_entries:
    if e.hash == h_entry.hash and e.key == h_entry.key:
      if e.value == h_entry.value:
        # bail because the entry is an exact copy of an existing entry
        return m
      # matching key, so we remove it from the hash
      result.hash = result.hash xor entry_hash(e)
    else:
      result.node.entries.add(e)
  result.size = result.node.entries.len

func add_impl*[K, V](m: PMapRef[K, V], h_entry: HashedEntry[K, V]): PMapRef[K, V]  =
  result = PMapRef[K, V]()
  if m.size < ARRAY_WIDTH:
    add_to_array_map[K, V](m, h_entry)
  elif m.node.kind == Array:
    # We have an array but are at the size limit
    if m.contains(h_entry.hash, h_entry.key):
      add_to_array_map[K, V](m, h_entry)
    else:
      result = to_interior_map(m)
      ## TODO - don't do an immutable add here
      result = result.add_impl(h_entry)
  elif m.node.kind == Interior:
    result = PMapRef[K, V]()
    result.node = m.node
    result.hash = m.hash xor entry_hash(h_entry)
    result.size = m.size + 1
    var
      h = h_entry.hash
      bits = 0
      node: MapNodeRef[K, V]
      parent = cast[MapNodeRef[K, V]](result.node.addr)
      index = (h shr bits) and MASK
      node_list_entry = parent.nodes[index]
    while true:
      case node_list_entry.kind:
        of kEmpty:
          parent.nodes[index] = NodeListEntry[K, V](
            kind: kLeaf,
            hashed_entry: HashedEntryBox[K, V](hashed_entry: h_entry)
          )
          parent.count += 1
          return result
        of kLeaf:
          let existing_entry = node_list_entry.hashed_entry
          if h_entry.hash == existing_entry.hash:
            # we have a hash collision
            if h_entry.key == existing_entry.key:
              if h_entry.value == existing_entry.value:
                # bail early because we have an exact match
                return m
              else:
                # overwrite the existing entry
                parent.nodes[index] = NodeListEntry[K, V](
                  kind: kLeaf,
                  hashed_entry: HashedEntryBox[K, V](hashed_entry: h_entry)
                )
                result.hash = result.hash xor entry_hash(existing_entry)
                result.size -= 1
                return result
            else:
              parent.nodes[index] = NodeListEntry[K, V](
                kind: kCollision,
                hashed_entries: @[h_entry, existing_entry.unbox]
              )
              return result
          else:
            # we have to expand to an Interior node because our leaf was a shortcut
            # and we don't create collisions at shortcuts
            bits += INDEX_BITS
            var
              new_node = MapNodeRef[K, V](kind: Interior)
              curr_node = new_node
              new_idx_for_h_entry = (h shr bits) and MASK
              new_idx_for_existing_entry = (existing_entry.hash shr bits) and MASK
            while true:
              if new_idx_for_h_entry == new_idx_for_existing_entry:
                if bits < BRANCH_WIDTH:
                  # keep building deeper
                  var new_node = MapNodeRef[K, V](kind: Interior)
                  curr_node.nodes[new_idx_for_h_entry] = NodeListEntry[K, V](kind: kInterior, node: new_node)
                  curr_node.count = 1
                  curr_node = new_node
                  bits += INDEX_BITS
                  new_idx_for_h_entry = (h shr bits) and MASK
                  new_idx_for_existing_entry = (existing_entry.hash shr bits) and MASK
                else:
                  # build collision
                  curr_node.nodes[new_idx_for_h_entry] = NodeListEntry[K, V](
                    kind: kCollision,
                    hashed_entries: @[h_entry, existing_entry.unbox]
                  )
                  curr_node.count = 1
                  break
              else:
                curr_node.nodes[new_idx_for_h_entry] = NodeListEntry[K, V](
                  kind: kLeaf,
                  hashed_entry: HashedEntryBox[K, V](hashed_entry: h_entry)
                )
                curr_node.nodes[new_idx_for_existing_entry] = NodeListEntry[K, V](
                  kind: kLeaf,
                  hashed_entry: HashedEntryBox[K, V](hashed_entry: existing_entry.unbox)
                )
                curr_node.count = 2
                break
            parent.nodes[index] = NodeListEntry[K, V](kind: kInterior, node: new_node)
            return result
        of kCollision:
          discard
          var new_entries = @[h_entry]
          for e in node_list_entry.hashed_entries:
            if e.key == h_entry.key:
              result.hash = result.hash xor entry_hash(e)
              result.size -= 1
              if e.value == h_entry.value:
                break
            else:
              new_entries.add(e)
          parent.nodes[index] = NodeListEntry[K, V](
            kind: kCollision,
            hashed_entries: new_entries
          )
          return result
        of kInterior:
          bits += INDEX_BITS
          node = copy_interior_node(node_list_entry.node)
          parent.nodes[index] = NodeListEntry[K, V](
            kind: kInterior,
            node: node
          )
          parent = node
          index = (h shr bits) and MASK
          node_list_entry = parent.nodes[index]
template add*[K, V](m: PMapRef[K, V], key: K, value: V): PMapRef[K, V] =
  add_impl(m, HashedEntry[K, V](hash: hash(key), entry: (key, value)))
template add*[K, V](m: PMapRef[K, V], pair: (K, V)): PMapRef[K, V] =
  add_impl(m, HashedEntry[K, V](hash: hash(pair[0]), entry: pair))
template add*[K, V](m: PMapRef[K, V], he: HashedEntry[K, V]): PMapRef[K, V] =
  add_impl(m, he)
template add_by_hash*[K, V](m: PMapRef[K, V], h: Hash, key: K, value: V): PMapRef[K, V] =
  add_impl(m, HashedEntry[K, V](hash: h, entry: (key, value)))
template add_by_hash*[K, V](m: PMapRef[K, V], h: Hash, pair: (K, V)): PMapRef[K, V] =
  add_impl(m, HashedEntry[K, V](hash: h, entry: pair))

func delete_by_hash*[K, V](m: PMapRef[K, V], h: Hash, key: K): PMapRef[K, V] =
  ## Deletes the key-value pair at `key` from the `PMap`
  result = PMapRef[K, V]()
  if m.node.kind == Array:
    result.node = MapNode[K, V](kind: Array)
    result.hash = m.hash
    result.size = m.size
    for e in m.hashed_entries:
      if e.hash == h and e.key == key:
        result.hash = result.hash xor entry_hash(e)
        result.size -= 1
      else:
        result.node.entries.add(e)
    if result.size < m.size: return result
    else: return m
  else:
    result = PMapRef[K, V]()
    result.node = m.node
    result.hash = m.hash
    result.size = m.size + 1
    var
      bits = 0
      node: MapNodeRef[K, V]
      parent = cast[MapNodeRef[K, V]](result.node.addr)
      index = (h shr bits) and MASK
      node_list_entry = parent.nodes[index]
      # We have to have this stack in the case that we have a chain of single
      # interior nodes and we clip the value at the very end. That way we walk
      # back up the stack and clip off the interior nodes.
      stack: PathStack[K, V]
    while true:
      stack.add((parent, index))
      case node_list_entry.kind:
        of kEmpty:
          return m
        of kLeaf:
          result.size = m.size
          let e_entry = node_list_entry.hashed_entry
          if e_entry.hash == h and e_entry.key == key:
            result.hash = m.hash xor entry_hash(e_entry)
            if m.size == ARRAY_WIDTH + 1:
              result.node = MapNode[K, V](kind: Array)
              result.size = ARRAY_WIDTH
              for e in m.hashed_entries:
                if e.hash == h and e_entry.key == key:
                  discard
                else:
                  result.node.entries.add(e)
              return result
            elif parent.count == 1:
              var idx = stack.len - 2
              while parent.count == 1:
                (parent, index) = stack[idx]
                idx -= 1
              parent.nodes[index] = NodeListEntry[K, V](kind: kEmpty)
              parent.count -= 1
              result.size -= 1
              return result
            else:
              parent.nodes[index] = NodeListEntry[K, V](kind: kEmpty)
              parent.count -= 1
              result.size -= 1
              return result
          else:
            return m
        of kCollision:
          var new_entries: seq[HashedEntry[K, V]]
          for e in node_list_entry.hashed_entries:
            if e.key == key:
              result.hash = result.hash xor entry_hash(e)
              result.size -= 1
            else:
              new_entries.add(e)
          if new_entries.len > 1:
            parent.nodes[index] = NodeListEntry[K, V](
              kind: kCollision,
              hashed_entries: new_entries
            )
          else:
            parent.nodes[index] = NodeListEntry[K, V](
              kind: kLeaf,
              hashed_entry: HashedEntryBox[K, V](hashed_entry: new_entries[0])
            )
          return result
        of kInterior:
          bits += INDEX_BITS
          node = copy_interior_node(node_list_entry.node)
          parent.nodes[index] = NodeListEntry[K, V](
            kind: kInterior,
            node: node
          )
          parent = node
          index = (h shr bits) and MASK
          node_list_entry = parent.nodes[index]
template delete*[K, V](m: PMapRef[K, V], key: K): PMapRef[K, V] =
  delete_by_hash(m, hash(key), key)

template get_impl*[K, V](m: PMapRef[K, V], h: Hash, key: K, SUCCESS, FAILURE: untyped): untyped =
  if m.node.kind == Array:
    for h_entry in m.node.entries:
      if h_entry.hash == h and h_entry.key == key:
        SUCCESS(h_entry)
    FAILURE
  else:
    var
      bits = 0
      node = cast[MapNodeRef[K, V]](m.node.addr)
    while true:
      var
        index = (h shr bits) and MASK
        node_list_entry = node.nodes[index]
      case node_list_entry.kind:
        of kEmpty:
          break
        of kLeaf:
          let h_entry = node_list_entry.hashed_entry
          if h_entry.hash == h and h_entry.key == key:
            SUCCESS(h_entry)
          break
        of kCollision:
          for h_entry in node_list_entry.hashed_entries:
            if h_entry.hash == h and h_entry.key == key:
              SUCCESS(h_entry)
          break
        of kInterior:
          node = node_list_entry.node
          bits += INDEX_BITS
    FAILURE

template get_success(h_entry: untyped): untyped =
  return h_entry.value
template get_failure(): untyped =
  raise newException(KeyError, "Key not found")
func get*[K, V](m: PMapRef[K, V], key: K): V =
  let h = hash(key)
  get_impl[K, V](m, h, key, get_success, get_failure)
template `[]`*[K, V](m: PMapRef[K, V], key: K): V = m.get(key)
func get_by_hash*[K, V](m: PMapRef[K, V], h: Hash, key: K): V =
  get_impl[K, V](m, h, key, get_success, get_failure)

template get_or_default_failure() {.dirty.} =
  return def
func get_or_default*[K, V](m: PMapRef[K, V], key: K, def: V): V =
  let h = hash(key)
  get_impl[K, V](m, h, key, get_success, get_or_default_failure)
template get_or_default*[K, V](m: PMapRef[K, V], key: K): V =
  get_or_default[K, V](m, key, V.default)
func get_or_default_by_hash*[K, V](m: PMapRef[K, V], h: Hash, key: K, def: V): V =
  get_impl[K, V](m, h, key, get_success, get_or_default_failure)
template get_or_default_by_hash*[K, V](m: PMapRef[K, V], h: Hash, key: K): V =
  get_or_default_by_hash[K, V](m, h, key, V.default)

template get_tuple_success(h_entry: untyped): untyped =
  return (h_entry.value, true)
template get_tuple_failure(): untyped =
  return (default[V](V), false)
func get_tuple_by_hash[K, V](m: PMapRef[K, V], h: Hash, key: K): (V, bool) =
  get_impl[K, V](m, h, key, get_tuple_success, get_tuple_failure)

template contains_success(h_entry: untyped): untyped =
  return true
template contains_failure(): untyped =
  return false
func contains*[K, V](m: PMapRef[K, V], key: K): bool =
  let h = hash(key)
  get_impl[K, V](m, h, key, contains_success, contains_failure)
func contains*[K, V](m: PMapRef[K, V], h: Hash, key: K): bool =
  get_impl[K, V](m, h, key, contains_success, contains_failure)

func update_by_hash*[K, V](m: PMapRef[K, V], h: Hash, key: K, update_fn: UpdateFn[V]): PMapRef[K, V] =
  let (old_val, exists) = m.get_tuple_by_hash(h, key)
  let (new_val, should_exist) = update_fn(old_val, exists)
  if should_exist:
    return m.add_by_hash(h, key, new_val)
  elif exists:
    return m.delete_by_hash(h, key)
  else:
    return m
template update*[K, V](m: PMapRef[K, V], key: K, update_fn: UpdateFn[V]): PMapRef[K, V] =
  update_by_hash(m, hash(key), key, update_fn)

proc `==`*[K, V](m1: PMapRef[K, V], m2: PMapRef[K, V]): bool  =
  ## Returns whether the `PMap`s are equal
  if m1.isNil:
    if m2.isNil: return true
    return false
  if m2.isNil: return false
  if m1.len != m2.len: return false
  if m1.hash != m2.hash: return false
  for (k, v) in m1.pairs:
    let (m2_v, exists) = m2.get_tuple_by_hash(hash(k), k)
    if not(exists) or v != m2_v: return false
  return true

func to_map*[K, V](arr: openArray[(K, V)]): PMapRef[K, V] =
  ## Returns a `PMap` containing the key-value pairs in `arr`
  var m = init_map[K, V]()
  for (k, v) in arr:
    m = m.add(k, v)
  m

func `$`*[K, V](m: PMapRef[K, V]): string =
  ## Returns a string representing the `PMap`
  var x = newSeq[string]()
  for (k, v) in m.pairs:
    x.add($k & ": " & $v)
  "{" & strutils.join(x, ", ") & "}"

func hash*[K, V](m: PMapRef[K, V]): Hash  =
  return m.hash

func to_json*[K, V](m: PMapRef[K, V]): string =
  if m.len == 0: return "{}"
  result.add("{\n")
  block:
    for he in m.hashed_entries:
      result.add(&"  \"{he.key}\": \"{he.value}\",\n")
    # trim off the last comma because json doesn't allow trailing commas
    result.delete((result.len - 2)..<result.len)
  result.add("\n}")

proc debug_json*[K, V](he: HashedEntry[K, V]): string =
  result.add(&"{he.key}: {he.value} [{he.hash.to_hex}]")

proc debug_json*[K, V](nle: NodeListEntry[K, V]): string =
  if nle.kind == kEmpty:
    result.add("{ ")
    result.add(&"\"kind\": \"{nle.kind}\"")
    result.add(" }")
    return result
  result.add("{\n")
  result.add(&"  \"kind\": \"{nle.kind}\",\n")
  case nle.kind:
    of kEmpty: discard
    of kLeaf:
      result.add(&"  \"hashed_entry\": \"{nle.hashed_entry.unbox.debug_json}\"\n")
    of kCollision:
      let entries = nle.hashed_entries
        .map(proc (he: HashedEntry[K, V]): string = &"\"{he.debug_json}\"")
        .join(", ")
      result.add(&"  \"hashed_entries\": [{entries}]\n")
    of kInterior:
      result.add(&"  \"node\": {nle.node.debug_json}\n")
  result.add("}")

proc debug_json*[K, V](n: MapNodeRef[K, V]): string =
  result.add("{\n")
  result.add(&"  \"kind\": \"{n.kind}\",\n")
  if n.kind == Array:
    result.add(&"  \"entries\": \"{n.entries}\"")
  else:
    result.add(&"  \"count\": {n.count},\n")
    let nodes = n.nodes.map(debug_json).join(", ")
    result.add(&"  \"nodes\": [{nodes}]")
  result.add("}")

proc debug_json*[K, V](m: PMapRef[K, V]): string =
  result.add("{\n")
  result.add(&"  \"size\": {m.size},\n")
  result.add(&"  \"hash\": {m.hash},\n")
  let node = cast[MapNodeRef[K, V]](m.node.addr)
  result.add(&"  \"node\": {debug_json[K, V](node)}\n")
  result.add("}")

func valid*[K, V](m: PMapRef[K, V]): bool =
  var size = 0
  if m.node.kind == Array:
    size = m.node.entries.len
  else:
    var c: uint8 = 0
    for n in m.interior_nodes:
      c = 0
      for n_l_entry in n.nodes:
        if n_l_entry.kind != kEmpty:
          c += 1
        if n_l_entry.kind == kLeaf:
          size += 1
        if n_l_entry.kind == kCollision:
          size += n_l_entry.hashed_entries.len
      if c != n.count:
        debugEcho "node.count should be: ", c, " but got: ", n.count
        debugEcho m.debug_json
        debugEcho ""
        return false
  if size != m.len:
    debugEcho "size should be: ", size, " but got: ", m.len
    debugEcho m.debug_json
    debugEcho ""
    return false
  var hash: Hash = 0
  for he in m.hashed_entries:
    hash = hash xor entry_hash(he)
  if m.hash != hash:
    debugEcho "hash should be: ", hash, " but got: ", m.hash
    return false
  return true

func concat*[K, V](m1, m2: PMapRef[K, V]): PMapRef[K, V] =
  ## This is shamefully inneficient
  ## TODO - implement transients so that this can be faster.
  result = m1
  for he in m2.hashed_entries:
    result = result.add(he)
template `&`*[K, V](m1, m2: PMapRef[K, V]): PMapRef[K, V] =
  concat(m1, m2)

# ====================================================================
# Set
# ====================================================================

type
  EmptyValue = object
  PSet*[K] = distinct PMap[K, EmptyValue]
  PSetRef*[K] = ref PSet[K]

const empty = EmptyValue()

template as_set*[K](m: PMapRef[K, EmptyValue]): PSetRef[K] =
  cast[PSetRef[K]](m)
template as_map*[K](s: PSetRef[K]): PMapRef[K, EmptyValue] =
  cast[PMapRef[K, EmptyValue]](s)

func init_set*[K](): PSetRef[K] =
  result = init_map[K, EmptyValue]().as_set
func to_set*[K](arr: openArray[K]): PSetRef[K] =
  var m = init_map[K, EmptyValue]()
  for k in arr:
    m = m.add(k, empty)
  return m.as_set
template hash*[K](s: PSetRef[K]): Hash =
  s.as_map.hash
template incl*[K](s: PSetRef[K], key: K): PSetRef[K] =
  s.as_map.add(key, empty).as_set
template excl*[K](s: PSetRef[K], key: K): PSetRef[K] =
  s.as_map.delete(key).as_set
template contains*[K](s: PSetRef[K], key: K): bool =
  s.as_map.contains(key)
template len*[K](s: PSetRef[K]): Natural =
  s.as_map.len
iterator items*[K](s: PSetRef[K]): K =
  for k in s.as_map.keys:
    yield k
iterator values*[K](s: PSetRef[K]): K =
  for k in s.as_map.keys:
    yield k
iterator keys*[K](s: PSetRef[K]): K =
  for k in s.as_map.keys:
    yield k
func `==`*[K](s1, s2: PSetRef[K]): bool =
  if s1.isNil:
    if s2.isNil: return true
    return false
  if s2.isNil: return false
  if s1.len != s2.len: return false
  if s1.hash != s2.hash: return false
  else:
    for k in s1.items:
      if k notin s2:
        return false
    return true
template valid*[K](s: PSetRef[K]): bool =
  s.as_map.valid
func to_json*[K](s: PSetRef[K]): string =
  if s.len == 0: return "[]"
  result.add("[ ")
  block:
    for k in s.keys:
      result.add(&"\"{k}\", ")
    # trim off the last comma because json doesn't allow trailing commas
    result.delete((result.len - 2)..<result.len)
  result.add(" ]")
func `$`*[K](s: PSetRef[K]): string =
  if s.len == 0: return "{}"
  result.add("{ ")
  block:
    for k in s.keys:
      result.add(&"{k}, ")
    # trim off the last comma because json doesn't allow trailing commas
    result.delete((result.len - 2)..<result.len)
  result.add(" }")

# ====================================================================
# Multiset
# ====================================================================

type
  PMultiset*[K] = distinct PMap[K, int]
  PMultisetRef*[K] = ref PMultiset[K]

template as_mset*[K](m: PMapRef[K, int]): PMultisetRef[K] =
  cast[PMultisetRef[K]](m)
template as_map*[K](s: PMultisetRef[K]): PMapRef[K, int] =
  cast[PMapRef[K, int]](s)

func `[]`*[K](s: PMultisetRef[K]): PMap[K, int] =
  return as_map(s)[]

func multiset_incl_update*[V](v: V, exists: bool): (V, bool) =
  if v == -1: return (0, false)
  return (v + 1, true)
func multiset_excl_update*[V](v: V, exists: bool): (V, bool) =
  if v == 1: return (0, false)
  return (v - 1, true)
template incl*[K](s: PMultisetRef[K], key: K): PMultisetRef[K] =
  s.as_map.update(key, multiset_incl_update).as_mset
template excl*[K](s: PMultisetRef[K], key: K): PMultisetRef[K] =
  s.as_map.update(key, multiset_excl_update).as_mset

var multiset_update_count = 1
proc multiset_incl_update_count*[V](v: V, exists: bool): (V, bool) =
  let new_v = v + multiset_update_count
  if new_v == 0: return (0, false)
  return (new_v, true)
proc multiset_excl_update_count*[V](v: V, exists: bool): (V, bool) =
  let new_v = v - multiset_update_count
  if new_v == 0: return (0, false)
  return (new_v, true)
proc incl*[K](s: PMultisetRef[K], key: K, count: int): PMultisetRef[K] =
  multiset_update_count = count
  result = s.as_map.update(key, multiset_incl_update_count).as_mset
  multiset_excl_update_count = 1
proc excl*[K](s: PMultisetRef[K], key: K, count: int): PMultisetRef[K] =
  multiset_update_count = count
  result = s.as_map.update(key, multiset_excl_update_count).as_mset
  multiset_excl_update_count = 1

func init_multiset*[K](): PMultisetRef[K] =
  result = init_map[K, int]().as_mset
func to_multiset*[K](arr: openArray[K]): PMultisetRef[K] =
  var m = init_multiset[K]()
  for k in arr:
    m = m.incl(k)
  return m
func to_multiset*[K](arr: openArray[(K, int)]): PMultisetRef[K] =
  var m = init_multiset[K]()
  for (k, i) in arr:
    m = m.incl(k, i)
  return m
template hash*[K](m: PMultisetRef[K]): Hash =
  m.as_map.hash
template contains*[K](m: PMultisetRef[K], key: K): bool =
  m.as_map.contains(key)
template get_count*[K](m: PMultisetRef[K], key: K): int =
  m.as_map.get_or_default(key, 0)
template natural_count*[K](m: PMultisetRef[K], key: K): bool =
  m.as_map.get_or_default(key, 0) >= 0
template positive_count*[K](m: PMultisetRef[K], key: K): bool =
  m.as_map.get_or_default(key, 0) > 0
template negative_count*[K](m: PMultisetRef[K], key: K): bool =
  m.as_map.get_or_default(key, 0) < 0
template len*[K](m: PMultisetRef[K]): Natural =
  m.as_map.len
iterator items*[K](m: PMultisetRef[K]): K =
  for k in m.as_map.keys:
    yield k
iterator keys*[K](m: PMultisetRef[K]): K =
  for k in m.as_map.keys:
    yield k
iterator pairs*[K](m: PMultisetRef[K]): K =
  for p in m.as_map.pairs:
    yield p
template `==`*[K](m1, m2: PMultisetRef[K]): bool =
  m1.as_map == m2.as_map
template valid*[K](m: PMultisetRef[K]): bool =
  m.as_map.valid
template to_json*[K](m: PMultisetRef[K]): string =
  m.as_map.to_json
