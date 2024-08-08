import std/[tables, strutils, sequtils, algorithm]
import ../../src/[test_utils]
import ../../src/persistent/[vec]
# import ../src/persistent/[sumtree]

## 
## TODO
## 
## API
## [x] to_vec
## [x] concat
## [x] del
## [x] slice
## [x] take
## [x] drop
## [x] set_len
## [x] insert
## [x] splice
## [x] map
## [x] filter
## [x] reverse
## [ ] transient mutation
## 
## Impl
## [x] get ImArray to use this
## [x] add hash to pvec summary
## [ ] use `distinct` so that we can have monomorphic impls come after sumtree
##   - look at impl of https://github.com/Nycto/RBTreeNim/blob/master/src/rbtree.nim
## [ ] parameterize branch size and buffer size
##   - i'd like to benchmark different sizes
## [ ] top-level sumtree should not be a ref
## [ ] store cumulative size array on the sumtree node
## [x] add validate fn for testing and debugging
## [ ] transients
## 
## Test
## [ ] all the APIs
## [x] larger sizes
## [-] sparse arrays
## 

proc main* =
  suite "persistent vec":

    test "GC_ref and GC_unref":
      var v1 = init_vec[int]()
      GC_ref(v1)
      GC_unref(v1)

# #[
    test "clone":
      var v1 = init_vec[int]()
      var v2 = v1.clone()
      check v1.valid
      check v2.valid
      check v1 == v2

    test "simple append":
      var
        v1 = init_vec[int]()
        v2 = v1.push(0)
      check v1.valid
      check v2.valid
      check v1 != v2
      check v1.len == 0
      check v2.len == 1

    test "push":
      proc push_test(sz: int) =
        var v = init_vec[int]()
        for i in 0..<sz:
          v = v.push(i)
          check v.valid
        check v.len == sz
        check toSeq(v.items) == toSeq(0..<sz)
      var sizes = [1, 10, 100, 1_000, 10_000]
      for sz in sizes:
        push_test(sz)

    test "prepend":
      proc push_test(sz: int) =
        var v = init_vec[int]()
        for i in 0..<sz:
          v = v.prepend(i)
          check v.valid
        check v.len == sz
        check toSeq(v.items) == toSeq(countdown(sz - 1, 0))
      var sizes = [1, 10, 100, 1_000, 10_000]
      for sz in sizes:
        push_test(sz)

    test "push and iterator pairs":
      var
        v1 = init_vec[int]().push(10).push(11).push(12).push(13).push(14).push(15)
        s = toSeq(v1.pairs)
      check v1.valid
      check s == @[(0, 10), (1, 11), (2, 12), (3, 13), (4, 14), (5, 15)]

    test "push and iterator items":
      var
        v1 = init_vec[int]().push(10).push(11).push(12).push(13).push(14).push(15)
        s = toSeq(v1.items)
      check v1.valid
      check s == @[10, 11, 12, 13, 14, 15]

    test "push_front and iterator pairs":
      var
        v1 = init_vec[int]().push_front(10).push_front(11).push_front(12).push_front(13)
        s = toSeq(v1.pairs)
      check v1.valid
      check s == @[(0, 13), (1, 12), (2, 11), (3, 10)]

    test "push_front and iterator items":
      var
        v1 = init_vec[int]().push_front(10).push_front(11).push_front(12).push_front(13)
        s = toSeq(v1.items)
      check v1.valid
      check s == @[13, 12, 11, 10]

    test "to_vec":
      proc to_vec_test(size: int) =
        var
          s = toSeq(0..<size)
          v = to_vec(s)
        check v.valid
        check toSeq(v) == s
      to_vec_test(0)
      to_vec_test(1)
      to_vec_test(10)
      to_vec_test(100)
      to_vec_test(1_000)
      to_vec_test(10_000)
      to_vec_test(100_000)
      to_vec_test(1_000_000)
      # to_vec_test(10_000_000)

    test "to_vec internals":
      check [0].to_vec.depth_safe == 0
      check [1, 2, 3, 4, 5, 6].to_vec.depth_safe == 0
      check [0].to_vec.kind == kLeaf
      check [1, 2, 3, 4, 5, 6].to_vec.kind == kLeaf
      check toSeq(0..<100).to_vec.valid

    test "get idx":
      proc get_test(sz: int) =
        var
          offset = 5
          v = to_vec(toSeq(offset..<(sz + offset)))
        check v.valid
        for i in 0..<sz:
          var res = v.get(i) == i + offset
          check res
      var sizes = [1, 10, 100, 1_000, 10_000, 100_000]
      for sz in sizes:
        get_test(sz)

    test "get slice":
      proc get_test(sz: int) =
        var
          offset = 5
          s = toSeq(offset..<(sz + offset))
          v1 = to_vec(s)
          v2: type(v1)
          idx: int
          slices = [
            (sz div 2)..<sz,
            0..<(sz div 2),
            (sz div 3)..<((sz div 3) shl 1),
            (sz div 3)..(sz div 3),
          ]
        for slice in slices:
          v2 = v1.get(slice)
          check v2.valid
          check toSeq(v2) == s[slice]
      var sizes = [10, 100, 1_000, 10_000]
      for sz in sizes:
        get_test(sz)

    test "set by index":
      proc set_test(sz: int) =
        var
          offset = 5
          offset_seq = toSeq(offset..<(sz + offset))
          v = to_vec(toSeq(0..<sz))
        for i in 0..<sz:
          v = v.set(i, i + offset)
          check v.valid
        check toSeq(v.items) == offset_seq
      var sizes = [1, 10, 100, 1_000, 10_000]
      for sz in sizes:
        set_test(sz)
# ]#

    test "set by slice":
      # var sizes = [1]
      var sizes = [1, 10, 100, 1_000, 10_000]
      proc set_test(sz: int) =
        var
          offset = 77
          basic_seq = toSeq(0..<sz)
          seq_copy: seq[int]
          v1 = to_vec(basic_seq)
          v2: type(v1)
          v3: type(v1)
          slices = [
            (sz div 2)..<sz,
            0..<(sz div 2),
            (sz div 3)..<((sz div 3) shl 1),
            (sz div 3)..(sz div 3),
          ]
          to_insert_seq: seq[int]
          to_insert_vec: PVecRef[int]
        for size in sizes:
          to_insert_seq = toSeq(offset..<(sz + offset))
          to_insert_vec = to_insert_seq.to_vec
          for slice in slices:
            seq_copy = toSeq(basic_seq)
            seq_copy.delete(slice)
            seq_copy.insert(to_insert_seq, slice.a)
            v2 = v1.set(slice, to_insert_seq)
            v3 = v1.set(slice, to_insert_vec)
            check v2.valid
            check v3.valid
            check v2 == v3
            check toSeq(v2) == seq_copy
            check toSeq(v3) == seq_copy
      for sz in sizes:
        set_test(sz)

    test "simple equality":
      var
        v1 = init_vec[int]()
        v2 = v1.push(1)
        v3 = v1.push(1)
      check v1.valid
      check v2.valid
      check v3.valid
      check v2 == v3

    test "concat":
      proc concat_test(sz1, sz2: int) =
        var
          s1 = toSeq(0..<sz1)
          s2 = toSeq(0..<sz2)
          v1 = to_vec(s1)
          v2 = to_vec(s2)
          v3 = v1 & v2
        check v3.valid
        check toSeq(v3) == s1 & s2
      var sizes = [0, 1, 10, 100, 1_000, 10_000]
      for sz1 in sizes:
        for sz2 in sizes:
          concat_test(sz1, sz2)

    test "vec hashes":
      var
        v1 = [1, 2, 3, 4, 5, 6].to_vec
        v2 = [1, 2, 3, 4, 5, 6].to_vec
        v3 = [6, 5, 4, 3, 2, 1].to_vec
      check v1.valid
      check v2.valid
      check v3.valid
      check v1 == v2
      check v1 != v3

    test "pop":
      proc pop_test(sz: int) =
        var
          offset = 5
          offset_seq = toSeq(offset..<(sz + offset))
          item: int
          v = to_vec(offset_seq)
        check v.valid
        for i in 0..<sz:
          (v, item) = v.pop()
          check item == sz - 1 - i + offset
          check v.len == sz - 1 - i
          check v.valid
      var sizes = [1, 10, 100, 1_000, 10_000]
      for sz in sizes:
        pop_test(sz)

    test "take":
      proc take_test(sz: int) =
        var
          offset = 5
          offset_seq = toSeq(offset..<(sz + offset))
          v = to_vec(offset_seq)
          v2: type(v)
          counts = [0, 1, sz div 2, sz div 3, (sz div 3) shl 1, sz div 4, sz]
        for count in counts:
          v2 = v.take(count)
          check v2.valid
          check toSeq(v2) == offset_seq[0..<count]
      var sizes = [1, 10, 100, 1_000, 10_000]
      for sz in sizes:
        take_test(sz)

    test "drop":
      proc drop_test(sz: int) =
        var
          offset = 5
          offset_seq = toSeq(offset..<(sz + offset))
          v = to_vec(offset_seq)
          v2: type(v)
          counts = [0, 1, sz div 2, sz div 3, (sz div 3) shl 1, sz div 4, sz]
        for count in counts:
          v2 = v.drop(count)
          check v2.valid
          check toSeq(v2) == offset_seq[count..<sz]
      var sizes = [1, 10, 100, 1_000, 10_000]
      for sz in sizes:
        drop_test(sz)

    test "delete":
      proc delete_test(sz: int) =
        var
          offset = 5
          offset_seq = toSeq(offset..<(sz + offset))
          seq_copy: seq[int]
          v = to_vec(offset_seq)
          v2: type(v)
          slices = [
            (sz div 2)..<sz,
            0..<(sz div 2),
            (sz div 3)..<((sz div 3) shl 1),
            (sz div 3)..(sz div 3),
          ]
        for slice in slices:
          v2 = v.delete(slice)
          check v2.valid
          seq_copy = toSeq(offset_seq)
          seq_copy.delete(slice)
          check toSeq(v2) == seq_copy
      var sizes = [1, 10, 100, 1_000, 10_000]
      for sz in sizes:
        delete_test(sz)

    test "insert":
      var sizes = [1, 10, 100, 1_000, 10_000]
      proc insert_test(sz: int) =
        var
          to_insert_seq: seq[int]
          to_insert_vec: PVecRef[int]
          offset = 5
          offset_seq = toSeq(offset..<(sz + offset))
          seq_copy: seq[int]
          v = to_vec(offset_seq)
          v2: type(v)
          v3: type(v)
          idxes = [0, sz div 2, sz div 3, (sz div 3) shl 1, sz div 4]
        for size in sizes:
          to_insert_seq = toSeq(0..<size)
          to_insert_vec = to_insert_seq.to_vec
          for idx in idxes:
            v2 = v.insert(to_insert_seq, idx)
            v3 = v.insert(to_insert_vec, idx)
            check v2.valid
            check v3.valid
            check v2 == v3
            seq_copy = toSeq(offset_seq)
            seq_copy.insert(to_insert_seq, idx)
            check toSeq(v2) == seq_copy
            check toSeq(v3) == seq_copy
      for sz in sizes:
        insert_test(sz)

    test "set_len":
      var sizes = [1, 10, 100, 1_000, 10_000]
      proc set_len_test(sz: int) =
        var
          offset = 5
          offset_seq = toSeq(offset..<(sz + offset))
          seq_copy: seq[int]
          v = to_vec(offset_seq)
          v2: type(v)
        for new_len in sizes:
          v2 = v.set_len(new_len)
          check v2.valid
          seq_copy = toSeq(offset_seq)
          seq_copy.setLen(new_len)
          check toSeq(v2) == seq_copy
      for sz in sizes:
        set_len_test(sz)

    test "map":
      proc map_test(sz: int) =
        var
          offset = 5
          offset_seq = toSeq(offset..<(sz + offset))
          v = to_vec(offset_seq)
          op = proc (x: int): string = $(x + 91)
          v2 = v.map(op)
        check v2.valid
        check toSeq(v2) == offset_seq.map(op)
      var sizes = [1, 10, 100, 1_000, 10_000]
      for sz in sizes:
        map_test(sz)

    test "filter":
      proc filter_test(sz: int) =
        var
          offset = 5
          offset_seq = toSeq(offset..<(sz + offset))
          v = to_vec(offset_seq)
          predicate = proc (x: int): bool = 0 == (x mod 2)
          v2 = v.filter(predicate)
        check v2.valid
        check toSeq(v2) == offset_seq.filter(predicate)
      var sizes = [1, 10, 100, 1_000, 10_000]
      for sz in sizes:
        filter_test(sz)

    test "zip":
      proc zip_test(sz: int) =
        var
          seqq = toSeq(0..<sz)
          offset = 5
          offset_seq = toSeq(offset..<(sz + offset))
          v1 = to_vec(seqq)
          v2 = to_vec(offset_seq)
          v3 = zip(v1, v2)
        check v3.valid
        check toSeq(v3) == zip(seqq, offset_seq)
      var sizes = [1, 10, 100, 1_000, 10_000]
      for sz in sizes:
        zip_test(sz)
    
    test "reverse":
      proc reverse_test(sz: int) =
        var
          offset = 5
          offset_seq = toSeq(offset..<(sz + offset))
          v1 = to_vec(offset_seq)
          v2 = v1.reverse
        check v2.valid
        check toSeq(v2) == offset_seq.reversed
      var sizes = [1, 10, 100, 1_000, 10_000]
      for sz in sizes:
        reverse_test(sz)


#[
]#

  echo "done"
