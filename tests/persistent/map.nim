import std/[tables, strutils, sequtils, algorithm, hashes]
import ../../src/[test_utils]
import ../../src/persistent/[map]

proc main* =
  suite "persistent map":

    test "can use GC_ref and GC_unref":
      var m = {3: 5, 7: 9}.to_map
      GC_ref(m)
      GC_unref(m)
    
    test "integer keys":
      var m1 = {3: 5}.to_map()
      check m1.len == 1
      check m1.get(3) == 5
      check m1.getOrDefault(5) == 0
      check m1.getOrDefault(5, 10) == 10
      var m2 = m1.add(7, 13)
      check m2.len == 2
      check m2 != m1
      check m2.get(7) == 13
      var m3 = m1.add(3, 23)
      check m3.get(3) == 23
      check m3.len == 1
      var m4 = m2.delete(7)
      check m4.len == 1
      check m4.getOrDefault(7) == 0
      check m4 == m1
      var m5 = m4.delete(7)
      check m5 == m4
      discard m4.delete(0)

    test "to_map":
      proc to_map_test(sz: int) =
        var 
          m1 = toSeq(toSeq(0..<sz).pairs).to_map
        check m1.len == sz
        check m1.valid
        for i in 0..<sz:
          check i == m1[i]
      for sz in [0, 1, 10, 100, 1000, 10000]:
        to_map_test(sz)

    test "add":
      proc add_test(sz: int, check_validity = true) =
        var
          m1 = init_map[int, int]()
        for i in 0..<sz:
          m1 = m1.add(i, i)
          check m1.len == i + 1
          if check_validity:
            check m1.valid
        check m1.len == sz
        check m1.valid
        for i in 0..<sz:
          check i == m1[i]
      for sz in [1, 10, 100, 1000, 10000]:
        # validity checks are a little expensive so we don't check validity on
        # big runs
        if sz > 1000:
          add_test(sz, false)
        else:
          add_test(sz)

    test "delete":
      proc delete_test(sz: int, check_validity = true) =
        var 
          m1 = toSeq(toSeq(0..<sz).pairs).to_map
        for i in 0..<sz:
          m1 = m1.delete(i)
          check m1.len == sz - i - 1
          if check_validity:
            check m1.valid
        check m1.len == 0
        check m1.valid
        for i in 0..<sz:
          check m1.get_or_default(i, -1) == -1
          check i in m1 == false
      for sz in [1, 10, 100, 1000, 10000]:
        # validity checks are a little expensive so we don't check validity on
        # big runs
        if sz > 1000:
          delete_test(sz, false)
        else:
          delete_test(sz)
    
    test "init with duplicates":
      var
        m1 = {1: 3, 2: 3, 1: 4, 2: 4}.to_map()
      check m1.valid
      check m1.size == 2
      check m1[1] == 4
      check m1[2] == 4
    
    test "big":
      var
        sz = 100_000
        m1 = toSeq(toSeq(0..<sz).pairs).to_map
        m2 = m1.delete(sz div 2)
        m3 = m2.add(sz div 2, sz div 2)
      check m1 == m3
      check m2 != m1
      check m1.len == m2.len + 1
      check m1.valid
      check m2.valid
      check m3.valid

    test "from parazoa":
      let m1 = init_map[string, string]()
      let m2 = m1.add("hello", "world")
      expect(map.KeyError):
        discard m1.get("hello")
      check m2.get("hello") == "world"
      check m1.getOrDefault("hello", "") == ""
      check m2.getOrDefault("hello", "") == "world"
      check m2.contains("hello")
      let m3 = m2.add("hello", "goodbye")
      check m3.get("hello") == "goodbye"
      let m4 = m3.add("what's", "up")
      let m5 = m3.delete("what's").delete("asdf")
      check m5.get("hello") == "goodbye"
      expect(map.KeyError):
        discard m5.get("what's")
      check m1.len == 0
      check m2.len == 1
      check m3.len == 1
      check m4.len == 2
      check m5.len == 1
      check m2 == {"hello": "world"}.to_map
      # large map
      var m6 = init_map[string, string]()
      for i in 0 .. 1024:
        m6 = m6.add($i, $i)
      check m6.len == 1025
      check m6.get("1024") == "1024"
      # pairs
      var m7 = init_map[string, string]()
      for (k, v) in m6.pairs:
        m7 = m7.add(k, v)
      check m7.len == 1025
      # keys
      var m8 = init_map[string, string]()
      for k in m7.keys:
        m8 = m8.add(k, k)
      check m8.len == 1025
      # values
      var m9 = init_map[string, string]()
      for v in m8.values:
        m9 = m9.add(v, v)
      check m9.len == 1025
      # equality
      check m1 == m1
      check m1 != m2
      check m2 != m3
      check m8 == m9

  echo "done"