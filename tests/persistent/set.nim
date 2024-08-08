import std/[tables, strutils, sequtils, algorithm]
import ../../src/[test_utils]
import ../../src/persistent/[map]

proc main* =
  suite "persistent set":

    test "integer keys":
      var s1 = [3].to_set()
      check s1.len == 1
      check s1.contains(3)
      check not(s1.contains(5))
      var s2 = s1.incl(7)
      check s2.len == 2
      check s2 != s1
      check s2.contains(7)
      var s3 = s1.incl(3)
      check s3.contains(3)
      check s3.len == 1
      var s4 = s2.excl(7)
      check s4.len == 1
      check 7 notin s4
      check s4 == s1
      var s5 = s4.excl(7)
      check s5 == s4
      discard s4.excl(0)

    test "to_set":
      proc to_set_test(sz: int) =
        var 
          s1 = toSeq(0..<sz).to_set
        check s1.len == sz
        check s1.valid
        for i in 0..<sz:
          check i in s1
      for sz in [0, 1, 10, 100, 1000, 10000]:
        to_set_test(sz)

    test "incl":
      proc incl_test(sz: int, check_validity = true) =
        var
          s1 = init_set[int]()
        for i in 0..<sz:
          s1 = s1.incl(i)
          s1 = s1.incl(i)
          check s1.len == i + 1
          if check_validity:
            check s1.valid
        check s1.len == sz
        check s1.valid
        for i in 0..<sz:
          check i in s1
      for sz in [1, 10, 100, 1000, 10000]:
        # validity checks are a little expensive so we don't check validity on
        # big runs
        if sz > 1000:
          incl_test(sz, false)
        else:
          incl_test(sz)

    test "excl":
      proc excl_test(sz: int, check_validity = true) =
        var 
          s1 = toSeq(0..<sz).to_set
        for i in 0..<sz:
          s1 = s1.excl(i)
          s1 = s1.excl(i)
          check s1.len == sz - i - 1
          if check_validity:
            check s1.valid
        check s1.len == 0
        check s1.valid
        for i in 0..<sz:
          check i in s1 == false
      for sz in [1, 10, 100, 1000, 10000]:
        # validity checks are a little expensive so we don't check validity on
        # big runs
        if sz > 1000:
          excl_test(sz, false)
        else:
          excl_test(sz)

    test "big":
      var
        sz = 100_000
        s1 = toSeq(0..<sz).to_set
        s2 = s1.excl(sz div 2)
        s3 = s2.incl(sz div 2)
      check s1 == s3
      check s2 != s1
      check s1.len == s2.len + 1
      check s1.valid
      check s2.valid
      check s3.valid

    test "from parazoa":
      let s1 = init_set[string]()
      let s2 = s1.incl("hello")
      check not s1.contains("hello")
      check s2.contains("hello")
      let s3 = s1.incl("goodbye")
      check s3.contains("goodbye")
      let s4 = s3.incl("what's")
      let s5 = s3.excl("what's").excl("asdf")
      check s1.len == 0
      check s2.len == 1
      check s3.len == 1
      check s4.len == 2
      check s5.len == 1
      check s2 == ["hello"].to_set
      # large set
      var s6 = init_set[string]()
      for i in 0 .. 1024:
        s6 = s6.incl($i)
      check s6.len == 1025
      check s6.contains("1024")
      # items
      var s7 = init_set[string]()
      for k in s6.items:
        s7 = s7.incl(k)
      check s7.len == 1025
      # equality
      check s1 == s1
      check s1 != s2
      check s6 == s7

  echo "done"