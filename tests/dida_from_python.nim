import std/[sugar, sequtils]
import ../src/[test_utils, dida_from_python, values]

template COL*(rows: openArray[Row]): Collection = init_collection(rows)
template VER*(timestamps: openArray[int]): Version = init_version(timestamps)
template FTR*(versions: openArray[Version]): Frontier = init_frontier(versions)

proc main* =
  suite "collection":
    test "simple":
      var
        a = COL([])
        b = COL([])
        c = COL([(V [0, 1], 1)])
      check a == b
      check a != c

    test "various":
      var
        a = COL([
          (V ["apple", "$5"], 2),
          (V ["banana", "$2"], 1),
        ])
        b = COL([
          (V ["apple", "$3"], 1),
          (V ["apple", ["granny smith", "$2"]], 1),
          (V ["kiwi", "$2"], 1),
        ])
        c = COL([
          (V ["apple", "$5"], 2),
          (V ["banana", "$2"], 1),
          (V ["apple", "$2"], 20),
        ])
        d = COL([
          (V ["apple", 11], 1),
          (V ["apple", 3], 2),
          (V ["banana", 2], 3),
          (V ["coconut", 3], 1),
        ])
        # some results
        a_concat_b_result = COL([
          (V ["apple", "$5"], 2),
          (V ["banana", "$2"], 1),
          (V ["apple", "$3"], 1),
          (V ["apple", ["granny smith", "$2"]], 1),
          (V ["kiwi", "$2"], 1),
        ])
        a_join_b_result = COL([
          (V ["apple", ["$5", "$3"]], 2),
          (V ["apple", ["$5", ["granny smith", "$2"]]], 2),
        ])
        b_join_a_result = COL([
          (V ["apple", ["$3", "$5"]], 2),
          (V ["apple", [["granny smith", "$2"], "$5"]], 2),
        ])
      check a.concat(b) == a_concat_b_result
      check b.concat(a) == a_concat_b_result
      check a.join(b) == a_join_b_result
      check b.join(a) == b_join_a_result
      check a.filter(proc (e: Entry): bool = e.key == V "apple") == COL([
        (V ["apple", "$5"], 2),
      ])
      check a.map((e) => V([e.value, e.key])) == COL([
        (V ["$5", "apple"], 2),
        (V ["$2", "banana"], 1),
      ])
      check a.concat(b).count() == COL([
        (V ["apple", 4], 1),
        (V ["banana", 1], 1),
        (V ["kiwi", 1], 1),
      ])
      check a.concat(b).distinct() == COL([
        (V ["apple", "$5"], 1),
        (V ["banana", "$2"], 1),
        (V ["apple", "$3"], 1),
        (V ["apple", ["granny smith", "$2"]], 1),
        (V ["kiwi", "$2"], 1),
      ])
      check d.min() == COL([
        (V ["apple", 3], 1),
        (V ["banana", 2], 1),
        (V ["coconut", 3], 1),
      ])
      check d.max() == COL([
        (V ["apple", 11], 1),
        (V ["banana", 2], 1),
        (V ["coconut", 3], 1),
      ])
      check d.sum() == COL([
        (V ["apple", 17], 1),
        (V ["banana", 6], 1),
        (V ["coconut", 3], 1),
      ])
      check c.min() == COL([
        (V ["apple", "$2"], 1),
        (V ["banana", "$2"], 1),
      ])
      check c.max() == COL([
        (V ["apple", "$5"], 1),
        (V ["banana", "$2"], 1),
      ])

    test "flat_map":
      var a = COL([(V 1, 1), (V 2, 1), (V 3, 1)])
      proc multiply_odds(e: Value): iterator(): Value =
        let n = e.as_f64.int
        return iterator(): Value =
          if n mod 2 != 0:
            for i in 1..n:
              for j in 0..<i:
                yield V [n, i]
      check a.flat_map(multiply_odds) == COL([
        (V [1, 1], 1),
        (V [3, 1], 1),
        (V [3, 2], 2),
        (V [3, 3], 3),
      ])   

    test "negate":
      var a = COL([
        (V ["foo", Nil], 3),
        (V ["foo", Nil], 1),
        (V ["bar", Nil], 2),
      ])
      check a.negate == COL([
        (V ["foo", Nil], -3),
        (V ["foo", Nil], -1),
        (V ["bar", Nil], -2),
      ])

    test "consolidate":
      var a = COL([
        (V ["foo", Nil], 1),
        (V ["foo", Nil], 3),
        (V ["bar", Nil], 3),
        (V ["foo", Nil], 9),
        (V ["bar", Nil], 3),
        (V ["was", Nil], 3),
        (V ["foo", Nil], 1),
        (V ["bar", Nil], -47),
        (V ["was", Nil], -3),
      ])
      check a.consolidate == COL([
        (V ["foo", Nil], 14),
        (V ["bar", Nil], -41),
      ])
      check a.concat(a.negate).consolidate == COL([])

    test "iterate":
      var a = COL([(V [1, Nil], 1)])
      proc add_one(c: Collection): Collection =
        return c.map((e) => V([(e.key.as_f64 + 1.0).v, e.value]))
          .concat(c)
          .filter(proc (e: Entry): bool = e.key < V 5.0)
          .distinct
          .consolidate
      check a.iterate(add_one) == COL([
        (V [1, Nil], 1),
        (V [2, Nil], 1),
        (V [3, Nil], 1),
        (V [4, Nil], 1),
      ])

  suite "version":
    test "simple":
      var
        v0_0 = [0, 0].VER
        v1_0 = [1, 0].VER
        v0_1 = [0, 1].VER
        v1_1 = [1, 1].VER
        v2_0 = [2, 0].VER

      check v0_0.lt(v1_0)
      check v0_0.lt(v0_1)
      check v0_0.lt(v1_1)
      check v0_0.le(v1_0)
      check v0_0.le(v0_1)
      check v0_0.le(v1_1)

      check v0_0.join(v0_1) == v0_1

      check not(v1_0.lt(v1_0))
      check v1_0.le(v1_0)
      check not(v1_0.le(v0_1))
      check not(v0_1.le(v1_0))
      check v0_1.le(v1_1)
      check v1_0.le(v1_1)
      check v0_0.le(v1_1)
  
  suite "frontier":
    test "simple":
      var
        v0_0 = [0, 0].VER
        v1_0 = [1, 0].VER
        v0_1 = [0, 1].VER
        v1_1 = [1, 1].VER
        v2_0 = [2, 0].VER
      
      check FTR([v0_0]).le(FTR([v0_0]))
      check FTR([v0_0]).le(FTR([v1_0]))
      check FTR([v0_0]).lt(FTR([v1_0]))
      check FTR([v2_0, v1_1]).lt(FTR([v2_0]))
      check FTR([v0_0]) != (FTR([v1_0]))
      check FTR([v2_0, v1_1]) == (FTR([v1_1, v2_0]))

      check [v1_0].FTR.meet([v0_0].FTR) == [v0_0].FTR
      check [v0_0].FTR.meet([v1_0].FTR) == [v0_0].FTR
      check [v1_0, v0_1].FTR.truncate == [[0].VER].FTR

  suite "dida":
    test "simple on_row and on_collection":
      var
        initial_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [0, 1], 1)].COL),
          ([0].VER, [(V [2, 3], 1)].COL),
        ]
        result_rows: seq[Row] = @[]
        result_data: seq[(Version, Collection)] = @[]
        correct_rows: seq[Row] = @[(V [0, 1], -1), (V [2, 3], -1)]
        correct_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [0, 1], -1)].COL),
          ([0].VER, [(V [2, 3], -1)].COL),
        ]
        b = init_builder()
          .negate()
          .on_row(proc (r: Row) = result_rows.add(r))
          .on_collection(proc (v: Version, c: Collection) = result_data.add((v, c)))
        g = b.graph
      for (v, c) in initial_data: g.send(v, c)
      g.send([[1].VER].FTR)
      g.step
      check result_rows == correct_rows
      check result_data == correct_data
    
    test "simple accumulate_results":
      var
        initial_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [0, 1], 1)].COL),
          ([0].VER, [(V [2, 3], 1)].COL),
        ]
        correct_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [0, 1], -1)].COL),
          ([0].VER, [(V [2, 3], -1)].COL),
        ]
        b = init_builder()
          .negate
          .accumulate_results
        g = b.graph
      for (v, c) in initial_data: g.send(v, c)
      g.send([[1].VER].FTR)
      g.step
      check b.node.results == correct_data

    test "simple map":
      var
        initial_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [0, 1], 1), (V [8, 9], 5)].COL),
          ([0].VER, [(V [2, 3], 1)].COL),
        ]
        correct_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [1, 0], 1), (V [9, 8], 5)].COL),
          ([0].VER, [(V [3, 2], 1)].COL),
        ]
        b = init_builder()
          .map((e) => V([e[1], e[0]]))
          .accumulate_results
        g = b.graph
      for (v, c) in initial_data: g.send(v, c)
      g.send([[1].VER].FTR)
      g.step
      check b.node.results == correct_data

    test "simple filter":
      var
        initial_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [0, 1], 1)].COL),
          ([0].VER, [(V [0, 1], 1), (V [8, 9], 5)].COL),
          ([0].VER, [(V [2, 3], 1)].COL),
        ]
        correct_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [8, 9], 5)].COL),
        ]
        b = init_builder()
          .filter((e) => e[0] > 5.0.v)
          .accumulate_results
        g = b.graph
      for (v, c) in initial_data: g.send(v, c)
      g.send([[1].VER].FTR)
      g.step
      check b.node.results == correct_data

    test "simple flat_map":
      var
        initial_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [0, 1], 1)].COL),
          ([0].VER, [(V [0, 1], 1), (V [8, 9], 5)].COL),
          ([0].VER, [(V [2, 3], 1)].COL),
        ]
        correct_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V 0, 1), (V 1, 1)].COL),
          ([0].VER, [(V 0, 1), (V 1, 1), (V 8, 5), (V 9, 5)].COL),
          ([0].VER, [(V 2, 1), (V 3, 1)].COL),
        ]
        b = init_builder()
          .flat_map(proc (e: Value): iterator(): Value =
            return iterator(): Value =
              yield e[0]
              yield e[1])
          .accumulate_results
        g = b.graph
      for (v, c) in initial_data: g.send(v, c)
      g.send([[1].VER].FTR)
      g.step
      check b.node.results == correct_data

    test "negate with concat":
      var
        initial_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [0, 1], 1)].COL),
          ([0].VER, [(V [0, 1], 1), (V [8, 9], 5)].COL),
          ([0].VER, [(V [2, 3], 1)].COL),
        ]
        correct_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [0, 1], 1)].COL),
          ([0].VER, [(V [0, 1], 1), (V [8, 9], 5)].COL),
          ([0].VER, [(V [2, 3], 1)].COL),
          ([0].VER, [(V [0, 1], -1)].COL),
          ([0].VER, [(V [0, 1], -1), (V [8, 9], -5)].COL),
          ([0].VER, [(V [2, 3], -1)].COL),
        ]
        input = init_builder()
        r = input.concat(input.negate).accumulate_results
        g = input.graph
      for (v, c) in initial_data: g.send(v, c)
      g.send([[1].VER].FTR)
      g.step
      check r.node.results == correct_data

    test "concat with consolidate":
      var
        initial_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [0, 1], 1)].COL),
          ([0].VER, [(V [0, 1], 1), (V [8, 9], 5)].COL),
          ([0].VER, [(V [2, 3], 1)].COL),
        ]
        correct_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [0, 1], 2), (V [0, 1], 2), (V [8, 9], 10), (V [2, 3], 2)].COL),
        ]
        input = init_builder()
        r = input.concat(input).consolidate().accumulate_results
        g = input.graph
      for (v, c) in initial_data: g.send(v, c)
      g.send([[1].VER].FTR)
      g.step
      check r.node.results == correct_data

    test "negate with concat with consolidate":
      var
        initial_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [0, 1], 1)].COL),
          ([0].VER, [(V [0, 1], 1), (V [8, 9], 5)].COL),
          ([0].VER, [(V [2, 3], 1)].COL),
        ]
        correct_data: seq[(Version, Collection)] = @[]
        input = init_builder()
        r = input.concat(input.negate).consolidate().accumulate_results
        g = input.graph
      for (v, c) in initial_data: g.send(v, c)
      g.send([[1].VER].FTR)
      g.step
      check r.node.results == correct_data

    test "distinct":
      var
        initial_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [0, 1], 1), (V [0, 2], 3), (V [2, 5], 3), (V [0, 1], 5)].COL),
          ([0].VER, [(V [0, 1], 1), (V [8, 9], 5), (V [0, 3], 1)].COL),
          ([2].VER, [(V [2, 3], 1), (V [0, 4], 1), (V [2, 3], 9)].COL),
        ]
        correct_data: seq[(Version, Collection)] = @[
          ([0].VER, [(V [0, 1], 1), (V [0, 2], 1), (V [2, 5], 1), (V [8, 9], 1), (V [0, 3], 1)].COL),
          ([2].VER, [(V [2, 3], 1), (V [0, 4], 1)].COL),
        ]
        input = init_builder()
        d = input.distinct.accumulate_results
        r = d.accumulate_results
        g = input.graph
      for (v, c) in initial_data: g.send(v, c)
      g.send([[3].VER].FTR)
      g.step
      check r.node.results == correct_data
      check d.node.output_frontier == [[3].VER].FTR

    test "geometric series":
      proc geometric_series(b: Builder): Builder =
        return b
          .map((x) => V(x.as_f64 * 2.0))
          .concat(b)
          .filter((x) => x.as_f64 < 50.0)
          .map((x) => V([x, Nil]))
          .distinct()
          .map((x) => x[0])
          .consolidate()
      var
        vmultiset = init_versioned_multiset()
        b = init_builder().iterate(geometric_series).sink(vmultiset)
        m = b.accumulate_messages
        r = m.accumulate_results
        g = m.graph
        v0 = [0].VER
        v1 = [1].VER
        v2 = [2].VER
        v3 = [3].VER
        fallback = 30

      g.send(v0, [(V 1, 1)].COL)
      g.send([v1].FTR)
      fallback = 30
      while m.node.probe_frontier_less_than([v1].FTR):
        g.step
        block:
          doAssert fallback > 0
          fallback -= 1
      var
        v1_results = @[
          to_message(v0, [(V 1, 1), (V 2, 1)].COL),
          to_message(v0, [(V 4, 1)].COL),
          to_message(v0, [(V 8, 1)].COL),
          to_message(v0, [(V 16, 1)].COL),
          to_message(v0, [(V 32, 1)].COL),
          to_message([v1].FTR),
        ]
      check m.node.messages == v1_results
      check vmultiset.to_collection(v0) == [(V 1, 1), (V 2, 1), (V 4, 1), (V 8, 1), (V 16, 1), (V 32, 1)].COL

      g.send(v1, [(V 16, 1), (V 3, 1)].COL)
      g.send([v2].FTR)
      fallback = 30
      while m.node.probe_frontier_less_than([v2].FTR):
        g.step
        block:
          doAssert fallback > 0
          fallback -= 1
      var
        v2_results = v1_results.concat(@[
          to_message(v1, [(V 3, 1), (V 6, 1), (V 16, 1), (V 32, 1)].COL),
          to_message(v1, [(V 12, 1)].COL),
          to_message(v1, [(V 24, 1)].COL),
          to_message(v1, [(V 16, -1), (V 48, 1)].COL),
          to_message(v1, [(V 32, -1)].COL),
          to_message([v2].FTR),
        ])
      check m.node.messages == v2_results
      check vmultiset.to_collection(v1) == [(V 3, 1), (V 6, 1), (V 12, 1), (V 24, 1), (V 48, 1)].COL

      g.send(v2, [(V 3, -1)].COL)
      g.send([v3].FTR)
      fallback = 30
      while m.node.probe_frontier_less_than([v3].FTR):
        g.step
        block:
          doAssert fallback > 0
          fallback -= 1
      var
        v3_results = v2_results.concat(@[
          to_message(v2, [(V 3, -1), (V 6, -1)].COL),
          to_message(v2, [(V 12, -1)].COL),
          to_message(v2, [(V 24, -1)].COL),
          to_message(v2, [(V 48, -1)].COL),
          to_message([v3].FTR),
        ])
      check m.node.messages == v3_results
      check vmultiset.to_collection(v2) == [(V 3, -1), (V 6, -1), (V 12, -1), (V 24, -1), (V 48, -1)].COL
    
    test "game of life":
      proc game_of_life(b: Builder): Builder =
        var
          maybe_live_cells_flat_map_fn = proc (e: Value): iterator(): Value =
            return iterator(): Value =
              var
                x = e[0]
                x_0 = x.as_f64
                x_m_1 = (x_0 - 1.0).v
                x_p_1 = (x_0 + 1.0).v
                y = e[1]
                y_0 = y.as_f64
                y_m_1 = (y_0 - 1.0).v
                y_p_1 = (y_0 + 1.0).v
              yield V [x_m_1, y_m_1]
              yield V [x_m_1, y    ]
              yield V [x_m_1, y_p_1]
              yield V [x,     y_m_1]
              yield V [x,     y_p_1]
              yield V [x_p_1, y_m_1]
              yield V [x_p_1, y    ]
              yield V [x_p_1, y_p_1]
          maybe_live_cells = b.flat_map(maybe_live_cells_flat_map_fn)
            .map((e) => V([e, Nil])).count()
          live_with_3_neighbors = maybe_live_cells
            .filter((e) => e[1] == 3)
            .map((e) => e[0])
          live_with_2_neighbors = maybe_live_cells
            .filter((e) => e[1] == 2)
            .join(b.map(proc (e: Value): Value = V([e, Nil])))
            .map((e) => e[0])
          live_next_round = live_with_2_neighbors
            .concat(live_with_3_neighbors)
            .distinct()
        return live_next_round
      const 
        W = 6
        H = 6
      var
        board_window: array[H, array[W, bool]]
        reset_board_window = proc () =
          for y in 0..<H:
            for x in 0..<W:
              board_window[y][x] = false
        print_board_window = proc () =
          for y in board_window:
            var s = ""
            for x in y:
              if x: s.add("#")
              else: s.add("_")
            echo s
        set_collection_in_board_window = proc (c: Collection) =
          for r in c:
            if r.multiplicity > 0:
              board_window[r.value.as_f64.int][r.key.as_f64.int] = true
            else:
              board_window[r.value.as_f64.int][r.key.as_f64.int] = false
        on_message_fn = proc (m: Message) =
          case m.tag:
            of tData:
              for r in m.collection:
                if r.multiplicity > 0:
                  board_window[r.value.as_f64.int][r.key.as_f64.int] = true
                else:
                  board_window[r.value.as_f64.int][r.key.as_f64.int] = false
              print_board_window()
            of tFrontier:
              reset_board_window()
        vmultiset = init_versioned_multiset()
        initial_data = [(V [2, 2], 1), (V [2, 3], 1), (V [2, 4], 1), (V [3, 2], 1)].COL
        v0 = [0].VER
        v1 = [1].VER
        fallback = 20
        b = init_builder().iterate(game_of_life).sink(vmultiset)
          # for debugging
          # .print("game_of_life")
          # .on_message(on_message_fn)
        g = b.graph
      g.send(v0, initial_data)
      g.send([v1].FTR)
      while b.node.probe_frontier_less_than([v1].FTR):
        g.step
        block:
          doAssert fallback > 0
          fallback -= 1
      check vmultiset.to_collection(v0) == [
        (V [2, 1], 1),
        (V [1, 2], 1), (V [3, 2], 1),
        (V [1, 3], 1), (V [3, 3], 1),
        (V [2, 4], 1),
      ].COL

#[

Rete-like Interface

Rule Requirements
- named
- optional agenda
- "when" clauses
  - match clauses
  - filter clauses
- "then" clauses
  - callback/actions
  - can assert new facts
  - can retract facts
  - (nools has "modify" in addition to "assert" and "retract")
- use other rules as sources (composition)
- truth maintenance
- parameterized rules
- should compose with differential dataflow? (this seems hard)
- "not" operator
  - NCC - "not" around an "and"
- "or" operator
- aggregations?
  - "count"?
- nools' "from" operator is really cool
  - http://noolsjs.com/#defining-rule
  - similar to verse's use of superpositions
- "exists" operator
  - similar to verse's "one"
  - opposite of "not"?
- salience? conflict resolution?

Match Requirements
- can bind to entire value
- can bind to parts of value
- default to implicit alpha network with option to use explicit alphas
- unpacking/destructuring binding?
- try to not to use macros?

Questions
- It might be worth handling genericity before we get deep into this?
- How do we determine which part of the alpha network a given match clause
  should connect/listen to?
- How do we handle async?
- How do we handle errors?
  - Seems like it would be good to have atomic transactions with rollback?
    - maybe that should be a part of dida and not our logic system



type
  Match = object
    # (p)ath - where to find the value part to consider for filtering or binding
    # `p: Nil`, it's the entire value
    p: Value
    # (i)dent - the identifier to use in the binding
    i: Value
    # (e)qual - literal match
    e: Value
    # (f)ilter - a predicate used to determine if there's a match
    f: proc (v: Value, b: Value): bool
  M = Match

# Some example match operators simulating the "when" portion of a rule
beta_network
  .match(alpha, [
    M(p: V [0], i: V "Parent"),
    M(p: V [1], e: V "age"),
    M(p: V [2], i: V "ParentAge", f: (v, b) => v > 40.0)
  ])
  .match(alpha, [M(i: V "Thing")])
  .match(alpha, [M(i: V "Thing", f: (v, b) => v < 4.0)])
  .match(alpha, [
    M(p: V ["foo", 1, [4, "bar"]], i: V "Child"),
    M(p: V ["foo", Nil, "shoot"], e: V "age"),
    M(p: V [3, 4, 5], i: V "ChildAge", (v, b) => c < 4.0)
  ])

]#


      #[
      # Rete-like approaches
      var
        alpha_network = init_builder()
        alpha = alpha_network
        beta_network = init_builder()
        # An approach without tuples (just the bare value)
        attempt1 = beta_network
          .var_bind(alpha, Var(ident: "s",
            match: (bindings, candidate) => candidate != 0))
          .var_bind(alpha, Var(ident: "e",
            match: (b, c) => c != b["s"]))
          .var_bind(alpha, Var(ident: "n",
            match: (b, c) => c != b["s"] and c != b["e"]))
          .var_bind(alpha, Var(ident: "d",
            match: (b, c) => c != b["s"] and c != b["e"] and c != b["n"]))
          .var_bind(alpha, Var(ident: "m",
            match: (b, c) => c != b["s"] and c != b["e"] and c != b["n"] and c != b["d"] and c != 0))
          .var_bind(alpha, Var(ident: "o",
            match: (b, c) => c != b["s"] and c != b["e"] and c != b["n"] and c != b["d"] and c != b["m"]))
          .var_bind(alpha, Var(ident: "r",
            match: (b, c) => c != b["s"] and c != b["e"] and c != b["n"] and c != b["d"] and c != b["m"] and c != b["o"]))
          .var_bind(alpha, Var(ident: "y",
            match: (b, c) => c != b["s"] and c != b["e"] and c != b["n"] and c != b["d"] and c != b["m"] and c != b["o"] and c != b["r"]))
        # An alternative approach that uses tuples (seqs)
        attempt2 = beta_network
          .match(alpha, @[Var(id: "s", fn: (b, c) => c != 0)])
          .match(alpha, @[Var(id: "e", fn: (b, c) => c != b["s"])])
          .match(alpha, @[Var(id: "n", fn: (b, c) => c != b["s"] and c != b["e"])])
          .match(alpha, @[Var(id: "d", fn: (b, c) => c != b["s"] and c != b["e"] and c != b["n"])])
          # ...
        # This seems to work fine for tuples but not for other things like maps or for bare values
        # Also still not clear how the `Var` thing is supposed to work
        attempt3 = beta_network
          .match(alpha, @[Var(id: "Parent"), V "age", Var(id: "ParentAge", fn: (bindings, candidate) => candidate > 40.0)])
          .match(alpha, @[Var(id: "Parent"), V "child", Var(id: "Child")])
          .match(alpha, @[Var(id: "Child"), V "age", Var(id: "ChildAge", fn: (b, c) => c < 4.0)])
        # This is more generic: (is_binding, key, value)
        # But not as convenient to write by hand, so we need some helpers
        # So this approach seems like it would work for maps and arrays but doesn't yet work for bare Values
        # We could use flags instead of booleans for the first element in the tuple
        # We don't yet have a solution for functions
        attempt4 = beta_network
          .match(alpha, @[
            (true, 0.0, V {id: "Parent"}),
            (false, 1.0, V "age"),
            (true, 2.0, V {id: "ParentAge", fn: (bindings, candidate) => candidate > 40.0}),
          ])
          .match(alpha, @[
            (true, 0.0, V {id: "Parent"}),
            (false, 1.0, V "child"),
            (true, 2.0, V {id: "Child"}),
          ])
          .match(alpha, @[
            (true, 0.0, V {id: "Child"}),
            (false, 1.0, V "age"),
            (true, 2.0, V {id: "ChildAge", fn: (b, c) => c < 4.0})
          ])
        # tuple (path_to_value, exact_match, binding, predicate)
        # if the path is empty (len 0) or Nil, then the canidate is the bare object
        # should just make this an object instead of a tuple
        # still unclear how to handle functions
        #
        # the path thing might not be the best approach? it would be cool to figure
        # out how to make unpacking or structural matching work
        attempt5 = beta_network
          # indexing into Arr
          .match(alpha, @[
            (V [0],                    Nil.v,   V "Parent",    Nil.v),
            (V [1],                    V "age", Nil.v,         Nil.v),
            (V [2],                    Nil.v,   V "ParentAge", (bindings, candidate) => candidate > 40.0),
          ])
          # binding to bare value
          .match(alpha, @[
            (V [],                     Nil.v,   V "Thing",     Nil.v),
          ])
          # binding to bare value with predicate
          .match(alpha, @[
            (Nil.v,                    Nil.v,   V "Thing",     (b, c) => c < 4.0),
          ])
          # Map
          .match(alpha, @[
            (V ["foo", 1, [4, "bar"]], Nil.v,   V "Child",     Nil.v),
            (V ["foo", Nil, "shoot"],  V "age", Nil.v,         Nil.v),
            (V [3, 4, 5],              Nil.v,   V "ChildAge",  (b, c) => c < 4.0),
          ])

      ## Matching/unpacking
      ## 
      ## There are a few issues here:
      ## - what should definition syntax look like in Nim
      ## - what should definition syntax look like in some DSL?
      ## - what algorithm implementation to use?
      ## - can the same algorithm be used for regex/strings?
      ## - can the same algorithm be used for large texts? tree-sitter style?
      ## - how do we get extra context into the predicate fns?
      ## - how do we get bindings out (efficiently)?
      ]#

    if false:
      test "task: send more money":
        var
          S = "s".v
          E = "e".v
          N = "n".v
          D = "d".v
          M = "m".v
          O = "o".v
          R = "r".v
          Y = "y".v
          initial_data: seq[(Version, Collection)] = @[
            ([0].VER, toSeq(0..<10).map(i => (V i, 1)).COL)
          ]
          correct_data: seq[(Version, Collection)] = @[
            ([0].VER, [(V {S: 9, E: 5, N: 6, D: 7, M: 1, O: 0, R: 8, Y: 2}, 1)].COL),
          ]
          input = init_builder()
          s = input.flat_map(proc (e: Entry): iterator(): Entry =
              return iterator(): Entry =
                if e != 0: yield V({S: e}))
            .product(input)
          e = s.flat_map(proc (e: Entry): iterator(): Entry =
              return iterator(): Entry =
                let m = e[0]
                let n = e[1]
                if m[S] != n: yield m.set(E, n))
            .product(input)
          n = e.flat_map(proc (e: Entry): iterator(): Entry =
              return iterator(): Entry =
                let m = e[0]
                let n = e[1]
                if m[S] != n and m[E] != n: yield m.set(N, n))
            .product(input)
          d = n.flat_map(proc (e: Entry): iterator(): Entry =
              return iterator(): Entry =
                let m = e[0]
                let n = e[1]
                for v in m.values:
                  if v == n: return
                yield m.set(D, n))
            .product(input)
          m = d.flat_map(proc (e: Entry): iterator(): Entry =
              return iterator(): Entry =
                let m = e[0]
                let n = e[1]
                if n == 0: return
                for v in m.values:
                  if v == n: return
                yield m.set(M, n))
            .product(input)
          o = m.flat_map(proc (e: Entry): iterator(): Entry =
              return iterator(): Entry =
                let m = e[0]
                let n = e[1]
                for v in m.values:
                  if v == n: return
                yield m.set(O, n))
            .product(input)
          r = o.flat_map(proc (e: Entry): iterator(): Entry =
              return iterator(): Entry =
                let m = e[0]
                let n = e[1]
                for v in m.values:
                  if v == n: return
                yield m.set(R, n))
            .product(input)
          y = r.flat_map(proc (e: Entry): iterator(): Entry =
              return iterator(): Entry =
                let m = e[0]
                let n = e[1]
                for v in m.values:
                  if v == n: return
                yield m.set(Y, n))
          final = y.filter(proc (e: Entry): bool =
            let
              map = e
              s = map[S].as_f64
              e = map[E].as_f64
              n = map[N].as_f64
              d = map[D].as_f64
              m = map[M].as_f64
              o = map[O].as_f64
              r = map[R].as_f64
              y = map[Y].as_f64
            return             s * 1000 + e * 100 + n * 10 + d +
                               m * 1000 + o * 100 + r * 10 + e ==
                   m * 10000 + o * 1000 + n * 100 + e * 10 + y
          )
          results = final.accumulate_results
          g = input.graph
        for (v, c) in initial_data: g.send(v, c)
        g.send([[1].VER].FTR)
        g.step
        check results.node.results == correct_data

  echo "\nok"