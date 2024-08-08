import std/[tables, sets, bitops, strutils]
import hashes

when defined(isNimSkull):
  {.pragma: ex, exportc, dynlib.}
else:
  import std/[macros]
  macro ex*(t: typed): untyped =
    if t.kind notin {nnkProcDef, nnkFuncDef}:
      error("Can only export procedures", t)
    let
      newProc = copyNimTree(t)
      codeGen = nnkExprColonExpr.newTree(ident"codegendecl",
          newLit"EMSCRIPTEN_KEEPALIVE $# $#$#")
    if newProc[4].kind == nnkEmpty:
      newProc[4] = nnkPragma.newTree(codeGen)
    else:
      newProc[4].add codeGen
    newProc[4].add ident"exportC"
    result = newStmtList()
    result.add:
      quote do:
        {.emit: "/*INCLUDESECTION*/\n#include <emscripten.h>".}
    result.add:
      newProc
  # {.pragma: ex, exportc, dynlib.}

type
  Value* = float64

  Row* = object
    values*: seq[Value]
  
  Bag* = object
    rows*: Table[Row, int]

  Timestamp* = object
    coords*: seq[int]
  
  Frontier* = object
    timestamps*: HashSet[Timestamp]
  
  SupportedFrontier* = object
    support*: Table[Timestamp, int]
    frontier*: Frontier

  FrontierChange* = object
    timestamp*: Timestamp
    diff*: int
  
  Change* = object
    row*: Row
    timestamp*: Timestamp
    diff*: int

  ChangeBatch* = object
    lower_bound*: Frontier
    changes*: seq[Change]

  ChangeBatchBuilder* = object
    changes*: seq[Change]

  Index* = object
    change_batches*: seq[ChangeBatch]

  Mapper* = object
    map_fn*: "TODO"
  Reducer* = object
    reducer_fn*: "TODO"

  Node* = object
    id*: Natural
  NodeInput* = object
    node*: Node
    input_ix*: Natural
  NodeSpecTag* = enum
    tInput
    tMap
    tIndex
    tJoin
    tOutput
    tTimestampPush
    tTimestampIncrement
    tTimestampPop
    tUnion
    tDistinct
    tReduce
  NodeSpec* = object
    case tag*: NodeSpecTag:
      of tMap:
        input*: Node
        mapper*: Mapper
      of tJoin:
        inputs*: (Node, Node)
        key_columns*: Natural
      of tUnion:
        inputs*: (Node, Node)
      of tReduce:
        input*: Node
        key_columns*: Natural
        init_value*: Value
        reducer*: Reducer
      of tIndex, tOutput, tTimestampPush, tTimestampIncrement, tTimestampPop, tDistinct:
        input*: Node
      else:
        discard
  NodeState* = object
    case tag*: NodeSpecTag:
      of tInput:
        frontier*: Frontier
        unflushed_changes*: ChangeBatchBuilder
      of tIndex:
        index*: Index
        pending_changes*: seq[Change]
      of tJoin:
        index_input_frontiers*: (Frontier, Frontier)
      of tOutput:
        unpopped_change_batches*: seq[ChangeBatch]
      of tDistinct, tReduce:
        index*: Index
        pending_corrections*: Table[Row, HashSet[Timestamp]]
      else:
        discard
      
  Subgraph* = object
    id*: Natural

  Graph* = object
    node_specs*: seq[NodeSpec]
    node_subgraphs*: seq[seq[Subgraph]]
    subgraph_parents*: seq[Subgraph]
    downstream_node_inputs*: seq[NodeInput]

  GraphBuilder* = object
    node_specs*: seq[NodeSpec]
    node_subgraphs*: seq[Subgraph]
    subgraph_parents*: seq[Subgraph]
  
  ChangeBatchAtNodeInput* = object
    change_batch*: ChangeBatch
    input_frontier*: Frontier
    node_input*: NodeInput

  Pointstamp* = object
    node_input*: NodeInput
    subgraphs*: seq[Subgraph]
    timestamp*: Timestamp

  Shard* = object
    graph*: Graph
    node_states*: seq[NodeState]
    node_frontiers*: seq[SupportedFrontier]
    unprocessed_change_batches*: seq[ChangeBatchAtNodeInput]
    unprocessed_frontier_updates*: Table[Pointstamp, int]



    