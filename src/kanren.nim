##
## https://github.com/shd101wyy/logic.js/blob/master/lib/logic.js
## 

import std/[macros, strutils, sequtils, strformat]
import values
export values

# ====================================================================
# ITER HELPERS
# ====================================================================

const DEBUG_ITER* = false
const ITER_ID* = 0

## a list of which indents are in use (true is in use; false is free)
## We start with the first value in use. This corresponds to the main thread.
var indents = @[true]
## map of ids (the index) to indent (the value)
## We start with the first value in use. This corresponds to the main thread. 
var registry = @[ITER_ID]

proc print_impl*(id: int, s: string) =
  let indent = registry[id]
  echo indent(&"{id} {s}", indent, "|   ")
template print*(the_print_string: string) {.dirty.} =
  when DEBUG_ITER:
    print_impl(ITER_ID, the_print_string)
  else:
    echo the_print_string

proc register(debug_label: string = ""): int = 
  var idx = indents.find(false)
  if idx < 0:
    idx = indents.len
    indents.add(true)
  indents[idx] = true
  let id = registry.len
  registry.add(idx)
  print_impl(id, &"START {debug_label}")
  return id

proc deregister(id: int) =
  indents[registry[id]] = false
  print_impl(id, "END")

macro iter(name, params, yield_type, body: untyped): untyped =
  let name_string = name.strVal
  var resolved_params = @[newIdentNode(yield_type.strVal)]
  for colon_expr in params:
    resolved_params.add(nnkIdentDefs.newTree(
      colon_expr[0],
      colon_expr[1],
      newEmptyNode()
    ))
  let new_body = quote do: 
    when DEBUG_ITER:
      let ITER_ID {.inject.} = register(`name_string`)
      `body`
      deregister(ITER_ID)
    else:
      `body`
  result = newProc(
    params = resolved_params,
    procType = nnkIteratorDef,
    body = new_body)

macro gen_iter(name, params, yield_type, body: untyped): untyped =
  let name_string = name.strVal
  var iterator_type = newNimNode(nnkIteratorTy).add(
    newNimNode(nnkFormalParams).add(@[yield_type]),
    newEmptyNode(),
  )
  var resolved_params = @[iterator_type]
  for colon_expr in params:
    resolved_params.add(nnkIdentDefs.newTree(
      colon_expr[0],
      colon_expr[1],
      newEmptyNode()
    ))
  let new_body = quote do: 
    when DEBUG_ITER:
      result = iterator(): `yield_type` =
        let ITER_ID {.inject.} = register(`name_string`)
        `body`
        deregister(ITER_ID)
    else:
      result = iterator(): `yield_type` =
        `body`
  result = newProc(
    params = resolved_params,
    body = new_body)

template yeet*(val: untyped) =
  let THE_VALUE = val
  when DEBUG_ITER:
    print("yeet: " & $THE_VALUE)
  yield THE_VALUE

# ====================================================================
# KANREN CORE
# ====================================================================

type
  Val* = ImValue

  Stream* = iterator(): Val
  GenStream* = proc(x: Val): iterator(): Val

macro Var*(x): untyped = newCall("v", Sym_impl(x))

let dot* = Var "."

template is_lvar*(x: Val): bool = x.is_symbol and x != dot

proc walk(key, smap: Val): Val =
  if key.is_lvar:
    let x = smap[key]
    if x == Nil: return key
    return walk(x, smap)
  else:
    return key

proc deep_walk(key, smap: Val): Val =
  let x = walk(key, smap)
  if x.is_array:
    var o = init_array().v
    for i in 0..<(x.len):
      let y = x[i]
      if y == dot:
        let rest = deep_walk(x[i + 1], smap)
        o = o.concat(rest)
        break
      else:
        o = o.add(deep_walk(y, smap))
    return o
  else:
    return x

proc unify_array*(x, y, smap: Val): Val

proc unify*(x, y, smap: Val): Val =
  let rx = x.walk(smap)
  let ry = y.walk(smap)
  if rx == ry: return smap
  if rx.is_lvar: return smap.set(rx, ry)
  if ry.is_lvar: return smap.set(ry, rx)
  if rx.is_array and ry.is_array: return unify_array(rx, ry, smap)
  return Nil.v

proc unify_array*(x, y, smap: Val): Val =
  if x.len == 0 and y.len == 0: return smap
  if x[0] == dot: return unify(x[1], y, smap)
  if y[0] == dot: return unify(y[1], x, smap)
  if x.len == 0 or y.len == 0: return Nil.v
  let s = unify(x[0], y[0], smap)
  if s != Nil: return unify(x.slice(1, x.len), y.slice(1, y.len), s)
  return s

proc ando_helper(clauses: seq[GenStream], offset: int, smap: Val): Stream =
  return iter(ando_helper, (), Val):
    if offset == clauses.len: return
    let clause = clauses[offset]
    var it = clause(smap)
    for x in it():
      if x == Nil: yeet x                             # error?
      elif offset == clauses.len - 1: yeet x
      else:
        var it = ando_helper(clauses, offset + 1, x)
        for y in it():
          yeet y
proc ando*(clauses: varargs[GenStream]): GenStream =
  let c = toSeq(clauses)
  return gen_iter(ando, (smap: Val), Val):
    var it = ando_helper(c, 0, smap)
    for x in it():
      yeet x

proc oro_helper(clauses: seq[GenStream], offset, sol_num: int, smap: Val): Stream =
  return iter(oro_helper, (), Val):
    if offset != clauses.len:
      let clause = clauses[offset]
      var x = smap
      var s_num = sol_num
      var it = clause(smap)
      for x in it():
        if x != Nil:
          yeet x
          s_num += 1
      it = oro_helper(clauses, offset + 1, s_num, x)
      for y in it():
        yeet y
proc oro*(clauses: varargs[GenStream]): GenStream =
  let
    offset = 0
    sol_num = 0
    c = toSeq(clauses)
  return gen_iter(oro, (smap: Val), Val):
    var it = oro_helper(c, offset, sol_num, smap)
    for x in it():
      yeet x

proc conde*(conds: varargs[seq[GenStream]]): GenStream =
  return oro(conds.map(proc(c: seq[GenStream]): GenStream = ando(c)))

proc run*(vars: openArray[Val], goal: GenStream): Stream =
  let lvars = toSeq(vars)
  return iter(run, (), Val):
    let smap = init_map().v
    var it = goal(smap)
    for x in it():
      if x != Nil:
        var new_map = init_map().v
        for lvar in lvars:
          new_map = new_map.set(lvar, deep_walk(lvar, x))
        yeet new_map
proc run*(num: int, vars: openArray[Val], goal: GenStream): seq[Val] =
  var n = num
  var it = run(vars, goal)
  for x in it():
    if n == 0: break
    n -= 1
    result.add(x)

proc noto*(goal: GenStream): GenStream =
  return gen_iter(noto, (smap: Val), Val):
    var inner_goal_success = false
    var it = goal(smap)
    for x in it():
      if x != Nil:
        inner_goal_success = true
        break
    if inner_goal_success: yeet Nil.v
    else:                  yeet smap

proc nando*(clauses: varargs[GenStream]): GenStream =
  let c = toSeq(clauses)
  var goal = gen_iter(nando, (smap: Val), Val):
    var it = ando_helper(c, 0, smap)
    for x in it():
      yeet x
  return noto(goal)

proc noro*(clauses: varargs[GenStream]): GenStream =
  let
    offset = 0
    sol_num = 0
    c = toSeq(clauses)
  var goal = gen_iter(noro, (smap: Val), Val):
    var it = oro_helper(c, offset, sol_num, smap)
    for x in it():
      yeet x
  return noto(goal)

macro fresh*(lvars, body: untyped): untyped =
  template def_lvar(x): untyped =
    let x = Var x
  var defs = newStmtList()
  for lvar in lvars:
    defs.add(getAst(def_lvar(lvar)))
  let gen_iter = bindSym("gen_iter")
  result = quote do: (proc(): GenStream =
    `defs`
    return `gen_iter`(fresh, (smap: Val), Val):
      var bod = `body`
      var it = bod(smap)
      for z in it():
        yeet z
  )()

template C*(p: string, args: varargs[NimNode]): NimNode = newCall(p, args)
template Z*(x: untyped): untyped = V_impl(x)

proc eqo_impl*(x, y: Val): GenStream =
  return gen_iter(eqo_impl, (smap: Val), Val):
    yeet unify(x, y, smap)
macro eqo*(x, y): untyped = C("eqo_impl", Z(x), Z(y))

proc neqo_impl*(x, y: Val): GenStream =
  return noto(eqo(x, y))
macro neqo*(x, y): untyped = C("neqo_impl", Z(x), Z(y))

# ====================================================================
# KANREN ADDITIONAL OPERATORS
# ====================================================================

proc conso_impl*(first, rest, output: Val): GenStream =
  if rest.is_lvar: return eqo(V [first, dot, rest], output)
  return eqo(rest.prepend(first), output)
macro conso*(first, rest, output): untyped = C("conso_impl", Z(first), Z(rest), Z(output))

proc firsto_impl*(first, output: Val): GenStream =
  return conso(first, Var(rest), output)
macro firsto*(first, output): untyped = C("firsto_impl", Z(first), Z(output))

proc resto_impl*(rest, output: Val): GenStream =
  return conso(Var(first), rest, output)
macro resto*(rest, output): untyped = C("resto_impl", Z(rest), Z(output))

proc emptyo_impl*(x: Val): GenStream =
  return eqo(x, V Arr [])
macro emptyo*(x): untyped = C("emptyo_impl", Z(x))

proc membero_impl*(x, arr: Val): GenStream =
  return oro(@[
    fresh([first], ando(@[firsto(first, arr), eqo(first, x)])),
    fresh([rest], ando(@[resto(rest, arr), membero_impl(x, rest)])),
  ])
macro membero*(x, arr): untyped = C("membero_impl", Z(x), Z(arr))

proc appendo_impl*(arr1, arr2, output: Val): GenStream =
  return oro(@[
    ando(@[emptyo(arr1), eqo(arr2, output)]),
    fresh([first, rest, rec], ando(@[
      conso(first, rest, arr1),
      conso(first, rec, output),
      appendo_impl(rest, arr2, rec),
    ]))
  ])
macro appendo*(arr1, arr2, output): untyped = C("appendo_impl", Z(arr1), Z(arr2), Z(output)) 

proc predo*(fn: proc(smap: Val, walk: proc(key, smap: Val): Val): bool): GenStream =
  return gen_iter(predo, (smap: Val), Val):
    if fn(smap, walk): yeet smap
    else: yeet Nil.v

proc stringo_impl*(x: Val): GenStream =
  return gen_iter(stringo_impl, (smap: Val), Val):
    if walk(x, smap).is_string: yeet smap
    else: yeet Nil.v
macro stringo*(x): untyped = C("stringo_impl", Z(x))

proc numbero_impl*(x: Val): GenStream =
  return gen_iter(numbero_impl, (smap: Val), Val):
    if walk(x, smap).is_num: yeet smap
    else: yeet Nil.v
macro numbero*(x): untyped = C("numbero_impl", Z(x))

proc arrayo_impl*(x: Val): GenStream =
  return gen_iter(arrayo_impl, (smap: Val), Val):
    if walk(x, smap).is_array: yeet smap
    else: yeet Nil.v
macro arrayo*(x): untyped = C("arrayo_impl", Z(x))

proc add_impl*(a, b, c: Val): GenStream =
  ## a + b = c
  return gen_iter(add_impl, (smap: Val), Val):
    let x = walk(a, smap)
    let y = walk(b, smap)
    let z = walk(c, smap)
    var
      lvars_count = 0
      lvar = Nil.v
    if x.is_symbol:
      lvars_count += 1
      lvar = x
    if y.is_symbol:
      lvars_count += 1
      lvar = y
    if z.is_symbol:
      lvars_count += 1
      lvar = z
    if lvars_count == 0:
      if x + y == z: yeet smap
      else: yeet Nil.v
    elif lvars_count == 1:
      if lvar == x:
        if y.is_num and z.is_num:
          var it = eqo(x, z - y)(smap)
          for a in it(): yeet a
        else: yeet Nil.v
      elif lvar == y:
        if x.is_num and z.is_num:
          var it = eqo(y, z - x)(smap)
          for b in it(): yeet b
        else: yeet Nil.v
      else:
        if x.is_num and y.is_num:
          var it = eqo(z, x + y)(smap)
          for c in it(): yeet c
        else: yeet Nil.v
    else: yeet Nil.v
macro add*(a, b, c): untyped = C("add_impl", Z(a), Z(b), Z(c))
macro sub*(a, b, c): untyped = C("add_impl", Z(b), Z(c), Z(a))

proc mul_impl*(a, b, c: Val): GenStream =
  ## a * b = c
  return gen_iter(mul_impl, (smap: Val), Val):
    let x = walk(a, smap)
    let y = walk(b, smap)
    let z = walk(c, smap)
    var
      lvars_count = 0
      lvar = Nil.v
    if x.is_symbol:
      lvars_count += 1
      lvar = x
    if y.is_symbol:
      lvars_count += 1
      lvar = y
    if z.is_symbol:
      lvars_count += 1
      lvar = z
    if lvars_count == 0:
      if x * y == z: yeet smap
      else: yeet Nil.v
    elif lvars_count == 1:
      if lvar == x:
        if y.is_num and z.is_num:
          var it = eqo(x, z / y)(smap)
          for a in it(): yeet a
        else: yeet Nil.v
      elif lvar == y:
        if x.is_num and z.is_num:
          var it = eqo(y, z / x)(smap)
          for b in it(): yeet b
        else: yeet Nil.v
      else:
        if x.is_num and y.is_num:
          var it = eqo(z, x * y)(smap)
          for c in it(): yeet c
        else: yeet Nil.v
    else: yeet Nil.v
macro mul*(a, b, c): untyped = C("mul_impl", Z(a), Z(b), Z(c))
macro dis*(a, b, c): untyped = C("mul_impl", Z(b), Z(c), Z(a))

proc mapi[T, U](a: openArray[T], fn: proc(v: T, i: int): U): seq[U] =
  var i = 0
  for v in a:
    result.add(fn(v, i))
    i += 1

proc lt_impl*(x, y: Val): GenStream =
  return gen_iter(lt, (smap: Val), Val):
    let a = walk(x, smap)
    let b = walk(y, smap)
    if a.is_num and b.is_num and a < b: yeet smap
    elif a.is_string and b.is_string and a < b: yeet smap
    else: yeet Nil.v
macro lt*(x, y): untyped = C("lt_impl", Z(x), Z(y))
macro gt*(x, y): untyped = C("lt_impl", Z(y), Z(x))

proc le_impl*(x, y: Val): GenStream =
  return gen_iter(lt, (smap: Val), Val):
    let a = walk(x, smap)
    let b = walk(y, smap)
    if a.is_num and b.is_num and a <= b: yeet smap
    elif a.is_string and b.is_string and a <= b: yeet smap
    else: yeet Nil.v
macro le*(x, y): untyped = C("le_impl", Z(x), Z(y))
macro ge*(x, y): untyped = C("le_impl", Z(y), Z(x))

proc succeedo*(): GenStream =
  return gen_iter(succeedo, (smap: Val), Val):
    yeet smap

proc failo*(): GenStream =
  return gen_iter(failo, (smap: Val), Val):
    yeet Nil.v

proc anyo*(goal: GenStream): GenStream =
  return oro(@[goal, fresh([], anyo(goal))])

# ====================================================================
# OTHER
# ====================================================================

proc facts*(facs: seq[Val]): proc(args: seq[Val]): GenStream =
  result = proc(args: seq[Val]): GenStream =
    result = oro(facs.map(
      proc(fac: Val): GenStream =
        ando(args.mapi(
          proc(arg: Val, i: int): GenStream =
            eqo(arg, fac[i])
        ))
    ))
