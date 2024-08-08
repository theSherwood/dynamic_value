import std/[math, algorithm, strutils, strformat]
import ../../../src/[values]

const WARMUP* = 100_000 # microseconds
const TIMEOUT* = 100_000

when defined(wasm):
  proc get_time*(): float64 {.importc.}
  proc write_row_string(p: ptr, len: int): void {.importc.}
  proc write_row*(row: string): void =
    write_row_string(row[0].addr, row.len)
  when defined(wasm32):
    const sys* = "wasm32"
  when defined(wasm64):
    const sys* = "wasm64"
else:
  from std/times import cpuTime
  # We have to multiply our seconds by 1_000_000 to get microseconds
  const SCALE = 1_000_000
  proc get_time*(): float64 =
    return cpuTime() * SCALE
  let file_name = "./benchmark/results_native.csv"
  let fd = file_name.open(fmWrite)
  proc write_row*(row: string): void =
    fd.writeLine(row)
  const sys* = "native"

type
  TaskResult* = ref object
    key*, desc*: string
    runs*: seq[float64]

var csv_rows*: seq[TaskResult] = @[] 

template form*(f: float64): string = f.formatFloat(ffDecimal, 2)

proc to_row*(tr: TaskResult): string =
  var
    l = tr.runs.len
    s = &"\"{tr.key}\",\"{sys}\",\"{tr.desc}\",{l},"
    sorted_runs = tr.runs.sorted()
    sum     = 0.0
    minimum = Inf
    maximum = 0.0
    mean    = 0.0
    median  = 0.0
  for r in sorted_runs:
    sum += r
    minimum = min(minimum, r)
    maximum = max(maximum, r)
  mean = sum / l.float64
  if sorted_runs.len == 1:
    median = sorted_runs[0]
  else:
    median = (
      sorted_runs[(l / 2).floor.int] + sorted_runs[(l / 2).ceil.int]
    ) / 2
  s = &"{s}{minimum.form},{maximum.form},{mean.form},{median.form}"
  return s

template add*(tr: TaskResult, v: float64) = tr.runs.add(v)

proc make_tr*(key, desc: string): TaskResult = 
  var tr = TaskResult()
  tr.key = key
  tr.desc = desc
  tr.runs = @[]
  csv_rows.add(tr)
  return tr

proc warmup*() =
  var
    Start = get_time()
    End = get_time()
  while WARMUP > End - Start:
    End = get_time()

proc bench*(
  key, desc: string,
  fn: proc(tr: TaskResult, size, iterations: int): void,
  size, iterations, timeout: int
  ) =
  var
    tr = make_tr(&"{key}_{size}_{iterations}", desc)
    Start = get_time()
    End = get_time()
  # run it at least once
  block:
    fn(tr, size, iterations)
    End = get_time()
  while timeout.float64 > (End - Start):
    fn(tr, size, iterations)
    End = get_time()
  echo &"done {tr.key}"
template bench*(
  key, desc: string,
  fn: proc(tr: TaskResult, size, iterations: int): void,
  size, iterations: int
  ) =
  bench(key, desc, fn, size, iterations, TIMEOUT)
