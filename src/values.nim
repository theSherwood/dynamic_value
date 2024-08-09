## TODO
## 
## -[ ] add Infinity, -Infinity
## -[ ] add a converter from int to Value
## -[ ] add an ability to push onto the end of an array
## -[ ] differentiate between inner api and outer api
##   - Eg.
##     - proc inner_contains(v1, v2: Value): bool
##     - proc outer_contains(v1, v2: Value): Value
## -[ ] find a way to not have to reimplement iterators (eg, for VMap as well as Value)

import std/[tables, sets, bitops, strutils, sequtils, strformat, macros]
import hashes
# import persistent/[sumtree]
import ../submodules/persistent/src/vec except `==`
import ../submodules/persistent/src/map except `==`

## # Immutable Value Types
## =======================
##
## ## Priority
## -----------
##
##   Immediates:
##     number(float64), NaN, nil, bool, atom(small string)
##
##   Heaps:
##     string, bignum, array, map, set
##
## ## Some additional types we could add later
## -------------------------------------------
##
##   Immediates:
##     atom-symbol, timestamp(no timezone?), bitset(48-bit), binary(48-bit),
##     int, small byte array (48-bit, useful for small tuples)
##
##   Heaps:
##     regex, time, date, datetime, pair, tuple, closure, symbol, tag, path,
##     var/box(reactive?), email, version, typedesc/class, vector(homogenous),
##     bitset, binary, unit(measurements), ...
##     (...or mutable types?:)
##     mut-array, mut-map, mut-set, mut-vector, ...
##     (...or ruliad-specific:) 
##     id, branch, patch
##
##
## # NaN-boxing scheme for Immediates (it's the same for 32-bit and 64-bt)
## =======================================================================
##
## 32 bits                          | 32 bits
## XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX | XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  number(float64)
##
## 0000... - plain NaN
##
## +- Immediate bit (1)
## |+- Exponent bits (11)
## ||          +- Quiet bit (1)
## ||          |
## 01111111111110000000000000000000 | 00000000000000000000000000000000  NaN
##
## Immediate types (3 bits, because atom needs a 48-bit payload)
## 000 - (cannot use because of collision with NaN)
## 001 - logical (nil | true | false)
## 010 - atom (string of max 6 bytes)
## 011-111 - (unused, 5 values)
##
## If there are other types that don't need 6 bytes of payload, we could add
## a lot more types. If we only need 4 bytes of payload, for example, we
## could add thousands of types. So we really aren't short of bits for
## specifying types.
##
## +- Immediate bit (1)
## |+- Exponent bits (11)
## ||          +- Quiet bit (1)
## ||          |+- Immediate type bits (3)
## ||          ||  +- Payload bits (48)
## ||          ||  |
## 01111111111110010000000000000000 | 00000000000000000000000000000000  nil
## 01111111111110011000000000000000 | 00000000000000000000000000000000  false
## 01111111111110011100000000000000 | 00000000000000000000000000000000  true
## 0111111111111010XXXXXXXXXXXXXXXX | XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  atom
##
## 
## # NaN-boxing scheme for Heaps (differs for 32-bit and 64-bt)
## ============================================================
##
## Specifically, the smaller pointers of a 32-bit system let us take
## advantage of the lower 15 or 16 bits of the top 32 bits to store a short
## hash. This lets us do equality checks for values of the same type without
## following the pointer and without interning/hash-consing. Each heap-
## allocated value has a full hash as well.
## 
## With 64-bit systems, we make use of the lower 48-bits as a pointer. In
## order to perform equality checks for values of the same type, we have to
## dereference the pointer to get to the full hash.
## 
## Currently, the designs have the Heap types avoiding 000, but this may not
## be necessary because we should be able to discriminate by using the
## leading/sign/heap bit.
## 
## 
## ## 32-bit systems
## -----------------
## 
## ### OPTION 1 : (4 bits, leaves 15-bit short hash, 32768 values)
## 
## Heap types (4 bits)
## 0001 - string
## 0010 - bignum
## 0011 - array
## 0100 - set
## 0101 - map
## 0110-1111 - (unused, 10 values)
##
## ### OPTION 2 : (3 bits, leaves 16-bit short hash, 65536 values)
## 
## Heap types (3 bits)
## 001 - string
## 010 - bignum
## 011 - array
## 100 - set
## 101 - map
## 110 - symbol
## 111 - (unused)
##
## ### Going with OPTION 2 for now
## 
## Option 2 is more consistent with 64-bit systems
##
## +- Heap bit (1)
## |            +- Heap type bits (4)
## |            |   +- Short content hash (15 bits, only 32768 values)
## |            |   |                 +- Pointer (32)
## |            |   |                 |
## 11111111111110001XXXXXXXXXXXXXXX | XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  string
## 11111111111110010XXXXXXXXXXXXXXX | XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  bignum
## 11111111111110011XXXXXXXXXXXXXXX | XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  array
## 11111111111110100XXXXXXXXXXXXXXX | XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  set
## 11111111111110101XXXXXXXXXXXXXXX | XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  map
##
##
## ## 64-bit systems
## -----------------
##
## Heap types (3 bits)
## 001 - string
## 010 - bignum
## 011 - array
## 100 - set
## 101 - map
## 110 - symbol
## 111 - (unused)
##
## +- Heap bit (1)
## |            +- Heap type bits (3)
## |            |  +- Pointer (48 bits)
## |            |  |
## 1111111111111001XXXXXXXXXXXXXXXX | XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  string
## 1111111111111010XXXXXXXXXXXXXXXX | XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  bignum
## 1111111111111011XXXXXXXXXXXXXXXX | XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  array
## 1111111111111100XXXXXXXXXXXXXXXX | XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  set
## 1111111111111101XXXXXXXXXXXXXXXX | XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  map
##

const c32 = defined(cpu32)

# Types #
# ---------------------------------------------------------------------

type
  TypeException* = object of CatchableError

  ValueKind* = enum
    # Immediate Kinds
    kNil
    kBool
    kNum           # like js, we just have a float64 number type
    # Heap Kinds
    kSym
    kStr
    kVec
    kMap
    kSet
  
when c32:
  type Value* = object
    tail: uint32
    head: uint32

  proc `=destroy`(x: var Value)
  proc `=copy`(x: var Value, y: Value)

else:
  type Value* = distinct uint64

type
  VStrPayload* = object
    hash: Hash
    data: string
  VVecPayload* = Vec[Value]
  VMapPayload* = Map[Value, Value]
  VSetPayload* = Set[Value]
  VSymPayload* = object
    id: uint
    data: string
  VStrPayloadRef* = ref VStrPayload
  VVecPayloadRef* = ref VVecPayload
  VMapPayloadRef* = ref VMapPayload
  VSetPayloadRef* = ref VSetPayload
  VSymPayloadRef* = ref VSymPayload

  VNil*    = distinct uint64
  VBool*   = distinct uint64

when c32:
  type
    VStr* = object
      tail*: VStrPayloadRef
      head*: uint32
    VVec* = object
      tail*: VVecPayloadRef
      head*: uint32
    VMap* = object
      tail*: VMapPayloadRef
      head*: uint32
    VSet* = object
      tail*: VSetPayloadRef
      head*: uint32
    VSym* = object
      tail*: VSymPayloadRef
      head*: uint32
else:
  type
    MaskedRef*[T] = object
      # distinct should work in theory, but I'm not entirely sure how well phantom types work with distinct at the moment
      p: pointer
    VStr* = MaskedRef[VStrPayload]
    VVec* = MaskedRef[VVecPayload]
    VMap* = MaskedRef[VMapPayload]
    VSet* = MaskedRef[VSetPayload]
    VSym* = MaskedRef[VSymPayload]

# Forward Declarations #
# ---------------------------------------------------------------------

## Forward declare these so that the equality procs in the collection
## implementations can rely on them.
func `==`*(v1, v2: Value): bool
func `==`*(v: Value, f: float64): bool
func `==`*(f: float64, v: Value): bool
func `==`*(v1, v2: VStr): bool
func `==`*(v1, v2: VMap): bool
func `==`*(v1, v2: VVec): bool
func `==`*(v1, v2: VSet): bool
func `==`*(v1, v2: VSym): bool

## Import the `==` of the persistent collections so that they can "see" the
## forward declarations of this module.
from ../submodules/persistent/src/vec import `==`
from ../submodules/persistent/src/map import `==`

## Forward declare this so that we make sure the same hash function is always
## used for every operation involving Value
func hash*(v: Value): Hash

# Casts #
# ---------------------------------------------------------------------

template as_f64*(v: typed): float64 = cast[float64](v)
template as_u64*(v: typed): uint64 = cast[uint64](v)
template as_i64*(v: typed): int64 = cast[int64](v)
template as_u32*(v: typed): uint32 = cast[uint32](v)
template as_i32*(v: typed): int32 = cast[int32](v)
template as_hash*(v: typed): Hash = cast[Hash](v)
template as_p*(v: typed): pointer = cast[pointer](v)
template as_byte_array_8*(v: typed): array[8, byte] = cast[array[8, byte]](v)
template as_v*(v: typed): Value = cast[Value](cast[uint64](v))
template as_str*(v: typed): VStr = cast[VStr](cast[uint64](v))
template as_vec*(v: typed): VVec = cast[VVec](cast[uint64](v))
template as_map*(v: typed): VMap = cast[VMap](cast[uint64](v))
template as_set*(v: typed): VSet = cast[VSet](cast[uint64](v))
template as_sym*(v: typed): VSym = cast[VSym](cast[uint64](v))

# Conversions #
# ---------------------------------------------------------------------

template v*(x: Value): Value = x
template v*(x: VBool): Value = x.as_v
template v*(x: VStr): Value = x.as_v
template v*(x: VSet): Value = x.as_v
template v*(x: VVec): Value = x.as_v
template v*(x: VMap): Value = x.as_v
template v*(x: VSym): Value = x.as_v
template v*(x: VNil): Value = x.as_v

# A couple of forward declarations for the conversions
proc init_string*(s: string = ""): VStr
proc init_array*(init_data: openArray[Value]): VVec

template v*(x: float64): Value = x.as_v
template v*(x: int): Value = x.float64.v
template v*(x: bool): Value = (if x: True.v else: False.v)
template v*(x: string): Value = x.init_string.v
template v*(x: openArray[int]): Value = toSeq(x).map(x => x.v).init_array.v
template v*(x: openArray[float64]): Value = toSeq(x).map(x => x.v).init_array.v
template v*(x: openArray[Value]): Value = x.init_array.v

template to_int*(x: Value): int = x.as_f64.int

# Masks #
# ---------------------------------------------------------------------

when c32:
  # const MASK_SIGN        = 0b10000000000000000000000000000000'u32
  const MASK_EXPONENT    = 0b01111111111100000000000000000000'u32
  # const MASK_QUIET       = 0b00000000000010000000000000000000'u32
  const MASK_EXP_OR_Q    = 0b01111111111110000000000000000000'u32
  const MASK_SIGNATURE   = 0b11111111111111111000000000000000'u32
  const MASK_SHORT_HASH  = 0b00000000000000000111111111111111'u32
  const MASK_HEAP        = 0b11111111111110000000000000000000'u32

  # const MASK_TYPE_NAN    = 0b00000000000000000000000000000000'u32
  const MASK_TYPE_NIL    = 0b00000000000000010000000000000000'u32
  const MASK_TYPE_FALSE  = 0b00000000000000011000000000000000'u32
  const MASK_TYPE_TRUE   = 0b00000000000000011100000000000000'u32
  const MASK_TYPE_BOOL   = 0b00000000000000011000000000000000'u32
  const MASK_TYPE_ATOM   = 0b00000000000000100000000000000000'u32

  const MASK_TYPE_STR    = 0b10000000000000010000000000000000'u32
  const MASK_TYPE_ARR    = 0b10000000000000110000000000000000'u32
  const MASK_TYPE_SET    = 0b10000000000001000000000000000000'u32
  const MASK_TYPE_MAP    = 0b10000000000001010000000000000000'u32
  const MASK_TYPE_SYM    = 0b10000000000001100000000000000000'u32

else:
  # const MASK_SIGN        = 0b10000000000000000000000000000000'u64 shl 32
  const MASK_EXPONENT    = 0b01111111111100000000000000000000'u64 shl 32
  # const MASK_QUIET       = 0b00000000000010000000000000000000'u64 shl 32
  const MASK_EXP_OR_Q    = 0b01111111111110000000000000000000'u64 shl 32
  const MASK_SIGNATURE   = 0b11111111111111110000000000000000'u64 shl 32
  const MASK_HEAP        = 0b11111111111110000000000000000000'u64 shl 32

  # const MASK_TYPE_NAN    = 0b00000000000000000000000000000000'u64 shl 32
  const MASK_TYPE_NIL    = 0b00000000000000010000000000000000'u64 shl 32
  const MASK_TYPE_FALSE  = 0b00000000000000011000000000000000'u64 shl 32
  const MASK_TYPE_TRUE   = 0b00000000000000011100000000000000'u64 shl 32
  const MASK_TYPE_BOOL   = 0b00000000000000011000000000000000'u64 shl 32
  const MASK_TYPE_ATOM   = 0b00000000000000100000000000000000'u64 shl 32

  const MASK_TYPE_STR    = 0b10000000000000010000000000000000'u64 shl 32
  const MASK_TYPE_ARR    = 0b10000000000000110000000000000000'u64 shl 32
  const MASK_TYPE_SET    = 0b10000000000001000000000000000000'u64 shl 32
  const MASK_TYPE_MAP    = 0b10000000000001010000000000000000'u64 shl 32
  const MASK_TYPE_SYM    = 0b10000000000001100000000000000000'u64 shl 32

  const MASK_POINTER     = 0x0000ffffffffffff'u64

# const MASK_SIG_NAN     = MASK_EXP_OR_Q
const MASK_SIG_NIL     = MASK_EXP_OR_Q or MASK_TYPE_NIL
const MASK_SIG_FALSE   = MASK_EXP_OR_Q or MASK_TYPE_FALSE
const MASK_SIG_TRUE    = MASK_EXP_OR_Q or MASK_TYPE_TRUE
const MASK_SIG_BOOL    = MASK_EXP_OR_Q or MASK_TYPE_BOOL
const MASK_SIG_STR     = MASK_EXP_OR_Q or MASK_TYPE_STR
const MASK_SIG_VEC     = MASK_EXP_OR_Q or MASK_TYPE_ARR
const MASK_SIG_SET     = MASK_EXP_OR_Q or MASK_TYPE_SET
const MASK_SIG_MAP     = MASK_EXP_OR_Q or MASK_TYPE_MAP
const MASK_SIG_SYM     = MASK_EXP_OR_Q or MASK_TYPE_SYM

# Get Payload #
# ---------------------------------------------------------------------

when c32:
  template payload*(v: VStr): VStrPayloadRef = v.tail
  template payload*(v: VMap): VMapPayloadRef = v.tail
  template payload*(v: VVec): VVecPayloadRef = v.tail
  template payload*(v: VSet): VSetPayloadRef = v.tail
  template payload*(v: VSym): VSymPayloadRef = v.tail
else:
  template to_clean_ptr(v: typed): pointer =
    cast[pointer](bitand((v).as_u64, MASK_POINTER))

  template payload*(v: VStr): VStrPayloadRef = cast[VStrPayloadRef](to_clean_ptr(v.p))
  template payload*(v: VMap): VMapPayloadRef = cast[VMapPayloadRef](to_clean_ptr(v.p))
  template payload*(v: VVec): VVecPayloadRef = cast[VVecPayloadRef](to_clean_ptr(v.p))
  template payload*(v: VSet): VSetPayloadRef = cast[VSetPayloadRef](to_clean_ptr(v.p))
  template payload*(v: VSym): VSymPayloadRef = cast[VSymPayloadRef](to_clean_ptr(v.p))

# Type Detection #
# ---------------------------------------------------------------------

when c32:
  template type_bits*(v: typed): uint32 =
    v.as_v.head
else:
  template type_bits*(v: typed): uint64 =
    v.as_u64

template is_num*(v: typed): bool =
  bitand(bitnot(v.type_bits), MASK_EXPONENT) != 0
template is_nil*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_NIL
template is_str*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_STR
template is_vec*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_VEC
template is_set*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_SET
template is_map*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_MAP
template is_sym*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_SYM
template is_bool*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_BOOL
template is_heap*(v: typed): bool =
  bitand(v.type_bits, MASK_HEAP) == MASK_HEAP

proc get_type*(v: Value): ValueKind =
  let type_carrier = v.type_bits
  if v.is_num: return kNum
  let signature = bitand(type_carrier, MASK_SIGNATURE)
  case signature:
    of MASK_SIG_NIL:    return kNil
    of MASK_SIG_BOOL:   return kBool
    of MASK_SIG_STR:    return kStr
    # of MASK_SIG_BIGNUM: return kBigNum
    of MASK_SIG_VEC:    return kVec
    of MASK_SIG_SET:    return kSet
    of MASK_SIG_MAP:    return kMap
    of MASK_SIG_SYM:    return kSym
    else:               echo "Unknown Type!"

# GC Hooks #
# ---------------------------------------------------------------------

when c32:
  proc `=destroy`(x: var Value) =
    if x.is_map:
      GC_unref(cast[VMapPayloadRef](x.tail))
    elif x.is_vec:
      GC_unref(cast[VVecPayloadRef](x.tail))
    elif x.is_set:
      GC_unref(cast[VSetPayloadRef](x.tail))
    elif x.is_str:
      GC_unref(cast[VStrPayloadRef](x.tail))
    elif x.is_sym:
      GC_unref(cast[VSymPayloadRef](x.tail))
  proc `=copy`(x: var Value, y: Value) =
    try:
      if x.as_u64 == y.as_u64: return
      if y.is_map:
        GC_ref(cast[VMapPayloadRef](y.tail))
      elif y.is_vec:
        GC_ref(cast[VVecPayloadRef](y.tail))
      elif y.is_set:
        GC_ref(cast[VSetPayloadRef](y.tail))
      elif y.is_str:
        GC_ref(cast[VStrPayloadRef](y.tail))
      elif y.is_sym:
        GC_ref(cast[VSymPayloadRef](y.tail))
      `=destroy`(x)
      x.head = y.head
      x.tail = y.tail
    except:
      raise newException(AssertionDefect, "Failed copy")
else:
  proc `=destroy`[T](x: var MaskedRef[T]) =
    GC_unref(cast[ref T](to_clean_ptr(x.p)))
  proc `=copy`[T](x: var MaskedRef[T], y: MaskedRef[T]) =
    GC_ref(cast[ref T](to_clean_ptr(y.p)))
    x.p = y.p

# Globals #
# ---------------------------------------------------------------------

when c32:
  proc u64_from_mask(mask: uint32): uint64 =
    return (mask.as_u64 shl 32).as_u64
  let Nil*   = cast[VNil](u64_from_mask(MASK_SIG_NIL)).v
  let True*  = cast[VBool](u64_from_mask(MASK_SIG_TRUE)).v
  let False* = cast[VBool](u64_from_mask(MASK_SIG_FALSE)).v
else:
  let Nil*   = cast[VNil]((MASK_SIG_NIL)).v
  let True*  = cast[VBool]((MASK_SIG_TRUE)).v
  let False* = cast[VBool]((MASK_SIG_FALSE)).v

template default*(v: Value): Value = Nil.v

let Infinity*       = Inf
let PosInfinity*    = Inf
let NegInfinity*    = NegInf
let MaxNumber*      = (0x7fefffffffffffff'u64).as_f64
let MinNumber*      = (0xffefffffffffffff'u64).as_f64
let MinSafeInteger* = -9007199254740991.0'f64
let MaxSafeInteger* = 9007199254740991.0'f64

# Equality Testing #
# ---------------------------------------------------------------------

template initial_eq_heap_value(v1, v2: typed): bool =
  when c32:
    v1.head == v2.head
  else:
    bitand(v1.as_u64, MASK_SIGNATURE) == bitand(v2.as_u64, MASK_SIGNATURE)
template eq_heap_payload(t1, t2: typed) =
  result = false
  if t1.hash == t2.hash:
    result = t1.data == t2.data
template eq_heap_value_specific(v1, v2: typed) =
  result = false
  if initial_eq_heap_value(v1, v2):
    eq_heap_payload(v1.payload, v2.payload)
template eq_heap_value_generic*(v1, v2: typed) =
  if initial_eq_heap_value(v1, v2):
    when c32:
      let signature = bitand(v1.head, MASK_SIGNATURE)
    else:
      let signature = bitand(v1.as_u64, MASK_SIGNATURE)
    case signature:
      of MASK_SIG_STR: eq_heap_payload(v1.as_str.payload, v2.as_str.payload)
      of MASK_SIG_VEC:
        result = v1.as_vec.payload == v2.as_vec.payload
      of MASK_SIG_MAP:
        result = v1.as_map.payload == v2.as_map.payload
      of MASK_SIG_SET:
        result = v1.as_set.payload == v2.as_set.payload
      of MASK_SIG_SYM:
        result = v1.as_sym.payload.id == v2.as_sym.payload.id
      else: discard

func `==`*(v1, v2: Value): bool =
  if bitand(MASK_HEAP, v1.type_bits) == MASK_HEAP: eq_heap_value_generic(v1, v2)
  else: return v1.as_u64 == v2.as_u64
func `==`*(v: Value, f: float64): bool = return v == f.as_v
func `==`*(f: float64, v: Value): bool = return v == f.as_v
    
func `==`*(v1, v2: VStr): bool = eq_heap_value_specific(v1, v2)
func `==`*(v1, v2: VMap): bool =
  return v1.payload == v2.payload
func `==`*(v1, v2: VVec): bool =
  return v1.payload == v2.payload
func `==`*(v1, v2: VSet): bool =
  return v1.payload == v2.payload
func `==`*(v1, v2: VSym): bool =
  return v1.payload.id == v2.payload.id

proc `<`*(a, b: Value): bool
proc `<=`*(a, b: Value): bool

template `<`*(a: float64, b: Value): bool = return a.v < b.v
template `<`*(a: Value, b: float64): bool = return a.v < b.v
template `<=`*(a: float64, b: Value): bool = return a.v <= b.v
template `<=`*(a: Value, b: float64): bool = return a.v <= b.v

# Automatic Conversions #
# ---------------------------------------------------------------------

converter toValue*(n: VNil): Value = n.v
converter toValue*(x: VVec): Value = x.v
converter toValue*(x: VMap): Value = x.v
converter toValue*(x: VStr): Value = x.v
converter toValue*(x: VSet): Value = x.v
converter toValue*(x: VSym): Value = x.v
converter toValue*(b: VBool): Value = b.v

converter toValue(f: float64): Value = f.v
converter toValue(i: int): Value = i.v
converter toValue(b: bool): Value = b.v
converter toValue(s: string): Value = s.v

converter toBool*(n: VNil): bool = false
converter toBool*(b: VBool): bool = b == True

# Debug String Conversion #
# ---------------------------------------------------------------------

proc to_hex*(f: float64): string = return toHex(f.as_u64)
proc to_hex*(v: Value): string = return toHex(v.as_u64)
proc to_bin_str*(v: Value): string = return toBin(v.as_i64, 64)
proc to_bin_str*(v: uint32): string = return toBin(v.as_i64, 32)
proc to_bin_str*(v: int32): string = return toBin(v.as_i64, 32)
proc to_bin_str*(v: int64): string = return toBin(v, 64)
proc to_bin_str*(v: uint64): string = return toBin(v.as_i64, 64)

proc `$`*(k: ValueKind): string =
  case k:
    of kNum:  return "Number"
    of kNil:  return "Nil"
    of kStr:  return "String"
    of kMap:  return "Map"
    of kVec:  return "Vector"
    of kSet:  return "Set"
    of kSym:  return "Symbol"
    of kBool: return "Boolean"
    else:     return "<unknown>"

proc `$`*(v: Value): string =
  let kind = get_type(v)
  case kind:
    of kNum:              return $(v.as_f64)
    of kNil:              return "Nil"
    of kStr:              return $(v.as_str.payload.data)
    of kMap:              return $(v.as_map.payload)
    of kVec:              return $(v.as_vec.payload)
    of kSet:              return $(v.as_set.payload) 
    of kSym:              return "'" & v.as_sym.payload.data & "." & $(v.as_sym.payload.id)
    of kBool:
      if v == True.as_v:  return "True"
      if v == False.as_v: return "False"
      # TODO - type error
    else:                 discard

proc debug*(v: Value): string =
  let kind = get_type(v)
  when c32:
    let shallow_str = "( head: " & to_hex(v.head) & ", tail: " & to_hex(v.tail) & " )"
  else:
    let shallow_str = "( " & to_hex(v.as_u64) & " )"
  case kind:
    of kNum:              return "Num" & shallow_str
    of kNil:              return "Nil" & shallow_str
    of kStr:              return "Str" & shallow_str
    of kMap:              return "Map" & shallow_str
    of kVec:              return "Vec" & shallow_str
    of kSet:              return "Set" & shallow_str
    of kSym:              return "Sym" & shallow_str
    of kBool:
      if v == True.as_v:  return "True" & shallow_str
      if v == False.as_v: return "False" & shallow_str
      # TODO - type error
    else:                 discard

template type_label*(v: Value): string = $(v.get_type)

# Hash Handling #
# ---------------------------------------------------------------------

# XOR is commutative, associative, and is its own inverse.
# So we can use this same function to unhash as well.
when c32:
  template calc_hash(i1, i2: typed): Hash = bitxor(i1.as_u32, i2.as_u32).as_hash
else:
  template calc_hash(i1, i2: typed): Hash = bitxor(i1.as_u64, i2.as_u64).as_hash

func hash*(v: Value): Hash =
  if is_heap(v):
    if is_map(v):   result = v.as_map.payload.hash
    elif is_vec(v): result = v.as_vec.payload.summary.hash
    elif is_set(v): result = v.as_set.payload.hash
    elif is_sym(v): result = v.as_sym.payload.id.as_hash
    else:
      # We cast to VStr so that we can get the hash, but all the ImHeapValues have a hash in the tail.
      let vh = cast[VStr](v)
      result = vh.payload.hash.as_hash
  else:
    when c32:
      # We fold it and hash it for 32-bit stack values because a lot of them
      # don't have anything interesting happening in the top 32 bits.
      result = calc_hash(v.head, v.tail).as_hash
    else:
      result = v.as_u64.as_hash

when c32:
  # full_hash is 32 bits
  # short_hash is something like 15 bits (top 17 are zeroed)
  func update_head(previous_head: uint32, full_hash: uint32): uint32 =
    let short_hash = bitand(full_hash.uint32, MASK_SHORT_HASH)
    let truncated_head = bitand(previous_head, bitnot(MASK_SHORT_HASH))
    return bitor(truncated_head, short_hash.uint32).as_u32

const INITIAL_STR_HASH = MASK_TYPE_STR.as_hash
const INITIAL_SET_HASH = MASK_TYPE_SET.as_hash
const INITIAL_ARR_HASH = MASK_TYPE_ARR.as_hash
const INITIAL_MAP_HASH = MASK_TYPE_MAP.as_hash

# VStr Impl #
# ---------------------------------------------------------------------

template buildVStr(new_hash, new_data: typed) {.dirty.} =
  when c32:
    let h = new_hash
    var new_string = VStr(
      head: update_head(MASK_SIG_STR, h.as_u32).as_u32,
      tail: VStrPayloadRef(hash: h, data: new_data)
    )
  else:
    var re = new VStrPayload
    GC_ref(re)
    re.hash = new_hash
    re.data = new_data
    var new_string = VStr(p: bitor(MASK_SIG_STR, re.as_p.as_u64).as_p)
  
func init_string_empty(): VStr =
  let hash = INITIAL_STR_HASH
  let data = ""
  buildVStr(hash, data)
  return new_string

let empty_string = init_string_empty()

proc init_string*(s: string = ""): VStr =
  if s.len == 0: return empty_string
  let hash = hash(s)
  buildVStr(hash, s)
  return new_string
template to_str*(s: string): VStr = s.init_string

proc `[]`*(s: VStr, i: int): Value =
  result = Nil.as_v
  if i < s.payload.data.len:
    if i >= 0:
      result = (init_string($s.payload.data[i])).as_v

proc concat*(s1, s2: VStr): VStr =
  let new_s = s1.payload.data & s2.payload.data
  return init_string(new_s)
proc `&`*(s1, s2: VStr): VStr =
  let new_s = s1.payload.data & s2.payload.data
  return init_string(new_s)

func size*(s: VStr): int =
  return s.payload.data.len.int

func `<`*(v1, v2: VStr): bool = return v1.payload.data < v2.payload.data
func `<=`*(v1, v2: VStr): bool = return v1.payload.data <= v2.payload.data

func to_nim_string(s: VStr): string = s.payload.data

# VSym Impl #
# ---------------------------------------------------------------------

var symbol_id: uint = 0

proc get_symbol_id(): uint =
  symbol_id += 1
  return symbol_id

proc init_symbol*(str: string = ""): VSym = 
  let new_sym_id = get_symbol_id()
  let re = VSymPayloadRef(id: new_sym_id, data: str)
  when c32:
    result = VSym(
      head: update_head(MASK_SIG_SYM, new_sym_id.as_u32).as_u32,
      tail: re
    )
  else:
    GC_ref(re)
    result = VSym(p: bitor(MASK_SIG_SYM, re.as_p.as_u64).as_p)

func to_nim_string(s: VSym): string = s.payload.data

# VMap Impl #
# ---------------------------------------------------------------------

template map_from_pmap(pmap: typed) {.dirty.} =
  when c32:
    var new_map = VMap(
      head: update_head(MASK_SIG_MAP, 0),
      tail: pmap
    )
  else:
    GC_ref(pmap)
    var new_map = VMap(p: bitor(MASK_SIG_MAP, pmap.as_u64).as_p)

func init_map_empty(): VMap =
  var pmap = map.init_map[Value, Value]()
  map_from_pmap(pmap)
  return new_map
  
let empty_map = init_map_empty()

template hash_entry(k, v: typed): Hash = (hash(k).as_u64 + hash(v).as_u64).as_hash

proc init_map*(): VMap = return empty_map
proc init_map*(init_data: openArray[(Value, Value)]): VMap =
  if init_data.len == 0: return empty_map
  var pmap = map.init_map[Value, Value]()
  for (k, v) in init_data:
    if v == Nil.v:
      pmap = pmap.delete(k)
    else:
      pmap = pmap.add(k, v)
  map_from_pmap(pmap)
  return new_map

# There's probably no point in having this. It suggests reference semantics.
proc clear*(m: VMap): VMap =
  return empty_map

proc contains*(m: VMap, k: Value): bool = m.payload.contains(k)
template contains*(m: VMap, k: typed): bool = m.payload.contains(k.v)

proc get_impl(m: VMap, k: Value): Value =
  return m.payload.get_or_default(k, Nil.v)
template `[]`*(m: VMap, k: typed): Value = get_impl(m, k.v)
template get*(m: VMap, k: typed): Value = get_impl(m, k.v)

proc del*(m: VMap, k: Value): VMap =
  if not(k in m): return m
  let new_pmap = m.payload.delete(k)
  map_from_pmap(new_pmap)
  return new_map
template del*(m: VMap, key: typed): VMap = m.del(key.v)

proc set*(m: VMap, k, v: Value): VMap =
  if v == Nil.as_v: return m.del(k)
  let new_pmap = m.payload.add(k, v)
  map_from_pmap(new_pmap)
  return new_map
template set*(m: VMap, key, val: typed): VMap = set(m, key.v, val.v)

func size*(m: VMap): int =
  return m.payload.len.int

iterator values*(m: VMap): Value =
  for v in m.payload.values:
    yield v
iterator keys*(m: VMap): Value =
  for k in m.payload.keys:
    yield k
iterator pairs*(m: VMap): (Value, Value) =
  for p in m.payload.pairs:
    yield p

proc merge*(m1, m2: VMap): VMap =
  ## Asymmetric. Entries in m2 overwrite m1
  if m2.size == 0: return m1
  if m1.size == 0: return m2
  let new_pmap = m1.payload.concat(m2.payload)
  map_from_pmap(new_pmap)
  return new_map
template `&`*(m1, m2: VMap): VMap = m1.merge(m2)

# VVec Impl #
# ---------------------------------------------------------------------

template array_from_vec(pvec: typed) {.dirty} =
  when c32:
    var new_array = VVec(
      head: update_head(MASK_SIG_VEC, 0),
      tail: pvec
    )
  else:
    GC_ref(pvec)
    var new_array = VVec(p: bitor(MASK_SIG_VEC, pvec.as_u64).as_p)

proc init_array_empty(): VVec =
  let vec = init_vec[Value]()
  array_from_vec(vec)
  return new_array

let empty_array = init_array_empty()

proc init_array*(): VVec = return empty_array
proc init_array*(init_data: openArray[Value]): VVec =
  if init_data.len == 0: return empty_array
  let vec = init_data.to_vec
  array_from_vec(vec)
  return new_array

proc size*(a: VVec): int =
  return a.payload.len

## TODO
## - Value indices
## - Negative indices
## - range indices
template get_impl(a: VVec, i: int) =
  return a.payload.getOrDefault(i, Nil.v)
template get_impl(a: VVec, i: Value) =
  if i.is_num:
    get_impl(a, i.as_f64.int)
  else:
    raise newException(TypeException, &"Cannot get with index of {$i} of type {i.type_label}")
template get_impl(a: VVec, i: float64) =
  if i.is_num:
    get_impl(a, i.as_f64.int)
  else:
    raise newException(TypeException, &"Cannot get with index of {$i} of type {i.type_label}")

proc `[]`*(a: VVec, i: int): Value     = get_impl(a, i)
proc `[]`*(a: VVec, i: Value): Value = get_impl(a, i)
proc `[]`*(a: VVec, i: float64): Value = get_impl(a, i)
proc get*(a: VVec, i: int): Value      = get_impl(a, i)
proc get*(a: VVec, i: Value): Value  = get_impl(a, i)
proc get*(a: VVec, i: float64): Value  = get_impl(a, i)

iterator items*(a: VVec): Value =
  for v in a.payload.items:
    yield v

proc slice*(a: VVec, i1, i2: int): VVec =
  let new_vec = a.payload.get(i1..<i2)
  array_from_vec(new_vec)
  return new_array
proc slice*(a: VVec, i1, i2: Value): VVec =
  if i1.is_num and i2.is_num:
    return a.slice(i1.as_f64.int, i2.as_f64.int)
  else:
    raise newException(TypeException, &"Cannot slice with arguments of {$i1} of type {i1.type_label} and {$i2} of type {i2.type_label}")
template slice*(a: VVec, i1, i2: typed): VVec = a.slice(i1.v, i2.v)

template set_impl*(a: VVec, i: int, v: Value) =
  let new_vec = a.payload.set(i, v)
  array_from_vec(new_vec)
  return new_array
template set_impl*(a: VVec, i: Value, v: Value) =
  if i.is_num: set_impl(a, i.as_f64.int, v)
  else:
    raise newException(TypeException, &"Cannot set with index of {$i} of type {i.type_label}")
template set_impl*(a: VVec, i: float64, v: Value) =
  if i.is_num: set_impl(a, i.as_f64.int, v)
  else:
    raise newException(TypeException, &"Cannot set with index of {$i} of type {i.type_label}")

## TODO
## - Value indices
## - Negative indices
## - range indices???
## - indices beyond the end of the sequence (fill the gap with Nil)
proc set*(a: VVec, i: int, v: Value): VVec = set_impl(a, i, v)
proc set*(a: VVec, i: Value, v: Value): VVec = set_impl(a, i, v)
proc set*(a: VVec, i: float64, v: Value): VVec = set_impl(a, i, v)

proc add*(a: VVec, v: Value): VVec =
  let new_vec = a.payload.append(v)
  array_from_vec(new_vec)
  return new_array
template push*(a: VVec, v: Value): VVec = a.add(v)
template append*(a: VVec, v: Value): VVec = a.add(v)

proc prepend*(a: VVec, v: Value): VVec =
  let new_vec = a.payload.prepend(v)
  array_from_vec(new_vec)
  return new_array
template push_front*(a: VVec, v: Value): VVec = a.prepend(v)

proc pop*(a: VVec): (Value, VVec) =
  case a.size:
    of 0: return (Nil.v, a)
    of 1: return (a.payload[0], empty_array)
    else:
      let (new_vec, datum) = a.payload.pop()
      array_from_vec(new_vec)
      return (datum, new_array)

proc merge*(a1, a2: VVec): VVec =
  let new_vec = a1.payload & a2.payload
  array_from_vec(new_vec)
  return new_array
template concat*(a1, a2: VVec): VVec = a1.merge(a2)
template `&`*(a1, a2: VVec): VVec = a1.merge(a2)

proc `<`*(v1, v2: VVec): bool =
  for (it1, it2) in zip_iter(v1.payload, v2.payload):
    if it1 < it2: return true
    if it2 < it1: return false
  if v1.size < v2.size: return true
  return false
proc `<=`*(v1, v2: VVec): bool =
  let l = min(v1.size, v2.size)
  for (it1, it2) in zip_iter(v1.payload, v2.payload):
    if it1 < it2: return true
    if it2 < it1: return false
  if v1.size > v2.size: return false
  return true

# VSet Impl #
# ---------------------------------------------------------------------

template set_from_pset(pset: typed) {.dirty.} =
  when c32:
    var new_set = VSet(
      head: update_head(MASK_SIG_SET, 0),
      tail: pset
    )
  else:
    GC_ref(pset)
    var new_set = VSet(p: bitor(MASK_SIG_SET, pset.as_u64).as_p)

proc init_set_empty(): VSet =
  let pset = map.init_set[Value]()
  set_from_pset(pset)
  return new_set

let empty_set = init_set_empty()

proc init_set*(): VSet = return empty_set
proc init_set*(init_data: openArray[Value]): VSet =
  if init_data.len == 0: return empty_set
  let pset = map.to_set[Value](init_data)
  set_from_pset(pset)
  return new_set

proc contains*(s: VSet, k: Value): bool = s.payload.contains(k)
template contains*(s: VSet, k: typed): bool = s.payload.contains(k.v)

# template has_inner(s: VSet, k: typed) =
#   let derefed = s.payload
#   if k.as_v in derefed.data: return True
#   return False
# proc has*(s: VSet, k: Value): VBool = has_inner(s, k)
# proc has*(s: VSet, k: float64): VBool = has_inner(s, k)

proc get*(s: VSet, k: Value): Value =
  if k.v in s: return k.v else: return Nil.v
proc get*(s: VSet, k: float): Value =
  if k.v in s: return k.v else: return Nil.v

proc add*(s: VSet, k: Value): VSet =
  let pset = s.payload.incl(k)
  set_from_pset(pset)
  return new_set

proc del*(s: VSet, k: Value): VSet =
  let pset = s.payload.excl(k)
  set_from_pset(pset)
  return new_set

proc size*(s: VSet): int =
  return s.payload.len.int

# More Conversions #
# ---------------------------------------------------------------------

proc V_impl*(x: NimNode): NimNode =
  case x.kind:
    # a plain tuple of Values
    of nnkTupleConstr, nnkPar:
      var tup = quote do: ()
      for c in x.children:
        tup.add(V_impl(c))
      return tup
    # a VVec as Value
    of nnkBracket:
      var brak = copyNimNode(x)
      for c in x.children:
        brak.add(V_impl(c))
      return newCall("v", newCall("init_array", brak))
    # a VSet as Value
    of nnkCurly:
      var brak = quote do: []
      for c in x.children:
        brak.add(V_impl(c))
      return newCall("v", newCall("init_set", brak))
    # a VMap as Value
    of nnkTableConstr:
      var brak = quote do: []
      var parens = quote do: ()
      for colon_expr in x.children:
        parens = quote do: ()
        for c in colon_expr.children:
          parens.add(V_impl(c))
        brak.add(parens)
      return newCall("v", newCall("init_map", brak))
    # some other Value that doesn't have any special treatment of literals
    else: 
      return newCall("v", x)
macro V*(x: untyped): untyped =
  V_impl(x)

proc Map_impl*(x: NimNode): NimNode =
  case x.kind:
    of nnkBracket:
      var brak = copyNimNode(x)
      var parens = quote do: ()
      for tup in x.children:
        if tup.kind != nnkTupleConstr:
          raise newException(TypeException, &"Cannot call Map on {x.repr}")
        if tup.len != 2:
          raise newException(TypeException, &"Cannot call Map on {x.repr}")
        parens = quote do: ()
        for c in tup.children:
          parens.add(V_impl(c))
        brak.add(parens)
      return quote do: init_map(`brak`).v
    of nnkCurly:
      var brak = quote do: []
      if x.len > 0:
        raise newException(TypeException, &"Cannot call Map on {x.repr}")
      return quote do: init_map(`brak`).v
    of nnkTableConstr:
      var brak = quote do: []
      var parens = quote do: ()
      for colon_expr in x.children:
        parens = quote do: ()
        for c in colon_expr.children:
          parens.add(V_impl(c))
        brak.add(parens)
      return quote do: init_map(`brak`).v
    else:
      return quote do: init_map(`x`).v
macro Map*(x: untyped): untyped =
  Map_impl(x)
macro Map*(): untyped =
  return quote do: init_map([]).v

proc Vec_impl*(x: NimNode): NimNode =
  case x.kind:
    of nnkBracket:
      var brak = copyNimNode(x)
      for c in x.children:
        brak.add(V_impl(c))
      return quote do: init_array(`brak`).v
    else: 
      return quote do: init_array(`x`).v
macro Vec*(x: untyped): untyped =
  Vec_impl(x)
macro Vec*(): untyped =
  return quote do: init_array([]).v

proc Set_impl*(x: NimNode): NimNode =
  case x.kind:
    of nnkBracket, nnkCurly:
      var brak = quote do: []
      for c in x.children:
        brak.add(V_impl(c))
      return quote do: init_set(`brak`).v
    else: 
      return quote do: init_set(`x`).v
macro Set*(x: untyped): untyped =
  Set_impl(x)
macro Set*(): untyped =
  return quote do: init_set([]).v

## TODO
## - add format string capabilities
template Str*(x: string): Value = x.init_string.v

proc Sym_impl*(x: NimNode): NimNode =
  if x.kind == nnkStrLit:
    return quote do: init_symbol(`x`).v
  let str_val = x.repr
  return quote do: init_symbol(`str_val`).v
macro Sym*(x: untyped): untyped =
  Sym_impl(x)
macro Sym*(): untyped =
  return quote do: init_symbol().v

# Value Fns #
# ---------------------------------------------------------------------

proc get_in(it: Value, path: openArray[Value], i: int, default: Value): Value =
  var new_it: Value
  if it.is_map:     new_it = it.as_map.get(path[i].v)
  elif it.is_vec: new_it = it.as_vec.get(path[i].v)
  elif it.is_set:   new_it = it.as_set.get(path[i].v)
  elif it == Nil.v:
    return default
  else:
    # TODO - error
    discard
  if i == path.high:
    if new_it != Nil.v: return new_it
    return default
  else: return get_in(new_it, path, i + 1, default)
template get_in*(it: Value, path: openArray[Value], default: Value): Value =
  get_in(it, path, 0, default)
template get_in*(it: Value, path: openArray[Value]): Value =
  get_in(it, path, 0, Nil.v)

## If a key in the path does not exist, maps are created
proc set_in*(it: Value, path: openArray[Value], v: Value): Value =
  var payload = v
  var stack = newSeq[Value]()
  var k: Value
  var curr = it
  var max = 0
  for i in 0..path.high:
    k = path[i]
    if curr.is_map:
      stack.add(curr)
      curr = curr.as_map.get(k)
    elif curr.is_vec:
      stack.add(curr)
      curr = curr.as_vec.get(k)
    elif curr == Nil.v:
      for j in countdown(path.high, i):
        k = path[j]
        payload = init_map([(k, payload)]).v
      break
    else:
      echo "TODO - add exceptions"
    max = i
  for i in countdown(max, 0):
    k = path[i]
    curr = stack[i]
    if curr.is_map:     payload = curr.as_map.set(k, payload).v
    elif curr.is_vec: payload = curr.as_vec.set(k, payload).v
    else:               echo "TODO - add exceptions2"
  return payload

proc `<`*(a, b: Value): bool =
  if a.is_num and b.is_num: return a.as_f64 < b.as_f64
  let a_sig = bitand(a.type_bits, MASK_SIGNATURE)
  let b_sig = bitand(b.type_bits, MASK_SIGNATURE)
  case a_sig:
    of MASK_SIG_STR:
      if b_sig == MASK_SIG_STR: return a.as_str < b.as_str
    of MASK_SIG_VEC:
      if b_sig == MASK_SIG_VEC: return a.as_vec < b.as_vec
    else: discard
  raise newException(TypeException, &"Cannot compare {a.type_label} and {b.type_label}")
  
proc `<=`*(a, b: Value): bool =
  if a.is_num and b.is_num: return a.as_f64 <= b.as_f64
  let a_sig = bitand(a.type_bits, MASK_SIGNATURE)
  let b_sig = bitand(b.type_bits, MASK_SIGNATURE)
  case a_sig:
    of MASK_SIG_STR:
      if b_sig == MASK_SIG_STR: return a.as_str <= b.as_str
    of MASK_SIG_VEC:
      if b_sig == MASK_SIG_VEC: return a.as_vec <= b.as_vec
    else: discard
  raise newException(TypeException, &"Cannot compare {a.type_label} and {b.type_label}")

proc `[]`*(coll: Value, k: int): Value =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_VEC: return coll.as_vec[k]
    of MASK_SIG_MAP: return coll.as_map[k.v]
    # of MASK_SIG_SET: return coll.as_set[k]
    of MASK_SIG_STR: return coll.as_str[k]
    else: discard
  raise newException(TypeException, &"Cannot index into {$coll} of type {coll.type_label} with {$(k.v)} of type {(k.v).type_label}")
proc `[]`*(coll, k: Value): Value =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_VEC: return coll.as_vec[k]
    of MASK_SIG_MAP: return coll.as_map[k]
    # of MASK_SIG_SET: return coll.as_set[k]
    of MASK_SIG_STR: return coll.as_str[k]
    else: discard
  raise newException(TypeException, &"Cannot index into {$coll} of type {coll.type_label} with {$k} of type {k.type_label}")
template `[]`*(coll: Value, k: typed): Value = coll[k.v]
template get*(coll: Value, k: int): Value = coll[k]
template get*(coll: Value, k: typed): Value = coll[k.v]

proc slice*(coll, i1, i2: Value): Value =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_VEC: return coll.as_vec.slice(i1, i2).v
    # of MASK_SIG_MAP: return coll.as_map[k]
    # of MASK_SIG_SET: return coll.as_set[k]
    # of MASK_SIG_STR: return coll.as_map[k]
    else: discard
  raise newException(TypeException, &"Cannot slice into {$coll} of type {coll.type_label} with {$i1} of type {i1.type_label} and {$i2} of type {i2.type_label}")
template slice*(coll: Value, i1, i2: typed): Value = coll.slice(i1.v, i2.v)

proc set*(coll, k, v: Value): Value =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_VEC: return coll.as_vec.set(k, v).v
    of MASK_SIG_MAP: return coll.as_map.set(k, v).v
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else: discard
  raise newException(TypeException, &"Cannot set into {$coll} of type {coll.type_label} with key {$k} of type {k.type_label} and value {$v} of type {v.type_label}")
template set*(coll: Value, key, val: typed): Value = set(coll, key.v, val.v)

proc add*(coll, v: Value): Value =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_VEC: return coll.as_vec.add(v).v
    of MASK_SIG_SET: return coll.as_set.add(v).v
    # of MASK_SIG_MAP: return coll.as_map.set(k, v).v
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else: discard
  raise newException(TypeException, &"Cannot add onto {$coll} of type {coll.type_label} with value {$v} of type {v.type_label}")
template add*(coll: Value, val: typed): Value = add(coll, val.v)
template append*(coll: Value, val: typed): Value = add(coll, val.v)

proc push*(coll, v: Value): Value =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_VEC: return coll.as_vec.push(v).v
    of MASK_SIG_SET: return coll.as_set.add(v).v
    # of MASK_SIG_MAP: return coll.as_map.set(k, v).v
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else: discard
  raise newException(TypeException, &"Cannot push onto {$coll} of type {coll.type_label} with value {$v} of type {v.type_label}")
template push*(coll: Value, val: typed): Value = push(coll, val.v)

proc prepend*(coll, v: Value): Value =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_VEC: return coll.as_vec.prepend(v).v
    # of MASK_SIG_SET: return coll.as_set.add(v).v
    # of MASK_SIG_MAP: return coll.as_map.set(k, v).v
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else: discard
  raise newException(TypeException, &"Cannot prepend onto {$coll} of type {coll.type_label} with value {$v} of type {v.type_label}")
template push_front*(coll: Value, val: typed): Value = prepend(coll, val.v)

proc del*(coll, k: Value): Value =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    # of MASK_SIG_VEC: return coll.as_vec.set(k, v).v
    of MASK_SIG_MAP: return coll.as_map.del(k).v
    of MASK_SIG_SET: return coll.as_set.del(k).v
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else: discard
  raise newException(TypeException, &"Cannot del from {$coll} of type {coll.type_label} with key {$k} of type {k.type_label}")
template del*(coll: Value, key: typed): Value = coll.del(key.v)

proc pop*(coll: Value): (Value, Value) =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_VEC: 
      var (popped, arr) = coll.as_vec.pop()
      return (popped, arr.v)
    # of MASK_SIG_SET: return coll.as_set.add(v).v
    # of MASK_SIG_MAP: return coll.as_map.set(k, v).v
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else: discard
  raise newException(TypeException, &"Cannot call pop on {$coll} of type {coll.type_label}")

proc size*(coll: Value): Value =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_VEC: return coll.as_vec.size.v
    of MASK_SIG_MAP: return coll.as_map.size.v
    of MASK_SIG_STR: return coll.as_str.size.v
    of MASK_SIG_SET: return coll.as_set.size.v
    else: discard
  raise newException(TypeException, &"Cannot get the size of {$coll} of type {coll.type_label}")
proc len*(coll: Value): int =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_VEC: return coll.as_vec.size
    of MASK_SIG_MAP: return coll.as_map.size
    of MASK_SIG_STR: return coll.as_str.size
    of MASK_SIG_SET: return coll.as_set.size
    else: discard
  raise newException(TypeException, &"Cannot get the len of {$coll} of type {coll.type_label}")

proc merge*(v1, v2: Value): Value =
  let v1_sig = bitand(v1.type_bits, MASK_SIGNATURE)
  let v2_sig = bitand(v2.type_bits, MASK_SIGNATURE)
  if v1_sig == v2_sig:
    case v1_sig:
      of MASK_SIG_VEC: return v1.as_vec.merge(v2.as_vec).v
      of MASK_SIG_MAP: return v1.as_map.merge(v2.as_map).v
      # of MASK_SIG_SET: return v1.as_set.merge(v2.as_set).v
      of MASK_SIG_STR: return v1.as_str.concat(v2.as_str).v
      else: discard
  raise newException(TypeException, &"Cannot merge {$v1} of type {v1.type_label} with {$v2} of type {v2.type_label}")
template concat*(v1, v2: Value): Value = v1.merge(v2)
template `&`*(v1, v2: Value): Value = v1.merge(v2)

proc contains*(coll, k: Value): bool =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    # of MASK_SIG_VEC: return coll.as_vec.set(k, v)
    of MASK_SIG_MAP: return coll.as_map.contains(k)
    of MASK_SIG_SET: return coll.as_set.contains(k)
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else: discard
  raise newException(TypeException, &"Cannot check whether {$coll} of type {coll.type_label} contains {$k} of type {k.type_label}")
template contains*(coll: Value, key: typed): bool = coll.contains(key.v)
template has*(coll: Value, key: typed): bool = coll.contains(key.v)

iterator keys*(coll: Value): Value =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    # of MASK_SIG_VEC: return coll.as_vec.keys
    of MASK_SIG_MAP:
      for k in coll.as_map.keys:
        yield k
    # of MASK_SIG_SET: return coll.as_set.keys
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else:
      raise newException(TypeException, &"Cannot iterate the keys of {$coll} of type {coll.type_label}")
iterator values*(coll: Value): Value =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_VEC:
      for v in coll.as_vec.items:
        yield v
    of MASK_SIG_MAP:
      for v in coll.as_map.values:
        yield v
    # of MASK_SIG_SET: return coll.as_set.values
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else:
      raise newException(TypeException, &"Cannot iterate the values of {$coll} of type {coll.type_label}")
iterator items*(coll: Value): Value =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_VEC:
      for v in coll.as_vec.items:
        yield v
    of MASK_SIG_MAP:
      for v in coll.as_map.values:
        yield v
    # of MASK_SIG_SET: return coll.as_set.values
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else:
      raise newException(TypeException, &"Cannot iterate the values of {$coll} of type {coll.type_label}")
iterator pairs*(coll: Value): (Value, Value) =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    # of MASK_SIG_VEC: return coll.as_vec.pairs
    of MASK_SIG_MAP:
      for p in coll.as_map.pairs:
        yield p
    # of MASK_SIG_SET: return coll.as_set.pairs
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else:
      raise newException(TypeException, &"Cannot iterate the pairs of {$coll} of type {coll.type_label}")

proc `+`*(x, y: Value): Value = 
  if x.is_num and y.is_num: return (x.as_f64 + y.as_f64).v
  else: raise newException(TypeException, &"Cannot call `+` on {x.type_label} and {y.type_label}")
proc `-`*(x, y: Value): Value = 
  if x.is_num and y.is_num: return (x.as_f64 - y.as_f64).v
  else: raise newException(TypeException, &"Cannot call `-` on {x.type_label} and {y.type_label}")
proc `*`*(x, y: Value): Value = 
  if x.is_num and y.is_num: return (x.as_f64 * y.as_f64).v
  else: raise newException(TypeException, &"Cannot call `*` on {x.type_label} and {y.type_label}")
proc `/`*(x, y: Value): Value = 
  if x.is_num and y.is_num: return (x.as_f64 / y.as_f64).v
  else: raise newException(TypeException, &"Cannot call `/` on {x.type_label} and {y.type_label}")
proc `mod`*(x, y: Value): Value =
  if x.is_num and y.is_num: return (system.mod(x.to_int, y.to_int)).v
  else: raise newException(TypeException, &"Cannot call `mod` on {x.type_label} and {y.type_label}")

proc `+`*(x: Value, y: int): Value = 
  if x.is_num: return (x.as_f64 + y.float64).v
  else: raise newException(TypeException, &"Cannot call `+` on {x.type_label} and int")
proc `-`*(x: Value, y: int): Value = 
  if x.is_num: return (x.as_f64 - y.float64).v
  else: raise newException(TypeException, &"Cannot call `-` on {x.type_label} and int")
proc `*`*(x: Value, y: int): Value = 
  if x.is_num: return (x.as_f64 * y.float64).v
  else: raise newException(TypeException, &"Cannot call `*` on {x.type_label} and int")
proc `/`*(x: Value, y: int): Value = 
  if x.is_num: return (x.as_f64 / y.float64).v
  else: raise newException(TypeException, &"Cannot call `/` on {x.type_label} and int")
proc `mod`*(x: Value, y: int): Value =
  if x.is_num: return (system.mod(x.to_int, y)).v
  else: raise newException(TypeException, &"Cannot call `mod` on {x.type_label} and int")

##
## nil < boolean < number < string < set < array < map
## 
## What about bignum and the rest of the gang?
proc compare*(a, b: Value): int =
  let a_sig = bitand(a.type_bits, MASK_SIGNATURE)
  let b_sig = bitand(b.type_bits, MASK_SIGNATURE)

  # Nil
  block:
    if a_sig == MASK_SIG_NIL:
      if b_sig == MASK_SIG_NIL: return 0
      return -1
    if b_sig == MASK_SIG_NIL: return 1
  
  # Bool
  block:
    if a_sig == MASK_SIG_BOOL:
      if b_sig != MASK_SIG_BOOL: return -1
      if a == False.v:
        if b == False.v: return 0
        return -1
      if b == False.v: return 1
      if b == True.v: return 0
      return -1
    if b_sig == MASK_SIG_BOOL: return 1
  
  # Number
  block:
    if a.is_num:
      if b.is_num:
        if a.as_f64 < b.as_f64: return -1
        if a.as_f64 > b.as_f64: return 1
        return 0
      return -1
    if b.is_num: return 1

  # String
  block:
    if a_sig == MASK_SIG_STR:
      if b_sig == MASK_SIG_STR:
        if a.as_str.payload.data < b.as_str.payload.data: return -1
        if a.as_str.payload.data > b.as_str.payload.data: return 1
        return 0
      return -1
    if b_sig == MASK_SIG_STR: return 1
    
        
