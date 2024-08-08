## TODO
## 
## -[ ] add Infinity, -Infinity
## -[ ] add a converter from int to ImValue
## -[ ] add an ability to push onto the end of an array
## -[ ] differentiate between inner api and outer api
##   - Eg.
##     - proc inner_contains(v1, v2: ImValue): bool
##     - proc outer_contains(v1, v2: ImValue): ImValue
## -[ ] find a way to not have to reimplement iterators (eg, for ImMap as well as ImValue)

import std/[tables, sets, bitops, strutils, sequtils, strformat, macros]
import hashes
# import persistent/[sumtree]
import persistent/vec except `==`
import persistent/map except `==`

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

  ImValueKind* = enum
    # Immediate Kinds
    kNaN
    kNil
    kBool
    kNumber           # like js, we just have a float64 number type
    kAtom
    # Heap Kinds
    kString
    kBigNum
    kArray
    kMap
    kSet
    kSymbol
  
when c32:
  type ImValue* = object
    tail: uint32
    head: uint32

  proc `=destroy`(x: var ImValue)
  proc `=copy`(x: var ImValue, y: ImValue)

else:
  type ImValue* = distinct uint64

type
  ImStringPayload* = object
    hash: Hash
    data: string
  ImArrayPayload* = PVec[ImValue]
  ImMapPayload* = PMap[ImValue, ImValue]
  ImSetPayload* = PSet[ImValue]
  ImSymbolPayload* = object
    id: uint
    data: string
  ImStringPayloadRef* = ref ImStringPayload
  ImArrayPayloadRef*  = ref ImArrayPayload
  ImMapPayloadRef*    = ref ImMapPayload
  ImSetPayloadRef*    = ref ImSetPayload
  ImSymbolPayloadRef* = ref ImSymbolPayload

  ImNaN*    = distinct uint64
  ImNil*    = distinct uint64
  ImBool*   = distinct uint64
  ImAtom*   = distinct uint64

when c32:
  type
    ImString* = object
      tail*: ImStringPayloadRef
      head*: uint32
    ImArray* = object
      tail*: ImArrayPayloadRef
      head*: uint32
    ImMap* = object
      tail*: ImMapPayloadRef
      head*: uint32
    ImSet* = object
      tail*: ImSetPayloadRef
      head*: uint32
    ImSymbol* = object
      tail*: ImSymbolPayloadRef
      head*: uint32
else:
  type
    MaskedRef*[T] = object
      # distinct should work in theory, but I'm not entirely sure how well phantom types work with distinct at the moment
      p: pointer
    ImString* = MaskedRef[ImStringPayload]
    ImArray*  = MaskedRef[ImArrayPayload]
    ImMap*    = MaskedRef[ImMapPayload]
    ImSet*    = MaskedRef[ImSetPayload]
    ImSymbol* = MaskedRef[ImSymbolPayload]

type
  ImSV* = ImNaN or ImNil or ImBool or ImAtom
  ImHV* = ImString or ImArray or ImMap or ImSet or ImSymbol
  ImV* = ImSV or ImHV

# Forward Declarations #
# ---------------------------------------------------------------------

## Forward declare these so that the equality procs in the collection
## implementations can rely on them.
func `==`*(v1, v2: ImValue): bool
func `==`*(v: ImValue, f: float64): bool
func `==`*(f: float64, v: ImValue): bool
func `==`*(v1, v2: ImString): bool
func `==`*(v1, v2: ImMap): bool
func `==`*(v1, v2: ImArray): bool
func `==`*(v1, v2: ImSet): bool
func `==`*(v1, v2: ImSymbol): bool
func `==`*(v1, v2: ImHV): bool
func `==`*(v1, v2: ImSV): bool
func `==`*(v1, v2: ImV): bool
func `==`*(v: ImV, f: float64): bool
func `==`*(f: float64, v: ImV): bool

## Import the `==` of the persistent collections so that they can "see" the
## forward declarations of this module.
from persistent/vec import `==`
from persistent/map import `==`

## Forward declare this so that we make sure the same hash function is always
## used for every operation involving ImValue
func hash*(v: ImValue): Hash

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
template as_v*(v: typed): ImValue = cast[ImValue](cast[uint64](v))
template as_str*(v: typed): ImString = cast[ImString](cast[uint64](v))
template as_arr*(v: typed): ImArray = cast[ImArray](cast[uint64](v))
template as_map*(v: typed): ImMap = cast[ImMap](cast[uint64](v))
template as_set*(v: typed): ImSet = cast[ImSet](cast[uint64](v))
template as_sym*(v: typed): ImSymbol = cast[ImSymbol](cast[uint64](v))

# Conversions #
# ---------------------------------------------------------------------

template v*(x: ImValue): ImValue = x
template v*(x: ImString): ImValue = x.as_v
template v*(x: ImSet): ImValue = x.as_v
template v*(x: ImArray): ImValue = x.as_v
template v*(x: ImMap): ImValue = x.as_v
template v*(x: ImSymbol): ImValue = x.as_v
template v*(x: ImNil): ImValue = x.as_v
template v*(x: ImBool): ImValue = x.as_v

# A couple of forward declarations for the conversions
proc init_string*(s: string = ""): ImString
proc init_array*(init_data: openArray[ImValue]): ImArray

template v*(x: float64): ImValue = x.as_v
template v*(x: int): ImValue = x.float64.v
template v*(x: bool): ImValue = (if x: True.v else: False.v)
template v*(x: string): ImValue = x.init_string.v
template v*(x: openArray[int]): ImValue = toSeq(x).map(x => x.v).init_array.v
template v*(x: openArray[float64]): ImValue = toSeq(x).map(x => x.v).init_array.v
template v*(x: openArray[ImValue]): ImValue = x.init_array.v

template to_int*(x: ImValue): int = x.as_f64.int

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
  const MASK_TYPE_BIGNUM = 0b10000000000000100000000000000000'u32
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
  const MASK_TYPE_BIGNUM = 0b10000000000000100000000000000000'u64 shl 32
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
const MASK_SIG_ATOM    = MASK_EXP_OR_Q or MASK_TYPE_ATOM
const MASK_SIG_STR     = MASK_EXP_OR_Q or MASK_TYPE_STR
const MASK_SIG_BIGNUM  = MASK_EXP_OR_Q or MASK_TYPE_BIGNUM
const MASK_SIG_ARR     = MASK_EXP_OR_Q or MASK_TYPE_ARR
const MASK_SIG_SET     = MASK_EXP_OR_Q or MASK_TYPE_SET
const MASK_SIG_MAP     = MASK_EXP_OR_Q or MASK_TYPE_MAP
const MASK_SIG_SYM     = MASK_EXP_OR_Q or MASK_TYPE_SYM

# Get Payload #
# ---------------------------------------------------------------------

when c32:
  template payload*(v: ImString): ImStringPayloadRef = v.tail
  template payload*(v: ImMap): ImMapPayloadRef       = v.tail
  template payload*(v: ImArray): ImArrayPayloadRef   = v.tail
  template payload*(v: ImSet): ImSetPayloadRef       = v.tail
  template payload*(v: ImSymbol): ImSymbolPayloadRef = v.tail
else:
  template to_clean_ptr(v: typed): pointer =
    cast[pointer](bitand((v).as_u64, MASK_POINTER))

  template payload*(v: ImString): ImStringPayloadRef = cast[ImStringPayloadRef](to_clean_ptr(v.p))
  template payload*(v: ImMap): ImMapPayloadRef       = cast[ImMapPayloadRef](to_clean_ptr(v.p))
  template payload*(v: ImArray): ImArrayPayloadRef   = cast[ImArrayPayloadRef](to_clean_ptr(v.p))
  template payload*(v: ImSet): ImSetPayloadRef       = cast[ImSetPayloadRef](to_clean_ptr(v.p))
  template payload*(v: ImSymbol): ImSymbolPayloadRef = cast[ImSymbolPayloadRef](to_clean_ptr(v.p))

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
template is_bool*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_BOOL
template is_atom*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_ATOM
template is_string*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_STR
template is_bignum*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_BIGNUM
template is_array*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_ARR
template is_set*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_SET
template is_map*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_MAP
template is_symbol*(v: typed): bool =
  bitand(v.type_bits, MASK_SIGNATURE) == MASK_SIG_SYM
template is_heap*(v: typed): bool =
  bitand(v.type_bits, MASK_HEAP) == MASK_HEAP

proc get_type*(v: ImValue): ImValueKind =
  let type_carrier = v.type_bits
  if v.is_num: return kNumber
  let signature = bitand(type_carrier, MASK_SIGNATURE)
  case signature:
    of MASK_SIG_NIL:    return kNil
    of MASK_SIG_BOOL:   return kBool
    of MASK_SIG_ATOM:   return kAtom
    of MASK_SIG_STR:    return kString
    of MASK_SIG_BIGNUM: return kBigNum
    of MASK_SIG_ARR:    return kArray
    of MASK_SIG_SET:    return kSet
    of MASK_SIG_MAP:    return kMap
    of MASK_SIG_SYM:    return kSymbol
    else:               echo "Unknown Type!"

# GC Hooks #
# ---------------------------------------------------------------------

when c32:
  proc `=destroy`(x: var ImValue) =
    if x.is_map:
      GC_unref(cast[ImMapPayloadRef](x.tail))
    elif x.is_array:
      GC_unref(cast[ImArrayPayloadRef](x.tail))
    elif x.is_set:
      GC_unref(cast[ImSetPayloadRef](x.tail))
    elif x.is_string:
      GC_unref(cast[ImStringPayloadRef](x.tail))
    elif x.is_symbol:
      GC_unref(cast[ImSymbolPayloadRef](x.tail))
  proc `=copy`(x: var ImValue, y: ImValue) =
    try:
      if x.as_u64 == y.as_u64: return
      if y.is_map:
        GC_ref(cast[ImMapPayloadRef](y.tail))
      elif y.is_array:
        GC_ref(cast[ImArrayPayloadRef](y.tail))
      elif y.is_set:
        GC_ref(cast[ImSetPayloadRef](y.tail))
      elif y.is_string:
        GC_ref(cast[ImStringPayloadRef](y.tail))
      elif y.is_symbol:
        GC_ref(cast[ImSymbolPayloadRef](y.tail))
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
  let Nil*   = cast[ImNil](u64_from_mask(MASK_SIG_NIL))
  let True*  = cast[ImBool](u64_from_mask(MASK_SIG_TRUE))
  let False* = cast[ImBool](u64_from_mask(MASK_SIG_FALSE))
else:
  let Nil*   = cast[ImNil]((MASK_SIG_NIL))
  let True*  = cast[ImBool]((MASK_SIG_TRUE))
  let False* = cast[ImBool]((MASK_SIG_FALSE))

template default*(v: ImValue): ImValue = Nil.v

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
      of MASK_SIG_STR:    eq_heap_payload(v1.as_str.payload, v2.as_str.payload)
      of MASK_SIG_ARR:
        result = v1.as_arr.payload == v2.as_arr.payload
      of MASK_SIG_MAP:
        result = v1.as_map.payload == v2.as_map.payload
      of MASK_SIG_SET:
        result = v1.as_set.payload == v2.as_set.payload
      of MASK_SIG_SYM:
        result = v1.as_sym.payload.id == v2.as_sym.payload.id
      else:               discard

func `==`*(v1, v2: ImValue): bool =
  if bitand(MASK_HEAP, v1.type_bits) == MASK_HEAP: eq_heap_value_generic(v1, v2)
  else: return v1.as_u64 == v2.as_u64
func `==`*(v: ImValue, f: float64): bool = return v == f.as_v
func `==`*(f: float64, v: ImValue): bool = return v == f.as_v
    
func `==`*(v1, v2: ImString): bool = eq_heap_value_specific(v1, v2)
func `==`*(v1, v2: ImMap): bool =
  return v1.payload == v2.payload
func `==`*(v1, v2: ImArray): bool =
  return v1.payload == v2.payload
func `==`*(v1, v2: ImSet): bool =
  return v1.payload == v2.payload
func `==`*(v1, v2: ImSymbol): bool =
  return v1.payload.id == v2.payload.id

func `==`*(v1, v2: ImHV): bool = eq_heap_value_generic(v1, v2)
func `==`*(v1, v2: ImSV): bool = return v1.as_u64 == v2.as_u64
  
func `==`*(v1, v2: ImV): bool =
  if bitand(MASK_HEAP, v1.type_bits) == MASK_HEAP: eq_heap_value_generic(v1, v2)
  else: return v1.as_u64 == v2.as_u64
func `==`*(v: ImV, f: float64): bool = return v == f.as_v
func `==`*(f: float64, v: ImV): bool = return v == f.as_v

template `==`*(v1: ImValue, v2: ImV): bool = v1.as_v == v2.as_v
template `==`*(v1: ImV, v2: ImValue): bool = v1.as_v == v2.as_v

proc `<`*(a, b: ImValue): bool
proc `<=`*(a, b: ImValue): bool

template `<`*(a: float64, b: ImValue): bool = return a.v < b.v
template `<`*(a: ImValue, b: float64): bool = return a.v < b.v
template `<=`*(a: float64, b: ImValue): bool = return a.v <= b.v
template `<=`*(a: ImValue, b: float64): bool = return a.v <= b.v

# Automatic Conversions #
# ---------------------------------------------------------------------

converter toImValue*(n: ImNil): ImValue = n.v
converter toImValue*(b: ImBool): ImValue = b.v
converter toImValue*(x: ImArray): ImValue = x.v
converter toImValue*(x: ImMap): ImValue = x.v
converter toImValue*(x: ImString): ImValue = x.v
converter toImValue*(x: ImSet): ImValue = x.v
converter toImValue*(x: ImSymbol): ImValue = x.v

converter toImValue(f: float64): ImValue = f.v
converter toImValue(i: int): ImValue = i.v
converter toImValue(b: bool): ImValue = b.v
converter toImValue(s: string): ImValue = s.v

converter toBool*(b: ImBool): bool = b == True
converter toBool*(n: ImNil): bool = false

# Debug String Conversion #
# ---------------------------------------------------------------------

proc to_hex*(f: float64): string = return toHex(f.as_u64)
proc to_hex*(v: ImV): string = return toHex(v.as_u64)
proc to_hex*(v: ImValue): string = return toHex(v.as_u64)
proc to_bin_str*(v: ImV): string = return toBin(v.as_i64, 64)
proc to_bin_str*(v: ImValue): string = return toBin(v.as_i64, 64)
proc to_bin_str*(v: uint32): string = return toBin(v.as_i64, 32)
proc to_bin_str*(v: int32): string = return toBin(v.as_i64, 32)
proc to_bin_str*(v: int64): string = return toBin(v, 64)
proc to_bin_str*(v: uint64): string = return toBin(v.as_i64, 64)

proc `$`*(k: ImValueKind): string =
  case k:
    of kNumber: return "Number"
    of kNil:    return "Nil"
    of kString: return "String"
    of kMap:    return "Map"
    of kArray:  return "Array"
    of kSet:    return "Set"
    of kSymbol: return "Symbol"
    of kBool:   return "Boolean"
    else:       return "<unknown>"

proc `$`*(v: ImValue): string =
  let kind = get_type(v)
  case kind:
    of kNumber:           return $(v.as_f64)
    of kNil:              return "Nil"
    of kBool:
      if v == True.as_v:  return "True"
      if v == False.as_v: return "False"
      # TODO - type error
    of kString:           return $(v.as_str.payload.data)
    of kMap:              return $(v.as_map.payload)
    of kArray:            return $(v.as_arr.payload)
    of kSet:              return $(v.as_set.payload) 
    of kSymbol:           return "'" & v.as_sym.payload.data & "." & $(v.as_sym.payload.id)
    # of kString:           return "Str\"" & $(v.as_str.payload.data) & "\""
    # of kMap:              return "M[" & $(v.as_map.payload.data) & "]"
    # of kArray:            return "A[" & $(v.as_arr.payload.data) & "]" 
    # of kSet:              return "S[" & $(v.as_set.payload.data) & "]" 
    else:                 discard

proc debug*(v: ImValue): string =
  let kind = get_type(v)
  when c32:
    let shallow_str = "( head: " & to_hex(v.head) & ", tail: " & to_hex(v.tail) & " )"
  else:
    let shallow_str = "( " & to_hex(v.as_u64) & " )"
  case kind:
    of kNumber:           return "Num" & shallow_str
    of kNil:              return "Nil" & shallow_str
    of kBool:
      if v == True.as_v:  return "True" & shallow_str
      if v == False.as_v: return "False" & shallow_str
      # TODO - type error
    of kString:           return "Str" & shallow_str
    of kMap:              return "Map" & shallow_str
    of kArray:            return "Arr" & shallow_str
    of kSet:              return "Set" & shallow_str
    of kSymbol:           return "Sym" & shallow_str
    else:                 discard

template type_label*(v: ImValue): string = $(v.get_type)

# Hash Handling #
# ---------------------------------------------------------------------

# XOR is commutative, associative, and is its own inverse.
# So we can use this same function to unhash as well.
when c32:
  template calc_hash(i1, i2: typed): Hash = bitxor(i1.as_u32, i2.as_u32).as_hash
else:
  template calc_hash(i1, i2: typed): Hash = bitxor(i1.as_u64, i2.as_u64).as_hash

func hash*(v: ImValue): Hash =
  if is_heap(v):
    if is_map(v):
      result = v.as_map.payload.hash
    elif is_array(v):
      result = v.as_arr.payload.summary.hash
    elif is_set(v):
      result = v.as_set.payload.hash
    elif is_symbol(v):
      result = v.as_sym.payload.id.as_hash
    else:
      # We cast to ImString so that we can get the hash, but all the ImHeapValues have a hash in the tail.
      let vh = cast[ImString](v)
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

# ImString Impl #
# ---------------------------------------------------------------------

template buildImString(new_hash, new_data: typed) {.dirty.} =
  when c32:
    let h = new_hash
    var new_string = ImString(
      head: update_head(MASK_SIG_STR, h.as_u32).as_u32,
      tail: ImStringPayloadRef(hash: h, data: new_data)
    )
  else:
    var re = new ImStringPayload
    GC_ref(re)
    re.hash = new_hash
    re.data = new_data
    var new_string = ImString(p: bitor(MASK_SIG_STR, re.as_p.as_u64).as_p)
  
func init_string_empty(): ImString =
  let hash = INITIAL_STR_HASH
  let data = ""
  buildImString(hash, data)
  return new_string

let empty_string = init_string_empty()

proc init_string*(s: string = ""): ImString =
  if s.len == 0: return empty_string
  let hash = hash(s)
  buildImString(hash, s)
  return new_string
template to_str*(s: string): ImString = s.init_string

proc `[]`*(s: ImString, i: int): ImValue =
  result = Nil.as_v
  if i < s.payload.data.len:
    if i >= 0:
      result = (init_string($s.payload.data[i])).as_v

proc concat*(s1, s2: ImString): ImString =
  let new_s = s1.payload.data & s2.payload.data
  return init_string(new_s)
proc `&`*(s1, s2: ImString): ImString =
  let new_s = s1.payload.data & s2.payload.data
  return init_string(new_s)

func size*(s: ImString): int =
  return s.payload.data.len.int

func `<`*(v1, v2: ImString): bool = return v1.payload.data < v2.payload.data
func `<=`*(v1, v2: ImString): bool = return v1.payload.data <= v2.payload.data

func to_nim_string(s: ImString): string = s.payload.data

# ImSymbol Impl #
# ---------------------------------------------------------------------

var symbol_id: uint = 0

proc get_symbol_id(): uint =
  symbol_id += 1
  return symbol_id

proc init_symbol*(str: string = ""): ImSymbol = 
  let new_sym_id = get_symbol_id()
  let re = ImSymbolPayloadRef(id: new_sym_id, data: str)
  when c32:
    result = ImSymbol(
      head: update_head(MASK_SIG_SYM, new_sym_id.as_u32).as_u32,
      tail: re
    )
  else:
    GC_ref(re)
    result = ImSymbol(p: bitor(MASK_SIG_SYM, re.as_p.as_u64).as_p)

func to_nim_string(s: ImSymbol): string = s.payload.data

# ImMap Impl #
# ---------------------------------------------------------------------

template map_from_pmap(pmap: typed) {.dirty.} =
  when c32:
    var new_map = ImMap(
      head: update_head(MASK_SIG_MAP, 0),
      tail: pmap
    )
  else:
    GC_ref(pmap)
    var new_map = ImMap(p: bitor(MASK_SIG_MAP, pmap.as_u64).as_p)

func init_map_empty(): ImMap =
  var pmap = map.init_map[ImValue, ImValue]()
  map_from_pmap(pmap)
  return new_map
  
let empty_map = init_map_empty()

template hash_entry(k, v: typed): Hash = (hash(k).as_u64 + hash(v).as_u64).as_hash

proc init_map*(): ImMap = return empty_map
proc init_map*(init_data: openArray[(ImValue, ImValue)]): ImMap =
  if init_data.len == 0: return empty_map
  var pmap = map.init_map[ImValue, ImValue]()
  for (k, v) in init_data:
    if v == Nil.v:
      pmap = pmap.delete(k)
    else:
      pmap = pmap.add(k, v)
  map_from_pmap(pmap)
  return new_map

# There's probably no point in having this. It suggests reference semantics.
proc clear*(m: ImMap): ImMap =
  return empty_map

proc contains*(m: ImMap, k: ImValue): bool = m.payload.contains(k)
template contains*(m: ImMap, k: typed): bool = m.payload.contains(k.v)

proc get_impl(m: ImMap, k: ImValue): ImValue =
  return m.payload.get_or_default(k, Nil.v)
template `[]`*(m: ImMap, k: typed): ImValue = get_impl(m, k.v)
template get*(m: ImMap, k: typed): ImValue = get_impl(m, k.v)

proc del*(m: ImMap, k: ImValue): ImMap =
  if not(k in m): return m
  let new_pmap = m.payload.delete(k)
  map_from_pmap(new_pmap)
  return new_map
template del*(m: ImMap, key: typed): ImMap = m.del(key.v)

proc set*(m: ImMap, k, v: ImValue): ImMap =
  if v == Nil.as_v: return m.del(k)
  let new_pmap = m.payload.add(k, v)
  map_from_pmap(new_pmap)
  return new_map
template set*(m: ImMap, key, val: typed): ImMap = set(m, key.v, val.v)

func size*(m: ImMap): int =
  return m.payload.len.int

iterator values*(m: ImMap): ImValue =
  for v in m.payload.values:
    yield v
iterator keys*(m: ImMap): ImValue =
  for k in m.payload.keys:
    yield k
iterator pairs*(m: ImMap): (ImValue, ImValue) =
  for p in m.payload.pairs:
    yield p

proc merge*(m1, m2: ImMap): ImMap =
  ## Asymmetric. Entries in m2 overwrite m1
  if m2.size == 0: return m1
  if m1.size == 0: return m2
  let new_pmap = m1.payload.concat(m2.payload)
  map_from_pmap(new_pmap)
  return new_map
template `&`*(m1, m2: ImMap): ImMap = m1.merge(m2)

# ImArray Impl #
# ---------------------------------------------------------------------

template array_from_vec(pvec: typed) {.dirty} =
  when c32:
    var new_array = ImArray(
      head: update_head(MASK_SIG_ARR, 0),
      tail: pvec
    )
  else:
    GC_ref(pvec)
    var new_array = ImArray(p: bitor(MASK_SIG_ARR, pvec.as_u64).as_p)

proc init_array_empty(): ImArray =
  let vec = init_vec[ImValue]()
  array_from_vec(vec)
  return new_array

let empty_array = init_array_empty()

proc init_array*(): ImArray = return empty_array
proc init_array*(init_data: openArray[ImValue]): ImArray =
  if init_data.len == 0: return empty_array
  let vec = init_data.to_vec
  array_from_vec(vec)
  return new_array

proc size*(a: ImArray): int =
  return a.payload.len

## TODO
## - ImValue indices
## - Negative indices
## - range indices
template get_impl(a: ImArray, i: int) =
  return a.payload.getOrDefault(i, Nil.v)
template get_impl(a: ImArray, i: ImValue) =
  if i.is_num:
    get_impl(a, i.as_f64.int)
  else:
    raise newException(TypeException, &"Cannot get with index of {$i} of type {i.type_label}")
template get_impl(a: ImArray, i: float64) =
  if i.is_num:
    get_impl(a, i.as_f64.int)
  else:
    raise newException(TypeException, &"Cannot get with index of {$i} of type {i.type_label}")

proc `[]`*(a: ImArray, i: int): ImValue     = get_impl(a, i)
proc `[]`*(a: ImArray, i: ImValue): ImValue = get_impl(a, i)
proc `[]`*(a: ImArray, i: float64): ImValue = get_impl(a, i)
proc get*(a: ImArray, i: int): ImValue      = get_impl(a, i)
proc get*(a: ImArray, i: ImValue): ImValue  = get_impl(a, i)
proc get*(a: ImArray, i: float64): ImValue  = get_impl(a, i)

iterator items*(a: ImArray): ImValue =
  for v in a.payload.items:
    yield v

proc slice*(a: ImArray, i1, i2: int): ImArray =
  let new_vec = a.payload.get(i1..<i2)
  array_from_vec(new_vec)
  return new_array
proc slice*(a: ImArray, i1, i2: ImValue): ImArray =
  if i1.is_num and i2.is_num:
    return a.slice(i1.as_f64.int, i2.as_f64.int)
  else:
    raise newException(TypeException, &"Cannot slice with arguments of {$i1} of type {i1.type_label} and {$i2} of type {i2.type_label}")
template slice*(a: ImArray, i1, i2: typed): ImArray = a.slice(i1.v, i2.v)

template set_impl*(a: ImArray, i: int, v: ImValue) =
  let new_vec = a.payload.set(i, v)
  array_from_vec(new_vec)
  return new_array
template set_impl*(a: ImArray, i: ImValue, v: ImValue) =
  if i.is_num: set_impl(a, i.as_f64.int, v)
  else:
    raise newException(TypeException, &"Cannot set with index of {$i} of type {i.type_label}")
template set_impl*(a: ImArray, i: float64, v: ImValue) =
  if i.is_num: set_impl(a, i.as_f64.int, v)
  else:
    raise newException(TypeException, &"Cannot set with index of {$i} of type {i.type_label}")

## TODO
## - ImValue indices
## - Negative indices
## - range indices???
## - indices beyond the end of the sequence (fill the gap with Nil)
proc set*(a: ImArray, i: int, v: ImValue): ImArray = set_impl(a, i, v)
proc set*(a: ImArray, i: ImValue, v: ImValue): ImArray = set_impl(a, i, v)
proc set*(a: ImArray, i: float64, v: ImValue): ImArray = set_impl(a, i, v)

proc add*(a: ImArray, v: ImValue): ImArray =
  let new_vec = a.payload.append(v)
  array_from_vec(new_vec)
  return new_array
template push*(a: ImArray, v: ImValue): ImArray = a.add(v)
template append*(a: ImArray, v: ImValue): ImArray = a.add(v)

proc prepend*(a: ImArray, v: ImValue): ImArray =
  let new_vec = a.payload.prepend(v)
  array_from_vec(new_vec)
  return new_array
template push_front*(a: ImArray, v: ImValue): ImArray = a.prepend(v)

proc pop*(a: ImArray): (ImValue, ImArray) =
  case a.size:
    of 0: return (Nil.v, a)
    of 1: return (a.payload[0], empty_array)
    else:
      let (new_vec, datum) = a.payload.pop()
      array_from_vec(new_vec)
      return (datum, new_array)

proc merge*(a1, a2: ImArray): ImArray =
  let new_vec = a1.payload & a2.payload
  array_from_vec(new_vec)
  return new_array
template concat*(a1, a2: ImArray): ImArray = a1.merge(a2)
template `&`*(a1, a2: ImArray): ImArray = a1.merge(a2)

proc `<`*(v1, v2: ImArray): bool =
  for (it1, it2) in zip_iter(v1.payload, v2.payload):
    if it1 < it2: return true
    if it2 < it1: return false
  if v1.size < v2.size: return true
  return false
proc `<=`*(v1, v2: ImArray): bool =
  let l = min(v1.size, v2.size)
  for (it1, it2) in zip_iter(v1.payload, v2.payload):
    if it1 < it2: return true
    if it2 < it1: return false
  if v1.size > v2.size: return false
  return true

# ImSet Impl #
# ---------------------------------------------------------------------

template set_from_pset(pset: typed) {.dirty.} =
  when c32:
    var new_set = ImSet(
      head: update_head(MASK_SIG_SET, 0),
      tail: pset
    )
  else:
    GC_ref(pset)
    var new_set = ImSet(p: bitor(MASK_SIG_SET, pset.as_u64).as_p)

proc init_set_empty(): ImSet =
  let pset = map.init_set[ImValue]()
  set_from_pset(pset)
  return new_set

let empty_set = init_set_empty()

proc init_set*(): ImSet = return empty_set
proc init_set*(init_data: openArray[ImValue]): ImSet =
  if init_data.len == 0: return empty_set
  let pset = map.to_set[ImValue](init_data)
  set_from_pset(pset)
  return new_set

proc contains*(s: ImSet, k: ImValue): bool = s.payload.contains(k)
template contains*(s: ImSet, k: typed): bool = s.payload.contains(k.v)

# template has_inner(s: ImSet, k: typed) =
#   let derefed = s.payload
#   if k.as_v in derefed.data: return True
#   return False
# proc has*(s: ImSet, k: ImValue): ImBool = has_inner(s, k)
# proc has*(s: ImSet, k: float64): ImBool = has_inner(s, k)

proc get*(s: ImSet, k: ImValue): ImValue =
  if k.v in s: return k.v else: return Nil.v
proc get*(s: ImSet, k: float): ImValue =
  if k.v in s: return k.v else: return Nil.v

proc add*(s: ImSet, k: ImValue): ImSet =
  let pset = s.payload.incl(k)
  set_from_pset(pset)
  return new_set

proc del*(s: ImSet, k: ImValue): ImSet =
  let pset = s.payload.excl(k)
  set_from_pset(pset)
  return new_set

proc size*(s: ImSet): int =
  return s.payload.len.int

# More Conversions #
# ---------------------------------------------------------------------

proc V_impl*(x: NimNode): NimNode =
  case x.kind:
    # a plain tuple of ImValues
    of nnkTupleConstr, nnkPar:
      var tup = quote do: ()
      for c in x.children:
        tup.add(V_impl(c))
      return tup
    # a ImArray as ImValue
    of nnkBracket:
      var brak = copyNimNode(x)
      for c in x.children:
        brak.add(V_impl(c))
      return newCall("v", newCall("init_array", brak))
    # a ImSet as ImValue
    of nnkCurly:
      var brak = quote do: []
      for c in x.children:
        brak.add(V_impl(c))
      return newCall("v", newCall("init_set", brak))
    # a ImMap as ImValue
    of nnkTableConstr:
      var brak = quote do: []
      var parens = quote do: ()
      for colon_expr in x.children:
        parens = quote do: ()
        for c in colon_expr.children:
          parens.add(V_impl(c))
        brak.add(parens)
      return newCall("v", newCall("init_map", brak))
    # some other ImValue that doesn't have any special treatment of literals
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
      return quote do: init_map(`brak`)
    of nnkCurly:
      var brak = quote do: []
      if x.len > 0:
        raise newException(TypeException, &"Cannot call Map on {x.repr}")
      return quote do: init_map(`brak`)
    of nnkTableConstr:
      var brak = quote do: []
      var parens = quote do: ()
      for colon_expr in x.children:
        parens = quote do: ()
        for c in colon_expr.children:
          parens.add(V_impl(c))
        brak.add(parens)
      return quote do: init_map(`brak`)
    else:
      return quote do: init_map(`x`)
macro Map*(x: untyped): untyped =
  Map_impl(x)
macro Map*(): untyped =
  return quote do: init_map([])

proc Arr_impl*(x: NimNode): NimNode =
  case x.kind:
    of nnkBracket:
      var brak = copyNimNode(x)
      for c in x.children:
        brak.add(V_impl(c))
      return quote do: init_array(`brak`)
    else: 
      return quote do: init_array(`x`)
macro Arr*(x: untyped): untyped =
  Arr_impl(x)
macro Arr*(): untyped =
  return quote do: init_array([])

proc Set_impl*(x: NimNode): NimNode =
  case x.kind:
    of nnkBracket, nnkCurly:
      var brak = quote do: []
      for c in x.children:
        brak.add(V_impl(c))
      return quote do: init_set(`brak`)
    else: 
      return quote do: init_set(`x`)
macro Set*(x: untyped): untyped =
  Set_impl(x)
macro Set*(): untyped =
  return quote do: init_set([])

## TODO
## - add format string capabilities
template Str*(x: string): ImString = x.init_string

proc Sym_impl*(x: NimNode): NimNode =
  if x.kind == nnkStrLit:
    return quote do: init_symbol(`x`)
  let str_val = x.repr
  return quote do: init_symbol(`str_val`)
macro Sym*(x: untyped): untyped =
  Sym_impl(x)
macro Sym*(): untyped =
  return quote do: init_symbol()

# ImValue Fns #
# ---------------------------------------------------------------------

proc get_in(it: ImValue, path: openArray[ImValue], i: int, default: ImValue): ImValue =
  var new_it: ImValue
  if it.is_map:     new_it = it.as_map.get(path[i].v)
  elif it.is_array: new_it = it.as_arr.get(path[i].v)
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
template get_in*(it: ImValue, path: openArray[ImValue], default: ImValue): ImValue =
  get_in(it, path, 0, default)
template get_in*(it: ImValue, path: openArray[ImValue]): ImValue =
  get_in(it, path, 0, Nil.v)

## If a key in the path does not exist, maps are created
proc set_in*(it: ImValue, path: openArray[ImValue], v: ImValue): ImValue =
  var payload = v
  var stack = newSeq[ImValue]()
  var k: ImValue
  var curr = it
  var max = 0
  for i in 0..path.high:
    k = path[i]
    if curr.is_map:
      stack.add(curr)
      curr = curr.as_map.get(k)
    elif curr.is_array:
      stack.add(curr)
      curr = curr.as_arr.get(k)
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
    elif curr.is_array: payload = curr.as_arr.set(k, payload).v
    else:               echo "TODO - add exceptions2"
  return payload

proc `<`*(a, b: ImValue): bool =
  if a.is_num and b.is_num: return a.as_f64 < b.as_f64
  let a_sig = bitand(a.type_bits, MASK_SIGNATURE)
  let b_sig = bitand(b.type_bits, MASK_SIGNATURE)
  case a_sig:
    of MASK_SIG_STR:
      if b_sig == MASK_SIG_STR: return a.as_str < b.as_str
    of MASK_SIG_ARR:
      if b_sig == MASK_SIG_ARR: return a.as_arr < b.as_arr
    else: discard
  raise newException(TypeException, &"Cannot compare {a.type_label} and {b.type_label}")
  
proc `<=`*(a, b: ImValue): bool =
  if a.is_num and b.is_num: return a.as_f64 <= b.as_f64
  let a_sig = bitand(a.type_bits, MASK_SIGNATURE)
  let b_sig = bitand(b.type_bits, MASK_SIGNATURE)
  case a_sig:
    of MASK_SIG_STR:
      if b_sig == MASK_SIG_STR: return a.as_str <= b.as_str
    of MASK_SIG_ARR:
      if b_sig == MASK_SIG_ARR: return a.as_arr <= b.as_arr
    else: discard
  raise newException(TypeException, &"Cannot compare {a.type_label} and {b.type_label}")

proc `[]`*(coll: ImValue, k: int): ImValue =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_ARR: return coll.as_arr[k]
    of MASK_SIG_MAP: return coll.as_map[k.v]
    # of MASK_SIG_SET: return coll.as_set[k]
    # of MASK_SIG_STR: return coll.as_map[k]
    else: discard
  raise newException(TypeException, &"Cannot index into {$coll} of type {coll.type_label} with {$(k.v)} of type {(k.v).type_label}")
proc `[]`*(coll, k: ImValue): ImValue =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_ARR: return coll.as_arr[k]
    of MASK_SIG_MAP: return coll.as_map[k]
    # of MASK_SIG_SET: return coll.as_set[k]
    # of MASK_SIG_STR: return coll.as_map[k]
    else: discard
  raise newException(TypeException, &"Cannot index into {$coll} of type {coll.type_label} with {$k} of type {k.type_label}")
template `[]`*(coll: ImValue, k: typed): ImValue = coll[k.v]
template get*(coll: ImValue, k: int): ImValue = coll[k]
template get*(coll: ImValue, k: typed): ImValue = coll[k.v]

proc slice*(coll, i1, i2: ImValue): ImValue =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_ARR: return coll.as_arr.slice(i1, i2).v
    # of MASK_SIG_MAP: return coll.as_map[k]
    # of MASK_SIG_SET: return coll.as_set[k]
    # of MASK_SIG_STR: return coll.as_map[k]
    else: discard
  raise newException(TypeException, &"Cannot slice into {$coll} of type {coll.type_label} with {$i1} of type {i1.type_label} and {$i2} of type {i2.type_label}")
template slice*(coll: ImValue, i1, i2: typed): ImValue = coll.slice(i1.v, i2.v)

proc set*(coll, k, v: ImValue): ImValue =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_ARR: return coll.as_arr.set(k, v).v
    of MASK_SIG_MAP: return coll.as_map.set(k, v).v
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else: discard
  raise newException(TypeException, &"Cannot set into {$coll} of type {coll.type_label} with key {$k} of type {k.type_label} and value {$v} of type {v.type_label}")
template set*(coll: ImValue, key, val: typed): ImValue = set(coll, key.v, val.v)

proc add*(coll, v: ImValue): ImValue =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_ARR: return coll.as_arr.add(v).v
    of MASK_SIG_SET: return coll.as_set.add(v).v
    # of MASK_SIG_MAP: return coll.as_map.set(k, v).v
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else: discard
  raise newException(TypeException, &"Cannot add onto {$coll} of type {coll.type_label} with value {$v} of type {v.type_label}")
template add*(coll: ImValue, val: typed): ImValue = add(coll, val.v)
template append*(coll: ImValue, val: typed): ImValue = add(coll, val.v)

proc push*(coll, v: ImValue): ImValue =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_ARR: return coll.as_arr.push(v).v
    of MASK_SIG_SET: return coll.as_set.add(v).v
    # of MASK_SIG_MAP: return coll.as_map.set(k, v).v
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else: discard
  raise newException(TypeException, &"Cannot push onto {$coll} of type {coll.type_label} with value {$v} of type {v.type_label}")
template push*(coll: ImValue, val: typed): ImValue = push(coll, val.v)

proc prepend*(coll, v: ImValue): ImValue =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_ARR: return coll.as_arr.prepend(v).v
    # of MASK_SIG_SET: return coll.as_set.add(v).v
    # of MASK_SIG_MAP: return coll.as_map.set(k, v).v
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else: discard
  raise newException(TypeException, &"Cannot prepend onto {$coll} of type {coll.type_label} with value {$v} of type {v.type_label}")
template push_front*(coll: ImValue, val: typed): ImValue = prepend(coll, val.v)

proc del*(coll, k: ImValue): ImValue =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    # of MASK_SIG_ARR: return coll.as_arr.set(k, v).v
    of MASK_SIG_MAP: return coll.as_map.del(k).v
    of MASK_SIG_SET: return coll.as_set.del(k).v
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else: discard
  raise newException(TypeException, &"Cannot del from {$coll} of type {coll.type_label} with key {$k} of type {k.type_label}")
template del*(coll: ImValue, key: typed): ImValue = coll.del(key.v)

proc pop*(coll: ImValue): (ImValue, ImValue) =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_ARR: 
      var (popped, arr) = coll.as_arr.pop()
      return (popped, arr.v)
    # of MASK_SIG_SET: return coll.as_set.add(v).v
    # of MASK_SIG_MAP: return coll.as_map.set(k, v).v
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else: discard
  raise newException(TypeException, &"Cannot call pop on {$coll} of type {coll.type_label}")

proc size*(coll: ImValue): ImValue =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_ARR: return coll.as_arr.size.v
    of MASK_SIG_MAP: return coll.as_map.size.v
    of MASK_SIG_STR: return coll.as_str.size.v
    of MASK_SIG_SET: return coll.as_set.size.v
    else: discard
  raise newException(TypeException, &"Cannot get the size of {$coll} of type {coll.type_label}")
proc len*(coll: ImValue): int =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_ARR: return coll.as_arr.size
    of MASK_SIG_MAP: return coll.as_map.size
    of MASK_SIG_STR: return coll.as_str.size
    of MASK_SIG_SET: return coll.as_set.size
    else: discard
  raise newException(TypeException, &"Cannot get the len of {$coll} of type {coll.type_label}")

proc merge*(v1, v2: ImValue): ImValue =
  let v1_sig = bitand(v1.type_bits, MASK_SIGNATURE)
  let v2_sig = bitand(v2.type_bits, MASK_SIGNATURE)
  if v1_sig == v2_sig:
    case v1_sig:
      of MASK_SIG_ARR: return v1.as_arr.merge(v2.as_arr).v
      of MASK_SIG_MAP: return v1.as_map.merge(v2.as_map).v
      # of MASK_SIG_SET: return v1.as_set.merge(v2.as_set).v
      # of MASK_SIG_STR: return coll.as_str.set(k, v)
      else: discard
  raise newException(TypeException, &"Cannot merge {$v1} of type {v1.type_label} with {$v2} of type {v2.type_label}")
template concat*(v1, v2: ImValue): ImValue = v1.merge(v2)
template `&`*(v1, v2: ImValue): ImValue = v1.merge(v2)

proc contains*(coll, k: ImValue): bool =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    # of MASK_SIG_ARR: return coll.as_arr.set(k, v)
    of MASK_SIG_MAP: return coll.as_map.contains(k)
    of MASK_SIG_SET: return coll.as_set.contains(k)
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else: discard
  raise newException(TypeException, &"Cannot check whether {$coll} of type {coll.type_label} contains {$k} of type {k.type_label}")
template contains*(coll: ImValue, key: typed): bool = coll.contains(key.v)
template has*(coll: ImValue, key: typed): bool = coll.contains(key.v)

iterator keys*(coll: ImValue): ImValue =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    # of MASK_SIG_ARR: return coll.as_arr.keys
    of MASK_SIG_MAP:
      for k in coll.as_map.keys:
        yield k
    # of MASK_SIG_SET: return coll.as_set.keys
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else:
      raise newException(TypeException, &"Cannot iterate the keys of {$coll} of type {coll.type_label}")
iterator values*(coll: ImValue): ImValue =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_ARR:
      for v in coll.as_arr.items:
        yield v
    of MASK_SIG_MAP:
      for v in coll.as_map.values:
        yield v
    # of MASK_SIG_SET: return coll.as_set.values
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else:
      raise newException(TypeException, &"Cannot iterate the values of {$coll} of type {coll.type_label}")
iterator items*(coll: ImValue): ImValue =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    of MASK_SIG_ARR:
      for v in coll.as_arr.items:
        yield v
    of MASK_SIG_MAP:
      for v in coll.as_map.values:
        yield v
    # of MASK_SIG_SET: return coll.as_set.values
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else:
      raise newException(TypeException, &"Cannot iterate the values of {$coll} of type {coll.type_label}")
iterator pairs*(coll: ImValue): (ImValue, ImValue) =
  let coll_sig = bitand(coll.type_bits, MASK_SIGNATURE)
  case coll_sig:
    # of MASK_SIG_ARR: return coll.as_arr.pairs
    of MASK_SIG_MAP:
      for p in coll.as_map.pairs:
        yield p
    # of MASK_SIG_SET: return coll.as_set.pairs
    # of MASK_SIG_STR: return coll.as_str.set(k, v)
    else:
      raise newException(TypeException, &"Cannot iterate the pairs of {$coll} of type {coll.type_label}")

proc `+`*(x, y: ImValue): ImValue = 
  if x.is_num and y.is_num: return (x.as_f64 + y.as_f64).v
  else: raise newException(TypeException, &"Cannot call `+` on {x.type_label} and {y.type_label}")
proc `-`*(x, y: ImValue): ImValue = 
  if x.is_num and y.is_num: return (x.as_f64 - y.as_f64).v
  else: raise newException(TypeException, &"Cannot call `-` on {x.type_label} and {y.type_label}")
proc `*`*(x, y: ImValue): ImValue = 
  if x.is_num and y.is_num: return (x.as_f64 * y.as_f64).v
  else: raise newException(TypeException, &"Cannot call `*` on {x.type_label} and {y.type_label}")
proc `/`*(x, y: ImValue): ImValue = 
  if x.is_num and y.is_num: return (x.as_f64 / y.as_f64).v
  else: raise newException(TypeException, &"Cannot call `/` on {x.type_label} and {y.type_label}")
proc `mod`*(x, y: ImValue): ImValue =
  if x.is_num and y.is_num: return (system.mod(x.to_int, y.to_int)).v
  else: raise newException(TypeException, &"Cannot call `mod` on {x.type_label} and {y.type_label}")

proc `+`*(x: ImValue, y: int): ImValue = 
  if x.is_num: return (x.as_f64 + y.float64).v
  else: raise newException(TypeException, &"Cannot call `+` on {x.type_label} and int")
proc `-`*(x: ImValue, y: int): ImValue = 
  if x.is_num: return (x.as_f64 - y.float64).v
  else: raise newException(TypeException, &"Cannot call `-` on {x.type_label} and int")
proc `*`*(x: ImValue, y: int): ImValue = 
  if x.is_num: return (x.as_f64 * y.float64).v
  else: raise newException(TypeException, &"Cannot call `*` on {x.type_label} and int")
proc `/`*(x: ImValue, y: int): ImValue = 
  if x.is_num: return (x.as_f64 / y.float64).v
  else: raise newException(TypeException, &"Cannot call `/` on {x.type_label} and int")
proc `mod`*(x: ImValue, y: int): ImValue =
  if x.is_num: return (system.mod(x.to_int, y)).v
  else: raise newException(TypeException, &"Cannot call `mod` on {x.type_label} and int")


##
## nil < boolean < number < string < set < array < map
## 
## What about bignum and the rest of the gang?
proc compare*(a, b: ImValue): int =
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
    
        
