# dynamic_value

Provides a dynamic value type that is basically a thin wrapper over my [persistent lib](https://github.com/theSherwood/persistent) to provide Clojure-like values. Compiles to 64-bit native or 32-bit webassembly.

**Disclaimer**: This was written using the Nimskull compiler, which has not reached a stable version and, as of the time of this writing, continues to see rapid changes. There are already several differences between the Nimskull and Nim compilers. As such, if you wish to use any of this code... good luck!

## Usage

The main way to define the dynamic values is through some macros.

```nim
let
  str_value   = Str "This is a string"
  sym_value   = Sym "This is a sym"
  nil_value   = Nil
  vec_value   = Vec [1, 2.7, Nil, "foo", {1, 2.7, Nil, "foo"}]
  set_value   = Set {1, 2.7, Nil, "foo", [1, 2.7, Nil, "foo"]}
  map_value   = Map {1: [1, 2.7, Nil, "foo"], 2.7: "foo", "foo": 2.7, [1, 2.7, Nil, "foo"]: 1}
  true_value  = True
  false_value = False

  str_val2    = V "This is another string"
  vec_val2    = V [1, 2.7, Nil, "foo"]
  set_val2    = V {1, 2.7, Nil, "foo"}
  map_val2    = V {1: "foo", 2.7: 1, "foo": 1}
```

Refer to the tests for additional usage code.

## Scripts and commands

### Build Native

```sh
./run.sh -tu native
```

### Test Native

```sh
./run.sh -tur native
```

### Test Wasm in Node

```sh
./run.sh -tur node32
```

### Test Wasm in Browser

Compile wasm:

```sh
./run.sh -tur browser32
```

Start the server:

```sh
dev start
```

Go to http://localhost:3000/


### Benchmark

```sh
./run.sh -bur
```

## State

The static type is `Value`, and it uses 64 bits to encode several dynamic values. This is done using NaN-boxing of a 64-bit float. There are differences between the ways this encoding works on 64-bit systems and 32-bit systems.

Currently supported types:

- `Bool`
- `Nil`
- `Num`      - 64-bit float
- `Str`
- `Vec`
- `Map`
- `Set`
- `Sym`      - unique reference value

All the types currently supported are immutable. Collection types are persistent data structures. All types currently supported are compared by value with the exception of `Sym`, which is a unique reference type. `Str` is just a thin wrapper over Nimskull `string`. There is currently no support for mutable types.

The code is something of a mess at the moment.

### TODO

- [ ] mutable collections
- [ ] user-defined types
- [ ] add Rope-like persistent strings
  - the current string is just a wrapper over Nimskull strings
- [ ] short strings stored inline
