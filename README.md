# dynamic_value

Provides a dynamic value type that is basically a thin wrapper over my [persistent](https://github.com/theSherwood/persistent) lib to provide Clojure-like values. Compiles to 64-bit native or 32-bit webassembly.

**Disclaimer**: This was written using the Nimskull compiler, which has not reached a stable version and, as of the time of this writing, continues to see rapid changes. There are already several differences between the Nimskull and Nim compilers. As such, if you wish to use any of this code... good luck!

## Usage


## Scripts and commands

### Build Native

```sh
./run.sh -tu native
```

OR

```sh
wach -o "src/**" "./run.sh -tu native"
```

TODO

## Test

### Test Native

```sh
./run.sh -tur native
```

OR

```sh
wach ./run.sh -tur native
```

### Test Wasm in Node

```sh
./run.sh -tur node32
```

OR

```sh
wach -o "src/**" "./run.sh -tur node32"
```

### Test Wasm in Browser

Compile wasm:

```sh
wach -o "src/**" "./run.sh -tu browser32"
```

Start the server:

```sh
dev start
```

Go to http://localhost:3000/

OR

```sh
./run.sh -tur browser32
```

### Benchmark

```sh
./run.sh -bur
```

## State

Currently, no mutable values are supported. All heap-based collections are persistent data structures.

Currently supported types:

- Bool
- Nil
- Num      - 64-bit float
- Sym   - unique reference value
- Str
- Vec
- Map
- Set

The code is something of a mess at the moment.

### TODO

- [ ] mutable collections
- [ ] user-defined types
- [ ] add Rope-like persistent strings
  - the current string is just a wrapper over Nimskull strings