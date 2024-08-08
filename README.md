# differential_dataflow

## Build

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

## Benchmark

```sh
./run.sh -bur
```
