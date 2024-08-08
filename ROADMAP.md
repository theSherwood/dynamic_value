# ROADMAP

## Differential Dataflow

- [ ] perf improvements for value types
  - [ ] HAMT
    - [ ] map
    - [ ] set
  - [ ] persistent bit-partitioned vector trie
    - [ ] arr
      - https://dmiller.github.io/clojure-clr-next/general/2023/02/12/PersistentVector-part-2.html
    - [ ] str
  - [ ] interning
- [ ] finish differential dataflow
  - [ ] operators
    - [x] distinct
    - [ ] iterate
      - [ ] test with game of life
    - [-] count
      - [This turns out to be really complicated. I'm not at all clear what the
        correct semantics are. I suspect that will also be the case for min, max
        and reduce.]
    - [ ] reduce
    - [ ] max
    - [ ] min
  - [!] genericity
    - [This turns out to be a real problem. We might need concepts to be able
      to do this. Which sucks.]
  - [ ] rollbacks on errors
  - [ ] streams?
  - [ ] sources and sinks (views) similar to materialite
    - refer to https://github.com/vlcn-io/materialite
    - [ ] sources
      - TODO...
      - [ ] map?
      - [ ] array?
      - [ ] tree?
    - [ ] sinks
      - These are ref types. How does this work with our immutable values?
        - In the short term we could just add a reactive atom or box type or
          something and just swap in the updated value
          - This doesn't really work with tracking the multiplicity but not
            returning it.
      - Must support rollbacks on errors
      - [ ] maps
        - what about multiplicity?
          - tracks multiplicity but only returns value on lookup if the
            multiplicity is positive
        - version -> key or key -> version
        - [ ] cumulative map
          - key -> value
          - this presupposes that the versions have a total order
        - [ ] versioned map
          - version -> key -> value
        - [ ] cumulative versioned map (probably should be the default)
          - version -> key -> value
          - every update given some version v1 also updates all versions > v1
      - [ ] arrays
        - TODO...
      - [ ] strings?
      - [ ] DOM?
- [ ] js interface for value types
- [ ] js interface for exceptions/errors

### Demos

- [ ] web demos of differential dataflow (include rendering)
  - refer to https://github.com/vlcn-io/materialite

## Logic Programming

- [ ] logic programming on top of differential_dataflow
  - [ ] unification for bindings and pattern-matching
    - refer to https://bguppl.github.io/interpreters/class_material.html
      - logic programming
      - type unification
      - functional substitution
- [ ] js interface for logic programming

### Demos

- [ ] web demos of logic programming (include rendering)

## Jackal/Ruliad State

- [ ] multi-version state
- [ ] multi-version sync

### Demos

- [ ] web demos of multi-version state and sync (include rendering)

## Custom Logic Programming Language

- [ ] logic programming language
  - [ ] design
    - refer to verse https://simon.peytonjones.org/assets/pdfs/verse-icfp23.pdf
  - [ ] parsing
  - [ ] IR
  - [ ] backend (1 of the following)
    - [ ] wasm
    - [ ] js
    - [ ] HVM

## Concurrency

- [ ] parallelism and concurrency
  - [ ] async support
  - [ ] threads
    - [ ] data parallelism
    - [ ] dataflow parallelism
    - [ ] task parallelism
  - [ ] structured concurrency within PL
