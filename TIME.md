# TIME

Multiple concepts of time we have to contend with.

- instruction time
- iteration time
- clock time
  - client clock time
  - server clock time
- concurrency time
  - promise/future
  - structured concurrency
- exception time
- branching history

Not every kind of time need find itself represented in the Differential Dataflow timestamps. It is probably best if we can find a way to reify these times in various ways. That will mean foregoing Differential Dataflow's implicit support for efficient processing of commutative operations across ordered timestamps, but may ultimately simplify our implementation for dimensions of time that are inherently sequential.

## Kinds of Time

### Instruction Time

Timestamp::

1-dimensional and 1-directional = monotonically increasing number

Were this the only form of time, we wouldn't even need to track time.

Can be used to model things happening using a simple, synchronous model of programming. This can be multiple sequential statements in a block of code or multiple, non-recursive invocations of a function.

Programming languages might represent this as an instruction pointer, but one wouldn't really have access to the pointer itself. Due to the way operators in Differential Dataflow effectively reify instructions, it may be useful to think of instruction pointers and how manipulating them can implement control flow. But thinking about what we want the API to allow is probably the first step.

### Iteration Time

This is the model of time required for nested invocations of Differential Dataflow's `iterator` operator. Differential Dataflow models this as 2-dimensional time, but I don't see any reason not to have this be more like a merging tree (something like a fork-and-join model), where every additional element in a tuple timestamp represents a nested iteration scope.

Specifically:

- [1] < [2]
- [1] < [1, 1]
- [1, 1] < [1, 2]
- [1] < [2, 1]
- [2] < [2, 1]
- [1, 5] < [2]
- [1, 5] < [2, 1]

This model is effectively linear (total order). So there's actually no reason not to use a monotonically increasing number. I must be missing something important.

### Clock Time

For a single machine, clock time is a simple monotonically increasing number. If trying to sync multiple machines, it's more complicated. In a peer-to-peer system, this gets really weird unless you use some kind of sync method like a Lamport Clock or something.

With just a client time and a server time, we could just handle this as two monotonically increasing numbers. Unlike Instruction Time, we still want to track it because the distance between two timestamps is meaningful.

Rather than think of this as Differential Dataflow timestamp, it is probably better to reify each clock time as a stream.

#### Client

Timestamp::

1-dimensional and 1-directional = monotonically increasing number

#### Server

Timestamp::

1-dimensional and 1-directional = monotonically increasing number

### Concurrency Time

Consider some structured concurrency primitive like Verse's `sync` where it is used like so:

```verse
sync:
  async_expression_1
  async_expression_2
  async_expression_3
some_synchronous_expression
```

We need some model of time that blocks `some_synchronous_expression` from running until the 3 async expressions (which may run in parellel) have completed. Basically acts like a `Promise.all` from JavaScript but in a blocking context.

Similarly:

```verse
race:
  async_expression_1
  async_expression_2
  async_expression_3
some_synchronous_expression
```

We need some model of time that blocks `some_synchronous_expression` from running until 1 of the 3 async expressions (which may run in parellel) have completed. Acts like a `Promise.rase` from JavaScript but in a blocking context.

Similarly:

```verse
async_expression
sync_expression
```

We need some model of time that blocks `sync_expression` until `async_expression` has completed.

```verse
let future = spawn(async_expression)
sync_expression
```

In this example, the `async_expression` should not block.

```verse
let future = spawn(async_expression)
sync_expression1
future.await()
sync_expression2
```

In this example, `future.await()` should block.

All of these examples seem to be assuming logic programming, which means that the data that is moving through the dataflow graph is going to be a bunch of bindings (a map). So we need to associate these futures with some map of bindings. We also probably need to associate the futures with some timestamp so that we can keep the different streams of history separate. We also probably need a way to cancel pending futures if the data that launched the async task is somehow undone. We also need to consider how async tasks interact with transactions. Surely, blocking async tasks should block the transaction from completing. Spawned tasks that do not block should be able to launch later transactions that somehow create a relationship to the transaction that they amend. There also needs to be some concept of timeouts. A transaction that depends on an async task that times out should cause the transaction to fail. Basically, there are lots of problems to try to figure out and solve.

In a list form, these are problems in need of solutions:

- Futures need to block by default
- Futures can be caused to not block
- Futures need to be cancelled if their data dependencies are undermined
- Futures need to remain associated with the bindings from which they sprang
- Futures need to remain associated with the timestamp that created them
- Futures that do not block may trigger further transactions that become associated with the transaction that created them
- Futures that block must have a timeout less than some maximum
- Futures that timeout in blocking contexts must cause the transaction to fail (rollback)
- Futures that block should prevent the transaction from commiting until they resolve

Open questions:

- In a transaction composed of multiple patches, should a blocking future prevent the next patch from processing?
- How does a blocking future somewhere in the dataflow graph prevent the transaction from commiting?
- What happens if the user attempts a new transaction while there is still a transaction open?

### Exception Time

TODO - I may need to understand a lot more about how WASM exceptions work before I can think this through. But we might want to think about instruction pointers and control flow in general (looping, blocks, break, continue, if/else, switch).

Exceptions must cause affected scopes to rollback. So uncaught exceptions must rollback the entire transaction. Caught exceptions must cause a partial rollback, meaning that every scope must have the ability to rollback effects and results created within that scope. This is potentially expensive but important for the sake of correctness.

An implication of this is that `iterate` isn't the only way to create a scope. `try`/`catch` (or effect handlers more generally?) must also create scopes. (Other things that **might** create scopes are structured concurrency or new binding contexts (similar to the notion of `scope` in most programming languages)).

### Branching History

Time in branching history is a tree with partial order and could be represented as tuples of numbers such that:

Timestamp::

1.5-dimensional - represented as a tuple of numbers

- [1] < [1, 1]
- [1, 1] < [1, 2]
- [1] < [2]
- [1, 1] and [2] are not comparable
- [1, 1] and [2, 1] are not comparable
- [1, 1, 1] and [1, 2, 1] are not comparable

The `Root` timestamp is [0]. A `History` is a set of time tuples that have been derived from the `Root` such that if a tuple T is in some `History` H, all tuples less than T are also members of H.

We have 2 operations for creating new timestamps from a given timestamp:

- `Increment` - increase the final element of the timestamp by 1
  - `Increment([2, 3])` -> `[2, 4]`
- `Fork` - concat a 1 to a timestamp
  - `Fork([2, 3])` -> `[2, 3, 1]`

Each tuple would represent some transaction's place in history. This scheme is limited because it implies a single alternate fork is possible from any point in history. We want to support infinite fork from any point in history. To do that, we will explode our current tuples and join them with unique identifiers that identify a particular fork from all other forks that fork at that particular point.

Timestamp::

1.5-dimensional - represented as a tuple of numbers interspersed with `Fork` ids

- [1] < [2]
- [1] < [1, a, 1]
- [1] < [1, b, 1]
- [1, a, 1] and [1, b, 1] are not comparable
- [1, a, 1] and [2] are not comparable
- [1, a, 2, a, 1] < [1, a, 2, a, 2]
- [1, a, 3, a, 1] and [1, a, 2, a, 2] are not comparable
- [1, a, 2, a, 1] and [1, a, 2, b, 1] are not comparable
- [1, a, 2, a, 1] and [1, b, 2, a, 1] are not comparable

With this scheme, the `Fork` operation concats a fork identifier and a 1.

- `Fork([2, a, 3])` -> `[2, a, 3, a, 1]`
  `Increment` is unchanged.

Assuming we have the concept of a `Branch` which is
points to some tuple T in a `History` H such that there exists no other tuple in H that is greater than T. `Main` is the `Branch` which points to a tuple with length 1 (there will only be one such `Branch`). Each `Branch` has a name which is just the `Branch`'s tuple with the last element popped off. This gives `Branch`es a consistent name even as there are updates to it. So `Main`'s name is always `[]`.

In truth, it's probably best to reify all this into source and sink trees instead of trying to handle it in timestamps, because in some queries we will want to read from multiple different points in time, especially different `Branch`es.

#### Optimistic Branches

Every tuple has a 1-to-1 relationship with a `TransactionId`, which we won't address in depth here except to talk about optimistic branches in server-client systems. In this view, `History` isn't a set of tuple timestamps, but a `Map(Timestamp, TransactionId)`. There is some `History` C on the client and some `History` S on the server. The client also maintains a `History` S', which is most up-to-date model of S. An optimistic branch is some `Branch` BC whose tuple TC is in C with a corresponding `Branch` BS whose tuple TS is in S' and where TS < TC.

If the client receives an update from the server that S has received a new entry, S' and C must add that entry. If the entry is in conflict with an existing entry in C that has the same key T, C can fork the predecessor of T to F, rederive T and all Timestamps that are greater than T to follow from F, add those new entries to C, and remove the conflicting entries. Only once the conflict has been resolved does C need to add the new entries.

This kind of reshuffling could cause a lot of rules to need to rerun and rederive their results. One very open question remaining is how this will interact with asynchronous operations that are already in flight and other non-linear instructions. Similarly, it would be useful to be able to differentiate easily between optimistic branches and those already in S' so as to allow references to optimistic branches to be changed to point to the new branch in the case of a reshuffle. This might be essential for smoothing UI when the underlying branch of history is shifting underneath the user.

#### Working with Async code

While reifying the `History` is probably essential, we do need some way to tie async operations (their effects and results) to the logical point in history in which they are supposed to transpire. This needs to work even in the midst of reshuffles (synchronizing the client and server `History`s).

One apparent solution is to have the `TransactionId` in the Differential Dataflow timestamp and have it be incomparable with every other `TransactionId`. The problem with this approach is that processing transactions needs to happen in the context of the state of some branch of history. If there's a reshuffle, we move that transaction to a different context, but one in which the state is almost entirely the same (other than the name of the `Branch`.) It may be better to simply cancel everything and rerun the transactions in the new context. Hard to say for sure. But if that's the case, then we should be able to just pass the `Timestamp` into the Differential Dataflow timestamp (assuming we cancel all async stuff in a reshuffle).

#### Remaining Questions

- What guarantees can we offer and invariants maintain?
- Can we guarantee that patches are commutative within a transaction? (order of processing doesn't matter)
- If a transaction is performed in the context of some branch but then is reshuffled to a new branch...
  - ...can we guarantee that all the patches are valid in the new context?
  - ...can we guarantee that the state is the same?
- If a transaction from the server is synced to the client
  - ...can we guarantee that the exact same persisted patches will be generated on the client as on the server?
  - ...can we guarantee that the resulting view of the persisted state is exactly the same on server and client?

## Bringing it all together

What structure of timestamps can we use to make this work?

```nim
type
  Timestamp = object
    # History Timestamp
    # immutable
    HT: seq[Natural]
    # transaction patch index
    # immutable
    TP: Natural
    # each element is a nested scope
    # deeply immutable
    scopes: seq[Natural]
    # 1 ScopeKind per scope
    # deeply immutable
    scope_kind: seq[ScopeKind]
    # 1 nil or seq[Future] per scope
    # the futures must be mutable in place
    parallel_futures: seq[nil | seq[Future]]
  
  ScopeKind = enum
    kIterate
    kTryCatch
    kBindings
  
  FutureState = enum
    FuturePending
    FutureSuccess
    FutureFailure
    # prevents any callbacks from running
    FutureCancelled

  Future = object
    state: FutureState
    value: Value
    env: Environment
    # in this context, we can use this to kick off the dataflow if
    # the other Futures in this environment are done
    success_cb: proc(env: Environment, v: Value): Value
    # in this context, this would create an exception
    failure_cb: proc(env: Environment, v: Value): Value
    # in this context, this would be nil
    finally_cb: proc(env: Environment, v: Value): void

  Environment = TODO
  Bindings = TODO
```

This "Timestamp" is **way too big** to pass around with every collection (many of which will be collections of size 1).

We also need some way of associating Futures with the transaction so that if the transaction is cancelled/rolled-back/undone for whatever reason, all the futures can be cancelled.

We also need a way of connecting Futures with js Promises.

There may be a completely different way of handling Futures uses structural differences rather than shoehorning them into the Timestamp. Something similar in spirit to `iterate`. I think this would be possible in the case of structural concurrency like verse's `sync` and `race`, but how does this work for just blocking in direct style code without infecting every operator? On the other hand, maybe our heavyweight Timestamp doesn't address this case either. This is a big open question.

It is really starting to look like we don't need the kind of 2D versions that Differential Dataflow uses. That may not allow us to simplify anything, though. Our branching history tree-like timestamps will still require indexes/arrangements.

How do we handle scoped rollback?

It would be nice to try to cordon off effects into their own corner.