## https://github.com/nim-lang/Nim/issues/23460
import std/sets

proc deepCopyImpl[T](dest: var T; src: T, alreadyCopied: var HashSet[pointer]) =
    # An optimisation could be made if it is possible to have the info about if object is acyclic (compiler knows it ?)
    when typeof(src) is ref or typeof(src) is ptr:
        if src != nil:
            let srcPtr = cast[pointer](src)
            if srcPtr in alreadyCopied:
                dest = src
            else:
                dest = T()
                alreadyCopied.incl srcPtr
                dest[] = src[]
                deepCopyImpl(dest[], src[], alreadyCopied)
    elif typeof(src) is object:
        for _, v1, v2 in fieldPairs(dest, src):
            deepCopyImpl(v1, v2, alreadyCopied)

proc deepCopy*[T](dest: var T; src: T) =
    ## This procedure copies values and create new object for references
    ## Also copies pointers, so unmanaged memory is unsafe if pointer is not wrapped into an object with a destructor
    # Should behave exactly like system.deepCopy
    dest = src
    var alreadyCopied: HashSet[pointer]
    deepCopyImpl(dest, src, alreadyCopied)