from ../src/test_utils import failures
from ./persistent/vec import nil
from ./persistent/map import nil
from ./persistent/set import nil
from ./persistent/multiset import nil
from sumtree import nil
from values import nil
from dida_from_python import nil
from kanren import nil

# Run tests
vec.main()
map.main()
set.main()
multiset.main()
# sumtree.main()
values.main()
dida_from_python.main()
kanren.main()

when defined(wasm):
  if failures > 0: raise newException(AssertionDefect, "Something failed.")
