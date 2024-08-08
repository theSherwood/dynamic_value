from ../src/test_utils import failures
from values import nil

# Run tests
values.main()

when defined(wasm):
  if failures > 0: raise newException(AssertionDefect, "Something failed.")
