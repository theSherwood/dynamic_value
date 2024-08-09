from ../src/test_utils import failures
from value import nil

# Run tests
value.main()

when defined(wasm):
  if failures > 0: raise newException(AssertionDefect, "Something failed.")
