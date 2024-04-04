Module: lox-test-suite


define test test-run ()
  assert-equal(3, run("1 + 2"));
end test;

// Use `_build/bin/lox-test-suite --help` to see options.
run-test-application()
