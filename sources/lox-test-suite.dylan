Module: lox-test-suite


define function %run (source :: <string>)
  run(make(<scanner>, source: source))
end;

define test test-addition (expected-to-fail-reason: "NYI")
  assert-equal(3, %run("1 + 2"));
end test;


// Use `_build/bin/lox-test-suite --help` to see options.
run-test-application()
