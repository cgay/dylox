Module: lox-test-suite


define function %eval (source :: <string>) => (value)
  let ev = make(<evaluator>,
                // Enable these to debug your tests.
                print-errors?: #f,
                print-s-expressions?: #f);
  let val = eval(ev, source);
  assert-false(ev.had-errors?);
  val
end;

define test test-eval-expressions ()
  assert-equal(3, %eval("1 + 2"));
  assert-equal(6, %eval("1 + 2 + 3"));
  assert-equal(2, %eval("3 - 1 - 0"));
  assert-equal(4, %eval("8 / 2"));
  assert-equal(8, %eval("2 * 4"));
end test;

define test test-eval-precedence ()
  assert-equal(8, %eval("2 * 3 + 4 / 2"));
  assert-equal(4, %eval("2 * 3 - 2"));
end test;
