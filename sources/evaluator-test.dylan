Module: lox-test-suite


define function %eval
    (source :: <string>, #key env = make(<global-environment>)) => (value)
  let statements = parse(source);
  let ev = make(<evaluator>, print-errors?: #t, print-ast?: #t);
  let val = unsupplied();
  for (statement in statements)
    val := eval(ev, statement, env);
  end;
  assert-false(ev.had-errors?);
  val
end;

define test test-eval-expression-statements ()
  assert-equal(3, %eval("1 + 2;"));
  assert-equal(6, %eval("1 + 2 + 3;"));
  assert-equal(2, %eval("3 - 1 - 0;"));
  assert-equal(4, %eval("8 / 2;"));
  assert-equal(8, %eval("2 * 4;"));
end test;

define test test-eval-precedence ()
  assert-equal(8, %eval("2 * 3 + 4 / 2;"));
  assert-equal(7, %eval("2*(3+4)/2;"));
  assert-equal(4, %eval("2 * 3 - 2;"));
end test;

define test test-eval-program ()
  assert-equal(10, %eval("2 + 2; 9 + 1;"));
end test;

define test test-eval-environments ()
  assert-signals(<eval-error>, %eval("a;"), "undefined variable");
  assert-signals(<eval-error>, %eval("var b=2; var b=3; b;"), "variable already exists");
  assert-equal(3.0d0, %eval("var c = 3; c;"));
  assert-equal("xy", %eval("""var d = "x"; var e = "y"; d + e;"""));
end test;
