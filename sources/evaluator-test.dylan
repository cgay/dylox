Module: lox-test-suite


define function %eval
    (source :: <string>, #key env = make(<global-environment>)) => (value)
  let statements = parse(make(<scanner>, source: source));
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
  assert-signals(<runtime-error>, %eval("a;"), "undefined variable");
  assert-signals(<runtime-error>, %eval("var b=2; var b=3;"), "variable already exists");
  assert-equal(3.0d0, %eval("var c = 3; c;"));
  assert-equal("xy", %eval("""var d = "x"; var e = "y"; d + e;"""));
end test;

define test test-variable-assignment ()
  assert-equal("b", %eval("""var g = "a"; g = "b"; g;"""),
               "assign global from global env");
  assert-equal("b", %eval("""var g = "a"; { g = "b"; } g;"""),
               "assign global from lexical env");
  assert-equal("a", %eval("""var g = "a"; { var g = "b"; g = "c"; } g;"""),
               "shadowed global");
  assert-equal("v3xx", %eval("""{ var v = "v"; var x="x"; { var v="v2"; v="v3"; x="xx"; v+x; } }"""),
               "shadowed local");
end test;

// The final example in chapter 8, modified to give us a result we can test
// instead of printing each result.
define test test-chapter-8-example ()
  let text = """
    var a = "global a";
    var b = "global b";
    var c = "global c";
    var r1 = "";
    var r2 = "";
    var r3 = "";
    {
      var a = "outer a";
      var b = "outer b";
      {
        var a = "inner a";
        r1 = a + b + c;
      }
      r2 = a + b + c;
    }
    r3 = a + b + c;
    r1 + "-" + r2 + "-" + r3;
    """;
  assert-equal("inner aouter bglobal c-outer aouter bglobal c-global aglobal bglobal c",
               %eval(text),
               "chapter 8 example");
end test;
