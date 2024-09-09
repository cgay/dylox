Module: lox-test-suite


define function %parse (source :: <string>) => (ast :: <statement>)
  let statements = parse(make(<scanner>, source: source));
  assert-equal(1, statements.size, "for now %parse only allows one statement");
  statements[0]
end function;

define test test-parser ()
  assert-equal(1.0d0, %parse("1;").s-expression);
  assert-equal(#(#"+", #(#"+", 1.0d0, #(#"*", 2.0d0, 3.0d0)), 4.0d0),
               %parse("1+2*3+4;").s-expression);
end test;

define test test-parse-call ()
  assert-equal(#(#"f", #()),         %parse("f();").s-expression);
  assert-equal(#(#(#"f", #()), #()), %parse("f()();").s-expression);
  assert-equal(#(#"f", #(1)),        %parse("f(1);").s-expression);
  assert-equal(#(#"f", #(1, 2)),     %parse("f(1, 2);").s-expression);

  // These could all be continuable warnings...
  assert-signals(<parser-error>, %parse("f(1, 2,);")); // trailing comma
  assert-signals(<parser-error>, %parse("f(,1, 2);")); // leading comma
  assert-signals(<parser-error>, %parse("f(1,, 2);")); // extra comma
end test;

define test test-parse-function-declaration ()
  assert-equal(#(#"fun", #"f", #(), #()), %parse("fun f (){}").s-expression);
  assert-equal(#(#"fun", #"f", #(#"a"), #()), %parse("fun f (a){}").s-expression);
  assert-equal(#(#"fun", #"f", #(#"a", #"b"), #()), %parse("fun f (a, b){}").s-expression);
  assert-equal(#(#"fun", #"f", #(), #(#"body")), %parse("fun f (){ body; }").s-expression);
end test;
