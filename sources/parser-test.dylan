Module: lox-test-suite


define function %parse (source :: <string>) => (ast :: <expression>)
  let scanner = make(<scanner>, source: source);
  let tokens = scan-tokens(scanner);
  parse-expression(make(<parser>, tokens: tokens))
end function;

define test test-parser ()
  assert-equal(1.0d0, %parse("1").s-expression);
  assert-equal(#(#"+", #(#"+", 1.0d0, #(#"*", 2.0d0, 3.0d0)), 4.0d0),
               %parse("1+2*3+4").s-expression);
end test;
