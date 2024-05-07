Module: lox-test-suite


define function %scan (source :: <string>) => (token-types :: <sequence>)
  map(%value, scan-tokens(make(<scanner>, source: source)))
end function;

define test test-scan-tokens ()
  assert-equal(list(#"!=", #"eof"),
               %scan("!="),
               "one token");
  assert-equal(list(#"!=", #"eof"),
               %scan(" !="),
               "leading whitespace");
  assert-equal(list(#"!=", #"eof"),
               %scan("!= "),
               "trailing whitespace");
  assert-equal(list(#"!=", #"!=", #"eof"),
               %scan(" != \t !="),
               "multiple tokens with whitespace");
  assert-equal(list(#"!=", " test", #"!=", #"eof"),
               %scan("!=// test\n!="),
               "eol comments");
end test;

define test test-scan-numbers ()
  assert-equal(list(123.0d0, #"eof"),
               %scan("123"),
               "integer");
  assert-equal(list(123.2d0, #"eof"),
               %scan("123.2"),
               "float");

  // As of Chapter 4 Lox doesn't actually err for this case. Maybe he'll
  // implement it later?
  // assert-signals(<scanner-error>,
  //                %scan(".123"),
  //                "leading dot");

  assert-signals(<scanner-error>,
                 %scan("123."),
                 "trailing dot");
end test;
