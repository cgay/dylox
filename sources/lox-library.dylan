Module: dylan-user

define library lox
  use common-dylan;
  use io;
  use strings;
  use system;
  use uncommon-dylan;

  export
    lox,
    lox-impl;
end library;

define module lox
  create
    <lox-error>,
    <scanner>, <scanner-error>, scan,
    <parser>, <parser-error>, parse,
    <evaluator>, <runtime-error>, eval, eval-top-level, had-errors?;
end module;

define module lox-impl
  use lox;

  use common-dylan;
  use format,      prefix: "io/";
  use format-out,  prefix: "io/";
  use print,       prefix: "io/";
  use strings;
  use streams,     prefix: "io/";
  use uncommon-utils;

  // for the test suite
  export
    %value,
    <expression>,
    parse-expression,
    s-expression,
    <environment>, <global-environment>, <lexical-environment>;
end module;
