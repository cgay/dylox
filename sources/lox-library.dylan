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
    run,
    run-file,
    run-prompt,
    <lox-error>,
    <scanner-error>;
end module;

define module lox-impl
  use lox;

  use common-dylan;
  use file-system, prefix: "fs/";
  use format, prefix: "io/";
  use format-out, prefix: "io/";
  use locators;
  use print, prefix: "io/";
  use standard-io;
  use streams, prefix: "io/";
  use strings;
  use uncommon-utils;

  // for the test suite
  export
    %value,
    <expression>,
    <parser>,
    <scanner>,
    parse-expression,
    scan-tokens,
    s-expression;
end module;
