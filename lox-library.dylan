Module: dylan-user

define library lox
  use common-dylan;
  use io;
  use system;

  export
    lox,
    lox-impl;
end library;

define module lox
  create
    run,
    run-file,
    run-prompt;
end module;

define module lox-impl
  use lox;

  use common-dylan;
  use file-system, prefix: "fs/";
  use format-out;
  use locators;
  use standard-io, prefix: "io/";
  use streams, prefix: "io/";
end module;
