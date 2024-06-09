Module: dylan-user
Synopsis: Dylan implementation of Lox language from Crafting Interpreters

define library dylox
  use common-dylan;
  use io;
  use lox;
  use system;
end library;

define module dylox
  use common-dylan;
  use file-system, prefix: "fs/";
  use format-out, prefix: "io/";
  use locators;
  use lox;
  use standard-io;
  use streams, prefix: "io/";
end module;
