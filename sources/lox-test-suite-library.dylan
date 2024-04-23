Module: dylan-user

define library lox-test-suite
  use common-dylan;
  use testworks;
  use lox;
end library;

define module lox-test-suite
  use common-dylan;
  use testworks;
  use lox;
  use lox-impl;
end module;
