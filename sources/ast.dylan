Module: lox-impl

// Abstract Syntax Tree (AST) creation. Section 5.2+

define abstract class <expression> (<object>) end;

define class <unary-expression> (<expression>)
  constant slot %operator :: <token>,   required-init-keyword: operator:;
  constant slot %right :: <expression>, required-init-keyword: right:;
end class;

define abstract class <binary-expression> (<expression>)
  constant slot %operator :: <token>,   required-init-keyword: operator:;
  constant slot %right :: <expression>, required-init-keyword: right:;
  constant slot %left :: <expression>,  required-init-keyword: left:;
end class;

// Not yet clear whether we need these to be separate. In the book he has
// LogicalExpression but not ArithmeticExpression.
define class <arithmetic-expression> (<binary-expression>) end;
define class <logical-expression> (<binary-expression>) end;

define class <assignment-expression> (<expression>)
  constant slot %name :: <token>,       required-init-keyword: name:;
  constant slot %value :: <expression>, required-init-keyword: value:;
end class;

define class <call-expression> (<expression>)
  constant slot %callee :: <expression>,  required-init-keyword: callee:;
  constant slot %paren :: <token>,        required-init-keyword: paren:;
  constant slot %arguments :: <sequence>, required-init-keyword: arguments:;
end class;

define class <get-expression> (<expression>)
  constant slot %object :: <expression>, required-init-keyword: object:;
  constant slot %name :: <token>,        required-init-keyword: name:;
end class;

define class <set-expression> (<get-expression>)
  constant slot %value :: <expression>, required-init-keyword: value:;
end class;

// A parenthesized expression.
define class <grouping-expression> (<expression>)
  constant slot %expression :: <expression>, required-init-keyword: expression:;
end class;

define class <literal-expression> (<expression>)
  constant slot %value :: <object>, required-init-keyword: value:;
end class;

define class <super-expression> (<expression>)
  constant slot %keyword :: <token>, required-init-keyword: keyword:;
  constant slot %method  :: <token>, required-init-keyword: method:;
end class;

define class <this-expression> (<expression>)
  constant slot %keyword :: <token>, required-init-keyword: keyword:;
end class;

define class <variable-expression> (<expression>)
  constant slot %name :: <token>, required-init-keyword: name:;
end class;
