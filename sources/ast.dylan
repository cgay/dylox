Module: lox-impl

// Abstract Syntax Tree (AST) representation. Section 5.2+

define abstract class <ast> (<object>) end;
define abstract class <expression> (<ast>) end;
define abstract class <statement> (<ast>) end;

define class <program> (<statement>)
  constant slot %statements = make(<stretchy-vector>);
end class;

define class <variable-declaration> (<statement>)
  constant slot %name :: <token>, required-init-keyword: name:;
  constant slot %initializer :: <expression>, required-init-keyword: initializer:;
end class;

// Not an expression, really, but what to name it? An expression followed by a
// semicolon becomes a statement.
define class <expression-statement> (<statement>)
  constant slot %expression :: <expression>, required-init-keyword: expression:;
end class;

// This matches the book, but we could consider making print a normal function
// call expression.
define class <print-statement> (<expression-statement>) end;

define class <unary-expression> (<expression>)
  constant slot %operator :: <token>,   required-init-keyword: operator:;
  constant slot %right :: <expression>, required-init-keyword: right:;
end class;

define class <binary-expression> (<expression>)
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


// Convert the AST to s-expression format for testing and REPL display purposes.
define generic s-expression (ast :: <ast>) => (s-expr :: <object>);

define method s-expression (ast :: <unary-expression>) => (s-expr :: <sequence>)
  list(ast.%operator.%value, ast.%right.s-expression)
end method;

define method s-expression (ast :: <binary-expression>) => (s-expr :: <sequence>)
  list(ast.%operator.%value, ast.%left.s-expression, ast.%right.s-expression)
end method;

define method s-expression (ast :: <assignment-expression>) => (s-expr :: <sequence>)
  list(#"=", ast.%name.%value, ast.%value.s-expression)
end method;

define method s-expression (ast :: <call-expression>) => (s-expr :: <sequence>)
  list(ast.%callee.s-expression, map-as(<list>, s-expression, ast.%arguments))
end method;

define method s-expression (ast :: <get-expression>) => (s-expr :: <sequence>)
  list(#".", ast.%object.s-expression)
end method;

define method s-expression (ast :: <set-expression>) => (s-expr :: <sequence>)
  list(#"=", next-method(), ast.%value.s-expression)
end method;

define method s-expression (ast :: <grouping-expression>) => (s-expr :: <sequence>)
  list(#"(", ast.%expression.s-expression, #")")
end method;

define method s-expression (ast :: <literal-expression>) => (literal-value)
  ast.%value
end method;

define method s-expression (ast :: <super-expression>) => (s-expr :: <sequence>)
  list(ast.%keyword.%value, ast.%method.%value)
end method;

define method s-expression (ast :: <this-expression>) => (s-expr :: <sequence>)
  ast.%keyword.%value
end method;

define method s-expression (ast :: <variable-expression>) => (variable-name :: <symbol>)
  ast.%name.%value
end method;

define method s-expression (ast :: <print-statement>) => (s-expr :: <sequence>)
  list(#"print", ast.%expression.s-expression)
end method;

define method s-expression (ast :: <expression-statement>) => (s-expr :: <sequence>)
  ast.%expression.s-expression
end method;

define method s-expression (ast :: <program>) => (s-expr :: <sequence>)
  list(#"program", map(s-expression, ast.%statements))
end method;

define method s-expression (ast :: <variable-declaration>) => (s-expr :: <sequence>)
  list(#"var", ast.%name.%value, ast.%initializer.s-expression)
end method;
