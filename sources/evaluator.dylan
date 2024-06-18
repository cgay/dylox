Module: lox-impl


// Main entry point for evaluator.
define constant <source> = type-union(<string>, <file-locator>, <ast>);
define generic eval
    (evaluator :: <evaluator>, source :: <source>, environment :: <environment>)
 => (value);

define class <eval-error> (<lox-error>) end;

define function eval-error (ev :: <evaluator>, fmt :: <string>, #rest args)
  signal(make(<eval-error>, format-string: fmt, format-arguments: args))
end function;

define class <evaluator> (<object>)
  constant slot %environment :: <global-environment> = make(<global-environment>);
  constant slot print-errors? = #t, init-keyword: print-errors?:;
  constant slot print-ast? = #f,    init-keyword: print-ast?:;
  slot had-errors? :: <boolean> = #f;
end class;

// Evaluate something in the top-level (global) context.
define function eval-top-level
    (ev :: <evaluator>, source :: <source>) => (value)
  eval(ev, source, ev.%environment)
end function;

define method eval
    (ev :: <evaluator>, source :: type-union(<string>, <file-locator>), env :: <environment>)
 => (value)
  let (statements :: <sequence>, errors) = parse(source);
  if (ev.print-errors?)
    for (error in errors)
      io/format-err("%s\n", error);
      io/force-err();
    end;
  end;
  let return-value = unsupplied();
  let env = make(<lexical-environment>, parent: ev.%environment);
  for (statement in statements)
    if (ev.print-ast?)
      io/format-out("AST: %=\n", statement.s-expression);
      io/force-out();
    end;
    block ()
      return-value := eval(ev, statement, env);
    exception (ex :: <eval-error>)
      had-errors?(ev) := #t;
      if (ev.print-errors?)
        io/format-err("%s\n", ex);
        io/force-err();
      end;
    end;
  end for;
  return-value
end method;

define method eval (ev :: <evaluator>, ast :: <ast>, env :: <environment>) => (value)
  eval-error(ev, "I don't know how to evaluate a %= yet", ast.object-class);
end method;

define method eval
    (ev :: <evaluator>, ast :: <literal-expression>, env :: <environment>) => (value)
  ast.%value
end method;

define method eval
    (ev :: <evaluator>, ast :: <grouping-expression>, env :: <environment>) => (value)
  eval(ev, ast.%expression, env)
end method;

define method eval
    (ev :: <evaluator>, ast :: <unary-expression>, env :: <environment>) => (value)
  select (ast.%operator)
    #"-" =>
      - eval(ev, ast.%right, env);
    #"!" =>
      ~truthy?(eval(ev, ast.%right, env));
    otherwise =>
      eval-error(ev, "expected either '-' or '!' for unary operator, got %=",
                 ast.%operator);
  end
end method;

define function truthy? (value) => (b :: <boolean>)
  ~(value == $nil | value == #f)
end function;

define method eval
    (ev :: <evaluator>, ast :: <binary-expression>, env :: <environment>) => (value)
  let left = eval(ev, ast.%left, env);
  let right = eval(ev, ast.%right, env);
  let op = ast.%operator.%value;
  local method check (type, value)
          if (~instance?(value, type))
            eval-error(ev, "invalid type for %= operation: got %=, want %=",
                       op, value.object-class, type);
          end;
          value
        end;
  select (op)
    #"-" => check(<double-float>, left) - check(<double-float>, right);
    #"/" => check(<double-float>, left) / check(<double-float>, right);
    #"*" => check(<double-float>, left) * check(<double-float>, right);
    #"+" =>
      if (instance?(left, <double-float>) & instance?(right, <double-float>))
        left + right
      elseif (instance?(left, <string>) & instance?(right, <string>))
        concatenate(left, right)
      else
        eval-error(ev,
                   "invalid types in %= + %=, operands must both be string or both numbers",
                   left, right)
      end;
    #">"  => check(<double-float>, left) >  check(<double-float>, right);
    #">=" => check(<double-float>, left) >= check(<double-float>, right);
    #"<"  => check(<double-float>, left) <  check(<double-float>, right);
    #"<=" => check(<double-float>, left) <= check(<double-float>, right);
    #"!=" => left ~= right;
    #"==" => left = right;
    otherwise =>
      eval-error(ev, "unexpected binary operator: %s", ast.%operator.%value);
  end
end method;

// Unlike the book we return the statement's value. We could do it like the
// book by having separate eval-expression and eval-statement functions.  It
// would make testing harder and it means you have to use "print" in the REPL
// to see any values.
define method eval
    (ev :: <evaluator>, ast :: <expression-statement>, env :: <environment>) => (value)
  eval(ev, ast.%expression, env)
end method;

define method eval
    (ev :: <evaluator>, ast :: <print-statement>, env :: <environment>) => (value)
  let value = next-method();
  io/format-out("%s\n", value);
  value
end method;

define function nyi (i, ast)
  eval-error(i, "evaluation of %= AST not yet implemented", ast);
end function;

define method eval
    (ev :: <evaluator>, ast :: <arithmetic-expression>, env :: <environment>) => (value)
  nyi(ev, ast);
end method;

define method eval
    (ev :: <evaluator>, ast :: <logical-expression>, env :: <environment>) => (value)
  nyi(ev, ast);
end method;

define method eval
    (ev :: <evaluator>, ast :: <call-expression>, env :: <environment>) => (value)
  nyi(ev, ast);
end method;

define method eval
    (ev :: <evaluator>, ast :: <get-expression>, env :: <environment>) => (value)
  nyi(ev, ast);
end method;

define method eval
    (ev :: <evaluator>, ast :: <set-expression>, env :: <environment>) => (value)
  nyi(ev, ast);
end method;

define method eval
    (ev :: <evaluator>, ast :: <super-expression>, env :: <environment>) => (value)
  nyi(ev, ast);
end method;

define method eval
    (ev :: <evaluator>, ast :: <this-expression>, env :: <environment>) => (value)
  nyi(ev, ast);
end method;

define method eval
    (ev :: <evaluator>, ast :: <variable-expression>, env :: <environment>) => (value)
  get-variable(ev, env, ast.%name.%value)
end method;

define method eval
    (ev :: <evaluator>, ast :: <variable-declaration>, env :: <environment>) => (value)
  create-variable(ev, env, ast.%name.%value, eval(ev, ast.%initializer, env))
end method;

define method eval
    (ev :: <evaluator>, ast :: <assignment-expression>, env :: <environment>) => (value)
  let name = ast.%name.%value;
  set-variable(ev, env, name, eval(ev, ast.%value, env))
end method;
