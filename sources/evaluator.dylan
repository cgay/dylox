Module: lox-impl


// Main entry point for evaluator.
define constant <source> = type-union(<string>, <file-locator>, <ast>);
define generic eval (evaluator :: <evaluator>, source :: <source>) => (value);

define class <eval-error> (<lox-error>) end;

define function eval-error (ev :: <evaluator>, fmt :: <string>, #rest args)
  signal(make(<eval-error>, format-string: fmt, format-arguments: args))
end function;


define class <environment> (<object>)
  constant slot %values = make(<table>); // <symbol> => <object>
end class;

define method set-variable (env :: <environment>, name :: <symbol>, value) => (value)
  env.%values[name] := value
end method;

define method get-variable (env :: <environment>, name :: <symbol>) => (value)
  element(env.%values, name, default: #f)
end method;


define class <evaluator> (<object>)
  constant slot %environment :: <environment> = make(<environment>);
  constant slot print-errors? = #t, init-keyword: print-errors?:;
  constant slot print-ast? = #f,    init-keyword: print-ast?:;
  slot had-errors? :: <boolean> = #f;
end class;

define method eval (ev :: <evaluator>, source :: type-union(<string>, <file-locator>)) => (value)
  let (statements :: <sequence>, errors) = parse(source);
  if (ev.print-errors?)
    for (error in errors)
      io/format-err("%s\n", error);
      io/force-err();
    end;
  end;
  let value = unsupplied();
  for (statement in statements)
    if (ev.print-ast?)
      io/format-out("AST: %=\n", statement.s-expression);
      io/force-out();
    end;
    block ()
      value := eval(ev, statement);
    exception (ex :: <eval-error>)
      had-errors?(ev) := #t;
      if (ev.print-errors?)
        io/format-err("%s\n", ex);
        io/force-err();
      end;
    end;
  end for;
  value
end method;

define method eval (ev :: <evaluator>, ast :: <ast>) => (value)
  eval-error(ev, "I don't know how to evaluate a %= yet", ast.object-class);
end method;

define method eval (ev :: <evaluator>, ast :: <literal-expression>) => (value)
  ast.%value
end method;

define method eval (ev :: <evaluator>, ast :: <grouping-expression>) => (value)
  eval(ev, ast.%expression)
end method;

define method eval (ev :: <evaluator>, ast :: <unary-expression>) => (value)
  select (ast.%operator)
    #"-" =>
      - eval(ev, ast.%right);
    #"!" =>
      ~truthy?(eval(ev, ast.%right));
    otherwise =>
      eval-error(ev, "expected either '-' or '!' for unary operator, got %=",
                 ast.%operator);
  end
end method;

define function truthy? (value) => (b :: <boolean>)
  ~(value == $nil | value == #f)
end function;

define method eval (ev :: <evaluator>, ast :: <binary-expression>) => (value)
  let left = eval(ev, ast.%left);
  let right = eval(ev, ast.%right);
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
define method eval (ev :: <evaluator>, ast :: <expression-statement>) => (value)
  eval(ev, ast.%expression)
end method;

define method eval (ev :: <evaluator>, ast :: <print-statement>) => (value)
  let value = next-method();
  io/format-out("%s\n", value);
  value
end method;

define function nyi (i, ast)
  eval-error(i, "evaluation of %= AST not yet implemented", ast);
end function;

define method eval (ev :: <evaluator>, ast :: <arithmetic-expression>) => (value)
  nyi(ev, ast);
end method;

define method eval (ev :: <evaluator>, ast :: <logical-expression>) => (value)
  nyi(ev, ast);
end method;

define method eval (ev :: <evaluator>, ast :: <call-expression>) => (value)
  nyi(ev, ast);
end method;

define method eval (ev :: <evaluator>, ast :: <get-expression>) => (value)
  nyi(ev, ast);
end method;

define method eval (ev :: <evaluator>, ast :: <set-expression>) => (value)
  nyi(ev, ast);
end method;

define method eval (ev :: <evaluator>, ast :: <super-expression>) => (value)
  nyi(ev, ast);
end method;

define method eval (ev :: <evaluator>, ast :: <this-expression>) => (value)
  nyi(ev, ast);
end method;

define method eval (ev :: <evaluator>, ast :: <variable-expression>) => (value)
  get-variable(ev.%environment, ast.%name.%value)
    | eval-error(ev, "attempt to read undefined variable %=", ast.%name.%value)
end method;

define method eval (ev :: <evaluator>, ast :: <variable-declaration>) => (value)
  set-variable(ev.%environment, ast.%name.%value, eval(ev, ast.%initializer))
end method;

define method eval (ev :: <evaluator>, ast :: <assignment-expression>) => (value)
  let name = ast.%name.%value;
  if (~get-variable(ev.%environment, name))
    eval-error(ev, "attempt to set undefined variable %=", ast.%name);
  end;
  set-variable(ev.%environment, name, eval(ev, ast.%value));
end method;
