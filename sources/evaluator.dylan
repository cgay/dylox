Module: lox-impl


// Main entry point for evaluator.

// Evaluate something in the top-level (global) context. This is the main external
// interface used by the dylox command so it's where the source is parsed and errors are
// handled. If die-on-error? is true then errors are signalled immediately with no
// attempt to recover. origin identifies the program source, such as a file name.
define function eval-top-level
    (ev :: <evaluator>, source :: <string>,
     #key die-on-error? :: <boolean>,
          origin :: <string> = "<stdin>")
 => (value)
  let handler <runtime-error>
    = method (err, next-handler)
        had-errors?(ev) := #t;
        if (ev.print-errors?)
          io/format-err("%s\n", err);
          io/force-err();
        end;
        if (die-on-error?)
          next-handler();       // decline to handle it
        end;
      end;
  // See also the <parser-error> handler in the parser.
  let handler (<parser-error>, test: method (c) ~die-on-error? end) = always(#f);

  let scanner = make(<scanner>,
                     source: source,
                     origin: origin);
  let statements :: <sequence> = parse(scanner);
  let value = $nil;
  for (statement in statements)
    if (ev.print-ast?)
      io/format-out("AST: %=\n", statement.s-expression);
      io/force-out();
    end;
    value := eval(ev, statement, ev.%environment);
    if (ev.print-ast?)
      io/format-out("==> %=\n", value);
      io/force-out();
    end;
  end;
  value
end function;


define class <runtime-error> (<lox-error>) end;

define function runtime-error (ev :: <evaluator>, fmt :: <string>, #rest args)
  signal(make(<runtime-error>, format-string: fmt, format-arguments: args))
end function;

define class <evaluator> (<object>)
  constant slot %environment :: <global-environment> = make(<global-environment>);
  constant slot print-errors? = #t, init-keyword: print-errors?:;
  constant slot print-ast? = #f,    init-keyword: print-ast?:;
  slot had-errors? :: <boolean> = #f;
end class;

define generic eval
    (evaluator :: <evaluator>, ast :: <ast>, environment :: <environment>)
 => (value);

define method eval (ev :: <evaluator>, ast :: <ast>, env :: <environment>) => (value)
  runtime-error(ev, "I don't know how to evaluate a %= yet", ast.object-class);
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
      runtime-error(ev, "expected either '-' or '!' for unary operator, got %=",
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
            runtime-error(ev, "invalid type for %= operation: got %=, want %=",
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
        runtime-error(ev, "invalid types in %= + %=, operands must both be"
                        " string or both numbers", left, right)
      end;
    #">"  => check(<double-float>, left) >  check(<double-float>, right);
    #">=" => check(<double-float>, left) >= check(<double-float>, right);
    #"<"  => check(<double-float>, left) <  check(<double-float>, right);
    #"<=" => check(<double-float>, left) <= check(<double-float>, right);
    #"!=" => left ~= right;
    #"==" => left = right;
    otherwise =>
      runtime-error(ev, "unexpected binary operator: %s", ast.%operator.%value);
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

define method eval (ev :: <evaluator>, ast :: <block>, env :: <environment>) => (value)
  let block-env = make(<lexical-environment>, parent: env);
  let result = $nil;
  for (statement in ast.%statements)
    result := eval(ev, statement, block-env);
  end;
  result
end method;

define function nyi (i, ast)
  runtime-error(i, "evaluation of %= AST not yet implemented", ast);
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

define method eval
    (ev :: <evaluator>, ast :: <if-statement>, env :: <environment>) => (value)
  if (truthy?(eval(ev, ast.%test, env)))
    eval(ev, ast.%then, env)
  elseif (ast.%else)
    eval(ev, ast.%else, env)
  end
end method;
