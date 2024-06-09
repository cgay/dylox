Module: lox-impl


// Main entry point for evaluator.
define constant <source> = type-union(<string>, <file-locator>, <expression>);
define generic eval (evaluator :: <evaluator>, source :: <source>) => (value);

define class <eval-error> (<lox-error>) end;

define function eval-error (i :: <evaluator>, fmt :: <string>, #rest args)
  let format-string = concatenate("runtime error: ", fmt);
  signal(make(<eval-error>,
              format-string: format-string,
              format-arguments: args))
end function;


define class <evaluator> (<object>)
  // not yet: constant slot %top-level-environment :: <table> = make(<table>);
  constant slot print-errors? = #t,        init-keyword: print-errors?:;
  constant slot print-s-expressions? = #f, init-keyword: print-s-expressions?:;
  slot had-errors? :: <boolean> = #f;
end class;

define method eval (ev :: <evaluator>, source :: type-union(<string>, <file-locator>)) => (value)
  let (ast, errors) = parse(source);
  if (ev.print-errors?)
    for (error in errors)
      io/format-err("%s\n", error);
      io/force-err();
    end;
  end;
  if (ast)
    if (ev.print-s-expressions?)
      io/format-out("%=\n", ast.s-expression);
      io/force-out();
    end;
    block ()
      eval(ev, ast)
    exception (ex :: <eval-error>)
      had-errors?(ev) := #t;
      if (ev.print-errors?)
        io/format-err("%s\n", ex);
        io/force-err();
      end;
      #f
    end
  end
end method;

define method eval (ev :: <evaluator>, ast :: <expression>) => (value)
  eval-error(ev, "unimplemented expression subclass: %=", ast.object-class)
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


define function nyi (i, ast)
  eval-error(i, "evaluation of %= AST not yet implemented", ast);
end function;

define method eval (ev :: <evaluator>, ast :: <arithmetic-expression>) => (value)
  nyi(ev, ast);
end method;

define method eval (ev :: <evaluator>, ast :: <logical-expression>) => (value)
  nyi(ev, ast);
end method;

define method eval (ev :: <evaluator>, ast :: <assignment-expression>) => (value)
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
  nyi(ev, ast);
end method;
