Module: lox-impl

// Recursive descent parser for Lox grammar.

// Main parsing entry point.
define function parse (scanner :: <scanner>) => (statements :: <sequence>)
  let parser = make(<parser>, tokens: scan(scanner));
  let statements = make(<stretchy-vector>);
  iterate loop ()
    let token = peek-token(parser);
    unless (instance?(token, <eof-token>))
      let statement = parse-declaration(parser); // #f if synchronize called
      statement & add!(statements, statement);
      loop();
    end;
  end;
  statements
end function;

define class <parser-error> (<lox-error>) end;

define function parser-error (p :: <parser>, fmt :: <string>, #rest args)
  let format-string = concatenate("parse error at token %s: ", fmt);
  // We assume the token hasn't been consumed...reasonable?
  let format-args = concatenate(list(peek-token(p)), args);
  signal(make(<parser-error>,
              format-string: format-string,
              format-arguments: format-args))
end function;

define function record-error (p :: <parser>, err :: <lox-error>) => ()
  add!(p.%errors, err);
  io/format-err("%s\n", err);
  io/force-err();
end function;

define class <parser> (<object>)
  constant slot %tokens :: <sequence>, required-init-keyword: tokens:;
           slot %index :: <integer> = 0;
  constant slot %errors :: <sequence> = make(<stretchy-vector>);
end class;

define function peek-token (p :: <parser>) => (t :: <token>)
  let tokens = p.%tokens;
  let index = p.%index;
  if (index >= tokens.size)
    tokens.last                 // always EOF
  else
    tokens[index]
  end
end function;

define function consume-token (p :: <parser>, #key expect) => (t :: <token>)
  let token = peek-token(p);
  if (~instance?(token, <eof-token>))
    inc!(p.%index);
  end;
  if (expect & ~ select (expect by instance?)
                   <string> => token.%text = expect;
                   <class> => instance?(token, expect);
                 end)
    parser-error(p, "expected %=", expect)
  end;
  token
end function;

// Return true if any of the tokens match the next token.
define function next-token-matches (p :: <parser>, #rest strings) => (matched? :: <boolean>)
  member?(p.peek-token.%text, strings, test: \=)
end function;

define function parse-declaration (p :: <parser>) => (d :: false-or(<statement>))
  let handler <parser-error>
    = method (err, next-handler)
        record-error(p, err);
        synchronize(p);
        // Decline to handle the error so that our higher-level handler can
        // decide whether or not to continue.
        next-handler()
      end;
  if (next-token-matches(p, "var"))
    consume-token(p);
    parse-variable-declaration(p)
  else
    parse-statement(p)
  end
end function;

define function parse-variable-declaration (p :: <parser>) => (s :: <variable-declaration>)
  let token = consume-token(p, expect: <identifier-token>);
  let init = if (next-token-matches(p, "="))
               consume-token(p);
               parse-expression(p)
             else
               make(<literal-expression>, value: $nil) // The book uses Java null here.
             end;
  consume-token(p, expect: ";");
  make(<variable-declaration>, name: token, initializer: init)
end function;

define function parse-statement (p :: <parser>) => (s :: <statement>)
  if (next-token-matches(p, "print"))
    parse-print-statement(p)
  else
    parse-expression-statement(p)
  end
end function;

define function parse-print-statement (p :: <parser>) => (s :: <print-statement>)
  consume-token(p, expect: "print");
  let expr = parse-expression(p);
  consume-token(p, expect: ";");
  make(<print-statement>, expression: expr)
end function;

define function parse-expression-statement (p :: <parser>) => (s :: <expression-statement>)
  let expr = parse-expression(p);
  consume-token(p, expect: ";");
  make(<expression-statement>, expression: expr)
end function;

// expression     → assignment ;
define function parse-expression (p :: <parser>) => (e :: <expression>)
  parse-assignment(p)
end function;

// assignment     → IDENTIFIER "=" assignment
//                | equality ;
define function parse-assignment (p :: <parser>) => (e :: <expression>)
  let expr = parse-equality(p);
  if (~next-token-matches(p, "="))
    expr
  else
    let equal = consume-token(p);
    let value-expr = parse-assignment(p);
    if (instance?(expr, <variable-expression>))
      make(<assignment-expression>, name: expr.%name, value: value-expr)
    else
      parser-error(p, "invalid assignment target %=", equal);
    end
  end
end function;

// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
define function parse-equality (p :: <parser>) => (e :: <expression>)
  iterate loop (left = parse-comparison(p))
    if (next-token-matches(p, "!=", "=="))
      let operator = consume-token(p);
      loop(make(<binary-expression>, operator: operator, left: left, right: parse-comparison(p)))
    else
      left
    end
  end iterate
end function;

// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
define function parse-comparison (p :: <parser>) => (e :: <expression>)
  iterate loop (left = parse-term(p))
    if (next-token-matches(p, ">", ">=", "<", "<="))
      let operator = consume-token(p);
      loop(make(<binary-expression>, operator: operator, left: left, right: parse-term(p)))
    else
      left
    end
  end iterate
end function;

// term           → factor ( ( "-" | "+" ) factor )* ;
define function parse-term (p :: <parser>) => (e :: <expression>)
  iterate loop (left = parse-factor(p))
    if (next-token-matches(p, "+", "-"))
      let operator = consume-token(p);
      loop(make(<binary-expression>, operator: operator, left: left, right: parse-factor(p)))
    else
      left
    end
  end iterate
end function;

// factor         → unary ( ( "/" | "*" ) unary )* ;
define function parse-factor (p :: <parser>) => (e :: <expression>)
  iterate loop (left = parse-unary(p))
    if (next-token-matches(p, "*", "/"))
      let operator = consume-token(p);
      loop(make(<binary-expression>, operator: operator, left: left, right: parse-factor(p)))
    else
      left
    end
  end iterate
end function;

// unary          → ( "!" | "-" ) unary
//                | primary ;
define function parse-unary (p :: <parser>) => (e :: <expression>)
  if (next-token-matches(p, "!", "-"))
    make(<unary-expression>, operator: consume-token(p), right: parse-unary(p))
  else
    parse-primary(p)
  end
end function;

// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")"
//                | IDENTIFIER ;
define function parse-primary (p :: <parser>) => (e :: <expression>)
  let token = consume-token(p);
  if (instance?(token, <literal-token>))
    make(<literal-expression>, value: token.%value)
  elseif (instance?(token, <identifier-token>))
    make(<variable-expression>, name: token)
  elseif (token.%text = "(")
    let expr = make(<grouping-expression>, expression: parse-expression(p));
    let tok = consume-token(p, expect: ")");
    expr
  else
    parser-error(p, "expected an expression but got %=", token);
  end
end function;


// Consume tokens until we get to the next statement.
define function synchronize (p :: <parser>) => (token :: <token>)
  // The book checks for SEMICOLON first but I don't see why it's necessary so
  // I'm leaving it out for now. Will it come back to bite me?
  iterate loop (token = peek-token(p))
    select (token.%value)
      #"eof", #"class", #"fun", #"var", #"for", #"if", #"while", #"print", #"return" =>
        token;
      otherwise =>
        consume-token(p);
        loop(peek-token(p))
    end
  end
end function;
