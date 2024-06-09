Module: lox-impl

// Recursive descent parser for Lox grammar.

// Main parsing entry point.

// TODO: I don't like returning errors as a second value. Use
// parse(make(<parser>, source: ...)) instead?
define generic parse (source :: <object>) => (statements :: <sequence>, errors :: <sequence>);

define method parse (scanner :: <scanner>) => (statements :: <sequence>, errors :: <sequence>)
  let parser = make(<parser>, tokens: scan(scanner));
  let statements = make(<stretchy-vector>);
  iterate loop ()
    let token = peek-token(parser);
    unless (instance?(token, <eof-token>))
      block ()
        let statement = parse-statement(parser);
        add!(statements, statement);
      exception (ex :: <parser-error>)
        record-error(parser, ex);
        // TODO: synchronize(parser);
      end;
      loop();
    end;
  end;
  values(statements, parser.%errors)
end method;

define method parse (source :: <string>) => (ast :: <sequence>, errors :: <sequence>)
  parse(make(<scanner>, source: source))
end method;

define method parse (file :: <file-locator>) => (ast :: <sequence>, errors :: <sequence>)
  fs/with-open-file (stream = file)
    parse(make(<scanner>,
               source: io/read-to-end(stream),
               file: file))
  end
end method;

define method parse (source :: <stream>) => (ast :: <sequence>, errors :: <sequence>)
  parse(io/read-to-end(source))
end method;



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
  constant slot parser-tokens :: <sequence>, required-init-keyword: tokens:;
           slot parser-index :: <integer> = 0;
  constant slot %errors :: <sequence> = make(<stretchy-vector>);
end class;

define function peek-token (p :: <parser>) => (t :: <token>)
  let tokens = p.parser-tokens;
  let index = p.parser-index;
  if (index >= tokens.size)
    tokens.last                 // always EOF
  else
    tokens[index]
  end
end function;

define function consume-token (p :: <parser>, #key expect) => (t :: <token>)
  let token = peek-token(p);
  if (~instance?(token, <eof-token>))
    inc!(p.parser-index);
  end;
  if (expect & expect ~= token.%text)
    parser-error(p, "expected %=", expect)
  end;
  token
end function;

// Return true if any of the tokens match the next token.
define function next-token-matches (p :: <parser>, #rest strings) => (matched? :: <boolean>)
  member?(p.peek-token.%text, strings, test: \=)
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

// expression     → equality ;
define function parse-expression (p :: <parser>) => (e :: <expression>)
  parse-equality(p)
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
//                | "(" expression ")" ;
define function parse-primary (p :: <parser>) => (e :: <expression>)
  let token = consume-token(p);
  if (instance?(token, <literal-token>))
    make(<literal-expression>, value: token.%value)
  elseif (token.%text = "(")
    let expr = make(<grouping-expression>, value: parse-expression(p));
    let tok = consume-token(p);
    if (tok.%text ~= ")")
      parser-error(p, "expected close paren, got %=", token);
    else
      expr
    end
  else
    parser-error(p, "expected an expression but got %=", token);
  end
end function;


// Consume tokens until we get to the next statement.
/* not yet:
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
*/
