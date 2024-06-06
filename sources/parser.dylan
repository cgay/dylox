Module: lox-impl

// Recursive descent parser for Lox grammar.

define class <parser-error> (<lox-error>) end;

define function parser-error (p :: <parser>, fmt :: <string>, #rest args)
  let format-string = concatenate("parse error at token %s: ", fmt);
  // We assume the token hasn't been consumed...reasonable?
  let format-args = concatenate(list(peek-token(p)), args);
  signal(make(<parser-error>,
              format-string: format-string,
              format-arguments: format-args))
end function;

define class <parser> (<object>)
  constant slot parser-tokens :: <sequence>, required-init-keyword: tokens:;
           slot parser-index :: <integer> = 0;
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

define function consume-token (p :: <parser>) => (t :: <token>)
  let t = peek-token(p);
  inc!(p.parser-index);
  t
end function;

// Return true if any of the tokens match the next token.
define function next-token-matches (p :: <parser>, #rest strings) => (matched? :: <boolean>)
  member?(p.peek-token.%text, strings, test: \=)
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
    parser-error(p, "expected true, false, nil, a number, a string, or '(', got %=", token);
  end
end function;

// TODO: continue at 6.3.1 Panic mode error recovery
