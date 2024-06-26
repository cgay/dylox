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
      if (statement)
        add!(statements, statement);
      end;
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
        // decide whether or not to continue (by returning #f).
        next-handler()
      end;
  if (next-token-matches(p, "var"))
    parse-variable-declaration(p)
  else
    parse-statement(p)
  end
end function;

define function parse-variable-declaration (p :: <parser>) => (s :: <variable-declaration>)
  consume-token(p, expect: "var");
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

// statement      → exprStmt
//                | forStmt
//                | ifStmt
//                | printStmt
//                | whileStmt
//                | block ;
define function parse-statement (p :: <parser>) => (s :: <statement>)
  case
    next-token-matches(p, "{")     => parse-block(p);
    next-token-matches(p, "for")   => parse-for-statement(p);
    next-token-matches(p, "if")    => parse-if-statement(p);
    next-token-matches(p, "print") => parse-print-statement(p);
    next-token-matches(p, "while") => parse-while-statement(p);
    otherwise                      => parse-expression-statement(p);
  end
end function;

// forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
//                  expression? ";"
//                  expression? ")" statement ;
define function parse-for-statement (p :: <parser>) => (s :: <statement>)
  consume-token(p, expect: "for");
  consume-token(p, expect: "(");
  let initializer = select (p.peek-token.%value)
                      #";" => #f;
                      #"var" => parse-variable-declaration(p);
                      otherwise => parse-expression-statement(p);
                    end;
  let condition = iff(p.peek-token.%value == #";",
                      make(<literal-expression>, value: #t),
                      parse-expression(p));
  consume-token(p, expect: ";");
  let step = if (p.peek-token.%value ~== #")")
               parse-expression(p)
             end;
  consume-token(p, expect: ")");
  let body = parse-statement(p);
  if (step)
    body := make(<block>,
                 statements: list(body, make(<expression-statement>, expression: step)));
  end;
  body := make(<while-statement>,
               test: condition,
               body: body);
  if (initializer)
    body := make(<block>, statements: list(initializer, body));
  end;
  body
end function;

// whileStmt      → "while" "(" expression ")" statement ;
define function parse-while-statement (p :: <parser>) => (s :: <while-statement>)
  consume-token(p, expect: "while");
  consume-token(p, expect: "(");
  let test-expr = parse-expression(p);
  consume-token(p, expect: ")");
  make(<while-statement>,
       test: test-expr,
       body: parse-statement(p))
end function;

// ifStmt         → "if" "(" expression ")" statement
//                ( "else" statement )? ;
define function parse-if-statement (p :: <parser>) => (s :: <if-statement>)
  consume-token(p, expect: "if");
  consume-token(p, expect: "(");
  let test-expr = parse-expression(p);
  consume-token(p, expect: ")");
  make(<if-statement>,
       test: test-expr,
       then: parse-statement(p),
       else: if (peek-token(p).%value == #"else")
               consume-token(p);
               parse-statement(p)
             end)
end function;

define function parse-print-statement (p :: <parser>) => (s :: <print-statement>)
  consume-token(p, expect: "print");
  let expr = parse-expression(p);
  consume-token(p, expect: ";");
  make(<print-statement>, expression: expr)
end function;

// block          → "{" declaration* "}" ;
define function parse-block (p :: <parser>) => (b :: <block>)
  consume-token(p, expect: "{");
  let statements = make(<stretchy-vector>);
  iterate loop ()
    let token = peek-token(p);
    if (instance?(token, <eof-token>))
      // Explicitly checking for EOF hopefully gives a friendlier error message.
      parser-error(p, "end of input while parsing a block");
    elseif (token.%value ~== #"}")
      let statement = parse-declaration(p); // #f if synchronize called
      if (statement)
        add!(statements, statement);
      end;
      loop();
    end;
  end;
  consume-token(p, expect: "}");
  make(<block>, statements: statements)
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
//                | logic_or ;
define function parse-assignment (p :: <parser>) => (e :: <expression>)
  let expr = parse-logical-or(p);
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

// logic_or       → logic_and ( "or" logic_and )* ;
define function parse-logical-or (p :: <parser>) => (e :: <expression>)
  let expr = parse-logical-and(p);
  iterate loop ()
    if (peek-token(p).%value == #"or")
      let op = consume-token(p);
      expr := make(<logical-expression>,
                   operator: op,
                   left: expr,
                   right: parse-logical-and(p));
      loop();
    end;
  end iterate;
  expr
end function;

// logic_and      → equality ( "and" equality )* ;
define function parse-logical-and (p :: <parser>) => (e :: <expression>)
  let expr = parse-equality(p);
  iterate loop ()
    if (peek-token(p).%value == #"and")
      let op = consume-token(p);
      expr := make(<logical-expression>,
                   operator: op,
                   left: expr,
                   right: parse-equality(p));
      loop();
    end;
  end iterate;
  expr
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
