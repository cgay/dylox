Module: lox-impl


// Main scanner entry point.
define generic scan (source :: <object>) => (tokens :: <sequence>);

define method scan (scanner :: <scanner>) => (tokens :: <sequence>)
  scan-tokens(scanner)
end method;

define method scan (source :: <string>) => (tokens :: <sequence>)
  scan(make(<scanner>, source: source))
end method;

define method scan (file :: <file-locator>) => (tokens :: <sequence>)
  let source = fs/with-open-file(stream = file)
                 io/read-to-end(stream)
               end;
  scan(make(<scanner>, source: source, file: file))
end method;


define constant $reserved-words
  = #["and", "class", "else", "false", "for", "fun", "if", "nil", "or",
      "print", "return", "super", "this", "true", "var", "while"];

define class <scanner> (<object>)
  constant slot %source :: <string>, required-init-keyword: source:;
  constant slot %file :: false-or(<file-locator>) = #f, init-keyword: file:;
  slot %line :: <integer> = 1;
  slot %token-start :: <integer> = 0; // book calls this start
  slot %index :: <integer> = 0;       // book calls this current
end class;

define class <scanner-error> (<lox-error>)
  constant slot %scanner :: <scanner>, required-init-keyword: scanner:;
end class;

define method condition-to-string (err :: <scanner-error>) => (s :: <string>)
  let scanner = err.%scanner;
  let fmt = concatenate("%s:%d: ", err.condition-format-string);
  let args = concatenate(list(scanner.%file | "<stdin>", scanner.%line),
                         err.condition-format-arguments);
  apply(io/format-to-string, fmt, args)
end method;

define function scanner-error (s :: <scanner>, fmt :: <string>, #rest args) => (false == #f)
  signal(make(<scanner-error>,
              scanner: s,       // a bit lazy, but gives access to all the error message bits
              format-string: fmt,
              format-arguments: args));
  #f  // in case signal returns
end function;

define function peek (scanner :: <scanner>) => (c :: false-or(<character>))
  let source = scanner.%source;
  let index = scanner.%index;
  if (index < source.size)
    source[index]
  end
end function;

define function peek-next (scanner :: <scanner>) => (c :: false-or(<character>))
  let source = scanner.%source;
  let index = scanner.%index;
  if (index < source.size - 1)
    source[index + 1]
  end
end function;

define method consume (scanner :: <scanner>) => (c :: <character>)
  let c = peek(scanner)
    | scanner-error(scanner, "unexpected end of file");
  inc!(scanner.%index);
  c
end method;

define function match-any
    (s :: <scanner>, #rest chars :: <character>) => (c :: false-or(<character>))
  let ch = peek(s);
  if (member?(ch, chars))
    consume(s)
  end
end function;

define function current-text (s :: <scanner>) => (s :: <string>)
  copy-sequence(s.%source, start: s.%token-start, end: min(s.%index, s.%source.size))
end function;

define class <token> (<object>)
  constant slot %text  :: <string>,  required-init-keyword: text:;
  constant slot %line  :: <integer>, required-init-keyword: line:;
  // %value is a symbol for most reserved words, all operator, and all punctuation.
  // For other token classes see scan-token, below.
  constant slot %value :: <object>, required-init-keyword: value:;
end class;

define class <punctuation-token>   (<token>) end; // single char, punctuation, {}[]
define class <operator-token>      (<token>) end;
define class <identifier-token>    (<token>) end;
define class <reserved-word-token> (<identifier-token>) end;
define class <literal-token>       (<token>) end;
define class <number-token>        (<literal-token>) end;
define class <string-token>        (<literal-token>) end;
define class <boolean-token>       (<reserved-word-token>, <literal-token>) end;
define class <comment-token>       (<token>) end;
define class <eof-token>           (<token>) end;

define method io/print-object (token :: <token>, stream :: <stream>) => ()
  io/printing-object (token, stream)
    io/format(stream, "%s %= line:%d", token.%text, token.%value, token.%line);
  end;
end method;

define function scan-tokens (scanner :: <scanner>) => (tokens :: <sequence>)
  let tokens = make(<stretchy-vector>);
  iterate loop ()
    let token = scan-token(scanner);
    add!(tokens, token);
    if (~instance?(token, <eof-token>))
      loop()
    end;
  end;
  tokens
end function;

define function scan-token (scanner :: <scanner>) => (t :: <token>)
  local
    // Make a token for the text between %token-start and %index.
    method token (class, #key value = unsupplied()) => (t :: <token>)
      let text = current-text(scanner);
      //io/format-out("text: %=, start: %=, index: %=\n", text, scanner.%token-start, scanner.%index);
      //io/force-out();
      scanner.%token-start := scanner.%index;
      make(class,
           text: text,
           line: scanner.%line,
           value: iff(unsupplied?(value),
                      as(<symbol>, text),
                      value))
    end;
  iterate loop (char = peek(scanner))
    if (~char)
      token(<eof-token>, value: #"eof")
    else
      consume(scanner);
      select (char)
        '(', ')', '{', '}', ',', '.', '-', '+', ';', '*' =>
          token(<punctuation-token>);
        '!', '=', '<', '>' =>
          match-any(scanner, '=');
          token(<operator-token>);
        ' ', '\r', '\t' =>
          inc!(scanner.%token-start);
          loop(peek(scanner));
        '\n' =>
          inc!(scanner.%line);
          inc!(scanner.%token-start);
          loop(peek(scanner));
        '/' =>
          if (match-any(scanner, '/'))
            // Drop end of line comments.
            until ((peek(scanner) | '\n') == '\n')
              consume(scanner);
            end;
            token(<comment-token>,
                  value: copy-sequence(scanner.%source,
                                       start: scanner.%token-start + 2,
                                       end: scanner.%index))
          else
            token(<punctuation-token>)
          end;
        '"' =>
          token(<string-token>, value: scan-string(scanner));
        otherwise =>
          if (decimal-digit?(char))
            token(<number-token>, value: scan-number(scanner))
          elseif (alphabetic?(char) | char == '_')
            let text = scan-identifier(scanner);
            let true? = text = "true";
            iff(true? | text = "false",
                token(<boolean-token>, value: true?),
                token(iff(member?(text, $reserved-words, test: \=),
                          <reserved-word-token>,
                          <identifier-token>)))
          else
            scanner-error(scanner, "unexpected character %=", char);
            token(<eof-token>, value: #"eof");
          end;
      end select
    end if
  end iterate
end function scan-token;

define function scan-string (s :: <scanner>) => (s :: <string>)
  // The open " has already been consumed.
  iterate loop (c = peek(s))
    select (c)
      #f =>
        scanner-error(s, "unterminated string literal");
      '"' =>
        consume(s);
        copy-sequence(s.%source, start: s.%token-start + 1, end: s.%index - 1);
      otherwise =>
        consume(s);
        loop(peek(s));
    end
  end
end function;

define function scan-number (s :: <scanner>) => (n :: <double-float>)
  while (decimal-digit?(peek(s) | 'x'))
    consume(s);
  end;
  if (peek(s) == '.')
    if (~decimal-digit?(peek-next(s) | 'x'))
      scanner-error(s, "trailing dot not allowed in number");
    end;
    consume(s);   // '.'
    while (decimal-digit?(peek(s) | 'x'))
      consume(s)
    end;
  end;
  string-to-float(current-text(s))
end function;

define function scan-identifier (s :: <scanner>) => (id :: <string>)
  while (peek(s) == '_' | alphanumeric?(peek(s) | '*'))
    consume(s);
  end;
  current-text(s)
end function;
