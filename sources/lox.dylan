Module: lox-impl


// Run Lox code stored in a file.
define function run-file
    (path :: <string>) => ()
  run(make(<scanner>,
           file: path,
           source: read-file(path)));
end function;

define function read-file (path :: <string>) => (contents :: <string>)
  fs/with-open-file (stream = path, direction: #"input")
    io/read-to-end(stream)
  end
end function;

// Infinite loop reading Lox code and interpreting it.
define function run-prompt
    () => ()
  iterate loop ()
    io/format-out("> ");
    io/force-out();
    // Simulate Java's InputStreamReader.readLine by returning #f on end of stream (C-d).
    let line = block ()
                 io/read-line(*standard-input*)
               exception (io/<end-of-stream-error>)
                 #f
               end;
    if (line)
      run(make(<scanner>, source: line));
      loop();
    end;
  end iterate;
end function;

// Interpret `source` as Lox code.
define function run
    (scanner :: <scanner>) => ()
  let error-count = 0;
  let handler <lox-error>
    = method (err, next-handler :: <function>)
        inc!(error-count);
        io/format-err("Error on line %d: %s (ignored)\n",
                      scanner.%line, err);
        io/force-err();
        #f
      end;
  let tokens = scan-tokens(scanner);
  for (token in tokens)
    io/format-out("%=\n", token);
  end;
  let parser = make(<parser>, tokens: tokens);
  let expr = parse-expression(parser);
  if (expr)
    io/format-out("%=\n", expr.s-expression);
    io/force-out();
  end;
  if (error-count > 0)
    signal(make(<lox-error>,
                format-string: "Found %d syntax errors",
                format-arguments: list(error-count)));
  end;
end function;
