Module: lox-impl


// Run Lox code stored in a file.
define function run-file
    (path :: <string>) => (errors :: <sequence>)
  run(as(<file-locator>, path))
end function;

// Infinite loop reading Lox code and interpreting it.
define function run-prompt
    () => (errors :: <sequence>)
  iterate loop (errors = make(<stretchy-vector>))
    io/format-out("> ");
    io/force-out();
    // Simulate Java's InputStreamReader.readLine by returning #f on end of stream (C-d).
    let line = io/read-line(*standard-input*, on-end-of-stream: #f);
    if (line)
      loop(concatenate(errors, run(line)))
    else
      errors
    end
  end iterate
end function;

// Interpret `source` as Lox code.
define function run (source) => (errors :: <sequence>)
  let (ast, errors) = parse(source);
  for (error in errors, i from 1)
    io/format-err("Error (%d): %s\n", i, error);
    io/force-err();
  end;
  if (ast)
    io/format-out("%=\n", ast.s-expression);
    io/force-out();
  end;
  errors
end function;
