Module: lox-impl


// Run Lox code stored in a file.
define function run-file
    (path :: <string>) => ()
  run(fs/with-open-file (stream = path, direction: #"input")
        io/read-to-end(stream)
      end)
end function;

// Infinite loop reading Lox code and interpreting it.
define function run-prompt
    () => ()
  iterate loop ()
    format-out("> ");
    force-out();
    // Simulate Java's InputStreamReader.readLine by returning #f on end of stream (C-d).
    let line = block ()
                 io/read-line(io/*standard-input*)
               exception (io/<end-of-stream-error>)
                 #f
               end;
    if (line)
      run(line);
      loop();
    end;
  end iterate;
end function;

// Interpret `source` as Lox code.
define function run
    (source :: <string>) => ()
  let scanner = make(<scanner>, text: source);
  for (token in scan-all(scanner))
    format-out("%=\n", token);
  end;
end function;
