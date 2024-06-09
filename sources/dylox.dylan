Module: dylox

define function main
    (name :: <string>, arguments :: <vector>) => ()
  let ev = make(<evaluator>);
  select (arguments.size)
    0 => repl(ev);
    1 => printf("%s\n", eval(ev, as(<file-locator>, arguments.first)));
    otherwise =>
      io/format-err("Usage: %s [script-file]\n", name);
      exit-application(64);
  end;
  exit-application(if (ev.had-errors?) 1 else 0 end);
end function;

define function repl (ev :: <evaluator>) => ()
  iterate loop ()
    printf("> ");
    // Simulate Java's InputStreamReader.readLine by returning #f on end of stream (C-d).
    let line = io/read-line(*standard-input*, on-end-of-stream: #f);
    if (line)
      let value = eval(ev, line);
      if (supplied?(value))
        printf("=> %s\n", value);
      end;
      loop();
    end;
  end;
end function;

define function printf (fmt :: <string>, #rest args)
  apply(io/format-out, fmt, args);
  io/force-out();
end function;

main(application-name(), application-arguments());
