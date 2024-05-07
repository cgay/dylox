Module: dylox

define function main
    (name :: <string>, arguments :: <vector>) => ()
  block ()
    select (arguments.size)
      0 => run-prompt();
      1 => run-file(arguments.first);
      otherwise =>
        io/format-err("Usage: %s [script-file]\n", name);
        exit-application(64);
    end;
  exception (err :: <error>)
    io/format-err("Error: %s\n", err);
    exit-application(1);
    end;
  exit-application(0);
end function;

main(application-name(), application-arguments());
