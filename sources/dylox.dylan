Module: dylox

define function main
    (name :: <string>, arguments :: <vector>) => ()
  let errors
    = select (arguments.size)
        0 => run-prompt();
        1 => run-file(arguments.first);
        otherwise =>
          io/format-err("Usage: %s [script-file]\n", name);
          exit-application(64);
      end;
  exit-application(if (errors.empty?) 0 else 1 end);
end function;

main(application-name(), application-arguments());
