Module: dylox

define function main
    (name :: <string>, arguments :: <vector>) => ()
  select (arguments.size)
    0 => run-prompt();
    1 => run-file(arguments.first);
    otherwise =>
      format-err("Usage: %s [script-file]\n", name);
      exit-application(64);
  end;
  exit-application(0);
end function;

main(application-name(), application-arguments());
