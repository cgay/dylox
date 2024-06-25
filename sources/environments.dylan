Module: lox-impl


define abstract class <environment> (<object>)
  constant slot %values = make(<table>); // <symbol> => <object>
end class;

define class <global-environment> (<environment>)
end class;

define class <lexical-environment> (<environment>)
  constant slot %parent :: <environment>, required-init-keyword: parent:;
end class;

define method create-variable
    (ev :: <evaluator>, env :: <environment>, name :: <symbol>, value) => (value)
  if (locally-bound?(env, name))
    runtime-error(ev, "variable already exists: %=", as(<string>, name));
  else
    env.%values[name] := value
  end
end method;

define method set-variable
    (ev :: <evaluator>, env :: <lexical-environment>, name :: <symbol>, value) => (value)
  if (locally-bound?(env, name))
    env.%values[name] := value
  else
    set-variable(ev, env.%parent, name, value)
  end
end method;

define method set-variable
    (ev :: <evaluator>, env :: <global-environment>, name :: <symbol>, value) => (value)
  if (locally-bound?(env, name))
    env.%values[name] := value
  else
    runtime-error(ev, "attempt to assign undefined variable %=", as(<string>, name));
  end
end method;

define inline function locally-bound?
    (env :: <environment>, name :: <symbol>) => (bound? :: <boolean>)
  supplied?(element(env.%values, name, default: unsupplied()))
end function;

define method get-variable
    (ev :: <evaluator>, env :: <environment>, name :: <symbol>) => (value)
  element(env.%values, name, default: unsupplied()) // #f is false in Lox
end method;

define method get-variable
    (ev :: <evaluator>, env :: <global-environment>, name :: <symbol>) => (value)
  let value = next-method();
  if (unsupplied?(value))
    runtime-error(ev, "unbound variable '%s'", name);
  else
    value
  end
end method;

define method get-variable
    (ev :: <evaluator>, env :: <lexical-environment>, name :: <symbol>) => (value)
  let value = next-method();
  if (unsupplied?(value))
    get-variable(ev, env.%parent, name)
  else
    value
  end
end method;
