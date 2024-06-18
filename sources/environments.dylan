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
  if (has-variable?(env, name))
    runtime-error(ev, "variable already exists: '%s'", name);
  else
    env.%values[name] := value
  end
end method;

// Note for now we have no way to set variables in a containing scope, due to
// the way has-variable? is defined. We'll need to change that to return the
// environment in which it is defined, or #f, instead.

define method set-variable
    (ev :: <evaluator>, env :: <environment>, name :: <symbol>, value) => (value)
  if (has-variable?(env, name))
    env.%values[name] := value
  else
    error("attempt to assign undefined variable '%s'", name)
  end
end method;

// Only check the immediate scope, for now.
define function has-variable?
    (env :: <environment>, name :: <symbol>) => (b :: <boolean>)
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
