Module: lox-impl

// All errors explicitly signalled in Lox are subclasses of this.
define class <lox-error> (<simple-error>) end;

// I shouldn't need this method.  https://github.com/dylan-lang/opendylan/issues/1429
define method io/print-message (cond :: <lox-error>, stream :: <stream>) => ()
  apply(io/format, stream,
        cond.condition-format-string,
        cond.condition-format-arguments);
  io/force-output(stream);
end method;
