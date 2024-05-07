Module: lox-impl

// All errors explicitly signalled in Lox are subclasses of this.
define class <lox-error> (<error>, <format-string-condition>) end;
