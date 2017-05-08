Compiled templates
------------------
If you ever had to generate much IL, you are probably good and tired of the endless
`il.Emit()`, `il.Create()` and `il.InsertBefore()`, not to mention `MakeGenericInstance()`.
Emitting IL directly is very powerful, but being a low-level approach, it suffers
from the usual problems of low-level approaches:

1) syntactic bloat: difficult to see at a glance what the emitted code is doing
2) you have to do the compiler's work in importing members and constructing generics by hand
3) impossible to step through the emitted code and run code coverage on it

Compiled templates solve all these problems. Instead of a rat's nest of opcodes,
the compiled templates library generates your emitted code from normal C# code
decorated with some attributes. Emitted code retains the template's debug information,
which means you can step through it and obtain useful code coverage.
In slightly more complicated cases, you can pick and choose which parts of the template
to apply, splice parts of the template into existing methods, or construct methods
from parts.