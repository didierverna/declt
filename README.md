# Declt
![Declt Logo](doc/logo.png "Generating Common Lisp documentation since 2010")
Declt (pronounce "dec' let") is a reference manual generator for Common Lisp
libraries. A Declt manual documents one specified ASDF system (considered as
the "main" system), and all its local dependencies (subsystems found in the
same distribution). This is what is collectively referred to as the *library*.

Declt doesn't perform any kind of static code analysis, but instead loads the
library, and then introspects the Lisp environment to discover what "belongs"
to it. The generated documentation includes the description of both
programmatic and ASDF components. Every such component description is called a
*definition*.

Declt manuals provide a detailed description of the library's infrastructure
by including definitions for every relevant ASDF component (systems, modules,
and files), and Lisp package.

Exported programmatic definitions are split from the internal ones, which
allows to separately browse either the library's public interface or its
implementation. Both sections of the manual include definitions for constants,
special variables, symbol macros, macros, setf expanders, compiler macros,
regular functions (including setf ones), generic functions and methods
(including setf ones), method combinations, conditions, structures, classes,
and types.

Programmatic definitions are as complete and exhaustive as introspection can
make them. Declt collects documentation strings, lambda lists (including
qualifiers and specializers where appropriate), slot definitions (including
type information, allocation type, initialization arguments, *etc.*),
definition sources, *etc.*

Every definition includes a full set of cross-references to related ones: ASDF
component dependencies, parents, and children, classes direct methods, super-
and sub-classes, slot readers and writers, setf expanders access and update
functions, *etc.*

Finally, Declt produces exhaustive and multiple-entry indexes to all
documented aspects of the library.

Declt manuals are generated in Texinfo format. From there it is possible to
produce readable / printable output in Info, HTML, PDF, PostScript, *etc.*

The primary example of documentation generated by Declt is the Declt
[Reference Manual](https://www.lrde.epita.fr/~didier/software/lisp/declt/reference/).

## Quick Start
In your favorite Lisp REPL, type this:
```
(asdf:load-system :net.didierverna.declt)
(net.didierverna.declt:nickname-package)
(declt:declt :my.asdf.system)
```
You will end up with a file named `my.asdf.system.texi` in the current
directory. This is a Texinfo file that can further be compiled into various
formats, such as Info, HTML, or PDF. For example, in order to get a PDF, type
this in the same directory:
```
makeinfo --pdf my.asdf.system.texi
```
And enjoy reading the resulting `my.asdf.system.pdf`...

## More information
Please see the projet's
[homepage](https://www.lrde.epita.fr/~didier/software/lisp/misc.php#declt),
and notably the Declt
[User Manual](https://www.lrde.epita.fr/~didier/software/lisp/declt/user/)
