# Declt
Declt (pronounce dec'let) is a reference manual generator for Common Lisp.
It extracts and formats documentation from ASDF systems, including the system
itself, its local dependencies (subsystems), components, packages and an
extensive list of definitions (variables, functions etc.). The formatted
documentation comes with full indexing and cross-references.

Reference manuals are generated in Texinfo, which can subsequently be
converted into info, HTML, DVI, PostScript or PDF.

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
The `DECLT` function accepts a number of keyword parameters for tweaking the
output. Please refer to the docstring for more information.

A [typical example](https://www.lrde.epita.fr/~didier/software/lisp/declt/reference/) of a Declt-generated reference manual is that of Declt itself.

Declt also comes with a user manual. Please see the projet's [homepage](https://www.lrde.epita.fr/~didier/software/lisp/misc.php#declt).
