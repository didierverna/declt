;;; com.dvlsoft.declt.asd --- ASDF system definition

;; Copyright (C) 2010-2013, 2015 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Declt.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(asdf:load-system :com.dvlsoft.declt.setup)

(asdf:defsystem :com.dvlsoft.declt
  :long-name "Documentation Extractor from Common Lisp to Texinfo"
  :description "A reference manual generator for Common Lisp libraries"
  :long-description "\
Declt (pronounce dec'let) is a reference manual generator for Common Lisp.
It extracts and formats documentation from ASDF systems, including the system
itself, its components, the packages defined in the system and an extensive
list of definitions (variables, functions etc.). The formatted documentation
comes with full indexing and cross-references.

Reference manuals are generated in Texinfo format which can subsequently be
converted into info, HTML, DVI, PostScript or PDF."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/misc.php#declt"
  :source-control "https://github.com/didierverna/declt"
  :license "BSD"
  :version #.(com.dvlsoft.declt.setup:version :short)
  :depends-on (:com.dvlsoft.declt.setup :com.dvlsoft.declt.core))

;;; com.dvlsoft.declt.asd ends here
