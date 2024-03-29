;;; net.didierverna.declt.asd --- Declt ASDF system definition

;; Copyright (C) 2010-2013, 2015, 2022 Didier Verna

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

(asdf:load-system :net.didierverna.declt.setup)

;; #### PORTME.
(asdf:defsystem :net.didierverna.declt
  :long-name "Documentation Extractor from Common Lisp to Texinfo"
  :description "A reference manual generator for Common Lisp libraries"
  :long-description "\
Declt (pronounce dec'let) is a reference manual generator for Common Lisp.
It extracts and formats documentation from ASDF systems, including the system
itself, its local dependencies (subsystems), components, packages and an
extensive list of definitions (variables, functions etc.). The formatted
documentation comes with full indexing and cross-references.

Reference manuals are generated in Texinfo format which can subsequently be
converted into info, HTML, DVI, PostScript or PDF."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage
  "http://www.lrde.epita.fr/~didier/software/lisp/typesetting.php#declt"
  :source-control "https://github.com/didierverna/declt"
  :license "BSD"
  :version #.(net.didierverna.declt.setup:version :short)
  :if-feature :sbcl
  :depends-on (:net.didierverna.declt.setup :net.didierverna.declt.core))

;;; net.didierverna.declt.asd ends here
