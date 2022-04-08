;;; net.didierverna.declt.assess.asd --- Declt assessment library, ASDF system

;; Copyright (C) 2021, 2022 Didier Verna

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



;;; Code:

;; #### PORTME.
(defsystem :net.didierverna.declt.assess
  :long-name
  "Documentation Extractor from Common Lisp to Texinfo, assessment library"
  :description
  "Declt library for extracting information from ASDF systems"
  :long-description
  "The Declt library that collects information from ASDF systems by
introspection, and produces an abstract representation, independent from both
the final manual's organization and the output format. For a more complete
description of Declt, see the `net.didierverna.declt' system."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/misc.php#declt"
  :source-control "https://github.com/didierverna/declt"
  :license "BSD"
   :version (:read-file-line #p"../make/version.make"
	     :at (1 (lambda (str) (subseq str 19))))
  :if-feature :sbcl
  :depends-on ((:feature :sbcl (:require :sb-introspect))
	       :net.didierverna.declt.setup)
  :serial t
  :components ((:file "package")
	       (:module "src"
		:serial t ;; not really, but who cares
		:components ((:file "util")
			     (:file "definition")
			     (:file "license")
			     (:file "symbol")
			     (:file "package")
			     (:file "asdf")
			     (:file "finalize")
			     (:file "assess")))))

;;; net.didierverna.declt.assess.asd ends here
