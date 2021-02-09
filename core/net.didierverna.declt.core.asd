;;; net.didierverna.declt.core.asd --- ASDF system definition, core library

;; Copyright (C) 2015 Didier Verna

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

(asdf:load-system :net.didierverna.declt.setup)

;; #### PORTME.
(asdf:defsystem :net.didierverna.declt.core
  :long-name
  "Documentation Extractor from Common Lisp to Texinfo, core library"
  :description "A reference manual generator for Common Lisp libraries"
  :long-description "\
Declt's core functionality. For a more complete description of Declt, see the
net.didierverna.declt system."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/misc.php#declt"
  :source-control "https://github.com/didierverna/declt"
  :license "BSD"
  :version #.(net.didierverna.declt.setup:version :short)
  :if-feature :sbcl
  :depends-on (:net.didierverna.declt.setup
	       (:feature :sbcl :sb-introspect))
  :serial t
  :components ((:file "quickutil")
	       (:file "meta")
	       (:module "src"
		:serial t
		:components ((:module "util"
			      :serial t
			      :components ((:file "misc")))
			     (:module "extract"
			      :serial t
			      :components ((:file "util")
					   (:file "definition")
					   (:file "license")
					   (:file "symbol")
					   (:file "package")
					   (:file "asdf")
					   (:file "finalize")
					   (:file "extract")))
			     (:module "doc"
			      :serial t
			      :components ((:file "texi")
					   (:file "doc")
					   (:file "symbol")
					   (:file "package")
					   (:file "asdf")))
			     (:file "declt")))))

;;; net.didierverna.declt.core.asd ends here
