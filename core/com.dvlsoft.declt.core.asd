;;; com.dvlsoft.declt.core.asd --- ASDF system definition, core library

;; Copyright (C) 2015 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Declt.

;; Declt is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3,
;; as published by the Free Software Foundation.

;; Declt is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:



;;; Code:

;; --------------------
;; Very early utilities
;; --------------------

;; Quickutil
(defun generate-quickutils ()
  "Generate the offline quickutil file."
  (funcall (intern "SAVE-UTILS-AS" :quickutil-client)
	   (merge-pathnames (make-pathname :name "quickutil" :type "lisp")
			    (asdf:system-source-directory :com.dvlsoft.declt))
	   :when-let))


;; -----------------
;; System definition
;; -----------------

(asdf:load-system :com.dvlsoft.declt.setup)

;; #### PORTME.
(asdf:defsystem :com.dvlsoft.declt.core
  :long-name
  "Documentation Extractor from Common Lisp to Texinfo, core library"
  :description "A reference manual generator for Common Lisp libraries"
  :long-description "\
Declt's core functionality. For a more complete description of Declt, see the
com.dvlsoft.declt system."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/misc.php#declt"
  :source-control "https://github.com/didierverna/declt"
  :license "GNU GPL"
  :version #.(com.dvlsoft.declt.setup:version :short)
  :depends-on (:com.dvlsoft.declt.setup :sb-introspect)
  :serial t
  :components ((:file "quickutil")
	       (:file "package")
	       (:module "src"
		:serial t
		:components ((:module "util"
			      :serial t
			      :components ((:file "misc")
					   (:file "asdf")
					   (:file "texi")))
			     (:module "item"
			      :serial t
			      :components ((:file "item")
					   (:file "symbol")
					   (:file "package")
					   (:file "asdf")))
			     (:module "doc"
			      :serial t
			      :components ((:file "doc")
					   (:file "symbol")
					   (:file "package")
					   (:file "asdf")))
			     (:file "declt")))))


;;; com.dvlsoft.declt.core.asd ends here
