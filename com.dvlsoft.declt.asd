;;; com.dvlsoft.declt.asd --- ASDF system definition

;; Copyright (C) 2010, 2011, 2012 Didier Verna.

;; Author:     Didier Verna <didier@lrde.epita.fr>
;; Maintainer: Didier Verna <didier@lrde.epita.fr>

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

;; Contents management by FCM version 0.1.


;;; Code:

(in-package :cl-user)


;; ------------------
;; Package definition
;; ------------------

(defpackage :com.dvlsoft.declt.asdf
  (:documentation
   "The Documentation Extractor from Common Lisp to Texinfo ASDF package.")
  (:use :cl))

(in-package :com.dvlsoft.declt.asdf)


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


;; Configuration

(defvar cl-user::com.dvlsoft.declt.configuration nil
  "The Declt configuration settings.
This variable contains a property list of configuration options.
Current options are:
- :swank-eval-in-emacs (Boolean).")

(defun configuration (key)
  "Return KEY's value in the current Declt configuration."
  (getf cl-user::com.dvlsoft.declt.configuration key))

(defun set-configuration (key value)
  "Set KEY to VALUE in the current Declt configuration."
  (setf (getf cl-user::com.dvlsoft.declt.configuration key) value))

(defsetf configuration set-configuration)


;; Versionning

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defconstant +release-major-level+ 1
  "The major level of this release.")

(defconstant +release-minor-level+ 0
  "The minor level of this release.")

(defconstant +release-status+ :beta
  "The status of this release.")

(defconstant +release-status-level+ 15
  "The status level of this release.")

(define-constant +release-name+ "James T. Kirk"
  "The name of this release.")

;; #### TODO: I'm sure the format strings can be improved
(defun %version (type major minor status level name)
  (ecase type
    (:number
     (apply #'+
       (* major 10000)
       (* minor 100)
       (when (eq status :patchlevel)
	 (list level))))
    (:short
     (format nil "~S.~S~
		 ~[~
		   a~*~S~;~
		   b~*~S~;~
		   rc~*~S~;~
		   ~:[.~S~;~*~]~
		 ~]"
       major
       minor
       (ecase status
	 (:alpha 0)
	 (:beta 1)
	 (:rc 2)
	 (:patchlevel 3))
       (zerop level)
       level))
    (:long
     (format nil "~S.~S ~
		 ~[~
		   alpha ~*~S ~;~
		   beta ~*~S ~;~
		   release candidate ~*~S ~;~
		   ~:[patchlevel ~S ~;~*~]~
		 ~]~
		 ~S"
       major
       minor
       (ecase status
	 (:alpha 0)
	 (:beta 1)
	 (:rc 2)
	 (:patchlevel 3))
       (zerop level)
       level
       name))))

(defun version (&optional (type :number))
  "Return the current version of Declt.
TYPE can be one of :number, :short or :long.

A version number is computed as major*10000 + minor*100 + patchlevel, leaving
two digits for each level. Alpha, beta and rc status are ignored in version
numbers.

A short version is something like 1.3{a,b,rc}4, or 1.3.4 for patchlevel.
Alpha, beta or rc levels start at 1. Patchlevels start at 0 but are ignored
in the output, so that 1.3.0 appears as just 1.3.

A long version is something like
1.3 {alpha,beta,release candidate,patchlevel} 4 \"Release Name\". As for
the short version, a patchlevel of 0 is ignored in the output."
  (%version type +release-major-level+ +release-minor-level+
	    +release-status+ +release-status-level+
	    +release-name+))


;; -----------------
;; System definition
;; -----------------

;; #### PORTME.
(asdf:defsystem :com.dvlsoft.declt
  :description "Documentation Extractor from Common Lisp to Texinfo."
  :long-description
  "Declt (pronounce dec'let) is a reference manual generator for Common Lisp.
It extracts and formats documentation from ASDF systems, including the system
itself and its components, the packages defined in the system and definitions
like constants, special variables, macros, functions, generic functions and
methods, conditions, structures and classes.

Reference manuals are generated in Texinfo format which can be subsequently
converted into info, HTML, DVI, PostScript or PDF. The generated manuals are
fully indexed and provide a complete set of cross-references between
documentation elements. For instance, files and packages point to the
definitions they provide, and those definitions point back to package and file
in which they can be found."
  :author "Didier Verna"
  :mailto "didier@lrde.epita.fr"
  :homepage
  "http://www.lrde.epita.fr/~didier/software/lisp/misc.php#declt"
  :source-control
  "http://www.lrde.epita.fr/~didier/software/lisp/declt/declt.git"
  :license "GNU GPL"
  :version #.(version :short)
  :depends-on (:sb-introspect)
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


;;; com.dvlsoft.declt.asd ends here
