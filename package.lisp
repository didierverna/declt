;;; package.lisp --- Common Lisp package definition

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

(defpackage :com.dvlsoft.declt
  (:documentation
   "The Documentation Extractor from Common Lisp to Texinfo package.")
  (:use :cl)
  (:shadow :*readtable*)
  (:import-from :com.dvlsoft.declt.asdf
    :configuration
    :define-constant
    :+release-major-level+
    :+release-minor-level+
    :+release-status+
    :+release-status-level+
    :+release-name+
    :version)
  (:import-from :asdf
    ;; Some functions or slot-unbound-proof accessors that we can import
    ;; directly (see src/util/asdf.lisp for wrappers around other ones).
    :component-name
    :component-load-dependencies
    :component-parent
    :component-pathname
    :component-relative-pathname
    :find-system
    :system-source-directory
    :system-source-file)
  (:export
   ;; From com.dvlsoft.declt.asd:
   :+release-major-level+
   :+release-minor-level+
   :+release-status+
   :+release-status-level+
   :+release-name+
   :version
   ;; From package.lisp:
   :nickname-package
   ;; From src/declt.lisp:
   :declt))


(in-package :com.dvlsoft.declt)


;; -------------------
;; External utilities:
;; -------------------

(defun nickname-package (&optional (nickname :declt))
  "Add NICKNAME (:DECLT by default) to the :COM.DVLSOFT.DECLT package."
  (rename-package :com.dvlsoft.declt
		  (package-name :com.dvlsoft.declt)
		  (adjoin nickname (package-nicknames :com.dvlsoft.declt)
			  :test #'string-equal)))


;; -------------------
;; Internal utilities:
;; -------------------

(defvar *readtable* (copy-readtable)
  "The Declt readtable.")


;; String concatenation
;; --------------------
(defun tilde-reader (stream char)
  "Read a series of ~\"string\" to be concatenated together."
  (declare (ignore char))
  (flet ((read-string (&aux (string (read stream t nil t)))
	   (check-type string string "a string")
	   string))
    (apply #'concatenate 'string
	   (read-string)
	   (loop :while (char= (peek-char t stream nil nil t) #\~)
		 :do (read-char stream t nil t)
		 :collect (read-string)))))

(set-macro-character #\~ #'tilde-reader nil *readtable*)

;; Emacs indentation
;; -----------------
(defun clindent (symbol indent)
  "Set SYMBOL's indentation to INDENT in (X)Emacs.
This function sets SYMBOL's common-lisp-indent-function property.
If INDENT is a symbol, use its indentation definition.
Otherwise, INDENT is considered as an indentation definition."
  (when (and (member :swank *features*)
	     (configuration :swank-eval-in-emacs))
    (funcall (intern "EVAL-IN-EMACS" :swank)
	     `(put ',symbol 'common-lisp-indent-function
		   ,(if (symbolp indent)
			`(get ',indent 'common-lisp-indent-function)
		      `',indent))
	     t)))

(defmacro defindent (symbol indent)
  "Set SYMBOL's indentation to INDENT in (X)Emacs.
SYMBOL and INDENT need not be quoted.
See CLINDENT for more information."
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (clindent ',symbol ',indent)))

(defun i-reader (stream subchar arg)
  "Read an argument list for the DEFINDENT macro."
  (declare (ignore subchar arg))
  (cons 'defindent (read stream)))

(set-dispatch-macro-character #\# #\i #'i-reader *readtable*)


(defmacro in-readtable (name)
  "Set the current readtable to the value of NAME::*READTABLE*."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf cl:*readtable* (symbol-value (find-symbol "*READTABLE*" ,name)))))


;;; package.lisp ends here
