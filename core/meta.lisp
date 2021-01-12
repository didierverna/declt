;;; meta.lisp --- Meta utilities

;; Copyright (C) 2010-2013, 2015, 2017 Didier Verna

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

(in-package :cl-user)

(defpackage :net.didierverna.declt
  (:documentation
   "The Documentation Extractor from Common Lisp to Texinfo package.")
  (:use :cl :net.didierverna.declt.setup)
  (:shadow :*readtable*)
  (:import-from :quickutil
    :when-let :when-let*)
  (:import-from :asdf
    :component-name
    :component-version
    :component-description
    :component-long-description
    :component-parent
    :component-children
    :component-pathname
    :component-relative-pathname
    :component-if-feature
    :component-sideway-dependencies
    :component-find-path
    :find-system
    :resolve-dependency-name
    :system-long-name
    :system-author
    :system-maintainer
    :system-mailto
    :system-homepage
    :system-source-control
    :system-bug-tracker
    :system-license
    :system-source-directory
    :system-source-file)
  (:export
    ;; From the :net.didierverna.declt.setup package:
    :*release-major-level*
    :*release-minor-level*
    :*release-status*
    :*release-status-level*
    :*release-name*
    :version
   ;; From package.lisp (this file):
   :nickname-package
   ;; From src/declt.lisp:
   :declt))


(in-package :net.didierverna.declt)


;; -------------------
;; External utilities:
;; -------------------

(defun nickname-package (&optional (nickname :declt))
  "Add NICKNAME (:DECLT by default) to the :NET.DIDIERVERNA.DECLT package."
  (rename-package :net.didierverna.declt
		  (package-name :net.didierverna.declt)
		  (adjoin nickname (package-nicknames :net.didierverna.declt)
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


;; --------------------
;; Very early utilities
;; --------------------

;; Quickutil
(defun generate-quickutils ()
  "Generate the offline quickutil file."
  (funcall (intern "SAVE-UTILS-AS" :quickutil-client)
	   (merge-pathnames (make-pathname :name "quickutil" :type "lisp")
			    (asdf:system-source-directory
			     :net.didierverna.declt.core))
	   :when-let))

;;; meta.lisp ends here
