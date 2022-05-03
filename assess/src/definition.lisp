;;; definition.lisp --- Definitions code base

;; Copyright (C) 2010, 2011, 2013, 2020-2022 Didier Verna

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

(in-package :net.didierverna.declt.assess)
(in-readtable :net.didierverna.declt)



;; ==========================================================================
;; Definition Basics
;; ==========================================================================

(defabstract definition ()
  ((object
    :documentation "The corresponding Lisp object, or NIL.
Only constants, special variables, symbol macros, and aliases lack such an
object."
    :initform nil :initarg :object :reader object)
   (uid :documentation "This definition's UID." :accessor uid)
   (source-file
    :documentation "The source file definition for this definition's object."
    :initform nil :accessor source-file)
   (foreign
    :documentation "Whether this definition is foreign."
    :initform nil :initarg :foreign :accessor foreignp))
  (:documentation "Abstract root class for all definitions.
All definitions respond to the following public protocols, which see:
- `name',
- `docstring'."))

(defmethod print-object ((definition definition) stream)
  "Show DEFINITION's name."
  (print-unreadable-object (definition stream :type t)
    (princ (name definition) stream)))



;; ---------------------------
;; Public definition protocols
;; ---------------------------

(defgeneric name (definition)
  (:documentation "The definition's name.
This is the native Lisp name for the definition's corresponding object.
It's either a string (for ASDF components and packages), a symbol,
or a list of the form (setf symbol)."))

(defgeneric docstring (definition)
  (:documentation "Return DEFINITION's docstring (Lisp documentation).")
  (:method (definition)
    "Return DEFINITION's object canonical documentation.
This is the default method."
    (documentation (object definition) t)))




;; ==========================================================================
;; Public Utility Protocols
;; ==========================================================================

(defgeneric public-definitions (object)
  (:documentation "Return OBJECT's public definitions.")
  (:method  (object)
    "Return OBJECT's public definitions from its definitions list.
This is the default method for heterogeneous definitions lists."
    (remove-if-not (lambda (definition)
		     (and (typep definition 'symbol-definition)
			  (publicp definition)))
	(definitions object))))

(defgeneric private-definitions (object)
  (:documentation "Return OBJECT's private definitions.")
  (:method (object)
    "Return OBJECT's private definitions from its definitions list.
This is the default method for heterogeneous definitions lists."
    (remove-if (lambda (definition)
		 (or (not (typep definition 'symbol-definition))
		     (publicp definition)))
	(definitions object))))




;; ==========================================================================
;; Internal Utility Protocols
;; ==========================================================================

#i(domesticp 2)
(defun domesticp (symbol pathname packages pathnames)
  "Return T if a definition for SYMBOL originating in PATHNAME is domestic.
A definition is considered domestic under the following conditions:
- its originating PATHNAME is known (non NIL) and one of domestic PATHNAMES,
- its originating PATHNAME is unknown, but the SYMBOL's home package is one of
  domestic PACKAGES.
Note that a definition for a domestic symbol, but originating in a foreign
source file is considered foreign."
  (if pathname
    (member pathname pathnames :test #'equal)
    (member (symbol-package symbol) packages)))

(defun find-definition (object definitions)
  "Find a definition for OBJECT in DEFINITIONS."
  ;; #### WARNING: we need an EQUAL test because some objects are compound
  ;; (e.g. medium form setf expanders).
  (find object definitions :key #'object :test #'equal))

;; #### NOTE: we're trying to be clever here, bypassing
;; FIND-DEFINITION-SOURCES-BY-NAME when possible, because it performs some
;; checks that we know we don't need. I'm not sure it's worth the trouble
;; anymore.
;; #### UPDATE: in fact, not only is it not worth it, it's dangerous. I've
;; just been bitten by this on method combinations, for which this default
;; method returns the wrong information (it works on method-combination-info
;; structures for which the definition file is an SBCL specific one).
(defgeneric source-pathname (definition)
  (:documentation "Return DEFINITION's source pathname.")
  ;; #### PORTME.
  (:method (definition)
    "Return DEFINITION's object source pathname (this is the default method)."
    (source-by-object (object definition))))

;;; definition.lisp ends here
