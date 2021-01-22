;;; base.lisp --- Code base for documentation extraction

;; Copyright (C) 2010, 2011, 2013, 2020 Didier Verna

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

(in-package :net.didierverna.declt)
(in-readtable :net.didierverna.declt)



;; ==========================================================================
;; Definition Basics
;; ==========================================================================

(defabstract definition ()
  ((object
    ;; #### NOTE: it is unclear to me why symbol macros don't provide access
    ;; to the corresponding expansion function in a way similar to regular
    ;; macros do. In other words, I'm not sure why there's no
    ;; SYMBOL-MACRO-FUNCTION in the standard. Probably for the same reason
    ;; they don't have docstrings (late addition notably, according to Kent;
    ;; Cf. the Twitter exchange). Also, it is probably possible to get an
    ;; object for types (look into sb-introspec), and why not symbol macro
    ;; expanders after all.
    :documentation "The corresponding Lisp object, or NIL.
Only constants, special variables, symbol macros, and types do not have an
associated Lisp object"
    :initform nil :initarg :object :reader object)
   (foreign
    :documentation "Whether this definition is foreign."
    :initform nil :initarg :foreign :reader foreignp))
  (:documentation "Abstract root class for all definitions."))




;; ==========================================================================
;; Extraction Protocols
;; ==========================================================================

(defgeneric name (definition)
  (:documentation "The definition's name.
This is the native Lisp name for the definition's corresponding object.
It's either a string (for ASDF components and packages), a symbol,
or a list of the form (setf symbol)."))

(defmethod print-object ((definition definition) stream)
  "Show DEFINITION's name."
  (print-unreadable-object (definition stream :type t)
    (princ (name definition) stream)))

;; #### NOTE: we're trying to be clever here, bypassing
;; FIND-DEFINITION-SOURCES-BY-NAME when possible, because it performs some
;; checks that we know we don't need. I'm not sure it's worth the trouble
;; anymore.
(defgeneric source (definition)
  (:documentation "Return DEFINITION's source pathname.")
  ;; #### PORTME.
  (:method (definition)
    "Return DEFINITION's object source pathname (this is the default method)."
    (object-source-pathname (object definition))))

(defgeneric docstring (definition)
  (:documentation "Return DEFINITION's docstring (Lisp documentation).")
  (:method (definition)
    "Return DEFINITION's object canonical documentation.
This is the default method."
    (documentation (object definition) t)))




;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun find-definition (object definitions)
  "Find a definition for OBJECT in DEFINITIONS."
  ;; #### WARNING: we need an EQUAL test because some objects are compound
  ;; (e.g. medium form setf expanders).
  (find object definitions :key #'object :test #'equal))

(defgeneric public-definitions (object)
  (:documentation "Return OBJECT's public definitions."))

(defgeneric private-definitions (object)
  (:documentation "Return OBJECT's private definitions."))

;;; base.lisp ends here
