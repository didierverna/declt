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
    ;; Cf. the Twitter exchange).
    :documentation "The corresponding Lisp object.
All definitions have an associated Lisp object, except for constants, special
variables, symbol macros, and types. In such cases, this slot is unbound."
    :initarg :object :reader object)
   (foreign
    :documentation "Whether this definition is foreign."
    :initform nil :initarg :foreign :reader foreignp))
  (:documentation "Abstract root class for all definitions."))


;; ----------------
;; Pseudo-accessors
;; ----------------

(defgeneric name (definition)
  (:documentation "The definition's name.
This is the native Lisp name for the definition's corresponding object.
It's either a string (for ASDF components and packages) or a symbol."))


;; #### FIXME: this should not exist. It's a generic function that does
;; different things (an accessor and an a computation).
(defgeneric definition-package (definition)
  (:documentation "Return DEFINITION's package."))

(defgeneric external-definitions (object)
  (:documentation "Return OBJECT's external definitions."))

(defgeneric internal-definitions (object)
  (:documentation "Return OBJECT's internal definitions."))



;; ==========================================================================
;; Extraction Protocols
;; ==========================================================================

;; ---------------
;; Source protocol
;; ---------------

;; #### FIXME: I have defined methods for definitions, but I think this is
;; wrong. We rather need a LOCATION protocol with source information
;; /relative/ to the extract's location instead.
(defgeneric source (item)
  (:documentation "Return ITEM's definition source pathname."))


;; ------------------
;; Docstring protocol
;; ------------------

;; #### FIXME: this protocol could be extended to ASDF components, by deciding
+;; that the docstring is, say, the (short) description (nothing prevents us
+;; from keeping the DESCRIPTION accessor as well). In the future, the
+;; docstring could then become a slot directly in the DEFINITION base class.
(defgeneric docstring (item)
  (:documentation "Return ITEM's docstring (Lisp documentation)."))

;;; base.lisp ends here
