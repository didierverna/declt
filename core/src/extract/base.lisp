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
;; Definitions
;; ==========================================================================

;; #### FIXME: abstact.
(defclass definition ()
  ;; #### NOTE: not all definitions make use of the FOREIGN slot currently,
  ;; but who knows which new ones may need to be advertised in the future.
  ((foreign :documentation "Whether this definition is foreign."
	    :initform nil :initarg :foreign :reader foreignp))
  (:documentation "The Definition class.
This is the base class for all definitions."))

(defgeneric definition-package (definition)
  (:documentation "Return DEFINITION's package."))



;; ==========================================================================
;; Extraction Protocols
;; ==========================================================================

;; ---------------
;; Source protocol
;; ---------------

(defgeneric source (item)
  (:documentation "Return ITEM's definition source pathname."))


;; ------------------
;; Docstring protocol
;; ------------------

(defgeneric docstring (item)
  (:documentation "Return ITEM's docstring (Lisp documentation)."))


;; ------------------
;; Type name protocol
;; ------------------

(defgeneric type-name (item)
  (:documentation "Return ITEM's type name."))

;;; base.lisp ends here
