;;; symbol.lisp --- Symbols rendering

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Thu Sep  2 23:22:59 2010
;; Last Revision: Thu Sep  2 23:22:59 2010

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

(in-package :com.dvlsoft.declt)


;; ==========================================================================
;; Utilities
;; ==========================================================================

(defgeneric pretty-specializer (specializer)
  (:documentation "Returns a printable form for SPECIALIZER.")
  (:method (specializer)
    (or (ignore-errors (class-name specializer))
	specializer))
  ;; #### PORTME.
  (:method ((specializer sb-mop:eql-specializer))
    ;; #### PORTME.
    `(eql ,(sb-mop:eql-specializer-object specializer))))

;; Hacked from Edi Weitz's write-lambda-list* in documentation-template.
(defun render-lambda-list (stream lambda-list &optional specializers)
  "Render LAMBDA-LIST on STREAM."
  (let ((firstp t)
	after-required-args-p)
    (dolist (part lambda-list)
      (when (and (consp part) after-required-args-p)
	(setq part (first part)))
      (unless firstp
	(write-char #\Space stream))
      (setq firstp nil)
      (cond ((consp part)
	     ;; a destructuring lambda list - recurse
	     (write-char #\( stream)
	     (render-lambda-list stream part)
	     (write-char #\) stream))
	    ((member part '(&optional &rest &key &allow-other-keys
			    &aux &environment &whole &body))
	     (setq after-required-args-p t)
	     (format stream "~A" (string-downcase part)))
	    (t
	     (let ((specializer (pretty-specializer (pop specializers))))
	       (cond ((and specializer (not (eq specializer t)))
		      ;; add specializers if there are any left
		      (format stream "(~A " part)
		      (write-string
		       (string-downcase (format nil " @t{~A})" specializer))
		       stream))
		     (t
		      (write-string (symbol-name part) stream)))))))))

(defun symbol-needs-rendering (symbol)
  "Return t when SYMBOL needs to be documented."
  (or (constantp symbol)
      ;; #### PORTME.
      (eql (sb-int:info :variable :kind symbol) :special)
      (find-class symbol nil)
      (fboundp symbol)))



;; ==========================================================================
;; Rendering Protocols
;; ==========================================================================

(defun render-symbol (stream symbol)
  "Render SYMBOL's documentation on STREAM."
  (when (constantp symbol)
    (@defconstant (stream (string-downcase symbol))
      (when (documentation symbol 'variable)
	(write-string (pretty-texify (documentation symbol 'variable))
		      stream))))
  ;; #### PORTME.
  (when (eql (sb-int:info :variable :kind symbol) :special)
    (@defspecial (stream (string-downcase symbol))
      (when (documentation symbol 'variable)
	(write-string (pretty-texify (documentation symbol 'variable))
		      stream))))
  (let ((class (find-class symbol nil)))
    (when class
      (@deftp (stream
	       ;; #### PORTME.
	       (cond ((eq (class-of class) (find-class 'structure-class))
		      "Structure")
		     ((subtypep class 'condition)
		      "Condition")
		     (t "Class"))
	       (string-downcase symbol))
	(when (documentation symbol 'type)
	  (write-string (pretty-texify (documentation symbol 'type))
			stream)))))
  (when (macro-function symbol)
    (@defmac (stream
	      (string-downcase symbol)
	      ;; #### PORTME.
	      (sb-introspect:function-lambda-list symbol))
      (when (documentation symbol 'function)
	(write-string (pretty-texify (documentation symbol 'function))
		      stream))))
  (when (and (fboundp symbol)
	     (or (consp symbol)
		 (not (macro-function symbol)))
	     (not (typep (fdefinition symbol) 'standard-generic-function)))
    (@defun (stream
	     (string-downcase symbol)
	     ;; #### PORTME.
	     (sb-introspect:function-lambda-list symbol))
      (when (documentation symbol 'function)
	(write-string (pretty-texify (documentation symbol 'function))
		      stream))))
  (when (and (fboundp symbol)
	     (typep (fdefinition symbol) 'generic-function))
    (@defgeneric (stream
		  (string-downcase symbol)
		  ;; #### PORTME.
		  (sb-introspect:function-lambda-list symbol))
      (when (documentation symbol 'function)
	(write-string (pretty-texify (documentation symbol 'function))
		      stream)))
    ;; #### PORTME.
    (dolist (method (sb-mop:generic-function-methods (fdefinition symbol)))
      (@defmethod (stream
		   (string-downcase symbol)
		   ;; #### PORTME.
		   (sb-mop:method-lambda-list method)
		   ;; #### PORTME.
		   (sb-mop:method-specializers method)
		   ;; #### PORTME.
		   (sb-mop:method-qualifiers method))
	(when (documentation method 't)
	  (write-string (pretty-texify (documentation method 't)) stream))))))


;;; symbol.lisp ends here
