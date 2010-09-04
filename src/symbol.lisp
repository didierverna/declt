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

(defun constant-symbol-p (symbol)
  "Return t if SYMBOL names a constant."
  (constantp symbol))

(defun special-symbol-p (symbol)
  "Return t if SYMBOL names a special variable."
  ;; #### PORTME.
  (eql (sb-int:info :variable :kind symbol) :special))

(defun class-symbol-p (symbol)
  "Return the class named by SYMBOL if any."
  (find-class symbol nil))

(defun function-symbol-p (symbol)
  "Return t if SYMBOL names a function."
  (fboundp symbol))

(defun macro-symbol-p (symbol)
  "Return t if SYMBOL names a macro."
  (macro-function symbol))

(defun ordinary-function-symbol-p (symbol)
  "Return t if SYMBOL names an ordinary function."
  (and (fboundp symbol)
       (or (consp symbol) (not (macro-symbol-p symbol)))
       (not (typep (fdefinition symbol) 'standard-generic-function))))

(defun generic-function-symbol-p (symbol)
  "Return t if SYMBOL names a generic function."
  (and (fboundp symbol)
       (typep (fdefinition symbol) 'standard-generic-function)))

(defun symbol-needs-rendering (symbol)
  "Return t when SYMBOL needs to be documented."
  (or (constantsymbol-p  symbol)
      (special-symbol-p  symbol)
      (class-symbol-p    symbol)
      (function-symbol-p symbol)))



;; ==========================================================================
;; Rendering Protocols
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
	     (format stream "~(~A~)" part))
	    (t
	     (let ((specializer (pretty-specializer (pop specializers))))
	       (cond ((and specializer (not (eq specializer t)))
		      ;; add specializers if there are any left
		      (format stream "(~A " part)
		      (write-string (format nil " @t{~(~A~)})" specializer)
				    stream))
		     (t
		      (write-string (symbol-name part) stream)))))))))

(defun render-constant (stream symbol)
  "Render SYMBOL as a constant on STREAM."
  (when (constant-symbol-p symbol)
    (@defconstant (stream (string-downcase symbol))
      (when (documentation symbol 'variable)
	(write-string (pretty-texify (documentation symbol 'variable))
		      stream)))))

(defun render-special (stream symbol)
  "Render SYMBOL as a special variable on STREAM."
  (when (special-symbol-p symbol)
    (@defspecial (stream (string-downcase symbol))
      (when (documentation symbol 'variable)
	(write-string (pretty-texify (documentation symbol 'variable))
		      stream)))))

(defun render-class (stream symbol)
  "Render SYMBOL as a class on STREAM."
  (let ((class (class-symbol-p symbol)))
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
			stream))))))

(defun render-macro (stream symbol)
  "Render SYMBOL as a macro on STREAM."
  (when (macro-symbol-p symbol)
    (@defmac (stream
	      (string-downcase symbol)
	      ;; #### PORTME.
	      (sb-introspect:function-lambda-list symbol))
      (when (documentation symbol 'function)
	(write-string (pretty-texify (documentation symbol 'function))
		      stream)))))

(defun render-function (stream symbol)
  "Render SYMBOL as an ordinary function on STREAM."
  (when (ordinary-function-symbol-p symbol)
    (@defun (stream
	     (string-downcase symbol)
	     ;; #### PORTME.
	     (sb-introspect:function-lambda-list symbol))
      (when (documentation symbol 'function)
	(write-string (pretty-texify (documentation symbol 'function))
		      stream)))))

(defun render-method (stream method)
  "Render METHOD on STREAM."
  (@defmethod (stream
	       ;; #### PORTME:
	       (string-downcase
		(sb-mop:generic-function-name
		 (sb-mop:method-generic-function method)))
	       ;; #### PORTME.
	       (sb-mop:method-lambda-list method)
	       ;; #### PORTME.
	       (sb-mop:method-specializers method)
	       ;; #### PORTME.
	       (sb-mop:method-qualifiers method))
    (when (documentation method 't)
      (write-string (pretty-texify (documentation method 't)) stream))))

(defun render-generic (stream symbol)
  "Render SYMBOL as a generic function on STREAM."
  (when (generic-function-symbol-p symbol)
    (@defgeneric (stream
		  (string-downcase symbol)
		  ;; #### PORTME.
		  (sb-introspect:function-lambda-list symbol))
      (when (documentation symbol 'function)
	(write-string (pretty-texify (documentation symbol 'function))
		      stream)))
    ;; #### PORTME.
    (dolist (method (sb-mop:generic-function-methods (fdefinition symbol)))
      (render-method stream method))))

(defun render-symbol (stream symbol)
  "Render SYMBOL's documentation on STREAM."
  (render-constant stream symbol)
  (render-special  stream symbol)
  (render-class    stream symbol)
  (render-macro    stream symbol)
  (render-function stream symbol)
  (render-generic  stream symbol))


;;; symbol.lisp ends here
