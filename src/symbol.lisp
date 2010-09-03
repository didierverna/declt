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
	     (let ((specializer (pop specializers)))
	       (cond ((and specializer (not (eq specializer t)))
		      ;; add specializers if there are any left
		      (write-string (string-downcase
				     (format nil "(~A ~A)"
				       part specializer)) stream))
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
    (format stream "@defvr Constant ~A~%" (string-downcase symbol))
    (when (documentation symbol 'variable)
      (write-string (pretty-texify (documentation symbol 'variable)) stream)
      (fresh-line))
    (format stream "@end defvr~%"))
  ;; #### PORTME.
  (when (eql (sb-int:info :variable :kind symbol) :special)
    (format stream "@defvr {Special Variable} ~A~%" (string-downcase symbol))
    (when (documentation symbol 'variable)
      (write-string (pretty-texify (documentation symbol 'variable)) stream)
      (fresh-line))
    (format stream "@end defvr~%"))
  (let ((class (find-class symbol nil)))
    (when class
      ;; #### PORTME.
      (let ((structure-p (eq (class-of class) (find-class 'structure-class)))
	    (error-condition-p (subtypep class 'condition))
	    (class-name (string-downcase symbol)))
	(format stream "@deftp {~A} ~A~%"
	  (cond (structure-p "Structure")
		(error-condition-p "Error Condition")
		(t "Class"))
	  class-name)
	(format stream "@tpindex @r{~A, }~A~%"
	  (cond (structure-p "Structure")
		(error-condition-p "Error Condition")
		(t "Class"))
	  class-name)
	(when (documentation symbol 'type)
	  (write-string (pretty-texify (documentation symbol 'type)) stream)
	  (fresh-line))
	(format stream "@end deftp~%"))))
  (when (macro-function symbol)
    (format stream "@defmac ~A " (string-downcase symbol))
    ;; #### PORTME.
    (render-lambda-list stream (sb-introspect:function-lambda-list symbol))
    (terpri stream)
    (format stream "@findex Macro, @t{~A}~%" (string-downcase symbol))
    (when (documentation symbol 'function)
      (write-string (pretty-texify (documentation symbol 'function)) stream)
      (fresh-line stream))
    (format stream "@end defmac~%"))
  (when (and (fboundp symbol)
	     (or (consp symbol)
		 (not (macro-function symbol)))
	     (not (typep (fdefinition symbol) 'standard-generic-function)))
    (format stream "@defun ~A " (string-downcase symbol))
    ;; #### PORTME.
    (render-lambda-list stream (sb-introspect:function-lambda-list symbol))
    (terpri stream)
    (format stream "@findex @r{Function}, ~A~%" (string-downcase symbol))
    (when (documentation symbol 'function)
      (write-string (pretty-texify (documentation symbol 'function)) stream)
      (fresh-line stream))
    (format stream "@end defun~%"))
  (when (and (fboundp symbol)
	     (typep (fdefinition symbol) 'standard-generic-function))
    (format stream "@deffn {Generic Function} ~A " (string-downcase symbol))
    ;; #### PORTME.
    (render-lambda-list stream (sb-introspect:function-lambda-list symbol))
    (terpri stream)
    (format stream "@findex @r{Generic Function}, ~A~%"
      (string-downcase symbol))
    (when (documentation symbol 'function)
      (write-string (pretty-texify (documentation symbol 'function)) stream)
      (fresh-line stream))
    (format stream "@end deffn~%")))


;;; symbol.lisp ends here
