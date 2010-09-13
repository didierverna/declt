;;; symbol.lisp --- Symbol based documentation

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sat Sep  4 15:27:31 2010
;; Last Revision: Thu Sep  9 18:53:22 2010

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
;; Rendering Routines
;; ==========================================================================

(defmacro @defvr (category name &body body)
  "Render BODY in a @defvr {CATEGORY} NAME environment."
  `(progn
    (format t "@defvr {~A} ~A~%" (escape ,category) (escape ,name))
    ,@body
    (format t "~&@end defvr~%")))

(defmacro @defconstant (name &body body)
  "Render BODY in a @defvr {Constant} NAME environment."
  `(@defvr "Constant" ,name ,@body))

(defmacro @defspecial (name &body body)
  "Render BODY in a @defvr {Special Variable} NAME environment."
  `(@defvr "Special Variable" ,name ,@body))

(defgeneric pretty-specializer (specializer)
  (:documentation "Returns a printable form for SPECIALIZER.")
  (:method (specializer)
    (or (ignore-errors (class-name specializer))
	specializer))
  ;; #### PORTME.
  (:method ((specializer sb-mop:eql-specializer))
    ;; #### PORTME.
    `(eql ,(sb-mop:eql-specializer-object specializer))))

;; Based on Edi Weitz's write-lambda-list* from documentation-template.
(defun render-lambda-list (lambda-list &optional specializers)
  "Render LAMBDA-LIST."
  (let ((firstp t)
	after-required-args-p)
    (dolist (part lambda-list)
      (when (and (consp part) after-required-args-p)
	(setq part (first part)))
      (unless firstp
	(write-char #\Space))
      (setq firstp nil)
      (cond ((consp part)
	     (write-char #\()
	     (render-lambda-list part)
	     (write-char #\)))
	    ((member part '(&optional &rest &key &allow-other-keys
			    &aux &environment &whole &body))
	     (setq after-required-args-p t)
	     (format t "~(~A~)" part))
	    (t
	     (let ((specializer (pretty-specializer (pop specializers))))
	       (if (and specializer (not (eq specializer t)))
		   (format t "(~A @t{~(~A~)})"
		     (escape part)
		     (escape specializer))
		 (write-string (escape (symbol-name part))))))))))

(defmacro @defmac (name lambda-list &body body)
  "Render BODY in a @defmac NAME LAMBDA-LIST environment."
  (let ((the-name (gensym "name")))
    `(let ((,the-name (escape ,name)))
      (format t "@defmac ~A " ,the-name)
      (render-lambda-list ,lambda-list)
      (terpri)
      (format t "@findex @r{Macro, }~A~%" ,the-name)
      ,@body
      (format t "~&@end defmac~%"))))

(defmacro @defun (name lambda-list &body body)
  "Render BODY in a @defun NAME LAMBDA-LIST environment."
  (let ((the-name (gensym "name")))
    `(let ((,the-name (escape ,name)))
      (format t "@defun ~A " ,the-name)
      (render-lambda-list ,lambda-list)
      (terpri)
      (format t "@findex @r{Function, }~A~%" ,the-name)
      ,@body
      (format t "~&@end defun~%"))))

(defmacro @deffn ((category name lambda-list &optional specializers qualifiers)
		  &body body)
  "Render BODY in a @deffn CATEGORY NAME LAMBDA-LIST environment."
  (let ((the-name (gensym "name"))
	(the-category (gensym "category")))
    `(let ((,the-name (escape ,name))
	   (,the-category (escape ,category)))
      (format t "@deffn {~A} ~A " ,the-category ,the-name)
      (render-lambda-list ,lambda-list ,specializers)
      (format t "~(~{ @t{~A}~^~}~)~%" (mapcar #'escape ,qualifiers))
      (format t "@findex @r{~A, }~A~%" ,the-category ,the-name)
      ,@body
      (format t "~&@end deffn~%"))))

(defmacro @defgeneric (name lambda-list &body body)
  "Render BODY in a @deffn {Generic Function} NAME LAMBDA-LIST environment."
  `(@deffn ("Generic Function" ,name ,lambda-list)
    ,@body))

(defmacro @defmethod (name lambda-list specializers qualifiers &body body)
  "Render BODY in a @deffn {Method} NAME LAMBDA-LIST environment."
  `(@deffn ("Method" ,name ,lambda-list ,specializers ,qualifiers)
    ,@body))

(defmacro @deftp (category name &body body)
  "Render BODY in a @deftp {CATEGORY} NAME environment."
  (let ((the-name (gensym "name"))
	(the-category (gensym "category")))
    `(let ((,the-name (escape ,name))
	   (,the-category (escape ,category)))
      (format t "@deftp {~A} ~A~%"  ,the-category ,the-name)
      (format t "@tpindex @r{~A, }~A~%" ,the-category ,the-name)
      ,@body
      (format t "~&@end deftp~%"))))

(defmacro @defstruct (name &body body)
  "Render BODY in a @deftp {Structure} NAME environment."
  `(@deftp "Structure" ,name ,@body))

(defmacro @defcond (name &body body)
  "Render BODY in a @deftp {Condition} NAME environment."
  `(@deftp "Condition" ,name ,@body))

(defmacro @defclass (name &body body)
  "Render BODY in a @deftp {Class} NAME environment."
  `(@deftp "Class" ,name ,@body))



;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

;; #### NOTE: there are no indexing methods for methods or symbols, because
;; indexing is done by the lower-level @defXXX routines (Texinfo does half the
;; job and we do the other rhalf).

(defmethod title ((constant constant-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) constant" (escape constant)))

(defmethod title ((special special-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) special variable" (escape special)))

(defmethod title ((macro macro-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) macro" (escape macro)))

(defmethod title ((function function-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) function" (escape function)))

(defmethod title ((method method-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~{ ~A~^~}~{ ~A~^~}~) method"
    (escape method)
    ;; #### PORTME.
    (mapcar #'escape (mapcar #'pretty-specializer
			     (sb-mop:method-specializers
			      (method-definition-method method))))
    (mapcar #'escape (sb-mop:method-qualifiers
		      (method-definition-method method)))))

(defmethod title ((generic generic-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) generic function" (escape generic)))

(defmethod title ((condition condition-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) condition" (escape condition)))

(defmethod title ((structure structure-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) structure" (escape structure)))

(defmethod title ((class class-definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) class" (escape class)))

(defmethod anchor ((definition definition) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "~A anchor" (title definition)))

(defmethod reference ((definition definition) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@ref{~A, , @t{~(~A}~)} (~A)~%"
    (anchor definition)
    (escape definition)
    (definition-type-name definition)))

(defun document-definition (definition relative-to kind)
  "Render DEFINITION's documentation contents as KIND."
  (format t "@anchor{~A}@c~%" (anchor definition))
  (@table ()
    (let ((documentation (documentation (definition-symbol definition) kind)))
      (when documentation
	(format t "@item Documentation~%")
	(render-text documentation)))
    (format t "~&@item Package~%")
    (reference (symbol-package (definition-symbol definition)))
    (render-source definition relative-to)))

(defmethod document ((constant constant-definition) relative-to)
  (@defconstant (string-downcase (definition-symbol constant))
    (document-definition constant relative-to 'variable)))

(defmethod document ((special special-definition) relative-to)
  (@defspecial (string-downcase (definition-symbol special))
    (document-definition special relative-to 'variable)))

(defmethod document ((macro macro-definition) relative-to)
  (@defmac (string-downcase (definition-symbol macro))
      ;; #### PORTME.
      (sb-introspect:function-lambda-list
       (macro-definition-function macro))
    (document-definition macro relative-to 'function)))

(defmethod document ((function function-definition) relative-to)
  (@defun (string-downcase (definition-symbol function))
      ;; #### PORTME.
      (sb-introspect:function-lambda-list
       (function-definition-function function))
    (document-definition function relative-to 'function)))

(defmethod document ((method method-definition) relative-to)
  (@defmethod
      ;; #### PORTME:
      (string-downcase (definition-symbol method))
      (sb-mop:method-lambda-list (method-definition-method method))
      (sb-mop:method-specializers (method-definition-method method))
      (sb-mop:method-qualifiers (method-definition-method method))
    (@table ()
      (format t "@anchor{~A}@c~%" (anchor method))
      (let ((documentation
	     (documentation (method-definition-method method) t)))
	(when documentation
	  (format t "@item Documentation~%")
	  (render-text documentation)))
      (render-source method relative-to))))

(defmethod document ((generic generic-definition) relative-to)
  (@defgeneric (string-downcase (definition-symbol generic))
      ;; #### PORTME.
      (sb-introspect:function-lambda-list
       (generic-definition-function generic))
    (document-definition generic relative-to 'function))
  ;; #### PORTME.
  (dolist (method
	    (sb-mop:generic-function-methods
	     (generic-definition-function generic)))
    (document (make-method-definition
	       :symbol (definition-symbol generic)
	       :method method)
	      relative-to)))

(defmethod document ((condition condition-definition) relative-to)
  (@defcond (string-downcase (definition-symbol condition))
    (document-definition condition relative-to 'type)))

(defmethod document ((structure structure-definition) relative-to)
  (@defstruct (string-downcase (definition-symbol structure))
    (document-definition structure relative-to 'type)))

(defmethod document ((class class-definition) relative-to)
  (@defclass (string-downcase (definition-symbol class))
    (document-definition class relative-to 'type)))



;; ==========================================================================
;; Definition Nodes
;; ==========================================================================

(defun add-category-node (parent location category definitions relative-to)
  "Add LOCATION CATEGORY node to PARENT for DEFINITIONS."
  (add-child parent
    (make-node :name (format nil "~@(~A ~A~)" location category)
	       :section-name (format nil "~@(~A~)" category)
	       :before-menu-contents
	       (render-to-string
		 (dolist (definition (sort definitions #'string-lessp
					   :key #'definition-symbol))
		   (document definition relative-to))))))

(defun add-categories-node (parent location symbols relative-to)
  "Add all relevant category nodes to PARENT for LOCATION SYMBOLS."
  (dolist (category +categories+)
    (let ((category-definitions
	   (loop :for symbol :in symbols
		 :when (symbol-definition symbol (first category))
		 :collect :it)))
      (when category-definitions
	(add-category-node parent location (third category)
			   category-definitions relative-to)))))

(defun add-definitions-node
    (parent system
     &aux (system-directory (system-directory system))
	  (definitions-node
	      (add-child parent
		(make-node :name "Definitions"
			   :synopsis "The symbols documentation"
			   :before-menu-contents(format nil "~
Definitions are sorted by export status, category, package, and then by
lexicographic order.")))))
  "Add the SYSTEM's definitions node to PARENT."
  (loop :for symbols :in (list (system-external-symbols system)
			       (system-internal-symbols system))
	:for status :in '("exported" "internal")
	:when symbols
	:do (let ((node (add-child definitions-node
			  (make-node :name (format nil "~@(~A~) definitions"
					     status)))))
	      (add-categories-node node status symbols system-directory))))


;;; symbol.lisp ends here
