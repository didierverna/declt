;;; definition.lisp --- Definitions rendering

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sat Sep  4 15:27:31 2010
;; Last Revision: Wed Sep  8 09:51:59 2010

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
;; Item Protocols
;; ==========================================================================

(defmethod location ((method method))
  ;; #### PORTME.
  (let* ((defsrc (sb-introspect:find-definition-source method)))
    (when defsrc
      (sb-introspect:definition-source-pathname defsrc))))



;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

;; #### NOTE: there are no indexing methods for methods or symbols, because
;; indexing is done by the lower-level @defXXX routines (Texinfo does half the
;; job and we do the othe rhalf).

(defun document-method (method relative-to)
  "Render METHOD's documentation."
  (@defmethod
      ;; #### PORTME:
      (string-downcase (sb-mop:generic-function-name
			(sb-mop:method-generic-function method)))
      (sb-mop:method-lambda-list method)
      (sb-mop:method-specializers method)
      (sb-mop:method-qualifiers method)
    (@table ()
      (render-location method relative-to)
      (let ((documentation (documentation method t)))
	(when documentation
	  (format t "@item Documentation~%")
	  (render-string documentation))))))

(defun document-definition-1 (definition relative-to type kind)
  "Render DEFINITION's documentation contents as KIND."
  (@table ()
    (format t "@item Package~%")
    (reference (symbol-package definition))
    ;; #### PORTME.
    (let* ((defsrc
	       ;; #### FIXME: why a list? How can there be several sources?
	       (car
		(sb-introspect:find-definition-sources-by-name definition
							       type)))
	   (pathname (when defsrc
		       (sb-introspect:definition-source-pathname defsrc))))
      (when pathname
	(render-location pathname relative-to)))
    (let ((documentation (documentation definition kind)))
      (when documentation
	(format t "@item Documentation~%")
	(render-string documentation)))))

(defun document-definition (definition relative-to category)
  "Render DEFINITION's documentation in CATEGORY."
  (ecase category
    (:constant
     (when (definitionp definition :constant)
       (@defconstant (string-downcase definition)
	 (document-definition-1 definition relative-to :constant 'variable))))
    (:special
     (when (definitionp definition :special)
       (@defspecial (string-downcase definition)
	 (document-definition-1 definition relative-to :variable 'variable))))
    (:macro
     (when (definitionp definition :macro)
       (@defmac (string-downcase definition)
	   ;; #### PORTME.
	   (sb-introspect:function-lambda-list definition)
	 (document-definition-1 definition relative-to :macro 'function))))
    (:function
     (when (definitionp definition :function)
       (@defun (string-downcase definition)
	   ;; #### PORTME.
	   (sb-introspect:function-lambda-list definition)
	 (document-definition-1 definition relative-to :function 'function))))
    (:generic
     (when (definitionp definition :generic)
       (@defgeneric (string-downcase definition)
	   ;; #### PORTME.
	   (sb-introspect:function-lambda-list definition)
	 (document-definition-1 definition relative-to :generic-function
				'function))
       ;; #### PORTME.
       (dolist (method
		 (sb-mop:generic-function-methods (fdefinition definition)))
	 (document-method method relative-to))))
    (:condition
     (when (definitionp definition :condition)
       (@defcond (string-downcase definition)
	 (document-definition-1 definition relative-to :condition 'type))))
    (:structure
     (when (definitionp definition :structure)
       (@defstruct (string-downcase definition)
	 (document-definition-1 definition relative-to :structure 'type))))
    (:class
     (when (definitionp definition :class)
       (@defclass (string-downcase definition)
	 (document-definition-1 definition relative-to :class 'type))))))



;; ==========================================================================
;; Definition Nodes
;; ==========================================================================

(defun add-category-node (parent location category definitions relative-to)
  "Add LOCATION CATEGORY node to PARENT for DEFINITIONS."
  (add-child parent
    (make-node :name (format nil "~@(~A ~A~)" location (third category))
	       :section-name (format nil "~@(~A~)" (third category))
	       :before-menu-contents
	       (render-to-string
		 (dolist (definition (sort definitions #'string-lessp))
		   (document-definition definition relative-to
					(first category)))))))

(defun add-categories-node (parent location definitions relative-to)
  "Add all relevant category nodes to PARENT for LOCATION DEFINITIONS."
  (dolist (category +categories+)
    (let ((category-definitions
	   (remove-if-not (lambda (definition)
			    (definitionp definition (first category)))
			  definitions)))
      (when category-definitions
	(add-category-node parent location category category-definitions
			   relative-to)))))

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
  (loop :for definitions :in (list (system-external-definitions system)
				   (system-internal-definitions system))
	:for location :in '("exported" "internal")
	:when definitions
	:do (let ((node (add-child definitions-node
			  (make-node :name (format nil "~@(~A~) definitions"
					     location)))))
	      (add-categories-node node location definitions
				   system-directory))))


;;; definition.lisp ends here
