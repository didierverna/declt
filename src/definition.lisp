;;; definition.lisp --- Definitions rendering

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sat Sep  4 15:27:31 2010
;; Last Revision: Sun Sep  5 21:49:36 2010

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

(define-constant +categories+
    '((:constant  "constant"          "constants")
      (:special   "special variable"  "special variables")
      (:macro     "macro"             "macros")
      (:function  "function"          "functions")
      (:generic   "generic function"  "generic functions")
      (:condition "condition"         "conditions")
      (:structure "structure"         "structures")
      (:class     "class"             "classes"))
  "The list of definition categories and how to typeset them.")

(defun add-category-node (parent location category symbols)
  "Add LOCATION CATEGORY node to PARENT for SYMBOLS."
  (add-child parent
    (make-node :name (format nil "~@(~A ~A~)" location (third category))
	       :section-name (format nil "~@(~A~)" (third category))
	       :before-menu-contents
	       (render-to-string
		 (dolist (symbol (sort symbols #'string-lessp))
		   (funcall
		    (fdefinition (intern (format nil "RENDER-~A"
					   (first category))
					 :com.dvlsoft.declt))
		    symbol))))))

(defun add-categories-node (parent location symbols)
  "Add all relevant category nodes to PARENT for LOCATION SYMBOLS."
  (dolist (category +categories+)
    (let ((category-symbols
	   (remove-if-not
	    (fdefinition
	     (intern (format nil "~A-DEFINITION-P" (first category))
		     :com.dvlsoft.declt))
	    symbols)))
      (when category-symbols
	(add-category-node parent location category category-symbols)))))

(defun add-definitions-node
    (parent system
     &aux (definitions-node
	      (add-child parent
		(make-node :name "Definitions"
			   :synopsis "The symbols documentation"
			   :before-menu-contents(format nil "~
Definitions are sorted by export status, category, package, and then by
lexicographic order.")))))
  "Add the SYSTEM's definitions node to PARENT."
  (loop :for symbols :in (list (system-external-symbols system)
			       (system-internal-symbols system))
	:for location :in '("exported" "internal")
	:when symbols
	:do (let ((node (add-child definitions-node
			  (make-node :name (format nil "~@(~A~) definitions"
					     location)))))
	      (add-categories-node node location symbols))))


;;; definition.lisp ends here
