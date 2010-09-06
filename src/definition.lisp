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
    '((:constant "constant"          "constants")
      (:special  "special variable"  "special variables")
      (:class    "class"             "classes")
      (:macro    "macro"             "macros")
      (:function "ordinary function" "ordinary functions")
      (:generic  "generic function"  "generic functions"))
  "The list of definition categories and how to typeset them.")

(defun add-category-node (parent category location package symbols)
  "Add PACKAGE's LOCATION definitions node to PARENT for SYMBOLS of CATEGORY."
  (let ((definitions
	    (remove-if-not
	     (fdefinition
	      (intern (format nil "~A-SYMBOL-P" category) :com.dvlsoft.declt))
	     symbols)))
    (when definitions
      (add-child parent
		 (make-node :name
			    (format nil "~@(~A~) ~A from the ~(~A~) package"
			      location
			      (third (assoc category +categories+))
			      (escape package))
			    :section-name
			    (format nil "~@(~A~)"
			      (third (assoc category +categories+)))
			    :before-menu-contents
			    (render-to-string
			     (dolist (definition definitions)
			       (funcall
				(fdefinition
				 (intern (format nil "RENDER-~A" category)
					 :com.dvlsoft.declt))
				definition))))))))

(defun add-location-node (parent location package)
  "Add PACKAGE's LOCATION node to PARENT.
LOCATION is either :external or :internal."
  (let ((symbols (funcall (fdefinition
			   (intern (format nil "PACKAGE-~A-SYMBOLS" location)
				   :com.dvlsoft.declt))
			  package)))
    (when symbols
      (let ((node
	     (add-child parent
			(make-node :name (format nil "~
~@(~A~) definitions from the ~(~A~) package"
					   location
					   (escape package))
				   :section-name (format nil "~
~:(~A~) definitions"
						   location)))))
	(dolist (category '(:constant :special :class
			    :macro :function :generic))
	  (add-category-node node category location package symbols))))))

(defun add-definitions-node
    (parent system
     &aux (definitions-node
	      (add-child parent
			 (make-node :name "Definitions"
				    :before-menu-contents(format nil "~
Definitions are sorted by package, export status, category and then by
lexicographic order."))))
	  (packages (system-packages system)))
  "Add the SYSTEM's definitions node to PARENT."
  (dolist (package packages)
    (let ((package-node
	   (add-child definitions-node
		      (make-node
		       :name (format nil "Definitions from the ~(~A~) package"
			       (escape package))
		       :section-name (format nil "From the ~(@t{~A}~) package"
				       (escape package))))))
      (dolist (location '(:external :internal))
	(add-location-node package-node location package)))))


;;; definition.lisp ends here
