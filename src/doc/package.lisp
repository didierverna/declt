;;; package.lisp --- Package documentation

;; Copyright (C) 2010, 2011, 2012 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

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
(in-readtable :com.dvlsoft.declt)


;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

(defmethod title ((package package) &optional relative-to)
  "Return PACKAGE's title."
  (declare (ignore relative-to))
  (format nil "the ~(~A~) package" (name package)))

(defmethod index ((package package) &optional relative-to)
  "Render PACKAGE's indexing command."
  (declare (ignore relative-to))
  (format t "@packageindex{~(~A~)}@c~%" (escape package)))

(defmethod reference ((package package) &optional relative-to)
  "Render PACKAGE's reference."
  (declare (ignore relative-to))
  (format t "@ref{~A, , @t{~(~A}~)}~%" (anchor-name package) (escape package)))

(defmethod document ((package package) context)
  "Render PACKAGE's documentation in CONTEXT."
  (anchor package)
  (index package)
  (@table ()
    (let ((docstring (docstring package)))
      (when docstring
	(@tableitem "Documentation"
	  (render-text docstring))))
    (let* ((nicknames (package-nicknames package))
	   (length (length nicknames)))
      (when nicknames
	(@tableitem (format nil "Nickname~p" length)
	  (if (eq length 1)
	      (format t "@t{~(~A~)}" (escape (first nicknames)))
	    (@itemize-list nicknames
			   :format "@t{~(~A~)}"
			   :key #'escape)))))
    (let* ((use-list (package-use-list package))
	   (length (length use-list)))
      (when use-list
	(@tableitem "Use List"
	  (if (eq length 1)
	      (format t "@t{~(~A~)}" (escape (first use-list)))
	    (@itemize-list (package-use-list package)
			   :format "@t{~(~A~)}"
			   :key #'escape)))))
    (render-source package context)
    (let ((external-definitions
	    (package-definitions package
				 (context-external-definitions context))))
      (when external-definitions
	(@tableitem "Exported definitions"
	  (@itemize-list
	   (sort external-definitions #'string-lessp :key #'definition-symbol)
	   :renderer #'reference))))
    (let ((internal-definitions
	    (package-definitions package
				 (context-internal-definitions context))))
      (when internal-definitions
	(@tableitem "Internal definitions"
	  (@itemize-list
	   (sort internal-definitions #'string-lessp :key #'definition-symbol)
	   :renderer #'reference))))))



;; ==========================================================================
;; Package Nodes
;; ==========================================================================

(defun add-packages-node
    (parent context
     &aux (packages-node
	   (add-child parent
	     (make-node :name "Packages"
			:synopsis "The packages documentation"
			:before-menu-contents (format nil "~
Packages are listed by definition order."))))
	  (packages (system-packages (context-system context))))
  "Add the packages node to PARENT in CONTEXT."
  (dolist (package packages)
    (add-child packages-node
      (make-node :name (escape (format nil "~@(~A~)" (title package)))
		 :section-name (format nil "@t{~(~A~)}" (escape package))
		 :before-menu-contents
		 (render-to-string (document package context))))))

;;; package.lisp ends here
