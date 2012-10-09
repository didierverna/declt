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

(defun render-use-list (list title context)
  "Render a package use/used-by LIST with TITLE in CONTEXT."
  (let ((length (length list)))
    (when list
      ;; #### NOTE: this is not as clean as for definitions. Definitions (in
      ;; fact, classoids currently) have a foreignp slot that helps the
      ;; REFERENCE method handle foreign definitions directly. We cannot do
      ;; that here because we're manipulating packages directly.
      (flet ((renderer (package)
	       (if (member package (context-packages context))
		   (reference package)
		 (format t "@t{~(~A~)}" (escape package)))))
	(@tableitem title
	  (if (eq length 1)
	      (renderer (first list))
	    (@itemize-list list :renderer #'renderer)))))))

(defmethod document ((package package) context)
  "Render PACKAGE's documentation in CONTEXT."
  (anchor package)
  (index package)
  (@table ()
    (render-docstring package)
    (render-source package context)
    (let* ((nicknames (package-nicknames package))
	   (length (length nicknames)))
      (when nicknames
	(@tableitem (format nil "Nickname~p" length)
	  (if (eq length 1)
	      (format t "@t{~(~A~)}" (escape (first nicknames)))
	    (@itemize-list nicknames
			   :format "@t{~(~A~)}"
			   :key #'escape)))))
    (render-use-list (package-use-list package) "Use List" context)
    (render-use-list (package-used-by-list package) "Used By List" context)
    (render-external-definitions-references
     (sort
      (package-definitions package (context-external-definitions context))
      #'string-lessp :key #'definition-symbol))
    (render-internal-definitions-references
     (sort
      (package-definitions package (context-internal-definitions context))
      #'string-lessp :key #'definition-symbol))))



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
Packages are listed by definition order.")))))
  "Add the packages node to PARENT in CONTEXT."
  (dolist (package (context-packages context))
    (add-child packages-node
      (make-node :name (escape (format nil "~@(~A~)" (title package)))
		 :section-name (format nil "@t{~(~A~)}" (escape package))
		 :before-menu-contents
		 (render-to-string (document package context))))))

;;; package.lisp ends here
