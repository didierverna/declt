;;; package.lisp --- Package documentation

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Sep  1 16:04:00 2010
;; Last Revision: Sun Sep  5 21:54:36 2010

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
;; Documentation Protocols
;; ==========================================================================

(defmethod title ((package package) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~(~A~) package" (escape package)))

(defmethod anchor-name ((package package) &optional relative-to)
  "Return PACKAGE's anchor name."
  (declare (ignore relative-to))
  (format nil "~A anchor" (title package)))

(defmethod index ((package package) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@packageindex{~(~A~)}@c~%" (escape package)))

(defmethod reference ((package package) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@ref{~A, , @t{~(~A}~)}~%" (anchor-name package) (escape package)))

(defmethod document ((package package) relative-to &key)
  "Render PACKAGE's documentation."
  (@anchor (anchor-name package))
  (index package)
  (@table ()
    (let* ((nicknames (package-nicknames package))
	   (length (length nicknames)))
      (when nicknames
	(format t "@item Nickname~p~%" length)
	(if (eq length 1)
	    (format t "@t{~(~A~)}" (escape (first nicknames)))
	  (@itemize-list nicknames
	    :format "@t{~(~A~)}"
	    :key #'escape))))
    (let* ((use-list (package-use-list package))
	   (length (length use-list)))
      (when use-list
	(format t "@item Use List~%")
	(if (eq length 1)
	    (format t "@t{~(~A~)}" (escape (first use-list)))
	  (@itemize-list (package-use-list package)
	    :format "@t{~(~A~)}"
	    :key #'escape))))
    (render-source package relative-to)
    (let ((external-definitions
	   (sort (package-external-definitions package) #'string-lessp
		 :key (lambda (definitions)
			(definition-symbol (first definitions)))))
	  (internal-definitions
	   (sort (package-internal-definitions package) #'string-lessp
		 :key (lambda (definitions)
			(definition-symbol (first definitions))))))
      ;; #### NOTE: since methods are listed directly below the corresponding
      ;; generic function, we don't reference them here explicitely.
      (when external-definitions
	(format t "@item Exported symbols~%")
	(@itemize ()
	  (dolist (definitions external-definitions)
	    (format t "@item~%")
	    (reference (first definitions))
	    (dolist (remaining-definition (cdr definitions))
	      (write-string ", ")
	      (reference remaining-definition)))))
      (when internal-definitions
	(format t "@item Internal symbols~%")
	(@itemize ()
	  (dolist (definitions internal-definitions)
	    (format t "@item~%")
	    (reference (first definitions))
	    (dolist (remaining-definition (cdr definitions))
	      (write-string ", ")
	      (reference remaining-definition))))))))



;; ==========================================================================
;; Package Nodes
;; ==========================================================================

(defun add-packages-node
    (node system
     &aux (packages-node
	   (add-child node
	     (make-node :name "Packages"
			:synopsis "The packages documentation"
			:before-menu-contents (format nil "~
Packages are listed by definition order."))))
	  (packages (system-packages system)))
  "Add SYSTEM's packages node to NODE."
  (dolist (package packages)
    (add-child packages-node
      (make-node :name (title package)
		 :section-name (format nil "@t{~(~A~)}" (escape package))
		 :before-menu-contents
		 (render-to-string
		  (document package (system-directory system)))))))


;;; package.lisp ends here
