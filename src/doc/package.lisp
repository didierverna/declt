;;; package.lisp --- Package documentation

;; Copyright (C) 2010, 2011 Didier Verna

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


;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

(defmethod title ((package package) &optional relative-to)
  "Return PACKAGE's title."
  (declare (ignore relative-to))
  (format nil "The ~(~A~) package" (name package)))

(defmethod index ((package package) &optional relative-to)
  "Render PACKAGE's indexing command."
  (declare (ignore relative-to))
  (format t "@packageindex{~(~A~)}@c~%" (escape package)))

(defmethod reference ((package package) &optional relative-to)
  "Render PACKAGE's reference."
  (declare (ignore relative-to))
  (format t "@ref{~A, , @t{~(~A}~)}~%" (anchor-name package) (escape package)))

(defmethod document ((package package) system &key)
  "Render SYSTEM's PACKAGE documentation."
  (anchor package)
  (index package)
  (@table ()
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
    (render-source package system)
    ;; #### NOTE: a package documentation currently includes the list of
    ;; *symbols* in that package, not the corresponding definitions. This
    ;; means that methods don't appear in the list (because they are
    ;; referenced under the generic function definition object) and that only
    ;; standalone writers appear (because the other ones are referenced under
    ;; the accessor function definition). The type indication which appears in
    ;; parentheses is the one of the first available definition for the symbol
    ;; (see +CATEGORIES+ for the priority order).
    (let ((external-definitions
	   (sort (package-external-definitions package) #'string-lessp
		 :key (lambda (definitions)
			(definition-symbol (first definitions)))))
	  (internal-definitions
	   (sort (package-internal-definitions package) #'string-lessp
		 :key (lambda (definitions)
			(definition-symbol (first definitions))))))
      (when external-definitions
	(@tableitem "Exported symbols"
	  (@itemize ()
	    (dolist (definitions external-definitions)
	      (dolist (definition definitions)
		(@item (reference definition)))))))
      (when internal-definitions
	(@tableitem "Internal symbols"
	  (@itemize ()
	    (dolist (definitions internal-definitions)
	      (dolist (definition definitions)
		(@item (reference definition))))))))))



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
      (make-node :name (escape (title package))
		 :section-name (format nil "@t{~(~A~)}" (escape package))
		 :before-menu-contents
		 (render-to-string (document package system))))))


;;; package.lisp ends here
