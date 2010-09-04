;;; package.lisp --- Package documentation

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Sep  1 16:04:00 2010
;; Last Revision: Wed Sep  1 17:44:46 2010

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


;;; Code:

(in-package :com.dvlsoft.declt)


;; ==========================================================================
;; Rendering Protocols
;; ==========================================================================

;; -----------------
;; Indexing protocol
;; -----------------

(defmethod index (stream (package package))
  (format stream "@packageindex{~(~A~)}@c~%" (package-name package)))


;; --------------------
;; Itemization protocol
;; --------------------

(defmethod itemize (stream (package package))
  (write-string "package" stream))


;; ---------------------
;; Tableization protocol
;; ---------------------

(defmethod tableize (stream (package package) &optional relative-to)
  "Describe PACKAGE's components."
  (declare (ignore relative-to))
  (when (package-nicknames package)
    (format stream "@item Nicknames~%~A~%"
      (list-to-string
       (mapcar (lambda (nickname)
		 (format nil "@t{~(~A~)}" nickname))
	       (package-nicknames package)))))
  (when (package-use-list package)
    (format stream "@item Use List~%~A~%"
      (list-to-string
       (mapcar (lambda (package)
		 (format nil "@t{~(~A~)}" (package-name package)))
	       (package-use-list package))))))



;; ==========================================================================
;; Package Nodes
;; ==========================================================================

(defun add-packages-node
    (node system
     &aux (packages-node
	   (add-child node (make-node :name "Packages"
				      :synopsis "The system's packages"
				      :before-menu-contents (format nil "~
Packages are listed by definition order."))))
	  (packages (system-packages system)))
  "Add SYSTEM's packages node to NODE."
  (dolist (package packages)
    (let ((package-node
	   (add-child packages-node
		      (make-node :name (package-name package)
				 :section-name (format nil "@t{~(~A~)}"
						 (package-name package))
				 :before-menu-contents
				 (with-output-to-string (str)
				   (tableize str package)))))
	  (external-symbols (package-external-symbols package))
	  (internal-symbols (package-internal-symbols package)))
      (when external-symbols
	(add-child package-node
		   (make-node
		    :name (format nil "@t{~(~A~)} External Symbols"
			    (package-name package))
		    :section-name "External Symbols"
		    :before-menu-contents
		    "Symbols are listed by lexicographic order."
		    :after-menu-contents
		    (with-output-to-string (str)
		      (dolist (symbol external-symbols))))))
      (when internal-symbols
	(add-child package-node
		   (make-node
		    :name (format nil "@t{~(~A~)} Internal Symbols"
			    (package-name package))
		    :section-name "Internal Symbols"
		    :before-menu-contents
		    "Symbols are listed by lexicographic order."
		    :after-menu-contents
		    (with-output-to-string (str)
		      (dolist (symbol internal-symbols)))))))))


;;; package.lisp ends here
