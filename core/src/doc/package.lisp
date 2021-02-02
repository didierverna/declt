;;; package.lisp --- Package documentation

;; Copyright (C) 2010-2013, 2015-2017, 2020 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Declt.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:



;;; Code:

(in-package :net.didierverna.declt)
(in-readtable :net.didierverna.declt)


;; ==========================================================================
;; Rendering Protocols
;; ==========================================================================

(defmethod type-name ((package-definition package-definition))
  "Return \"package\"."
  "package")



;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

(defmethod index ((definition package-definition))
  "Render package DEFINITION's indexing command."
  (format t "@packageindex{~(~A~)}@c~%" (escape (safe-name definition))))

(defmethod document ((definition package-definition) context &key)
  "Render package DEFINITION's documentation in context."
  (anchor-and-index definition)
  (render-docstring definition)
  (@table ()
    (when-let (source (source-file definition))
      (@tableitem "Source" (reference source)))
    (when-let* ((nicknames (nicknames definition))
		(length (length nicknames)))
      (@tableitem (format nil "Nickname~p" length)
	(if (eq length 1)
	    (format t "@t{~(~A~)}" (escape (first nicknames)))
	    (@itemize-list nicknames :format "@t{~(~A~)}" :key #'escape))))
    (render-references
     (use-definitions definition) "Use List")
    (render-references
     (used-by-definitions definition) "Used By List")
    (render-references (public-definitions definition) "Public Interface")
    (render-references (private-definitions definition) "Internals")))



;; ==========================================================================
;; Package Nodes
;; ==========================================================================

(defun add-packages-node (parent extract context)
  "Add the packages node to PARENT in EXTRACT."
  (when-let (definitions
	     (remove-if-not #'package-definition-p (definitions extract)))
    (let ((packages-node
	    (add-child parent
	      (make-node :name "Packages"
			 :synopsis "The packages documentation"
			 :before-menu-contents (format nil "~
Packages are listed by definition order.")))))
      (dolist (definition definitions)
	#+()(remove-if #'foreignp package-definitions)
	(add-child packages-node
	  (make-node :name (long-title definition)
		     :section-name (format nil "@t{~(~A~)}"
				     (escape (safe-name definition t)))
		     :before-menu-contents
		     (render-to-string (document definition context))))))))

;;; package.lisp ends here
