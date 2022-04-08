;;; package.lisp --- Package documentation

;; Copyright (C) 2010-2013, 2015-2017, 2020, 2021 Didier Verna

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
;; Documentation Protocols
;; ==========================================================================

(defmethod category-name ((definition package-definition))
  "Return \"package\"."
  "package")

(defmethod index-command-name ((definition package-definition))
  "Return \"packageindex\"."
  "packageindex")

(defmethod document ((definition package-definition) context &key)
  "Render package DEFINITION's documentation in context."
  (anchor-and-index definition)
  (render-docstring definition)
  (table ()
    (when-let (source (source-file definition))
      (item ("Source") (reference source context t)))
    (when-let* ((nicknames (nicknames definition))
		(length (length nicknames)))
      (item ((format nil "Nickname~p" length))
	(if (eq length 1)
	  (format t "@t{~(~A~)}" (escape (first nicknames)))
	  (itemize-list nicknames :format "@t{~(~A~)}" :key #'escape))))
    ;; #### WARNING: casing policy.
    (render-references "Use List"
      (sort (use-list definition) #'string-lessp :key #'name)
      context
      t)
    ;; #### WARNING: casing policy.
    (render-references "Used By List"
      (sort (used-by-list definition)  #'string-lessp :key #'name)
      context
      t)
    ;; #### NOTE: classoids and their slots are documented in a single bloc.
    ;; As a consequence, if a classoid belongs to this package, there's no
    ;; need to also reference (some of) its slots. On the other hand, we need
    ;; to reference slots for which the owner is elsewhere (admittedly, and
    ;; for the same reason, only one would suffice). In the case of generic
    ;; functions, methods don't need to be referenced at all, because they
    ;; share the same name.
    (flet ((organize-definitions (definitions)
	     (sort (remove-if
		       (lambda (definition)
			 (or (typep definition 'method-definition)
			     (and (typep definition 'slot-definition)
				  (eq (home-package definition)
				      (home-package (owner definition))))))
		       definitions)
		 #'string-lessp ;; #### WARNING: casing policy.
	       :key #'definition-symbol)))
      (render-references "Public Interface"
	(organize-definitions (public-definitions definition))
	context)
      (render-references "Internals"
	(organize-definitions (private-definitions definition))
	context))))



;; ==========================================================================
;; Package Nodes
;; ==========================================================================

(defun add-packages-node (parent report context)
  "Add REPORT's packages node to PARENT in CONTEXT."
  (when-let (definitions
	     (remove-if-not #'package-definition-p (definitions report)))
    (unless (and (every #'foreignp definitions)
		 (not (foreign-definitions context)))
      (let ((packages-node
	      (add-child parent
		(make-node :name "Packages"
			   :synopsis "The packages documentation"
			   :before-menu-contents (format nil "~
Packages are listed by definition order.")))))
	(dolist (definition definitions)
	  (let ((contents (render-to-string (document definition context))))
	    (unless (zerop (length contents))
	      (add-child packages-node
		(make-node :name (long-title definition)
			   :section-name (format nil "@t{~(~A~)}"
					   (escape (safe-name definition t)))
			   :before-menu-contents contents)))))))))

;;; package.lisp ends here
