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

;; #### FIXME: this needs to go away at some point, when package definitions
;; are in complete use. Or not?
(defmethod name ((package package))
  "Return PACKAGE's name."
  (reveal (package-name package)))

(defmethod name ((package-definition package-definition))
  "Return PACKAGE-DEFINITION's name."
  (reveal (package-name (package-definition-package package-definition))))



;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

(defmethod title ((package-definition package-definition))
  "Return PACKAGE's title."
  (format nil "~(~A~)" (name package-definition)))

;; #### FIXME: this needs to go away at some point, when package definitions
;; are in complete use.
(defmethod anchor-name ((package package))
  "Return PACKAGE's anchor name."
  (name package))

(defmethod anchor-name ((package-definition package-definition))
  "Return PACKAGE-DEFINITION's anchor name."
  (name package-definition))

(defmethod index ((package-definition package-definition))
  "Render PACKAGE-DEFINITION's indexing command."
  (format t "@packageindex{~(~A~)}@c~%" (escape package-definition)))

;; #### FIXME: this needs to go away at some point, when package definitions
;; are in complete use.
(defmethod reference ((package package))
  "Render PACKAGE's reference."
  (@ref (anchor-name package) package)
  (terpri))

;; #### NOTE: contrary to other REFERENCE methods, we don't specify the type
;; in parenthesis here, i.e. "(package)", because it is never ambiguous what
;; the item is in the documentation.
(defmethod reference ((package-definition package-definition))
  "Render PACKAGE-DEFINITION's reference."
  (@ref (anchor-name package-definition) package-definition)
  (terpri))

(defun render-use-list (list title extract)
  "Render a package use/used-by LIST with TITLE in EXTRACT."
  (when list
    ;; #### FIXME: the comment below is obsolete. We can now add a foreignp
    ;; slot to package definitions, implement a finalize function for package
    ;; definitions and sort out all those mutual references there.
    ;; #### NOTE: this is not as clean as for definitions. Definitions (in
    ;; fact, classoids currently) have a foreignp slot that helps the
    ;; REFERENCE method handle foreign definitions directly. We cannot do that
    ;; here because we're manipulating packages directly.
    (flet ((renderer (package)
	     (let ((package-definition
		     (find package (package-definitions extract)
			   :key #'package-definition-package)))
	       (if package-definition
		 (reference package-definition)
		 (format t "@t{~(~A~)}" (escape package))))))
      (@tableitem title
	(if (eq (length list) 1)
	    (renderer (first list))
	    (@itemize-list list :renderer #'renderer))))))

(defmethod document ((package-definition package-definition) extract &key)
  "Render PACKAGE-DEFINITION's documentation in EXTRACT."
  (anchor package-definition)
  (index package-definition)
  (render-docstring package-definition)
  (@table ()
    (render-source package-definition extract)
    (when-let* ((nicknames (package-definition-nicknames package-definition))
		(length (length nicknames)))
      (@tableitem (format nil "Nickname~p" length)
	(if (eq length 1)
	    (format t "@t{~(~A~)}" (escape (first nicknames)))
	    (@itemize-list nicknames
			   :format "@t{~(~A~)}"
			   :key #'escape))))
    (render-use-list (package-use-list
		      (package-definition-package package-definition))
		     "Use List" extract)
    (render-use-list (package-used-by-list
		      (package-definition-package package-definition))
		     "Used By List" extract)
    (render-external-definitions-references
     (sort
      (package-definition-definitions
       package-definition (external-definitions extract))
      #'string-lessp :key #'definition-symbol))
    (render-internal-definitions-references
     (sort
      (package-definition-definitions
       package-definition (internal-definitions extract))
      #'string-lessp :key #'definition-symbol))))



;; ==========================================================================
;; Package Nodes
;; ==========================================================================

(defun add-packages-node
    (parent extract &aux (package-definitions (package-definitions extract)))
  "Add the packages node to PARENT in EXTRACT."
  (when package-definitions
    (let ((packages-node
	    (add-child parent
	      (make-node :name "Packages"
			 :synopsis "The packages documentation"
			 :before-menu-contents (format nil "~
Packages are listed by definition order.")))))
      (dolist (package-definition package-definitions)
	(add-child packages-node
	  (make-node :name (format nil "~@(~A~)"
			     (title package-definition))
		     :section-name (format nil "@t{~(~A~)}"
				     (escape package-definition))
		     :before-menu-contents
		     (render-to-string
		       (document package-definition extract))))))))

;;; package.lisp ends here
