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

(defmethod name ((package package))
  "Return PACKAGE's name."
  (reveal (package-name package)))



;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

(defmethod title ((package package))
  "Return PACKAGE's title."
  (format nil "~(~A~)" (name package)))

(defmethod anchor-name ((package package))
  "Return PACKAGE's anchor name."
  (name package))

(defmethod index ((package package))
  "Render PACKAGE's indexing command."
  (format t "@packageindex{~(~A~)}@c~%" (escape package)))

;; #### NOTE: contrary to other REFERENCE methods, we don't specify the type
;; in parenthesis here, i.e. "(package)", because it is never ambiguous what
;; the item is in the documentation.
(defmethod reference ((package package))
  "Render PACKAGE's reference."
  (@ref (anchor-name package) package)
  (terpri))

(defun render-use-list (list title extract)
  "Render a package use/used-by LIST with TITLE in EXTRACT."
  (when list
    ;; #### NOTE: this is not as clean as for definitions. Definitions (in
    ;; fact, classoids currently) have a foreignp slot that helps the
    ;; REFERENCE method handle foreign definitions directly. We cannot do that
    ;; here because we're manipulating packages directly.
    (flet ((renderer (package)
	     (if (member package (packages extract))
		 (reference package)
		 (format t "@t{~(~A~)}" (escape package)))))
      (@tableitem title
	(if (eq (length list) 1)
	    (renderer (first list))
	    (@itemize-list list :renderer #'renderer))))))

(defmethod document ((package package) extract &key)
  "Render PACKAGE's documentation in EXTRACT."
  (anchor package)
  (index package)
  (render-docstring package)
  (@table ()
    (render-source package extract)
    (when-let* ((nicknames (package-nicknames package))
		(length (length nicknames)))
      (@tableitem (format nil "Nickname~p" length)
	(if (eq length 1)
	    (format t "@t{~(~A~)}" (escape (first nicknames)))
	    (@itemize-list nicknames
			   :format "@t{~(~A~)}"
			   :key #'escape))))
    (render-use-list (package-use-list package) "Use List" extract)
    (render-use-list (package-used-by-list package) "Used By List" extract)
    (render-external-definitions-references
     (sort
      (package-definitions package (external-definitions extract))
      #'string-lessp :key #'definition-symbol))
    (render-internal-definitions-references
     (sort
      (package-definitions package (internal-definitions extract))
      #'string-lessp :key #'definition-symbol))))



;; ==========================================================================
;; Package Nodes
;; ==========================================================================

(defun add-packages-node
    (parent extract &aux (packages (packages extract)))
  "Add the packages node to PARENT in EXTRACT."
  (when packages
    (let ((packages-node
	    (add-child parent
	      (make-node :name "Packages"
			 :synopsis "The packages documentation"
			 :before-menu-contents (format nil "~
Packages are listed by definition order.")))))
      (dolist (package packages)
	(add-child packages-node
	  (make-node :name (format nil "~@(~A~)" (title package))
		     :section-name (format nil "@t{~(~A~)}" (escape package))
		     :before-menu-contents
		     (render-to-string (document package extract))))))))

;;; package.lisp ends here
