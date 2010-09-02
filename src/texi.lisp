;;; texi.lisp --- Texinfo format routines

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Aug 24 11:48:19 2010
;; Last Revision: Tue Aug 24 15:29:38 2010

;; This file is part of Declt.

;; Declt is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

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


;; ===========================================================================
;; Texinfo Node Implementation
;; ===========================================================================

(define-constant +section-names+
  '((:numbered   nil
     "chapter"    "section"       "subsection"       "subsubsection")
    (:unnumbered "top"
     "unnumbered" "unnumberedsec" "unnumberedsubsec" "unnumberedsubsubsec")
    (:appendix   nil
     "appendix"   "appendixsec"   "appendixsubsec"   "appendixsubsubsec" ))
  "The numbered, unumbered and appendix section names sorted by level.")

(defvar *top-node* nil
  "The Top node.")

(defstruct node
  name synopsis
  (section-type :numbered) section-name
  next previous up
  children
  before-menu-contents after-menu-contents)

(defun add-child (parent child)
  "Add CHILD node to PARENT node and return CHILD."
  (let ((previous (car (last (node-children parent)))))
    (cond (previous
	   (setf (node-next previous) child)
	   (endpush child (node-children parent)))
	  (t
	   (setf (node-children parent) (list child))))
    (setf (node-previous child) previous)
    (setf (node-up child) parent))
  child)



;; ===========================================================================
;; Texinfo Rendering Routines
;; ===========================================================================

(defun texify (string)
  "Escape @ { and } characters for Texinfo format."
  (when string
    (coerce (loop :for char :across string
		  :if (member char '(#\@ #\{ #\})) :collect #\@
		  :collect char)
	    'string)))

(defun pretty-texify (string)
  "Attempt to embellish STRING for Texinfo format."
  (coerce (loop :for char :across (texify string)
		:if (char= char #\Newline) :collect #\@ :and :collect #\*
		:collect char)
	  'string))

(defun render-node (node level)
  "Render NODE at LEVEL and all its children at LEVEL+1."
  (cond ((<= level 1)
	 (format t "


@c ====================================================================
@c ~A
@c ====================================================================~%"
	   (node-name node)))
	((= level 2)
	 (let ((separator (make-string (length (node-name node))
			    :initial-element #\-)))
	   (format t
	       "

@c ~A
@c ~A
@c ~A~%"
	     separator (node-name node) separator)))
	(t (terpri)))
  (when (= level 0)
    (format t "@ifnottex~%"))
  (format t "@node ~A, ~@[~A~], ~@[~A~], ~A~%"
    (node-name node)
    (or (when (= level 0)
	  (node-name (car (node-children node))))
	(when (node-next node)
	  (node-name (node-next node))))
    (or (when (= level 0)
	  "(dir)")
	(when (node-previous node)
	  (node-name (node-previous node)))
	(node-name (node-up node)))
    (if (= level 0)
	"(dir)"
      (node-name (node-up node))))
  (format t "@~A ~A~%~%"
    (nth level (cdr (assoc (node-section-type node) +section-names+)))
    (or (node-section-name node) (node-name node)))
  (when (node-before-menu-contents node)
    (write-string (node-before-menu-contents node))
    (fresh-line))
  (when (node-children node)
    (when (node-before-menu-contents node)
      (terpri))
    (format t "@menu~%")
    (dolist (child (node-children node))
      ;; #### FIXME: this could be improved with proper alignment of synopsis.
      (format t "* ~A:: ~@[~A~]~%" (node-name child) (node-synopsis child)))
    (format t "@end menu~%"))
  (when (node-after-menu-contents node)
    (when (or (node-children node) (node-before-menu-contents node))
      (terpri))
    (write-string (node-after-menu-contents node))
    (fresh-line))
  (when (= level 0)
    (format t "@end ifnottex~%"))
  (dolist (child (node-children node))
    (render-node child (1+ level))))

(defun render-nodes ()
  "Render the whole nodes hierarchy."
  (render-node *top-node* 0)
  (values))


;;; texi.lisp ends here
