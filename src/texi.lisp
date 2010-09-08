;;; texi.lisp --- Texinfo format utilities

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Aug 24 11:48:19 2010
;; Last Revision: Wed Sep  8 14:28:52 2010

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


;; ==========================================================================
;; Rendering Protocols
;; ==========================================================================

(defgeneric escape (object)
  (:documentation
   "Return a printable form of OBJECT with @ { and } characters escaped.
If OBJECT is nil, return nil.")
  (:method (object)
    "Convert non-string OBJECTs as follows:
- SYMBOL -> SYMBOL-NAME,
- PACKAGE -> PACAKGE-NAME,
- PATHNAME -> NAMESTRING."
    (etypecase object
      (string object)
      (symbol (symbol-name object))
      (package (package-name object))
      (pathname (namestring object))))
  (:method :around (object)
    "Escape the conversion of non-nil OBJECT to string."
    (when object
      (coerce (loop :for char :across (call-next-method)
		      :if (member char '(#\@ #\{ #\})) :collect #\@
		      :collect char)
	      'string))))

(defun render-string (string)
  "Render STRING attempting to embellish the output."
  (when string
    (loop :for char :across (escape string)
	  :if (char= char #\Newline)
	    :do (progn (write-char #\@) (write-char #\*))
	  :do (write-char char))))

(defmacro @table ((&optional (kind :@strong)) &body body)
  "Render BODY in a @table KIND environment."
  `(progn
    (format t "@table ~(~A~)~%" ,kind)
    ,@body
    (format t "~&@end table~%")))

(defmacro @itemize ((&optional (kind :@bullet)) &body body)
  "Render BODY in an @itemize KIND environment."
  `(progn
    (format t "@itemize ~(~A~)~%" ,kind)
    ,@body
    (format t "~&@end itemize~%")))

(defun @itemize-list
    (list &key renderer (kind :@bullet) (format "~A") (key #'identity))
  "Render every LIST element in an @itemize KIND environment.
- If RENDERER is non-nil, it is a function of one argument (every LIST
  element) that performs the rendering directly. Otherwise, FORMAT is used.
- FORMAT is the format string to use when RENDERER is null.
- KEY is the function used to provide FORMAT with the necessary arguments.
  Multiple arguments should be returned as multiple values."
  (@itemize (kind)
    (dolist (elt list)
      (format t "~&@item~%")
      (if renderer
	  (funcall renderer elt)
	(apply #'format t format (multiple-value-list (funcall key elt)))))))

(defmacro render-to-string (&body body)
  "Render BODY to a string instead of *standard-output*."
  `(with-output-to-string (*standard-output*)
    ,@body))

(defvar *link-files* t
  "Whether to create links to files or directories in the reference manual.
When true (the default), pathnames are made clickable although the links are
specific to this particular installation.

Setting this to NIL is preferable for creating reference manuals meant to put
online, and hence independent of any specific installation.")

(defun render-location (item relative-to)
  "Render an itemized location line for ITEM RELATIVE-TO."
  (let ((location (location item)))
    (when location
      (format t "@item Location~%~
		 ~@[@url{file://~A, ignore, ~]@t{~A}~:[~;}~]~%"
	(when *link-files*
	  (escape location))
	(escape (relative-location location relative-to))
	*link-files*))))



;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

;; -----------------
;; Indexing protocol
;; -----------------

(defgeneric index (item &optional relative-to)
  (:documentation "Render ITEM's indexing command."))


;; --------------------
;; Referencing protocol
;; --------------------

(defgeneric reference (item &optional relative-to)
  (:documentation "Render ITEM's reference.")
  (:method :before (item &optional relative-to)
    (index item relative-to)))



;; ==========================================================================
;; Node Implementation
;; ==========================================================================

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
      (format t "* ~A::~@[ ~A~]~%" (node-name child) (node-synopsis child)))
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



;;; Local Variables:
;;; eval: (put '@itemize      'common-lisp-indent-function 1)
;;; eval: (put '@table        'common-lisp-indent-function 1)
;;; eval: (put '@itemize-list 'common-lisp-indent-function 1)
;;; eval: (put '@defvr        'common-lisp-indent-function 1)
;;; eval: (put '@defconstant  'common-lisp-indent-function 1)
;;; eval: (put '@defspecial   'common-lisp-indent-function 1)
;;; eval: (put '@defmac       'common-lisp-indent-function 2)
;;; eval: (put '@defun        'common-lisp-indent-function 2)
;;; eval: (put '@deffn        'common-lisp-indent-function 3)
;;; eval: (put '@defgeneric   'common-lisp-indent-function 2)
;;; eval: (put '@defmethod    'common-lisp-indent-function 4)
;;; eval: (put '@deftp        'common-lisp-indent-function 2)
;;; eval: (put '@defstruct    'common-lisp-indent-function 1)
;;; eval: (put '@defcond      'common-lisp-indent-function 1)
;;; eval: (put '@defclass     'common-lisp-indent-function 1)
;;; eval: (put 'add-child     'common-lisp-indent-function 1)
;;; End:

;;; texi.lisp ends here
