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
;; Texinfo Rendering Routines
;; ===========================================================================

(defun escape (string)
  "Return STRING with @ { and } characters escaped."
  (when string
    (coerce (loop :for char :across string
		  :if (member char '(#\@ #\{ #\})) :collect #\@
		  :collect char)
	    'string)))

(defun render-string (string)
  "Render STRING attempting to embellish the output."
  (when string
    (loop :for char :across (escape string)
	  :if (char= char #\Newline)
	    :do (progn (write-char #\@) (write-char #\*))
	  :do (write-char char))))

(defmacro @itemize ((stream &optional (kind :@bullet)) &body body)
  "Execute BODY in an @itemize KIND environment."
  `(progn
    (format ,stream "@itemize ~(~A~)~%" ,kind)
    ,@body
    (format ,stream "~&@end itemize~%")))

(defun @itemize-list
    (stream list &key (kind :@bullet) (format "~A") (key #'identity))
  "Render every LIST element in an @itemize KIND environment."
  (@itemize (stream kind)
    (dolist (elt list)
      (format stream "@item~%")
      (apply #'format stream format (multiple-value-list (funcall key elt)))
      (terpri stream))))

(defmacro @defvr ((stream category name) &body body)
  "Execute BODY in a @defvr environment."
  `(progn
    (format ,stream "@defvr {~A} ~A~%" ,category ,name)
    ,@body
    (format ,stream "~&@end defvr~%")))

(defmacro @defconstant ((stream name) &body body)
  "Execute BODY withing a @defvr {Constant} environment."
  `(@defvr (,stream "Constant" ,name) ,@body))

(defmacro @defspecial ((stream name) &body body)
  "Execute BODY withing a @defvr {Special Variable} environment."
  `(@defvr (,stream "Special Variable" ,name) ,@body))

(defmacro @defmac ((stream name lambda-list) &body body)
  "Execute BODY withing a @defmac environment."
  (let ((the-name (gensym "name")))
    `(let ((,the-name ,name))
      (format ,stream "@defmac ~A " ,the-name)
      (render-lambda-list ,stream ,lambda-list)
      (terpri ,stream)
      (format ,stream "@findex @r{Macro, }~A~%" ,the-name)
      ,@body
      (format ,stream "~&@end defmac~%"))))

(defmacro @defun ((stream name lambda-list) &body body)
  "Execute BODY withing a @defun environment."
  (let ((the-name (gensym "name")))
    `(let ((,the-name ,name))
      (format ,stream "@defun ~A " ,the-name)
      (render-lambda-list ,stream ,lambda-list)
      (terpri ,stream)
      (format ,stream "@findex @r{Function, }~A~%" ,the-name)
      ,@body
      (format ,stream "~&@end defun~%"))))

(defmacro @deffn
    ((stream category name lambda-list &optional specializers qualifiers)
     &body body)
  "Execute BODY withing a @defun environment."
  (let ((the-name (gensym "name")))
    `(let ((,the-name ,name))
      (format ,stream "@deffn {~A} ~A " ,category ,the-name)
      (render-lambda-list ,stream ,lambda-list ,specializers)
      (format ,stream "~(~{ @t{~S}~^~}~)~%" ,qualifiers)
      (terpri ,stream)
      (format ,stream "@findex @r{~A, }~A~%" ,category ,the-name)
      ,@body
      (format ,stream "~&@end deffn~%"))))

(defmacro @defgeneric ((stream name lambda-list) &body body)
  "Execute BODY withing a @deffn {Generic Function} environment."
  `(@deffn (,stream "Generic Function" ,name ,lambda-list) ,@body))

(defmacro @defmethod
    ((stream name lambda-list specializers qualifiers) &body body)
  "Execute BODY withing a @deffn {Method} environment."
  `(@deffn (,stream "Method" ,name ,lambda-list ,specializers ,qualifiers)
    ,@body))

(defmacro @deftp ((stream category name) &body body)
  "Execute BODY withing a @deftp environment."
  (let ((the-name (gensym "name")))
    `(let ((,the-name ,name))
      (format ,stream "@deftp {~A} ~A~%" ,category ,the-name)
      (format ,stream "@tpindex @r{~A, }~A~%" ,category ,the-name)
      ,@body
      (format ,stream "~&@end deftp~%"))))

(defmacro @defstruct ((stream name) &body body)
  "Execute BODY withing a @deftp {Structure} environment."
  `(@deftp (,stream "Structure" ,name) ,@body))

(defmacro @defcond ((stream name) &body body)
  "Execute BODY withing a @deftp {Condition} environment."
  `(@deftp (,stream "Condition" ,name) ,@body))

(defmacro @defclass ((stream name) &body body)
  "Execute BODY withing a @deftp {Class} environment."
  `(@deftp (,stream "Class" ,name) ,@body))



;; ==========================================================================
;; Rendering Protocols
;; ==========================================================================

;; -----------------
;; Indexing protocol
;; -----------------

(defgeneric index (stream item)
  (:documentation "Render an index command for ITEM on STREAM."))


;; ---------------------
;; Tableization protocol
;; ---------------------

(defgeneric document (stream item &optional relative-to)
  (:documentation "Render ITEM's documentation to STREAM.
ITEM's location is displayed RELATIVE-TO when appropriate.")
  (:method :before (stream item &optional relative-to)
    (declare (ignore relative-to))
    (format stream "@table @strong~%"))
  (:method :after (stream component &optional relative-to)
    (declare (ignore relative-to))
    (format stream "@end table~%")))



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



;;; Local Variables:
;;; eval: (put '@defvr       'common-lisp-indent-function 1)
;;; eval: (put '@defconstant 'common-lisp-indent-function 1)
;;; eval: (put '@defspecial  'common-lisp-indent-function 1)
;;; eval: (put '@defmac      'common-lisp-indent-function 1)
;;; eval: (put '@defun       'common-lisp-indent-function 1)
;;; eval: (put '@deffn       'common-lisp-indent-function 1)
;;; eval: (put '@defgeneric  'common-lisp-indent-function 1)
;;; eval: (put '@defmethod   'common-lisp-indent-function 1)
;;; eval: (put '@deftp       'common-lisp-indent-function 1)
;;; eval: (put '@defstruct   'common-lisp-indent-function 1)
;;; eval: (put '@defcond     'common-lisp-indent-function 1)
;;; eval: (put '@defclass    'common-lisp-indent-function 1)
;;; End:

;;; texi.lisp ends here
