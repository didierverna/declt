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

(defgeneric name (object)
  (:documentation "Return OBJECT's name as a string.")
  (:method ((symbol symbol))
    "Return SYMBOL's name."
    (symbol-name symbol))
  (:method ((string string))
    "Return STRING."
    string)
  (:method ((pathname pathname))
    "Return PATHNAME's name."
    (namestring pathname)))



;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun escape (object)
  "When OBJECT, escape its name for Texinfo."
  (when object
    (coerce (loop :for char :across (name object)
		  :if (member char '(#\@ #\{ #\})) :collect #\@
		  :collect char)
	    'string)))

(defun first-word-length (string)
  (or (position-if (lambda (char) (member char '(#\space #\tab #\newline)))
		   (string-trim '(#\space #\tab #\newline) string))
      (length string)))

(defun read-next-line (stream)
  (multiple-value-list (read-line stream nil :eof)))

(defun render-text (string)
  "Render STRING attempting to embellish the output."
  (when string
    (with-input-from-string (str string)
      (loop :for (ln1 mnl1p) :=    (read-next-line str)
			     :then (list ln2 mnl2p)
	    :for (ln2 mnl2p) :=    (read-next-line str)
			     :then (read-next-line str)
	    :until (eq ln1 :eof)
	    :if (zerop (length ln1))
	      :do (terpri)
	    :else :if (eq ln2 :eof)
	      :do (format t "~A~@[~%~]" (escape ln1) (not mnl1p))
	    :else :if (> (- 78 (length ln1)) (first-word-length ln2))
	      :do (format t "~A@*~%" (escape ln1))
	    :else
	      :do (format t "~A~%" (escape ln1))))))

(defun @anchor (anchor)
  "Render ANCHOR."
  (format t "@anchor{~A}@c~%" anchor))

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

(defmacro @defvr (category name &body body)
  "Render BODY in a @defvr {CATEGORY} NAME environment."
  `(progn
    (format t "@defvr {~A} ~A~%" (escape ,category) (escape ,name))
    ,@body
    (format t "~&@end defvr~%")))

(defmacro @defconstant (name &body body)
  "Render BODY in a @defvr {Constant} NAME environment."
  `(@defvr "Constant" ,name ,@body))

(defmacro @defspecial (name &body body)
  "Render BODY in a @defvr {Special Variable} NAME environment."
  `(@defvr "Special Variable" ,name ,@body))

(defgeneric pretty-specializer (specializer)
  (:documentation "Returns a printable form for SPECIALIZER.")
  (:method (specializer)
    (or (ignore-errors (class-name specializer))
	specializer))
  ;; #### PORTME.
  (:method ((specializer sb-mop:eql-specializer))
    ;; #### PORTME.
    `(eql ,(sb-mop:eql-specializer-object specializer))))

;; Based on Edi Weitz's write-lambda-list* from documentation-template.
(defun render-lambda-list (lambda-list &optional specializers)
  "Render LAMBDA-LIST."
  (let ((firstp t)
	after-required-args-p)
    (dolist (part lambda-list)
      (when (and (consp part) after-required-args-p)
	(setq part (first part)))
      (unless firstp
	(write-char #\Space))
      (setq firstp nil)
      (cond ((consp part)
	     (write-char #\()
	     (render-lambda-list part)
	     (write-char #\)))
	    ((member part '(&optional &rest &key &allow-other-keys
			    &aux &environment &whole &body))
	     (setq after-required-args-p t)
	     (format t "~(~A~)" part))
	    (t
	     (let ((specializer (pretty-specializer (pop specializers))))
	       (if (and specializer (not (eq specializer t)))
		   (format t "(~A @t{~(~A~)})"
		     (escape part)
		     (escape specializer))
		 (write-string (escape (symbol-name part))))))))))

(defmacro @defmac (name lambda-list &body body)
  "Render BODY in a @defmac NAME LAMBDA-LIST environment."
  `(progn
    (format t "@defmac ~A " (escape ,name))
    (render-lambda-list ,lambda-list)
    (terpri)
    ,@body
    (format t "~&@end defmac~%")))

(defmacro @defun (name lambda-list &body body)
  "Render BODY in a @defun NAME LAMBDA-LIST environment."
  `(progn
    (format t "@defun ~A " (escape ,name))
    (render-lambda-list ,lambda-list)
    (terpri)
    ,@body
    (format t "~&@end defun~%")))

(defmacro @deffn ((category name lambda-list &optional specializers qualifiers)
		  &body body)
  "Render BODY in a @deffn CATEGORY NAME LAMBDA-LIST environment."
  `(progn
    (format t "@deffn {~A} ~A " (escape ,category) (escape ,name))
    (render-lambda-list ,lambda-list ,specializers)
    (format t "~(~{ @t{~A}~^~}~)~%" (mapcar #'escape ,qualifiers))
    ,@body
    (format t "~&@end deffn~%")))

(defmacro @defgeneric (name lambda-list &body body)
  "Render BODY in a @deffn {Generic Function} NAME LAMBDA-LIST environment."
  `(@deffn ("Generic Function" ,name ,lambda-list)
    ,@body))

(defmacro @defmethod (name lambda-list specializers qualifiers &body body)
  "Render BODY in a @deffn {Method} NAME LAMBDA-LIST environment."
  `(@deffn ("Method" ,name ,lambda-list ,specializers ,qualifiers)
    ,@body))

(defmacro @deftp (category name &body body)
  "Render BODY in a @deftp {CATEGORY} NAME environment."
  `(progn
    (format t "@deftp {~A} ~A~%"  (escape ,category) (escape ,name))
    ,@body
    (format t "~&@end deftp~%")))

(defmacro @defstruct (name &body body)
  "Render BODY in a @deftp {Structure} NAME environment."
  `(@deftp "Structure" ,name ,@body))

(defmacro @defcond (name &body body)
  "Render BODY in a @deftp {Condition} NAME environment."
  `(@deftp "Condition" ,name ,@body))

(defmacro @defclass (name &body body)
  "Render BODY in a @deftp {Class} NAME environment."
  `(@deftp "Class" ,name ,@body))

(defmacro render-to-string (&body body)
  "Render BODY to a string instead of *standard-output*."
  `(with-output-to-string (*standard-output*)
    ,@body))



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
