;;; misc.lisp --- Miscellaneous utilities

;; Copyright (C) 2010, 2011, 2013, 2015-2017, 2019-2021 Didier Verna

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

(in-package :net.didierverna.declt.setup)
(in-readtable :net.didierverna.declt)


;; ==========================================================================
;; Standard Wannabees
;; ==========================================================================

(defmacro while (test &body body)
  "Execute BODY while TEST."
  `(do () ((not ,test)) ,@body))

(defmacro endpush (object place)
  "Push OBJECT at the end of PLACE."
  `(setf ,place (nconc ,place (list ,object))))

#i(retain 2)
(defun retain (object list &key (test #'eq) key pre-test)
  "Return a copy of LIST from which only OBJECT is retained.
Each item in LIST is TESTed with EQ by default. TEST is performed on the item
itself by default, or on the result of applying KEY to it. Optionally, only
items satisfying PRE-TEST are considered."
  (loop :for element :in list
	:when (and (or (not pre-test) (funcall pre-test element))
		   (funcall test
		     (if key (funcall key element) element)
		     object))
	  :collect element))

#i(find* 2)
(defun find* (object list &key (test #'eq) key pre-test)
  "Return the first finding of OBJECT in LIST, or NIL.
Each item in LIST is TESTed with EQ by default. TEST is performed on the item
itself by default, or on the result of applying KEY to it. Optionally, only
items satisfying PRE-TEST are considered."
  (loop :for element :in list
	:when (and (or (not pre-test) (funcall pre-test element))
		   (funcall test
		     (if key (funcall key element) element)
		     object))
	  :do (return element)))

;; Based on public domain Alexandria / Quickutil version
(defmacro when-let (bindings &body body)
  "Execute BODY only when all BINDINGS are non-nil.
BINDINGS must be either a single binding of the form (VARIABLE VALUE),
or a list of such. VALUEs are computed sequentially in the specified order,
and then VARIABLEs are bound to the corresponding VALUEs. If all VALUEs are
non-nil, BODY is executed."
  (when (and (consp bindings) (symbolp (car bindings)))
    (setq bindings (list bindings)))
  (let ((variables (mapcar #'car bindings)))
    `(let ,bindings
       (when (and ,@variables)
	 ,@body))))

;; Based on public domain Alexandria / Quickutil version
(defmacro when-let* (bindings &body body)
  "Execute BODY only when all BINDINGS are non-nil.
BINDINGS must be either a single binding of the form (VARIABLE VALUE),
or a list of such. VARIABLEs are bound to their respective VALUE sequentially,
so that each VALUE expression may refer to a previously bound VARIABLE.
Execution stops completely as soon as a null VALUE is encountered. Otherwise,
BODY is executed as an implicit PROGN."
  (when (and (consp bindings) (symbolp (car bindings)))
    (setq bindings (list bindings)))
  (labels ((bind (bindings body)
	     (if bindings
	       `((let (,(car bindings))
		   (when ,(caar bindings)
		     ,@(bind (cdr bindings) body))))
	       body)))
    `(let (,(car bindings))
       (when ,(caar bindings)
	 ,@(bind (cdr bindings) body)))))

(defun mapcat (function &rest lists)
  "Short for \"mapconcat\": non destructive version of MAPCAN.
That is, concatenate the results with APPEND rather than NCONC."
  (loop :for list :in (apply #'mapcar function lists) :append list))




;; ==========================================================================
;; CLOS Utilities
;; ==========================================================================

;; --------------------
;; Portability wrappers
;; --------------------

(defmacro declare-valid-superclass (class superclass)
  "Validate SUPERCLASS classes for CLASS classes."
  ;; #### PORTME.
  `(defmethod validate-superclass ((class ,class) (superclass ,superclass))
     #+ecl (declare (ignore class superclass))
     t))



;; ----------------
;; Abstract classes
;; ----------------

(defclass abstract-class (standard-class)
  ()
  (:documentation "The Abstract Class meta-class."))

(defmacro defabstract (class super-classes slots &rest options)
  "Like DEFCLASS, but define an abstract class."
  (when (assoc :metaclass options)
    (error "Defining abstract class ~S: explicit meta-class option." class))
  `(defclass ,class ,super-classes ,slots ,@options
     (:metaclass abstract-class)))

(defmethod make-instance ((class abstract-class) &rest initargs)
  (declare (ignore initargs))
  (error "Instanciating class ~S: is abstract." (class-name class)))

(declare-valid-superclass abstract-class standard-class)
(declare-valid-superclass standard-class abstract-class)




;; ==========================================================================
;; Other Additions
;; ==========================================================================

(defun non-empty-string-p (object)
  "Return T if OBJECT is a non-empty string."
  (and (stringp object) (not (zerop (length object)))))

(deftype non-empty-string () '(satisfies non-empty-string-p))

;;; util.lisp ends here
