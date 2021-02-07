;;; misc.lisp --- Miscellaneous utilities

;; Copyright (C) 2010, 2011, 2013, 2015-2017, 2019, 2020 Didier Verna

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


;; ==========================================================================
;; General
;; ==========================================================================

(defun current-time-string ()
  "Return the current time as a string."
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore dst-p))
    (format nil "~A ~A ~2,'0D ~2,'0D:~2,'0D:~2,'0D ~D GMT~@D"
      (nth day-of-week '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
      (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
			"Sep" "Oct" "Nov" "Dec"))
      date
      hour
      minute
      second
      year
      (- tz))))

(defun non-empty-string-p (object)
  "Return T if OBJECT is a non-empty string."
  (and (stringp object) (not (zerop (length object)))))

(deftype non-empty-string () '(satisfies non-empty-string-p))

(defun one-liner-p (string)
  "Return T if STRING is non empty and does not span multiple lines."
  (and string (not (or (zerop (length string)) (find #\Newline string)))))


;; #### NOTE: the functions below are relatively robust, without using the
;; whole regexp artillery.
(defun validate-email
    (string
     &aux (string (string-trim '(#\Space #\Tab) string))
	  (@-position (position #\@ string)))
  "Check that STRING is of the form nonblank@nonblank, after trimming.
Return that string, or issue a warning and return NIL."
  (if (and @-position (> @-position 0) (< @-position (1- (length string))))
    string
    (warn "Invalid email address: ~A." string)))

(defun parse-contact-string
    (string &aux (pos-< (position #\< string))
		 (pos-> (position #\> string)))
  "Parse STRING as \"My Name <my@address>\".
Both name and address are optional. If only an address is provided, the angle
brackets may be omitted. Return name and address, as two potentially NIL
values."
  (when (one-liner-p string)
    (cond ((and pos-< pos-> (< pos-< pos->))
	   (let ((name (if (zerop pos-<)
			 ""
			 (string-trim '(#\Space #\Tab)
				      (subseq string 0 (1- pos-<))))))
	     (when (zerop (length name))
	       (setq name nil))
	     (values name (validate-email (subseq string (1+ pos-<) pos->)))))
	  ((position #\@ string)
	   (values nil (validate-email string)))
	  (t
	   (setq string (string-trim '(#\Space #\Tab) string))
	   (values (unless (zerop (length string)) string) nil)))))

(defun |parse-contact(s)| (|contact(s)|)
  "Parse CONTACT(S) as either a contact string, or a list of such.
A contact string is of the form \"My Name <my@address>\", both name and
address being optional.

Return two values: a list of name(s) and a list of address(es). The two lists
maintain correspondence between names and addresses: they are of the same
length and may contain null elements, for contact strings lacking either
one."
  (if (stringp |contact(s)|)
    (multiple-value-bind (name email) (parse-contact-string |contact(s)|)
      (when (or name email)
	(values (list name) (list email))))
    (loop :for contact-string
	    :in (remove-duplicates |contact(s)| :from-end t :test #'string=)
	  :for (name email)
	    := (multiple-value-list (parse-contact-string contact-string))
	  :when (or name email)
	    :collect name :into names :and :collect email :into emails
	  :finally (return (values names emails)))))

;; #### PORTME.
(defun object-source-pathname (object)
  "Return OBJECT's source pathname."
  (when-let (source (sb-introspect:find-definition-source object))
    (sb-introspect:definition-source-pathname source)))



;; ==========================================================================
;; CLOS Utility Routines
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


;;; misc.lisp ends here
