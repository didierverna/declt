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
;; slot-unbound Proof Accessors
;; ==========================================================================

(defun defsystem-dependencies (system)
  "Return ASDF SYSTEM's defsystem dependencies."
  (when (slot-boundp system 'asdf::defsystem-depends-on)
    (asdf::system-defsystem-depends-on system)))



;; ==========================================================================
;; General
;; ==========================================================================

(defmacro endpush (object place)
  "Push OBJECT at the end of PLACE."
  `(setf ,place (nconc ,place (list ,object))))

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

;;; misc.lisp ends here
