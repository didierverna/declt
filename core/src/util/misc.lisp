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

(defun parse-contact-string
    (string &aux (pos-< (position #\< string :test #'char-equal))
		 (pos-> (position #\> string :test #'char-equal)))
  "Parse STRING as \"NAME <EMAIL>\".
Return NAME and EMAIL, as two potentially NIL values."
  (if (and pos-< pos-> (< pos-< pos->))
    (values (if (zerop pos-<) nil (subseq string 0 (1- pos-<)))
	    (subseq string (1+ pos-<) pos->))
    (values string nil)))

(defun |parse-contact(s)| (|contact(s)|)
  "Parse CONTACT(S) as either a contact string, or a list of such.
A contact string is of the form \"NAME <EMAIL>\", each part being optional.

Return two values: a list of name(s) and a list of email(s). The two lists
maintain correspondence between names and emails: they are of the same length
and may contain null elements, for contact strings lacking either part."
  (if (stringp |contact(s)|)
    (when (one-liner-p |contact(s)|)
      (multiple-value-bind (name email) (parse-contact-string |contact(s)|)
	(values (list name) (list email))))
    (loop :for contact-string
	    :in (remove-if-not #'one-liner-p
			       (remove-duplicates |contact(s)|
				 :from-end t :test #'string=))
	  :for (name email)
	    := (multiple-value-list (parse-contact-string contact-string))
	  :collect name :into names
	  :collect email :into emails
	  :finally (return (values names emails)))))

;;; misc.lisp ends here
