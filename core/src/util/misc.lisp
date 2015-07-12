;;; misc.lisp --- Miscellaneous utilities

;; Copyright (C) 2010, 2011, 2013 Didier Verna

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

(in-package :com.dvlsoft.declt)
(in-readtable :com.dvlsoft.declt)


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

;; Stolen from Tinaa.
(defun parse-author-string
    (string &aux (pos-< (position #\< string :test #'char-equal))
		 (pos-> (position #\> string :test #'char-equal)))
  "Parse STRING as \"NAME <EMAIL>\".
Return NAME and EMAIL as two values."
  (if (and pos-< pos-> (< pos-< pos->))
      (values (subseq string 0 (1- pos-<))
	      (subseq string (1+ pos-<) pos->))
    string))



;; ==========================================================================
;; Files
;; ==========================================================================

;; We need to protect against read-time errors. Let's just hope that nothing
;; fancy occurs in DEFPACKAGE...
(defun safe-read (stream)
  "Read once from STREAM protecting against errors."
  (handler-case (read stream nil stream)
    (error ())))

(defun file-packages (file)
  "Return the list of all packages defined in FILE."
  (with-open-file (stream file :direction :input)
    (loop :for form := (safe-read stream) :then (safe-read stream)
	  :until (eq form stream)
	  :if (and (consp form)
		   (eq (car form) 'defpackage))
	    :collect (find-package (cadr form)))))


;;; misc.lisp ends here
