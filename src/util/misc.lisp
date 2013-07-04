;;; misc.lisp --- Miscellaneous utilities

;; Copyright (C) 2010, 2011, 2013 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Declt.

;; Declt is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3,
;; as published by the Free Software Foundation.

;; Declt is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


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
  (handler-case (read stream nil :eof)
    (error ())))

(defun file-packages (file)
  "Return the list of all packages defined in FILE."
  (with-open-file (stream file :direction :input)
    (loop :for form := (safe-read stream) :then (safe-read stream)
	  :until (eq form :eof)
	  :if (and (consp form)
		   (eq (car form) 'defpackage))
	    :collect (find-package (cadr form)))))


;;; misc.lisp ends here
