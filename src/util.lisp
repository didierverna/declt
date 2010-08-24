;;; util.lisp --- General utilities

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Mon Aug 23 18:25:33 2010
;; Last Revision: Tue Aug 24 00:39:57 2010

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

;; Contents management by FCM version 0.1.


;;; Code:

(in-package :com.dvlsoft.declt)

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

;; Originally stolen from Tinaa.
(defun parse-author-string (string)
  "Extract a name and an email part from STRING.
Return these as two values.
STRING should look like \"NAME <EMAIL>\"."
  (let ((pos-< (position #\< string :test #'char-equal))
	(pos-> (position #\> string :test #'char-equal)))
    (if (and pos-< pos-> (< pos-< pos->))
	(values (subseq string 0 (1- pos-<))
		(subseq string (1+ pos-<) pos->))
      string)))

(defun escape (string)
  "Escape all @ characters in STRING."
  (when string
    (coerce (loop :for char :across string
		  :collect char
		  :if (char= char #\@) :collect #\@)
	    'string)))

(defun write-docstring (docstring)
  "Output DOCSTRING."
  (when docstring
    (write-string (coerce (loop :for char :across (escape docstring)
				:if (char= char #\Newline)
				  :collect #\@ :and :collect #\*
				:collect char)
			  'string))))


(defun write-node (node next previous top sectionning &optional contents)
  "Write a new NODE."
  (cond ((eq sectionning :chapter)
	 (format t
	     "


@c ====================================================================
@c ~A
@c ====================================================================~%"
	   node))
	((eq sectionning :section)
	 (let ((separator (make-string (length node) :initial-element #\-)))
	   (format t
	       "

@c ~A
@c ~A
@c ~A~%"
	     separator node separator))))
  (format t "@node ~A, ~@[~A~], ~A, ~A~%" node next (or previous top) top)
  (format t "@~A ~A~%~:[~;~%~]"
    (string-downcase sectionning)
    node
    (member sectionning '(:chapter :section)))
  (when contents
    (write-string contents)))


;;; util.lisp ends here
