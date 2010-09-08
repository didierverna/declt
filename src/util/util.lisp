;;; util.lisp --- General utilities

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Mon Aug 23 18:25:33 2010
;; Last Revision: Wed Sep  8 14:22:37 2010

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


;; ==========================================================================
;; Miscellaneous
;; ==========================================================================

(defmacro endpush (object place)
  "Like push, but at the end."
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



;; ==========================================================================
;; Symbol Related
;; ==========================================================================

;; #### PORTME.
(define-constant +categories+
    '((:constant  "constant"          "constants"
       (lambda (symbol)
	 (when (eql (sb-int:info :variable :kind symbol) :constant)
	   (symbol-value symbol))))
      (:special   "special variable"  "special variables"
       (lambda (symbol)
	 (when (eql (sb-int:info :variable :kind symbol) :special)
	   (symbol-value symbol))))
      (:macro     "macro"             "macros"
       (lambda (symbol)
	 (macro-function symbol)))
      (:function  "function"          "functions"
       (lambda (symbol)
	 (when (and (fboundp symbol)
		    (not (definitionp symbol :macro))
		    (not (definitionp symbol :generic)))
	   (fdefinition symbol))))
      (:generic   "generic function"  "generic functions"
       (lambda (symbol)
	 (when (and (fboundp symbol)
		    (typep (fdefinition symbol) 'generic-function))
	   (fdefinition symbol))))
      (:condition "condition"         "conditions"
       (lambda (symbol)
	 (let ((class (find-class symbol nil)))
	   (when (and class
		      (typep class 'condition))
	     class))))
      (:structure "structure"         "structures"
       (let ((class (find-class symbol nil)))
	 (when (and class
		    (eq (class-of class) 'structure-class))
	   class)))
      (:class     "class"             "classes"
       (lambda (symbol)
	 (let ((class (find-class symbol nil)))
	   (when (and class
		      (not (definitionp symbol :condition))
		      (not (definitionp symbol :structure)))
	     class)))))
  "The list of definition categories.")

(defun definitionp (symbol kind)
  "Return a value of some KIND defined by SYMBOL if any."
  (funcall (fourth (assoc kind +categories+)) symbol))

(defun symbol-needs-documenting (symbol)
  "Return t when SYMBOL needs to be documented."
  (some (lambda (category)
	  (definitionp symbol (first category)))
	+categories+))



;; ==========================================================================
;; Package Related
;; ==========================================================================

(defun package-external-definitions (package &aux external-definitions)
  "Return the list of PACKAGE's external symbols which need documenting."
  (do-external-symbols (symbol package external-definitions)
    (when (and (eq (symbol-package symbol) package)
	       (symbol-needs-documenting symbol))
      (push symbol external-definitions))))

(defun package-internal-definitions
    (package &aux (external-definitions (package-external-definitions package))
		  internal-definitions)
  "Return the list of PACKAGE's internal symbols which need documenting."
  (do-symbols (symbol package internal-definitions)
    (when (and (not (member symbol external-definitions))
	       (eq (symbol-package symbol) package)
	       (symbol-needs-documenting symbol))
      (push symbol internal-definitions))))



;; ==========================================================================
;; File Related
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


;;; util.lisp ends here