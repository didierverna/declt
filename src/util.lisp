;;; util.lisp --- General utilities

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Mon Aug 23 18:25:33 2010
;; Last Revision: Sun Sep  5 20:39:39 2010

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

#| Currently not used anymore
(defun list-to-string (list &key (key #'identity) (separator ", "))
  "Return a SEPARATOR-separated string of all LIST elements.
- KEY should provide a way to get a string from each LIST element.
- SEPARATOR is the string to insert between elements."
  (reduce (lambda (str1 str2) (concatenate 'string str1 separator str2))
	  list
	  :key key))|#

(defgeneric pretty-specializer (specializer)
  (:documentation "Returns a printable form for SPECIALIZER.")
  (:method (specializer)
    (or (ignore-errors (class-name specializer))
	specializer))
  ;; #### PORTME.
  (:method ((specializer sb-mop:eql-specializer))
    ;; #### PORTME.
    `(eql ,(sb-mop:eql-specializer-object specializer))))



;; ==========================================================================
;; Symbol Related
;; ==========================================================================

;; #### PORTME.

(defun constant-definition-p (symbol)
  "Return the constant defined by SYMBOL if any."
  (when (eql (sb-int:info :variable :kind symbol) :constant)
    (symbol-value symbol)))

(defun special-definition-p (symbol)
  "Return the special variable value defined by SYMBOL if any."
  (when (eql (sb-int:info :variable :kind symbol) :special)
    (symbol-value symbol)))

(defun generic-definition-p (symbol)
  "Return the generic function defined by SYMBOL if any."
  (when (and (fboundp symbol)
	     (typep (fdefinition symbol) 'generic-function))
    (fdefinition symbol)))

(defun macro-definition-p (symbol)
  "Return the macro function defined by SYMBOL if any."
  (macro-function symbol))

(defun function-definition-p (symbol)
  "Return t if SYMBOL names an ordinary function."
  (and (fboundp symbol)
       (or (consp symbol) (not (macro-definition-p symbol)))
       (not (typep (fdefinition symbol) 'standard-generic-function))))

(defun symbol-needs-rendering (symbol)
  "Return t when SYMBOL needs to be documented."
  (or (constant-definition-p symbol)
      (special-definition-p  symbol)
      (class-definition-p    symbol)
      (fbound-definition-p symbol)))

(defun condition-definition-p (symbol)
  "Return the condition named by SYMBOL if any."
  (let ((class (find-class symbol nil)))
    (when (and class
	       (typep class 'condition))
      class)))

(defun structure-definition-p (symbol)
  "Return the structure named by SYMBOL if any."
  (let ((class (find-class symbol nil)))
    (when (and class
	       (typep class 'structure))
      class)))

(defun class-definition-p (symbol)
  "Return the class named by SYMBOL if any."
  (let ((class (find-class symbol nil)))
    (when (and class
	       (not (condition-definition-p symbol))
	       (not (structure-definition-p symbol)))
      class)))



;; ==========================================================================
;; Package Related
;; ==========================================================================

;; #### FIXME: see how to handle shadowed symbols (not sure what happens with
;; the home package).
(defun package-external-symbols (package &aux external-symbols)
  "Return the list of symbols external to PACKAGE that need documenting."
  (do-external-symbols (symbol package)
    (when (and (eq (symbol-package symbol) package)
	       (symbol-needs-rendering symbol))
      (push symbol external-symbols)))
  (sort external-symbols #'string-lessp))

;; #### FIXME: see how to handle shadowed symbols (not sure what happens with
;; the home package).
(defun package-internal-symbols
    (package &aux (external-symbols (package-external-symbols package))
		  internal-symbols)
  "Return the list of symbols internal to PACKAGE that need documenting."
  (do-symbols (symbol package)
    (when (and (not (member symbol external-symbols))
	       (eq (symbol-package symbol) package)
	       (symbol-needs-rendering symbol))
      (push symbol internal-symbols)))
  (sort internal-symbols #'string-lessp))



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
