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



;; ==========================================================================
;; ASDF Specific
;; ==========================================================================

;; ----------------------------
;; slot-unbound proof accessors
;; ----------------------------

(defun system-description (system)
  "Return ASDF SYSTEM's description or nil."
  (when (slot-boundp system 'asdf::description)
    (asdf:system-description system)))

(defun system-long-description (system)
  "Return ASDF SYSTEM's long description or nil."
  (when (slot-boundp system 'asdf::long-description)
    (asdf:system-long-description system)))

(defun system-author (system)
  "Return ASDF SYSTEM's author or nil."
  (when (slot-boundp system 'asdf::author)
    (asdf:system-author system)))

(defun system-maintainer (system)
  "Return ASDF SYSTEM's maintainer or nil."
  (when (slot-boundp system 'asdf::maintainer)
    (asdf:system-maintainer system)))

(defun system-license (system)
  "Return ASDF SYSTEM's license or nil."
  ;; #### NOTE: yes, the slot is licenCe, but licenSe accessors are also
  ;; available.
  (when (slot-boundp system 'asdf::licence)
    (asdf:system-license system)))

(defun component-version (component)
  "Return ASDF COMPONENT's version or nil."
  (when (slot-boundp component 'asdf:version)
    (asdf:component-version component)))


;; ---------------
;; Other utilities
;; ---------------

(defun components (module type)
  "Return the list of all TYPE components from MODULE."
  (loop :for component :in (asdf:module-components module)
	:if (eq (type-of component) type)
	  :collect component
	:if (eq (type-of component) 'asdf:module)
	  :nconc (components component type)))

(defun lisp-components (module)
  "Return the list of all Lisp source file components from MODULE."
  (components module 'asdf:cl-source-file))

;; #### WARNING: do not confuse with asdf:mpdule-components!
(defun module-components (module)
  "Return the list of all module components from MODULE."
  (components module 'asdf:module))

(defun system-directory (system)
  "Return SYSTEM's directory."
  (component-relative-pathname system))

(defun system-base-name (system)
  "Return the basename part of SYSTEM's definition file."
  (file-namestring (system-definition-pathname system)))

(defun system-file-name (system)
  "Return the name part of SYSTEM's definition file."
  (pathname-name (system-definition-pathname system)))

(defun system-file-type (system)
  "Return the type part of SYSTEM's definition file."
  (pathname-type (system-definition-pathname system)))


;;; util.lisp ends here
