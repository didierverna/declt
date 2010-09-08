;;; item.lisp --- Items subject to documentation

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Sep  8 21:10:37 2010
;; Last Revision: Wed Sep  8 21:18:18 2010

;; This file is part of Declt.

;; Declt is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Declt is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; Contents management by FCM version 0.1.

;; Documentable items are ASDF components (systems, modules, files), packages
;; and some interesting symbols.


;;; Code:

(in-package :com.dvlsoft.declt)


;; ==========================================================================
;; Item Protocols
;; ==========================================================================

(defgeneric location (item)
  (:documentation "Return ITEM's pathname.")
  (:method ((pathname pathname))
    pathname))

(defun relative-location (item relative-to)
  "Return ITEM's location RELATIVE-TO."
  (let ((location (location item)))
    (when location
      (enough-namestring location relative-to))))



;; ==========================================================================
;; Symbols
;; ==========================================================================

;; #### PORTME.
(define-constant +categories+
    '((:constant  "constant"          "constants")
      (:special   "special variable"  "special variables")
      (:macro     "macro"             "macros")
      (:function  "function"          "functions")
      (:generic   "generic function"  "generic functions")
      (:condition "condition"         "conditions")
      (:structure "structure"         "structures")
      (:class     "class"             "classes"))
  "The list of definition categories.")

(defun definitionp (symbol kind)
  "Return a value of some KIND defined by SYMBOL if any."
  (ecase kind
    (:constant
     (when (eql (sb-int:info :variable :kind symbol) :constant)
       (symbol-value symbol)))
    (:special
     (when (eql (sb-int:info :variable :kind symbol) :special)
       (symbol-value symbol)))
    (:macro
     (macro-function symbol))
    (:function
     (when (and (fboundp symbol)
		(not (definitionp symbol :macro))
		(not (definitionp symbol :generic)))
       (fdefinition symbol)))
    (:generic
     (when (and (fboundp symbol)
		(typep (fdefinition symbol) 'generic-function))
       (fdefinition symbol)))
    (:condition
     (let ((class (find-class symbol nil)))
       (when (and class
		  (typep class 'condition))
	 class)))
    (:structure
     (let ((class (find-class symbol nil)))
       (when (and class
		  (eq (class-of class) 'structure-class))
	 class)))
    (:class
     (let ((class (find-class symbol nil)))
       (when (and class
		  (not (definitionp symbol :condition))
		  (not (definitionp symbol :structure)))
	 class)))))

(defun symbol-needs-documenting (symbol)
  "Return t when SYMBOL needs to be documented."
  (some (lambda (category)
	  (definitionp symbol (first category)))
	+categories+))



;; ==========================================================================
;; Packages
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
;; ASDF
;; ==========================================================================

(defmethod location ((component asdf:component))
  (component-pathname component))

;; #### NOTE: what we call the "system's location" is the pathname to the
;; source tree. Not to the systems directory symlink .
(defmethod location ((system asdf:system))
  (make-pathname :name (system-file-name system)
		 :type (system-file-type system)
		 :directory (pathname-directory (component-pathname system))))

(defun lisp-pathnames (system)
  "Return the list of all Lisp source file pathnames.
The list includes the system definition file."
  (mapcar #'location (cons system (lisp-components system))))

(defun system-packages (system)
  "Return the list of packages defined in SYSTEM."
  (remove-duplicates (mapcan #'file-packages (lisp-pathnames system))))

(defun system-external-definitions (system)
  "Return the list of SYSTEM's external symbols which need documenting."
  (mapcan #'package-external-definitions (system-packages system)))

(defun system-internal-definitions (system)
  "Return the list of SYSTEM's internal symbols which need documenting."
  (mapcan #'package-internal-definitions (system-packages system)))


;;; item.lisp ends here
