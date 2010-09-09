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

(defgeneric index (item &optional relative-to)
  (:documentation "Render ITEM's indexing command."))

(defgeneric reference (item &optional relative-to)
  (:documentation "Render ITEM's reference.")
  (:method :before (item &optional relative-to)
    (index item relative-to)))

(defvar *link-files* t
  "Whether to create links to files or directories in the reference manual.
When true (the default), pathnames are made clickable although the links are
specific to this particular installation.

Setting this to NIL is preferable for creating reference manuals meant to put
online, and hence independent of any specific installation.")

(defun render-location (item relative-to)
  "Render an itemized location line for ITEM RELATIVE-TO."
  (let ((location (location item)))
    (when location
      (format t "@item Location~%~
		 ~@[@url{file://~A, ignore, ~]@t{~A}~:[~;}~]~%"
	(when *link-files*
	  (escape location))
	(escape (relative-location location relative-to))
	*link-files*))))



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

;; ------------------
;; Redering protocols
;; ------------------

(defmethod to-string ((package package))
  "Return PACKAGE's name."
  (package-name package))


;; ------------------
;; Item protocols
;; ------------------

(defmethod location ((package package))
  ;; #### PORTME.
  (let* ((defsrc (sb-introspect:find-definition-source package)))
    (when defsrc
      (sb-introspect:definition-source-pathname defsrc))))

(defmethod index ((package package) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@packageindex{~(~A~)}@c~%" (escape package)))

(defmethod reference ((package package) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@t{~(~A~)}" (escape package)))


;; ---------
;; Utilities
;; ---------

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
;; ASDF
;; ==========================================================================

;; ------------------
;; Redering protocols
;; ------------------

(defmethod to-string ((component asdf:component))
  "Return COMPONENT's name."
  (component-name component))

(defgeneric component-type-name (component)
  (:documentation "Return COMPONENT's type name."))


;; ------------------
;; Item protocols
;; ------------------

(defmethod location ((component asdf:component))
  (component-pathname component))

;; #### NOTE: what we call the "system's location" is the pathname to the
;; source tree. Not to the systems directory symlink .
(defmethod location ((system asdf:system))
  (make-pathname :name (system-file-name system)
		 :type (system-file-type system)
		 :directory (pathname-directory (component-pathname system))))

(defmethod reference ((component asdf:component) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@t{~A} ~@[, version ~A~] (~A)~%"
    (escape component)
    (escape (component-version component))
    (component-type-name component)))

(defmethod index ((cl-source-file asdf:cl-source-file) &optional relative-to)
  (format t "@lispfileindex{~A}@c~%"
    (escape (relative-location cl-source-file relative-to))))

(defmethod index ((c-source-file asdf:c-source-file) &optional relative-to)
  (format t "@cfileindex{~A}@c~%"
    (escape (relative-location c-source-file relative-to))))

(defmethod index
    ((java-source-file asdf:java-source-file) &optional relative-to)
  (format t "@javafileindex{~A}@c~%"
    (escape (relative-location java-source-file relative-to))))

(defmethod index ((static-file asdf:static-file) &optional relative-to)
  (format t "@otherfileindex{~A}@c~%"
    (escape (relative-location static-file relative-to))))

(defmethod index ((doc-file asdf:doc-file) &optional relative-to)
  (format t "@docfileindex{~A}@c~%"
    (escape (relative-location doc-file relative-to))))

(defmethod index ((html-file asdf:html-file) &optional relative-to)
  (format t "@htmlfileindex{~A}@c~%"
    (escape (relative-location html-file relative-to))))

(defmethod component-type-name ((cl-source-file asdf:cl-source-file))
  "Lisp file")

(defmethod component-type-name ((c-source-file asdf:c-source-file))
  "C file")

(defmethod component-type-name ((java-source-file asdf:java-source-file))
  "Java file")

(defmethod component-type-name ((static-file asdf:static-file))
  "file")

(defmethod component-type-name ((doc-file asdf:doc-file))
  "doc file")

(defmethod component-type-name ((html-file asdf:html-file))
  "HTML file")

(defmethod index ((module asdf:module) &optional relative-to)
  (format t "@moduleindex{~A}@c~%"
    (escape (relative-location module relative-to))))

(defmethod component-type-name ((module asdf:module))
  "module")

(defmethod index ((system asdf:system) &optional relative-to)
  (declare (ignore relative-to))
  (values))


;; ---------
;; Utilities
;; ---------

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
