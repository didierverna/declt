;;; asdf.lisp --- ASDF Items

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Thu Sep  9 11:40:40 2010
;; Last Revision: Thu Sep  9 11:48:31 2010

;; This file is part of Declt.

;; Declt is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version3,
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
;; Components
;; ==========================================================================

;; -------------------
;; Rendering protocols
;; -------------------

(defmethod to-string ((component asdf:component))
  "Return COMPONENT's name."
  (component-name component))

(defgeneric component-type-name (component)
  (:documentation "Return COMPONENT's type name."))


;; --------------
;; Item protocols
;; --------------

(defmethod location ((component asdf:component))
  (component-pathname component))

(defmethod reference ((component asdf:component) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@t{~A} ~@[, version ~A~] (~A)~%"
    (escape component)
    (escape (component-version component))
    (component-type-name component)))



;; ==========================================================================
;; Files
;; ==========================================================================

;; -------------------
;; Rendering protocols
;; -------------------

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


;; --------------
;; Item protocols
;; --------------

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



;; ==========================================================================
;; Modules
;; ==========================================================================

;; -------------------
;; Rendering protocols
;; -------------------

(defmethod component-type-name ((module asdf:module))
  "module")


;; --------------
;; Item protocols
;; --------------

(defmethod index ((module asdf:module) &optional relative-to)
  (format t "@moduleindex{~A}@c~%"
    (escape (relative-location module relative-to))))



;; ==========================================================================
;; System
;; ==========================================================================

;; --------------
;; Item protocols
;; --------------

;; #### NOTE: what we call the "system's location" is the pathname to the
;; source tree. Not to the systems directory symlink .
(defmethod location ((system asdf:system))
  (make-pathname :name (system-file-name system)
		 :type (system-file-type system)
		 :directory (pathname-directory (component-pathname system))))

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


;;; asdf.lisp ends here
