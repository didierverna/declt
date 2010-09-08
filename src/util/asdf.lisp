;;; asdf.lisp --- ASDF utilities

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Fri Sep  3 21:59:21 2010
;; Last Revision: Wed Sep  8 14:30:10 2010

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


;;; Code:

(in-package :com.dvlsoft.declt)


;; ==========================================================================
;; slot-unbound Proof Accessors
;; ==========================================================================

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



;; ==========================================================================
;; Utilities
;; ==========================================================================

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

(defun lisp-pathnames (system)
  "Return the list of all Lisp source file pathnames.
The list includes the system definition file."
  (cons
   ;; We want the .asd file source location, not the installation location.
   (make-pathname :name (system-file-name system)
		  :type (system-file-type system)
		  :directory (pathname-directory (component-pathname system)))
   (mapcar #'component-pathname (lisp-components system))))

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