;;; asdf.lisp --- ASDF utilities

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Fri Sep  3 21:59:21 2010
;; Last Revision: Fri Sep  3 21:59:21 2010

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

;; Some other accessors or functions that we can import directly.
(import '(asdf:find-system
	  asdf:system-definition-pathname
	  asdf:module-components
	  asdf:component-name
	  asdf:component-pathname
	  asdf:component-relative-pathname))



;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun collect-components (components type)
  "Collect all components of TYPE in COMPONENTS."
  (loop :for component :in components
	:if (eq (type-of component) type)
	  :collect component
	:if (eq (type-of component) 'asdf:module)
	  :nconc (collect-components (module-components component)
				     type)))


;;; asdf.lisp ends here
