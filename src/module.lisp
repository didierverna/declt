;;; module.lisp --- ASDF module documentation

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Aug 25 16:04:44 2010
;; Last Revision: Wed Aug 25 17:48:29 2010

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


;;; Code:


(in-package :com.dvlsoft.declt)


;; ==========================================================================
;; Rendering Protocols
;; ==========================================================================

;; -----------------
;; Indexing protocol
;; -----------------

(defmethod index (stream (module asdf:module))
  (format stream "@moduleindex{~A}@c~%" (component-name module)))

;; --------------------
;; Itemization protocol
;; --------------------

(defmethod itemize (stream (module asdf:module))
  (write-string "module" stream))


;; ---------------------
;; Tableization protocol
;; ---------------------

(defmethod tableize (stream (module asdf:module) relative-to)
  "Also describe MODULE's components."
  (call-next-method)
  (format stream "@item Components~%@itemize @bullet~%")
  (dolist (component (module-components module))
    (itemize stream component))
  (format stream "@end itemize~%"))



;; ==========================================================================
;; Module Nodes
;; ==========================================================================

(defun module-node (module relative-to)
  "Create and return a MODULE node."
  (make-node :name (format nil "The ~A module" (component-name module))
	     :section-name (format nil "@t{~A}" (component-name module))
	     :before-menu-contents (with-output-to-string (str)
				     (index str module)
				     (tableize str module relative-to))))

(defun add-modules-node
    (node system &aux (system-directory (component-relative-pathname system))
		      (modules (collect-modules system)))
  "Add SYSTEM's modules node to NODE."
  (when modules
    (let ((modules-node
	   (add-child node (make-node :name "Modules"
				      :synopsis "The system's modules"
				      :before-menu-contents
				      (format nil "~
Modules are listed depth-first from the system components tree.")))))
      (dolist (module modules)
	(add-child modules-node (module-node module system-directory))))))


;;; module.lisp ends here
