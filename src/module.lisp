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

(defmethod index ((module asdf:module) &optional relative-to)
  (format t "@moduleindex{~A}@c~%"
    (escape (relative-pathname module relative-to))))


;; --------------------
;; Referencing protocol
;; --------------------

(defmethod component-type-name ((module asdf:module))
  "module")


;; ----------------------
;; Documentation protocol
;; ----------------------

(defmethod document ((module asdf:module) &optional relative-to)
  (call-next-method)
  (format t "@item Components~%")
  (@itemize-list (asdf:module-components module)
    :renderer (lambda (component)
		(reference component relative-to))))



;; ==========================================================================
;; Module Nodes
;; ==========================================================================

(defun module-node (module relative-to)
  "Create and return a MODULE node."
  (let ((name (escape (relative-pathname module relative-to))))
    (make-node :name (format nil "The ~A module" name)
	       :section-name (format nil "@t{~A}"name)
	       :before-menu-contents
	       (render-to-string (document module relative-to)))))

(defun add-modules-node
    (node system &aux (system-directory (system-directory system))
		      (modules (module-components system)))
  "Add SYSTEM's modules node to NODE."
  (when modules
    (let ((modules-node
	   (add-child node (make-node :name "Modules"
				      :synopsis "The modules documentation"
				      :before-menu-contents
				      (format nil "~
Modules are listed depth-first from the system components tree.")))))
      (dolist (module modules)
	(add-child modules-node (module-node module system-directory))))))


;;; module.lisp ends here
