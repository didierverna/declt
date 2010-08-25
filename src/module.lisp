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
  (format stream "@moduleindex{~A}@c~%"
    (asdf:component-name module)))

;; --------------------
;; Itemization protocol
;; --------------------

(defmethod itemize (stream (module asdf:module))
  (write-string "module" stream))



;; ==========================================================================
;; Module Nodes
;; ==========================================================================

(defun module-node (module)
  "Create and return a MODULE node."
  (make-node :name (format nil "The ~A module"
		     (asdf:component-name module))
	     :section-name (format nil "@t{~A}"
			     (asdf:component-name module))
	     :before-menu-contents
	     (with-output-to-string (str)
	       (index str module)
	       (format str "@table @strong~%")
	       (when (and (slot-boundp module 'asdf:version)
			  (asdf:component-version module))
		 (format str "@item Version~%~A~%"
		   (asdf:component-version module)))
	       ;; #### NOTE: currently, we simply extract all the dependencies
	       ;; regardless of the operations involved. We also assume that
	       ;; dependencies are of the form (OP (OP DEP...) ...), but I'm
	       ;; not sure this is always the case.
	       (let ((in-order-tos (slot-value module 'asdf::in-order-to))
		     dependencies)
		 (when in-order-tos
		   (dolist (in-order-to in-order-tos)
		     (dolist (op-dependency (cdr in-order-to))
		       (dolist (dependency (cdr op-dependency))
			 (pushnew dependency dependencies))))
		   (format str "@item Dependencies~%~A~%"
		     (list-to-string
		      (mapcar (lambda (dep)
				(format nil "@t{~A}" (string-downcase dep)))
			      dependencies)))))
	       (format str "@item Parent~%")
	       (index str (asdf:component-parent module))
	       (format str "@t{~A}~%"
		 (asdf:component-name (asdf:component-parent module)))
	       (format str "@item Location~%@t{~A}~%"
		 (asdf:component-relative-pathname module))
	       (format str "@item Components~%@itemize @bullet~%")
	       (dolist (component (asdf:module-components module))
		 (itemize str component))
	       (format str "@end itemize~%@end table"))))

(defun add-modules-node (node components)
  "Add COMPONENTS modules node to NODE."
  (let ((modules (collect-components components 'asdf:module)))
    (when modules
      (let ((modules-node
	     (add-child node (make-node :name "Modules"
					:synopsis "The system's modules"
					 :before-menu-contents
"Modules are listed depth-first from the system components tree."))))
	(dolist (module modules)
	  (add-child modules-node (module-node module)))))))


;;; module.lisp ends here
