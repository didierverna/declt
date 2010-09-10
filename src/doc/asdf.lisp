;;; asdf.lisp --- ASDF items documentation

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Thu Sep  9 11:59:59 2010
;; Last Revision: Thu Sep  9 17:04:02 2010

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
;; Utilities
;; ==========================================================================

(defun render-packages (packages)
  "Render a list of PACKAGES references."
  (when packages
    (let ((length (length packages)))
      (format t "@item Package~p~%" length)
      (if (eq length 1)
	  (reference (first packages))
	(@itemize-list packages :renderer #'reference)))))



;; ==========================================================================
;; Components
;; ==========================================================================

;; -----------------------
;; Documentation protocols
;; -----------------------

;; Since node references are boring in Texinfo, we prefer to create custom
;; anchors for ASDF components and link to them instead.
(defmethod anchor ((component asdf:component) &optional relative-to)
  (format nil "~A anchor" (title component relative-to)))

(defmethod reference ((component asdf:component) &optional relative-to)
  (format t "@ref{~A, , @t{~(~A}~)} (~A)~%"
    (anchor component relative-to)
    (escape component)
    (component-type-name component)))

(defgeneric document-component (component relative-to)
  (:documentation "Render COMPONENT's documentation.")
  (:method :around ((component asdf:component) relative-to)
    "Index COMPONENT and enclose its documentation in a @table environment."
    (format t "@anchor{~A}@c~%" (anchor component relative-to))
    (index component relative-to)
    (@table ()
      (call-next-method)))
  (:method ((component asdf:component) relative-to)
    (format t "~@[@item Version~%~A~%~]"
      (escape (component-version component)))
    ;; #### NOTE: currently, we simply extract all the dependencies regardless
    ;; of the operations involved. We also assume that dependencies are of the
    ;; form (OP (OP DEP...) ...), but I'm not sure this is always the case.
    (let ((in-order-tos (slot-value component 'asdf::in-order-to))
	  dependencies
	  length)
      (when in-order-tos
	(dolist (in-order-to in-order-tos)
	  (dolist (op-dependency (cdr in-order-to))
	    (dolist (dependency (cdr op-dependency))
	      (pushnew dependency dependencies))))
	(setq length (length dependencies))
	(format t "@item Dependenc~@p~%" length)
	(if (eq length 1)
	    (format t "@t{~(~A}~)" (escape (first dependencies)))
	  (@itemize-list dependencies :format "@t{~(~A}~)" :key #'escape))))
    (let ((parent (component-parent component)))
      (when parent
	(format t "@item Parent~%")
	(reference parent relative-to)))
    (cond ((eq (type-of component) 'asdf:system) ;; Yuck!
	   (when *link-files*
	     (format t "@item Source Directory~%~
			@url{file://~A, ignore, @t{~A}}~%"
	       (escape (component-pathname component))
	       (escape (component-pathname component))))
	   ;; That sucks. I need to fake a cl-source-file reference because
	   ;; the system file is not an ASDF component per-se.
	   (format t "@item Definition file~%")
	   (let ((system-base-name (escape (system-base-name component))))
	     (format t "@ref{The ~A file anchor, , @t{~(~A}~)} (Lisp file)~%"
	       system-base-name
	       system-base-name))
	   (when *link-files*
	     (let ((directory (escape
			       (directory-namestring
				(system-definition-pathname component)))))
	       (format t "@item Installation Directory~%~
			  @url{file://~A, ignore, @t{~A}}~%"
		 directory directory))))
	  (t
	   (render-location component relative-to)))))



;; ==========================================================================
;; Files
;; ==========================================================================

;; -----------------------
;; Documentation protocols
;; -----------------------

(defmethod title ((source-file asdf:source-file) &optional relative-to)
  (format nil "The ~A file"
    (escape (relative-location source-file relative-to))))

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

(defmethod document-component ((file asdf:cl-source-file) relative-to)
  (call-next-method)
  (render-packages (file-packages (component-pathname file))))


;; -----
;; Nodes
;; -----

(defun file-node (file relative-to)
  "Create and return a FILE node."
  (make-node :name (title file relative-to)
	     :section-name (format nil "@t{~A}"
			     (escape (relative-location file relative-to)))
	     :before-menu-contents
	     (render-to-string (document-component file relative-to))))

(defun add-files-node
    (node system &aux (system-directory (system-directory system))
		      (lisp-files (lisp-components system))
		      (other-files
		       (mapcar (lambda (type) (components system type))
			       '(asdf:c-source-file
				 asdf:java-source-file
				 asdf:doc-file
				 asdf:html-file
				 asdf:static-file)))
		      (files-node
		       (add-child node
			 (make-node :name "Files"
				    :synopsis "The files documentation"
				    :before-menu-contents (format nil "~
Files are sorted by type and then listed depth-first from the system
components tree."))))
		      (lisp-files-node
		       (add-child files-node
			 (make-node :name "Lisp files"
				    :section-name "Lisp"))))
  "Add SYSTEM's files node to NODE."
  (let ((system-base-name (escape (system-base-name system))))
    (add-child lisp-files-node
      ;; That sucks. I need to fake a file-node call because the system file
      ;; is not an ASDF component per-se.
      (make-node :name (format nil "The ~A file" system-base-name)
		 :section-name (format nil "@t{~A}" system-base-name)
		 :before-menu-contents
		 (render-to-string
		   (format t "@anchor{The ~A file anchor}@c~%"
		     system-base-name)
		   (format t "@lispfileindex{~A}@c~%" system-base-name)
		   (@table ()
		     (render-location system system-directory))))))
  (dolist (file lisp-files)
    (add-child lisp-files-node (file-node file system-directory)))
  (loop :with other-files-node
    :for files :in other-files
    :for name :in '("C files" "Java files" "Doc files" "HTML files"
		    "Other files")
    :for section-name :in '("C" "Java" "Doc" "HTML" "Other")
    :when files
    :do (setq other-files-node
	      (add-child files-node
		(make-node :name name :section-name section-name)))
    :and :do (dolist (file files)
	       (add-child other-files-node
		 (file-node file system-directory)))))



;; ==========================================================================
;; Modules
;; ==========================================================================

;; -----------------------
;; Documentation protocols
;; -----------------------

(defmethod title ((module asdf:module) &optional relative-to)
  (format nil "The ~A module"
    (escape (relative-location module relative-to))))

(defmethod index ((module asdf:module) &optional relative-to)
  (format t "@moduleindex{~A}@c~%"
    (escape (relative-location module relative-to))))

(defmethod document-component ((module asdf:module) relative-to)
  (call-next-method)
  (let* ((components (asdf:module-components module))
	 (length (length components)))
    (when components
      (format t "@item Component~p~%" length)
      (if (eq length 1)
	  (reference (first components) relative-to)
	(@itemize-list components
	  :renderer (lambda (component)
		      (reference component relative-to)))))))


;; -----
;; Nodes
;; -----

(defun module-node (module relative-to)
  "Create and return a MODULE node."
  (make-node :name (title module relative-to)
	     :section-name (format nil "@t{~A}"
			     (escape (relative-location module relative-to)))
	     :before-menu-contents
	     (render-to-string (document-component module relative-to))))

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



;; ==========================================================================
;; System
;; ==========================================================================

;; -----------------------
;; Documentation protocols
;; -----------------------

(defmethod title ((system asdf:system) &optional relative-to)
  (declare (ignore relative-to))
  (format nil "The ~A system" (escape system)))

(defmethod index ((system asdf:system) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@systemindex{~A}@c~%" (escape system)))

(defmethod document-component ((system asdf:system) relative-to)
  (format t "@item Name~%@t{~A}~%" (escape system))
  (when (system-description system)
    (format t "@item Description~%")
    (render-text (system-description system))
    (fresh-line))
  (when (system-long-description system)
    (format t "@item Long Description~%")
    (render-text (system-long-description system))
    (fresh-line))
  (multiple-value-bind (author email)
      (parse-author-string (system-author system))
    (when (or author email)
      (format t "@item Author~%~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
	(escape author) (and author email) (escape email))))
  (multiple-value-bind (maintainer email)
      (parse-author-string (system-maintainer system))
    (when (or maintainer email)
      (format t "@item Maintainer~%~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
	(escape maintainer) (and maintainer email) (escape email))))
  (format t "~@[@item License~%~A~%~]" (escape (system-license system)))
  (call-next-method)
  (render-packages (system-packages system)))


;; -----
;; Nodes
;; -----

(defun system-node (system &aux (relative-to (system-directory system)))
  "Create and return the SYSTEM node."
  (make-node :name "System"
	     :synopsis "The system documentation"
	     :before-menu-contents
	     (render-to-string (document-component system relative-to))))

(defun add-system-node (node system)
  "Add SYSTEM's system node to NODE."
  (add-child node (system-node system)))


;;; asdf.lisp ends here
