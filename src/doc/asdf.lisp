;;; asdf.lisp --- ASDF items documentation

;; Copyright (C) 2010, 2011 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

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

(defun relative-location (component relative-to)
  "Return COMPONENT's location RELATIVE-TO."
  (enough-namestring (component-pathname component) relative-to))

(defun render-packages (packages)
  "Render a list of PACKAGES references."
  (when packages
    (let ((length (length packages)))
      (@tableitem (format nil "Package~p" length)
	(if (eq length 1)
	    (reference (first packages))
	  (@itemize-list packages :renderer #'reference))))))



;; ==========================================================================
;; Components
;; ==========================================================================

;; -----------------------
;; Documentation protocols
;; -----------------------

(defmethod reference ((component asdf:component) &optional relative-to)
  "Render COMPONENT's reference."
  (format t "@ref{~A, , @t{~(~A}~)} (~A)~%"
    (escape (anchor-name component relative-to))
    (escape component)
    (type-name component)))

(defmethod document :around ((component asdf:component) system &key
			     &aux (system-directory (system-directory system)))
  "Anchor and index SYSTEM's COMPONENT, document it in a @table environment."
  (anchor component system-directory)
  (index component system-directory)
  (@table () (call-next-method)))

(defmethod document ((component asdf:component) system &key
		     &aux (system-directory (system-directory system)))
  "Render SYSTEM's COMPONENT's documentation."
  (format t "~@[@item Version~%~
		  ~A~%~]"
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
      (@tableitem (format nil "Dependenc~@p" length)
	(if (eq length 1)
	    (format t "@t{~(~A}~)" (escape (first dependencies)))
	  (@itemize-list dependencies :format "@t{~(~A}~)" :key #'escape)))))
  (let ((parent (component-parent component)))
    (when parent
      (@tableitem "Parent"
	(reference parent system-directory))))
  (cond ((eq (type-of component) 'asdf:system) ;; Yuck!
	 ;; That sucks. I need to fake a cl-source-file reference because the
	 ;; system file is not an ASDF component per-se.
	 (@tableitem "Definition file"
	   (let ((system-base-name (escape (system-base-name component))))
	     (format t "@ref{go to the ~A file, , @t{~(~A}~)} (Lisp file)~%"
	       system-base-name
	       system-base-name)))
	 (when *link-files*
	   (let ((system-source-directory
		  (escape (system-source-directory component)))
		 (installation-directory
		  (escape
		   (directory-namestring
		    (probe-file (system-definition-pathname component))))))
	     (@tableitem "Source Directory"
	       (format t "@url{file://~A, ignore, @t{~A}}~%"
		 system-source-directory
		 system-source-directory))
	     ;; With ASDF 2, we're not supposed to use the systems directory
	     ;; convention anymore, so the installation directory might just
	     ;; be the source one.
	     (unless (string= system-source-directory installation-directory)
	       (@tableitem "Installation Directory"
		 (format t "@url{file://~A, ignore, @t{~A}}~%"
		   installation-directory installation-directory))))))
	(t
	 (render-location (component-pathname component) system-directory))))



;; ==========================================================================
;; Files
;; ==========================================================================

;; -----------------------
;; Documentation protocols
;; -----------------------

(defmethod title ((source-file asdf:source-file) &optional relative-to)
  "Return SOURCE-FILE's title."
  (format nil "the ~A file" (relative-location source-file relative-to)))

(defmethod index ((cl-source-file asdf:cl-source-file) &optional relative-to)
  "Render CL-SOURCE-FILE's indexing command."
  (format t "@lispfileindex{~A}@c~%"
    (escape (relative-location cl-source-file relative-to))))

(defmethod index ((c-source-file asdf:c-source-file) &optional relative-to)
  "Render C-SOURCE-FILE's indexing command."
  (format t "@cfileindex{~A}@c~%"
    (escape (relative-location c-source-file relative-to))))

(defmethod index
    ((java-source-file asdf:java-source-file) &optional relative-to)
  "Render JAVA-SOURCE-FILE's indexing command."
  (format t "@javafileindex{~A}@c~%"
    (escape (relative-location java-source-file relative-to))))

(defmethod index ((static-file asdf:static-file) &optional relative-to)
  "Render STATIC-SOURCE-FILE's indexing command."
  (format t "@otherfileindex{~A}@c~%"
    (escape (relative-location static-file relative-to))))

(defmethod index ((doc-file asdf:doc-file) &optional relative-to)
  "Render DOC-SOURCE-FILE's indexing command."
  (format t "@docfileindex{~A}@c~%"
    (escape (relative-location doc-file relative-to))))

(defmethod index ((html-file asdf:html-file) &optional relative-to)
  "Render HTML-SOURCE-FILE's indexing command."
  (format t "@htmlfileindex{~A}@c~%"
    (escape (relative-location html-file relative-to))))

(defmethod document ((file asdf:cl-source-file) system
		     &key external-definitions internal-definitions)
  "Render SYSTEM's FILE's documentation."
  (call-next-method)
  (render-packages (file-packages (component-pathname file)))
  (when external-definitions
    (@tableitem "Exported definitions"
      (@itemize-list external-definitions :renderer #'reference)))
  (when internal-definitions
    (@tableitem "Internal definitions"
      (@itemize-list internal-definitions :renderer #'reference))))


;; -----
;; Nodes
;; -----

(defun file-definitions (file definitions)
  "Return the subset of DEFINITIONS that come from FILE."
  (sort (mapcan (lambda (definition)
		  (definition-file-definitions definition file))
		definitions)
	#'string-lessp
	:key #'definition-symbol))

(defun lisp-file-node (file system external-definitions internal-definitions
		       &aux (system-directory (system-directory system)))
  "Create and return a SYSTEM's Lisp FILE node."
  (make-node :name (escape (format nil "~@(~A~)"
			     (title file system-directory)))
	     :section-name (format nil "@t{~A}"
			     (escape
			      (relative-location file system-directory)))
	     :before-menu-contents
	     (render-to-string
	       (document file system
			 :external-definitions
			 (file-definitions
			  (component-pathname file) external-definitions)
			 :internal-definitions
			 (file-definitions
			  (component-pathname file) internal-definitions)))))

(defun file-node
    (file system &aux (system-directory (system-directory system)))
  "Create and return a SYSTEM's FILE node."
  (make-node :name (escape (format nil "~@(~A~)"
			     (title file system-directory)))
	     :section-name (format nil "@t{~A}"
			     (escape
			      (relative-location file system-directory)))
	     :before-menu-contents
	     (render-to-string (document file system))))

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
  (let ((system-base-name (escape (system-base-name system)))
	(external-definitions (system-external-definitions system))
	(internal-definitions (system-internal-definitions system)))
    (add-child lisp-files-node
      ;; That sucks. I need to fake a file-node call because the system file
      ;; is not an ASDF component per-se.
      (make-node :name (format nil "The ~A file" system-base-name)
		 :section-name (format nil "@t{~A}" system-base-name)
		 :before-menu-contents
		 (render-to-string
		   (@anchor
		    (format nil "go to the ~A file" system-base-name))
		   (format t "@lispfileindex{~A}@c~%" system-base-name)
		   (@table ()
		     (render-location (system-definition-pathname system)
				      system-directory)
		     (render-packages
		      (file-packages
		       (system-definition-pathname system)))
		     (let ((external-definitions
			    (file-definitions
			     (system-definition-pathname system)
			     external-definitions))
			   (internal-definitions
			    (file-definitions
			     (system-definition-pathname system)
			     internal-definitions)))
		       (when external-definitions
			 (@tableitem "Exported definitions"
			   (@itemize-list external-definitions
			     :renderer #'reference)))
		       (when internal-definitions
			 (@tableitem "Internal definitions"
			   (@itemize-list internal-definitions
			     :renderer #'reference))))))))
    (dolist (file lisp-files)
      (add-child lisp-files-node
	(lisp-file-node file system
			external-definitions internal-definitions))))
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
		 (file-node file system)))))



;; ==========================================================================
;; Modules
;; ==========================================================================

;; -----------------------
;; Documentation protocols
;; -----------------------

(defmethod title ((module asdf:module) &optional relative-to)
  "Return MODULE's title."
  (format nil "the ~A module" (relative-location module relative-to)))

(defmethod index ((module asdf:module) &optional relative-to)
  "Render MODULE's indexing command."
  (format t "@moduleindex{~A}@c~%"
    (escape (relative-location module relative-to))))

(defmethod document ((module asdf:module) system &key)
  "Render SYSTEM's MODULE documentation."
  (call-next-method)
  (let* ((components (asdf:module-components module))
	 (length (length components)))
    (when components
      (let ((system-directory (system-directory system)))
	(@tableitem (format nil "Component~p" length)
	  (if (eq length 1)
	      (reference (first components) system-directory)
	    (@itemize-list components
	      :renderer (lambda (component)
			  (reference component system-directory)))))))))


;; -----
;; Nodes
;; -----

(defun module-node (module system
		    &aux (system-directory (system-directory system)))
  "Create and return a SYSTEM's MODULE node."
  (make-node :name (escape (format nil "~@(~A~)"
			     (title module system-directory)))
	     :section-name (format nil "@t{~A}"
			     (escape
			      (relative-location module system-directory)))
	     :before-menu-contents
	     (render-to-string (document module system))))

(defun add-modules-node (node system &aux (modules (module-components system)))
  "Add SYSTEM's modules node to NODE."
  (when modules
    (let ((modules-node
	   (add-child node (make-node :name "Modules"
				      :synopsis "The modules documentation"
				      :before-menu-contents
				      (format nil "~
Modules are listed depth-first from the system components tree.")))))
      (dolist (module modules)
	(add-child modules-node (module-node module system))))))



;; ==========================================================================
;; System
;; ==========================================================================

;; -----------------------
;; Documentation protocols
;; -----------------------

(defmethod title ((system asdf:system) &optional relative-to)
  "Return SYSTEM's title."
  (declare (ignore relative-to))
  (format nil "the ~A system" (name system)))

(defmethod index ((system asdf:system) &optional relative-to)
  "Render SYSTEM's indexing command."
  (declare (ignore relative-to))
  (format t "@systemindex{~A}@c~%" (escape system)))

(defmethod document ((system asdf:system) reference-system &key)
  "Render SYSTEM's documentation."
  (declare (ignore reference-system))
  (@tableitem "Name"
    (format t "@t{~A}~%" (escape system)))
  (when (system-description system)
    (@tableitem "Description"
      (render-text (system-description system))
      (fresh-line)))
  (when (system-long-description system)
    (@tableitem "Long Description"
      (render-text (system-long-description system))
      (fresh-line)))
  (multiple-value-bind (author email)
      (parse-author-string (system-author system))
    (when (or author email)
      (@tableitem "Author"
	(format t "~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
	  (escape author) (and author email) (escape email)))))
  (multiple-value-bind (maintainer email)
      (parse-author-string (system-maintainer system))
    (when (or maintainer email)
      (@tableitem "Maintainer"
	(format t "~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
	  (escape maintainer) (and maintainer email) (escape email)))))
  (format t "~@[@item License~%~
	     ~A~%~]" (escape (system-license system)))
  (call-next-method system system))


;; -----
;; Nodes
;; -----

(defun system-node (system)
  "Create and return the SYSTEM node."
  (make-node :name "System"
	     :synopsis "The system documentation"
	     :before-menu-contents
	     (render-to-string (document system system))))

(defun add-system-node (node system)
  "Add SYSTEM's system node to NODE."
  (add-child node (system-node system)))


;;; asdf.lisp ends here
