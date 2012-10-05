;;; asdf.lisp --- ASDF items documentation

;; Copyright (C) 2010, 2011, 2012 Didier Verna

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
(in-readtable :com.dvlsoft.declt)


;; ==========================================================================
;; Utilities
;; ==========================================================================

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

(defmethod document :around
    ((component asdf:component) context
     &key
     &aux (relative-to (context-directory context)))
  "Anchor and index COMPONENT in CONTEXT. Document it in a @table environment."
  (anchor component relative-to)
  (index component relative-to)
  (@table () (call-next-method)))

(defmethod document ((component asdf:component) context
		     &key
		     &aux (relative-to (context-directory context)))
  "Render COMPONENT's documentation in CONTEXT."
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
    (when parent (@tableitem "Parent" (reference parent relative-to))))
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
	 (render-location (component-pathname component) relative-to))))



;; ==========================================================================
;; Files
;; ==========================================================================

;; -----------------------
;; Documentation protocols
;; -----------------------

(defmethod title ((source-file asdf:source-file) &optional relative-to)
  "Return SOURCE-FILE's title."
  (format nil "the ~A file" (relative-location source-file relative-to)))

(defmethod index ((lisp-file asdf:cl-source-file) &optional relative-to)
  "Render LISP-FILE's indexing command."
  (format t "@lispfileindex{~A}@c~%"
    (escape (relative-location lisp-file relative-to))))

(defmethod index ((c-file asdf:c-source-file) &optional relative-to)
  "Render C-FILE's indexing command."
  (format t "@cfileindex{~A}@c~%"
    (escape (relative-location c-file relative-to))))

(defmethod index ((java-file asdf:java-source-file) &optional relative-to)
  "Render JAVA-FILE's indexing command."
  (format t "@javafileindex{~A}@c~%"
    (escape (relative-location java-file relative-to))))

(defmethod index ((static-file asdf:static-file) &optional relative-to)
  "Render STATIC-FILE's indexing command."
  (format t "@otherfileindex{~A}@c~%"
    (escape (relative-location static-file relative-to))))

(defmethod index ((doc-file asdf:doc-file) &optional relative-to)
  "Render DOC-FILE's indexing command."
  (format t "@docfileindex{~A}@c~%"
    (escape (relative-location doc-file relative-to))))

(defmethod index ((html-file asdf:html-file) &optional relative-to)
  "Render HTML-FILE's indexing command."
  (format t "@htmlfileindex{~A}@c~%"
    (escape (relative-location html-file relative-to))))

(defmethod document
    ((file asdf:cl-source-file) context
     &key
     &aux (pathname (component-pathname file))
	  (external-definitions
	   (file-definitions pathname (context-external-definitions context)))
	  (internal-definitions
	   (file-definitions pathname (context-internal-definitions context))))
  "Render lisp FILE's documentation in CONTEXT."
  (call-next-method)
  (render-packages (file-packages pathname))
  (when external-definitions
    (@tableitem "Exported definitions"
      (@itemize-list
       (sort external-definitions #'string-lessp :key #'definition-symbol)
       :renderer #'reference)))
  (when internal-definitions
    (@tableitem "Internal definitions"
      (@itemize-list
       (sort internal-definitions #'string-lessp :key #'definition-symbol)
       :renderer #'reference))))


;; -----
;; Nodes
;; -----

(defun file-node
    (file context &aux (relative-to (context-directory context)))
  "Create and return a FILE node in CONTEXT."
  (make-node :name (escape (format nil "~@(~A~)" (title file relative-to)))
	     :section-name (format nil "@t{~A}"
			     (escape (relative-location file relative-to)))
	     :before-menu-contents (render-to-string (document file context))))

(defun add-files-node
    (parent context &aux (relative-to (context-directory context))
			 (system (context-system context))
			 (lisp-files (lisp-components system))
			 (other-files
			  (mapcar (lambda (type) (components system type))
				  '(asdf:c-source-file
				    asdf:java-source-file
				    asdf:doc-file
				    asdf:html-file
				    asdf:static-file)))
			 (files-node
			  (add-child parent
			    (make-node
			     :name "Files"
			     :synopsis "The files documentation"
			     :before-menu-contents (format nil "~
Files are sorted by type and then listed depth-first from the system
components tree."))))
			 (lisp-files-node
			  (add-child files-node
			    (make-node :name "Lisp files"
				       :section-name "Lisp"))))
  "Add the files node to PARENT in CONTEXT."
  (let ((system-base-name (escape (system-base-name system))))
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
		     (render-location
		      (system-definition-pathname system) relative-to)
		     (render-packages
		      (file-packages (system-definition-pathname system)))
		     (let ((external-definitions
			     (file-definitions
			      (system-definition-pathname system)
			      (context-external-definitions context)))
			   (internal-definitions
			     (file-definitions
			      (system-definition-pathname system)
			      (context-internal-definitions context))))
		       (when external-definitions
			 (@tableitem "Exported definitions"
			   (@itemize-list external-definitions
					  :renderer #'reference)))
		       (when internal-definitions
			 (@tableitem "Internal definitions"
			   (@itemize-list internal-definitions
					  :renderer #'reference)))))))))
  (dolist (file lisp-files)
    (add-child lisp-files-node (file-node file context)))
  (loop :with other-files-node
	:for files :in other-files
	:for name :in '("C files" "Java files" "Doc files" "HTML files"
			"Other files")
	:for section-name :in '("C" "Java" "Doc" "HTML" "Other")
	:when files
	  :do (setq other-files-node
		    (add-child files-node
		      (make-node :name name
				 :section-name section-name)))
	:and :do (dolist (file files)
		   (add-child other-files-node (file-node file context)))))



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

(defmethod document ((module asdf:module) context &key)
  "Render MODULE's documentation in CONTEXT."
  (call-next-method)
  (let* ((components (asdf:module-components module))
	 (length (length components)))
    (when components
      (let ((relative-to (context-directory context)))
	(@tableitem (format nil "Component~p" length)
	  (if (eq length 1)
	      (reference (first components) relative-to)
	    (@itemize-list components
			   :renderer
			   (lambda (component)
			     (reference component relative-to)))))))))


;; -----
;; Nodes
;; -----

(defun module-node (module context
		    &aux (relative-to (context-directory context)))
  "Create and return a MODULE node in CONTEXT."
  (make-node :name (escape (format nil "~@(~A~)" (title module relative-to)))
	     :section-name (format nil "@t{~A}"
			     (escape (relative-location module relative-to)))
	     :before-menu-contents
	     (render-to-string (document module context))))

(defun add-modules-node
    (parent context
     &aux (modules (module-components (context-system context))))
  "Add the modules node to PARENT in CONTEXT."
  (when modules
    (let ((modules-node
	    (add-child parent (make-node :name "Modules"
					 :synopsis "The modules documentation"
					 :before-menu-contents
					 (format nil "~
Modules are listed depth-first from the system components tree.")))))
      (dolist (module modules)
	(add-child modules-node (module-node module context))))))



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

(defmethod document ((system asdf:system) context &key)
  "Render SYSTEM's documentation in CONTEXT."
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
  (call-next-method))


;; -----
;; Nodes
;; -----

(defun system-node (context)
  "Create and return the system node in CONTEXT."
  (make-node :name "System"
	     :synopsis "The system documentation"
	     :before-menu-contents
	     (render-to-string (document (context-system context) context))))

(defun add-system-node (parent context)
  "Add the system node to PARENT in CONTEXT."
  (add-child parent (system-node context)))


;;; asdf.lisp ends here
