;;; asdf.lisp --- ASDF items documentation

;; Copyright (C) 2010-2013, 2016 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Declt.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:



;;; Code:

(in-package :net.didierverna.declt)
(in-readtable :net.didierverna.declt)


;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun render-packages-references (packages)
  "Render a list of PACKAGES references."
  (render-references packages "Packages"))

;; #### NOTE: the only ASDF components documented by Declt are source files,
;; #### modules and systems. This function is used by the corresponding
;; #### REFERENCE methods because the code is the same. The reason we don't do
;; #### this as a method on COMPONENT directly is that such a method (see
;; #### below) handles other components, such as extensions, that we don't
;; #### know about.
(defun reference-component (component)
  "Render COMPONENT's reference."
  (format t "@ref{~A, , @t{~(~A}~)} (~A)~%"
    (escape-anchor (anchor-name component))
    (escape component)
    (type-name component)))

(defun virtual-path (component)
  "Return CONMPONENT's virtual path.
This is the string of successive component names to access COMPONENT from the
toplevel system, separated by slashes."
  (format nil "~{~A~^/~}" (component-find-path component)))


;; ==========================================================================
;; Components
;; ==========================================================================

;; -----------------------
;; Documentation protocols
;; -----------------------

;; #### NOTE: this method is needed as a default method for potential ASDF
;; extensions. I'm not willing to hard-code all possible such extensions, past
;; present and future. Besides, those components would not be related to code,
;; so Declt will not document them.
(defmethod reference ((component asdf:component))
  "Render unreferenced COMPONENT."
  (format t "@t{~(~A}~) (other component)~%" (escape component)))

(defmethod document :around ((component asdf:component) context &key)
  "Anchor and index COMPONENT in CONTEXT. Document it in a @table environment."
  (anchor-and-index component)
  (@table () (call-next-method)))

(defgeneric render-dependency (dependency-def component relative-to)
  (:documentation "Render COMPONENT's DEPENDENCY-DEF RELATIVE-TO.
Dependencies are referenced only if they are RELATIVE-TO the system being
documented. Otherwise, they are just listed.")
  (:method (simple-component-name component relative-to
	    &aux (dependency
		  (resolve-dependency-name component simple-component-name)))
    "Render COMPONENT's SIMPLE-COMPONENT-NAME dependency RELATIVE-TO."
    (if (sub-component-p dependency relative-to)
	(reference dependency)
	(format t "@t{~(~A}~)" (escape simple-component-name))))
  ;; #### NOTE: this is where I'd like more advanced pattern matching
  ;; capabilities.
  (:method ((dependency-def list) component relative-to)
    "Render COMPONENT's DEPENDENCY-DEF (a list) RELATIVE-TO."
    (cond ((eq (car dependency-def) :feature)
	   (render-dependency (caddr dependency-def) component relative-to)
	   (format t " (for feature @t{~(~A}~))"
	     (escape (cadr dependency-def))))
	  ((eq (car dependency-def) :version)
	   (render-dependency (cadr dependency-def) component relative-to)
	   (format t " (at least version @t{~(~A}~))"
	     (escape (caddr dependency-def))))
	  ((eq (car dependency-def) :require)
	   (format t "required module @t{~(~A}~)"
		   (escape (cadr dependency-def))))
	  (t
	   (warn "Invalid ASDF dependency.")
	   (format t "@t{~(~A}~)"
	     (escape (princ-to-string dependency-def)))))))

(defun render-dependencies (dependencies component relative-to
			    &optional (prefix "")
			    &aux (length (length dependencies)))
  "Render COMPONENT's DEPENDENCIES RELATIVE-TO.
Optionally PREFIX the title."
  (@tableitem (format nil "~ADependenc~@p" prefix length)
    (if (eq length 1)
	(render-dependency (first dependencies) component relative-to)
	(@itemize-list dependencies
	  :renderer (lambda (dependency)
		      (render-dependency dependency component
					 relative-to))))))

(defmethod document ((component asdf:component) context
		     &key
		     &aux (relative-to (context-directory context)))
  "Render COMPONENT's documentation in CONTEXT."
  (when-let ((description (component-description component)))
    (@tableitem "Description"
      (render-text description)
      (fresh-line)))
  (when-let ((long-description (component-long-description component)))
    (@tableitem "Long Description"
      (render-text long-description)
      (fresh-line)))
  (format t "~@[@item Version~%~
		  ~A~%~]"
	  (escape (component-version component)))
  (when-let ((if-feature (component-if-feature component)))
    (@tableitem "If Feature"
      (format t "@t{~(~A}~)" (escape if-feature))))
  (when-let ((dependencies (when (typep component 'asdf:system)
			     (defsystem-dependencies component))))
    (render-dependencies dependencies component relative-to "Defsystem "))
  (when-let ((dependencies (component-sideway-dependencies component)))
    (render-dependencies dependencies component relative-to))
  (when-let ((parent (component-parent component)))
    (@tableitem "Parent" (reference parent)))
  (cond ((typep component 'asdf:system) ;; Yuck!
	 ;; #### WARNING: the system file is not an ASDF component per-se, so
	 ;; I need to fake a reference to a CL-SOURCE-FILE. This is done by
	 ;; reproducing the effect of REFERENCE-COMPONENT.
	 (when (system-source-file component)
	   (@tableitem "Source"
	     (let ((system-base-name (escape (system-base-name component))))
	       (format t "@ref{go to the ~A file, , @t{~(~A}~)} (Lisp file)~%"
		 (escape-anchor system-base-name)
		 (escape system-base-name)))))
	 (when (context-hyperlinksp context)
	   (let ((system-source-directory
		   (escape (system-source-directory component))))
	     (@tableitem "Directory"
	       (format t "@url{file://~A, ignore, @t{~A}}~%"
		 system-source-directory
		 system-source-directory)))))
	(t
	 (render-location (component-pathname component) context))))



;; ==========================================================================
;; Files
;; ==========================================================================

;; -----------------------
;; Documentation protocols
;; -----------------------

(defmethod title ((source-file asdf:source-file))
  "Return SOURCE-FILE's title."
  (format nil "the ~A file" (virtual-path source-file)))

(defmethod anchor-name ((source-file asdf:source-file))
  "Return SOURCE-FILE's anchor name."
  (format nil "~A file" (virtual-path source-file)))

(defmethod index ((lisp-file asdf:cl-source-file))
  "Render LISP-FILE's indexing command."
  (format t "@lispfileindex{~A}@c~%" (escape (virtual-path lisp-file))))

(defmethod index ((c-file asdf:c-source-file))
  "Render C-FILE's indexing command."
  (format t "@cfileindex{~A}@c~%" (escape (virtual-path c-file))))

(defmethod index ((java-file asdf:java-source-file))
  "Render JAVA-FILE's indexing command."
  (format t "@javafileindex{~A}@c~%" (escape (virtual-path java-file))))

(defmethod index ((static-file asdf:static-file))
  "Render STATIC-FILE's indexing command."
  (format t "@otherfileindex{~A}@c~%" (escape (virtual-path static-file))))

(defmethod index ((doc-file asdf:doc-file))
  "Render DOC-FILE's indexing command."
  (format t "@docfileindex{~A}@c~%" (escape (virtual-path doc-file))))

(defmethod index ((html-file asdf:html-file))
  "Render HTML-FILE's indexing command."
  (format t "@htmlfileindex{~A}@c~%" (escape (virtual-path html-file))))

(defmethod reference ((source-file asdf:source-file))
  "Render SOURCE-FILE's reference."
  (reference-component source-file))

(defmethod document ((file asdf:cl-source-file) context
		     &key
		     &aux (pathname (component-pathname file)))
  "Render lisp FILE's documentation in CONTEXT."
  (call-next-method)
  (render-packages-references (file-packages pathname))
  (render-external-definitions-references
   (sort (file-definitions pathname (context-external-definitions context))
	 #'string-lessp
	 :key #'definition-symbol))
  (render-internal-definitions-references
   (sort (file-definitions pathname (context-internal-definitions context))
	 #'string-lessp
	 :key #'definition-symbol)))


;; -----
;; Nodes
;; -----

;; #### FIXME: one of the casing problem is here. We shouldn't downcase file
;; names!
(defun file-node (file context)
  "Create and return a FILE node in CONTEXT."
  (make-node :name (format nil "~@(~A~)" (title file))
	     :section-name (format nil "@t{~A}" (escape (virtual-path file)))
	     :before-menu-contents (render-to-string (document file context))))

(defun add-files-node
    (parent context &aux (systems (context-systems context))
			 (lisp-files (mapcan #'lisp-components systems))
			 (other-files
			  (list
			   (mapcan (lambda (system)
				     (components system 'asdf:c-source-file))
				   systems)
			   (mapcan (lambda (system)
				     (components system 'asdf:java-source-file))
				   systems)
			   (mapcan (lambda (system)
				     (components system 'asdf:html-file))
				   systems)
			   (remove-if
			    (lambda (component)
			      (typep component 'asdf:html-file))
			    (mapcan (lambda (system)
				      (components system 'asdf:doc-file))
				    systems))
			   (remove-if
			    (lambda (component)
			      (typep component 'asdf:doc-file))
			    (mapcan (lambda (system)
				      (components system 'asdf:static-file))
				    systems))))
			 (files-node
			  (add-child parent
			    (make-node
			     :name "Files"
			     :synopsis "The files documentation"
			     :before-menu-contents (format nil "~
Files are sorted by type and then listed depth-first from the systems
components trees."))))
			 (lisp-files-node
			  (add-child files-node
			    (make-node :name "Lisp files"
				       :section-name "Lisp"))))
  "Add the files node to PARENT in CONTEXT."
  ;; #### NOTE: the .asd are Lisp files, but not components. I still want them
  ;; to be listed here (and first) so I need to duplicate some of what the
  ;; DOCUMENT method on lisp files does.
  ;; #### WARNING: multiple systems may be defined in the same .asd file.
  ;; #### FIXME: Arnesi lists the asd file as a static-file, so it appears
  ;; twice.
  (dolist (system (remove-duplicates
		   (remove-if #'null systems :key #'system-source-file)
		   :test #'equal :key #'system-source-file))
    (let ((system-base-name (escape (system-base-name system)))
	  (system-source-file (system-source-file system)))
      (add-child lisp-files-node
	(make-node :name (format nil "The ~A file" system-base-name)
		   :section-name (format nil "@t{~A}" system-base-name)
		   :before-menu-contents
		   (render-to-string
		     (@anchor
		      (format nil "go to the ~A file" system-base-name))
		     (format t "@lispfileindex{~A}@c~%" system-base-name)
		     (@table ()
		       (render-location system-source-file context)
		       (render-references
			(loop :for system :in systems
			      :when (equal (system-source-file system)
					   system-source-file)
				:collect system)
			"Systems")
		       (render-packages-references
			(file-packages system-source-file))
		       (render-external-definitions-references
			(sort (file-definitions system-source-file
						(context-external-definitions
						 context))
			      #'string-lessp
			      :key #'definition-symbol))
		       (render-internal-definitions-references
			(sort (file-definitions system-source-file
						(context-internal-definitions
						 context))
			      #'string-lessp
			      :key #'definition-symbol))))))))
  (dolist (file lisp-files)
    (add-child lisp-files-node (file-node file context)))
  (loop :with other-files-node
	:for files :in other-files
	:for name
	  :in '("C files" "Java files" "HTML files" "Doc files" "Other files")
	:for section-name :in '("C" "Java" "HTML" "Doc" "Other")
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

(defmethod title ((module asdf:module))
  "Return MODULE's title."
  (format nil "the ~A module" (virtual-path module)))

(defmethod title ((module asdf:module))
  "Return MODULE's title."
  (format nil "the ~A module" (virtual-path module)))

(defmethod anchor-name ((module asdf:module))
  "Return MODULE's anchor name."
  (format nil "~A module" (virtual-path module)))

(defmethod index ((module asdf:module))
  "Render MODULE's indexing command."
  (format t "@moduleindex{~A}@c~%" (escape (virtual-path module))))

(defmethod reference ((module asdf:module))
  "Render MODULE's reference."
  (reference-component module))

(defmethod document ((module asdf:module) context &key)
  "Render MODULE's documentation in CONTEXT."
  (call-next-method)
  (when-let* ((components (asdf:module-components module))
	      (length (length components)))
    (@tableitem (format nil "Component~p" length)
      (if (eq length 1)
	(reference (first components))
	(@itemize-list components :renderer #'reference)))))


;; -----
;; Nodes
;; -----

(defun module-node (module context)
  "Create and return a MODULE node in CONTEXT."
  (make-node :name (format nil "~@(~A~)" (title module))
	     :section-name (format nil "@t{~A}" (escape (virtual-path module)))
	     :before-menu-contents
	     (render-to-string (document module context))))

(defun add-modules-node
    (parent context
     &aux (modules (mapcan #'module-components (context-systems context))))
  "Add the modules node to PARENT in CONTEXT."
  (when modules
    (let ((modules-node (add-child parent
			  (make-node :name "Modules"
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

(defmethod title ((system asdf:system))
  "Return SYSTEM's title."
  (format nil "the ~A system" (name system)))

(defmethod anchor-name ((system asdf:system))
  "Return SYSTEM's anchor name."
  (format nil "~A system" (name system)))

(defmethod index ((system asdf:system))
  "Render SYSTEM's indexing command."
  (format t "@systemindex{~A}@c~%" (escape system)))

(defmethod reference ((system asdf:system))
  "Render SYSTEM's reference."
  (reference-component system))

(defmethod document ((system asdf:system) context &key)
  "Render SYSTEM's documentation in CONTEXT."
  (when-let ((long-name (system-long-name system)))
    (@tableitem "Long Name"
      (format t "~A~%" (escape long-name))))
  (multiple-value-bind (maintainers emails)
      (|parse-contact(s)| (system-maintainer system))
    (when maintainers
      (@tableitem (format nil "Maintainer~P" (length maintainers))
	;; #### FIXME: @* and map uglyness. I'm sure FORMAT can do all this.
	(format t "~@[~A~]~:[~; ~]~@[<@email{~A}>~]"
	  (escape (car maintainers)) (car emails) (escape (car emails)))
	(mapc (lambda (maintainer email)
		(format t "@*~%~@[~A~]~:[~; ~]~@[<@email{~A}>~]"
		  (escape maintainer) email (escape email)))
	  (cdr maintainers) (cdr emails)))
      (terpri)))
  (multiple-value-bind (authors emails)
      (|parse-contact(s)| (system-author system))
    (when authors
      (@tableitem (format nil "Author~P" (length authors))
	;; #### FIXME: @* and map uglyness. I'm sure FORMAT can do all this.
	(format t "~@[~A~]~:[~; ~]~@[<@email{~A}>~]"
	  (escape (car authors)) (car emails) (escape (car emails)))
	(mapc (lambda (author email)
		(format t "@*~%~@[~A~]~:[~; ~]~@[<@email{~A}>~]"
		  (escape author) email (escape email)))
	  (cdr authors) (cdr emails)))
      (terpri)))
  (when-let ((mailto (system-mailto system)))
    (@tableitem "Contact"
      (format t "@email{~A}~%" (escape mailto))))
  (when-let ((homepage (system-homepage system)))
    (@tableitem "Home Page"
      (format t "@uref{~A}~%" (escape homepage))))
  (when-let ((source-control (system-source-control system)))
    (@tableitem "Source Control"
      (etypecase source-control
	(string
	 (format t "@~:[t~;uref~]{~A}~%"
		 (search "://" source-control)
		 (escape source-control)))
	(t
	 (format t "@t{~A}~%"
		 (escape (format nil "~(~S~)" source-control)))))))
  (when-let ((bug-tracker (system-bug-tracker system)))
    (@tableitem "Bug Tracker"
      (format t "@uref{~A}~%" (escape bug-tracker))))
  (format t "~@[@item License~%~
	     ~A~%~]" (escape (system-license system)))
  (call-next-method))


;; -----
;; Nodes
;; -----

(defun system-node (system context)
  "Create and return a SYSTEM node in CONTEXT."
  (make-node :name (format nil "~@(~A~)" (title system))
	     :section-name (format nil "@t{~(~A~)}" (escape system))
	     :before-menu-contents
	     (render-to-string (document system context))))

(defun add-systems-node
    (parent context
     &aux (systems-node (add-child parent
			  (make-node :name "Systems"
				     :synopsis "The systems documentation"
				     :before-menu-contents
				     (format nil "~
The main system appears first, followed by any subsystem dependency.")))))
  "Add the systems node to PARENT in CONTEXT."
  (dolist (system (context-systems context))
    (add-child systems-node (system-node system context))))


;;; asdf.lisp ends here
