;;; asdf.lisp --- ASDF items documentation

;; Copyright (C) 2010-2013, 2015-2017, 2019-2022 Didier Verna

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
;; Components
;; ==========================================================================

;; #### NOTE: a simpler route to this is to use ASDF:COMPONENT-FIND-PATH.
;; The merit of this approach, however, is to stay at the definitions level
;; and not access the underlying objects.
(defmethod safe-name ((definition component-definition) &optional qualified)
  "Reveal component DEFINITION's name, possibly QUALIFIED.
A QUALIFIED component's name is of the form \"path/to/component\", each
element being the name of a component's parent."
  (if (and qualified (parent definition))
    (concatenate 'string (safe-name (parent definition) t)
		 "/"
		 (call-next-method definition))
    (call-next-method definition)))

(defun render-dependency (dependency context)
  "Render a resolved DEPENDENCY specification in CONTEXT.
See `resolve-dependency-specification' for more information."
  (if (listp (car dependency))
    (render-dependency (car dependency) context)
    (reference (car dependency) context))
  (when (cdr dependency)
    ;; #### WARNING: case conversion.
    (format t ", ~A~@[ @t{~(~A~)}~]"
      (ecase (second dependency)
	(:feature "for feature")
	(:version "at least version")
	(:require "required"))
      (when (third dependency) ;; NIL for :require
	(escape (prin1-to-string (third dependency)))))))

(defun render-dependencies
    (dependencies context
     &optional (prefix "")
     &aux (length (length dependencies)))
  "Render COMPONENT's DEPENDENCIES in CONTEXT, optionally PREFIXing the title."
  (item ((format nil "~ADependenc~@p" prefix length))
    (if (eq length 1)
      (render-dependency (first dependencies) context)
      (itemize-list dependencies
	:renderer (lambda (dependency)
		    (render-dependency dependency context))))))

(defmethod document :around
    ((definition component-definition) context &key)
  "Anchor, index and document component DEFINITION in CONTEXT.
Documentation is done in a @table environment."
  (anchor-and-index definition)
  (render-docstring definition)
  (table () (call-next-method)))

(defmethod document ((definition component-definition) context &key)
  "Render ASDF component DEFINITION's documentation in CONTEXT."
  ;; Reminder: this redirects to the component's short description.
  (when-let (long-description (long-description definition))
    (item ("Long Description")
      (render-text long-description)
      (fresh-line)))
  (when-let (version (definition-version definition))
    (item ("Version") (format t "~A~%" (escape version))))
  (when-let (if-feature (if-feature definition))
    (item ("If Feature")
      ;; #### WARNING: case conversion.
      (format t "@t{~(~A~)}" (escape (prin1-to-string if-feature)))))
  ;; #### NOTE: it is kludgy to insert a bit of system-specific documentation
  ;; in the general component method, but we want to group all dependency
  ;; references together.
  (when-let (dependencies (when (typep definition 'system-definition)
			    (defsystem-dependencies definition)))
    (render-dependencies dependencies context "Defsystem "))
  (when-let (dependencies (dependencies definition))
    (render-dependencies dependencies context))
  (when-let (source (source-file definition))
    (item ("Source") (reference source context t)))
  (when (locations context)
    (item ("Location")
      (format t "@url{file://~A, ignore, @t{~:*~A}}~%"
	(escape (location definition)))))
  (when-let (parent (parent definition))
    (item ("Parent Component") (reference parent context))))




;; ==========================================================================
;; Files
;; ==========================================================================

(defmethod safe-name :around
    ((definition file-definition)
     &optional qualify
     &aux (name (call-next-method))
	  (extension (extension definition)))
  "Append DEFINITION's file extension at the end, when applicable."
  (declare (ignore qualify))
  (when extension
    (setq name (concatenate 'string name "." (reveal extension))))
  name)

(defmethod category-name ((definition file-definition))
  "Return \"file\""
  "file")

(defmethod index-command-name ((definition file-definition))
  "Return \"fileindex\""
  "fileindex")

;; #### NOTE: other kinds of files are only documented as simple components.
(defmethod document :close ((definition lisp-file-definition) context &key)
  "Render lisp file DEFINITION's references in CONTEXT."
  ;; #### NOTE: I don't think it's worth referencing all components here, so
  ;; we're documenting systems only. This could be turned into a context
  ;; option someday.
  (render-references "ASDF Systems"
    (remove-if-not #'system-definition-p (definitions definition))
    context
    t)
  (render-references "Packages"
    (remove-if-not #'package-definition-p (definitions definition))
    context
    t)
  ;; #### NOTE: generic functions and their methods are documented in a single
  ;; bloc. As a consequence, if a generic function belongs to this file,
  ;; there's no need to also reference (some of) its methods. On the other
  ;; hand, we need to reference methods for which the owner is elsewhere
  ;; (admittedly, and for the same reason, only one would suffice) or doesn't
  ;; exist. In the case of classoids, slots don't need to be referenced at all
  ;; because a slot definition is at the same lexical place as its owner.
  (flet ((organize-definitions (definitions)
	   (sort (remove-if
		     (lambda (definition)
		       (or (typep definition 'slot-definition)
			   (and (typep definition 'method-definition)
				(when (owner definition)
				  (eq (source-file definition)
				      (source-file (owner definition)))))))
		     definitions)
	       #'string-lessp ;; #### WARNING: casing policy.
	     :key #'definition-symbol)))
    (render-references "Public Interface"
      (organize-definitions (public-definitions definition))
      context)
    (render-references "Internals"
      (organize-definitions (private-definitions definition))
      context)))



;; -----
;; Nodes
;; -----

(defun file-node (definition context)
  "Create and return a file DEFINITION node in CONTEXT."
  (make-node :name (long-title definition)
	     :section-name
	     (format nil "@t{~A}" (escape (safe-name definition t)))
	     :before-menu-contents
	     (render-to-string (document definition context))))

(defun add-files-node
    (parent report context
     &aux lisp-file-definitions c-file-definitions java-file-definitions
	  html-file-definitions doc-file-definitions
	  static-file-definitions source-file-definitions file-definitions
	  (files-node (add-child parent
			(make-node
			 :name "Files"
			 :synopsis "The files documentation"
			 :before-menu-contents (format nil "~
Files are sorted by type and then listed depth-first from the systems
components trees.")))))
  "Add REPORT's files node to PARENT in CONTEXT."
  (dolist (definition
	   (remove-if-not #'file-definition-p (definitions report)))
    (etypecase definition
      ;; #### WARNING: the order is important!
      (lisp-file-definition (push definition lisp-file-definitions))
      (c-file-definition (push definition c-file-definitions))
      (java-file-definition (push definition java-file-definitions))
      (html-file-definition (push definition html-file-definitions))
      (doc-file-definition (push definition doc-file-definitions))
      (static-file-definition (push definition static-file-definitions))
      (source-file-definition (push definition source-file-definitions))
      (file-definition (push definition file-definitions))))
  ;; #### FIXME: Arnesi lists the asd file as a static-file, so it appears
  ;; twice.
  (loop :with node
	:for definitions
	  :in (mapcar #'nreverse
		(list lisp-file-definitions
		      c-file-definitions java-file-definitions
		      html-file-definitions doc-file-definitions
		      static-file-definitions source-file-definitions
		      file-definitions))
	:for name
	  :in '("Lisp files" "C files" "Java files" "HTML files" "Doc files"
		"Static files" "Source files" "Other files")
	:for section-name
	  :in '("Lisp" "C" "Java" "HTML" "Doc" "Static" "Source" "Other")
	:when definitions
	  :do (setq node
		    (add-child files-node
		      (make-node :name name :section-name section-name)))
	  :and :do (dolist (definition definitions)
		     (add-child node (file-node definition context)))))




;; ==========================================================================
;; Modules
;; ==========================================================================

(defmethod category-name ((definition module-definition))
  "Return \"module\""
  "module")

(defmethod index-command-name ((definition module-definition))
  "Return \"moduleindex\""
  "moduleindex")

(defmethod document :close ((definition module-definition) context &key)
  "Render module DEFINITION's references in CONTEXT."
  (when-let* ((children (children definition))
	      (length (length children)))
    (item ((format nil "Child Component~p" length))
      (if (eq length 1)
	(reference (first children) context)
	(itemize-list children
	  :renderer (lambda (child) (reference child context)))))))



;; -----
;; Nodes
;; -----

(defun add-modules-node (parent report context)
  "Add REPORT's modules node to PARENT in CONTEXT."
  (when-let (definitions
	     (remove-if-not
		 (lambda (definition)
		   (and (module-definition-p definition)
			(not (system-definition-p definition))))
		 (definitions report)))
    (let ((modules-node (add-child parent
			  (make-node :name "Modules"
				     :synopsis "The modules documentation"
				     :before-menu-contents
				     (format nil "~
Modules are listed depth-first from the system components tree.")))))
      (dolist (definition definitions)
	(add-child modules-node
	  (make-node :name (long-title definition)
		     :section-name (format nil "@t{~A}"
				     (escape (safe-name definition t)))
		     :before-menu-contents
		     (render-to-string (document definition context))))))))




;; ==========================================================================
;; System
;; ==========================================================================

(defmethod category-name ((definition system-definition))
  "Return \"system\""
  "system")

(defmethod index-command-name ((definition system-definition))
  "Return \"systemindex\""
  "systemindex")

;; #### NOTE: the system specific bits appear first.
(defmethod document :open ((definition system-definition) context &key)
  "Render DEFINITION's system-specific bits in CONTEXT."
  (when-let (long-name (long-name definition))
    (item ("Long Name") (format t "~A~%" (escape long-name))))
  #i(render-contacts 1)
  (flet ((render-contacts (category names emails)
	   "Render a CATEGORY contact list of NAMES and EMAILS."
	   ;; Both names and emails are null or not at the same time.
	   (when names
	     (item ((format nil
			(concatenate 'string category "~P") (length names)))
	       ;; #### FIXME: @* and map ugliness. I'm sure FORMAT can do all
	       ;; #### this.
	       (format t "~@[~A~]~:[~; ~]~@[<@email{~A}>~]"
		 (escape (car names)) (car emails) (escape (car emails)))
	       (mapc (lambda (name email)
		       (format t "@*~%~@[~A~]~:[~; ~]~@[<@email{~A}>~]"
			 (escape name) email (escape email)))
		 (cdr names) (cdr emails)))
	     (terpri))))
    (render-contacts "Maintainer"
      (maintainer-names definition) (maintainer-emails definition))
    (render-contacts "Author"
      (author-names definition) (author-emails definition)))
  (when-let (mailto (mailto definition))
    (item ("Contact") (format t "@email{~A}~%" (escape mailto))))
  (when-let (homepage (homepage definition))
    (item ("Home Page") (format t "@uref{~A}~%" (escape homepage))))
  (when-let (source-control (source-control definition))
    (item ("Source Control")
      (etypecase source-control
	(string
	 (format t "@~:[t~;uref~]{~A}~%"
	   (search "://" source-control)
	   (escape source-control)))
	(t
	 ;; #### FIXME: why this before ?
	 ;; (escape (format nil "~(~S~)" source-control))
	 (format t "@t{~A}~%" (escape (format nil "~A" source-control)))))))
  (when-let (bug-tracker (bug-tracker definition))
    (item ("Bug Tracker") (format t "@uref{~A}~%" (escape bug-tracker))))
  ;; #### WARNING: this ASDF slot is not well defined, hence the STRING
  ;; coercion.
  (when-let (license-name (license-name definition))
    (item ("License") (format t "~A~%" (escape (string license-name))))))



;; -----
;; Nodes
;; -----

(defun add-systems-node
    (parent report context
     &aux (systems-node (add-child parent
			  (make-node :name "Systems"
				     :synopsis "The systems documentation"
				     :before-menu-contents
				     (format nil "~
The main system appears first, followed by any subsystem dependency.")))))
  "Add REPORT's systems node to PARENT in CONTEXT."
  (dolist (definition
	   (remove-if-not #'system-definition-p (definitions report)))
    (let ((contents (render-to-string (document definition context))))
      (unless (zerop (length contents))
	(add-child systems-node
	  (make-node :name (long-title definition)
		     :section-name (format nil "@t{~A}"
				     (escape (safe-name definition t)))
		     :before-menu-contents contents))))))

;;; asdf.lisp ends here
