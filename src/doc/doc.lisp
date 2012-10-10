;;; doc.lisp --- Items documentation

;; Copyright (C) 2010, 2011, 2012 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

;; This file is part of Declt.

;; Declt is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3,
;; as published by the Free Software Foundation

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
;; Documentation Contexts
;; ==========================================================================

(defstruct context
  "The documentation context structure."
  system
  packages
  external-definitions
  internal-definitions
  hyperlinksp)


;; This is used rather often so it is worth a shortcut
(defun context-directory (context)
  "Return CONTEXT's system directory."
  (system-directory (context-system context)))



;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

(defgeneric title (item &optional relative-to)
  (:documentation "Return ITEM's title."))

(defgeneric index (item &optional relative-to)
  (:documentation "Render ITEM's indexing command."))

(defgeneric reference (item &optional relative-to)
  (:documentation "Render ITEM's reference."))

(defgeneric document (item context)
  (:documentation "Render ITEM's documentation in CONTEXT."))



;; ==========================================================================
;; Utilities
;; ==========================================================================

;; Since node references are boring in Texinfo, we prefer to create custom
;; anchors for our items and link to them instead.
(defun anchor-name (item &optional relative-to)
  "Return ITEM's anchor name."
  (format nil "go to ~A" (title item relative-to)))

(defun anchor (item &optional relative-to)
  "Render ITEM's anchor."
  (@anchor (anchor-name item relative-to)))


;; #### NOTE: the use of PROBE-FILE below has two purposes:
;; 1/ making sure that the file does exist, so that it can actually be linked
;;    properly,
;; 2/ dereferencing an (ASDF 1) installed system file symlink (what we get) in
;;    order to link the actual file (what we want).
;; #### FIXME: not a Declt bug, but currently, SBCL sets the source file of
;; COPY-<struct> functions to its own target-defstruct.lisp file.
(defun render-location
    (pathname context
     &optional (title "Location")
     &aux (probed-pathname (probe-file pathname))
	  (relative-to (context-directory context))
	  (hyperlinkp (and (context-hyperlinksp context) probed-pathname)))
  "Render an itemized location line for PATHNAME in CONTEXT.
Rendering is done on *standard-output*."
  (@tableitem title
    (format t "~@[@url{file://~A, ignore, ~]@t{~A}~:[~;}~]~%"
      (when hyperlinkp
	(escape probed-pathname))
      (escape (if probed-pathname
		  (enough-namestring probed-pathname relative-to)
		(concatenate 'string (namestring pathname)
			     " (not found)")))
      hyperlinkp)))

(defun render-source (object context
		      &aux (source (source object))
			   (system (context-system context))
			   (relative-to (context-directory context)))
  "Render an itemized source line for OBJECT in CONTEXT.
Rendering is done on *standard-output*."
  (when source
    (cond
      ;; First, try the system definition file.
      ((equal source (system-definition-pathname system))
       ;; #### NOTE: Probing the pathname as the virtue of dereferencing
       ;; symlinks. This is good because when the system definition file is
       ;; involved, it is the installed symlink which is seen, whereas we want
       ;; to advertise the original one.
       (let ((location
	       (escape (enough-namestring (probe-file source) relative-to))))
	 (@tableitem "Source"
	   ;; #### FIXME: somewhat ugly. We fake a cl-source-file anchor name.
	   (format t "@ref{go to the ~A file, , @t{~(~A}~)} (Lisp file)~%"
	     location
	     location))))
      (t
       ;; Next, try a SYSTEM's cl-source-file.
       (let ((cl-source-file
	       (loop :for file :in (lisp-components system)
		     :when (equal source (component-pathname file))
		       :return file)))
	 (if cl-source-file
	     (@tableitem "Source"
	       (reference cl-source-file relative-to))
	   ;; Otherwise, the source file does not belong to the system. This
	   ;; may happen for automatically generated sources (sb-grovel does
	   ;; this for instance). So let's just reference the file itself.
	   (render-location source context "Source")))))))

(defun render-docstring (object)
  "Render an itemized documentation line for OBJECT.
Rendering is done on *standard-output*."
  (let ((docstring (docstring object)))
    (when docstring
      (@tableitem "Documentation"
	(render-text docstring)))))

(defun render-references (list title &aux (length (length list)))
  "Render references to a LIST of objects.
References are rendered in a table item named TITLE as a list, unless there is
only one object in LIST.

Rendering is done on *standard-output*."
  (unless (zerop length)
    (@tableitem title
      (if (= length 1)
	  (reference (first list))
	(@itemize-list list :renderer #'reference)))))

(defun render-internal-definitions-references (definitions)
  "Render references to a list of internal DEFINITIONS."
  (render-references definitions "Internal Definitions"))

(defun render-external-definitions-references (definitions)
  "Render references to a list of external DEFINITIONS."
  (render-references definitions "Exported Definitions"))

(defun render-definition-core (definition context)
  "Render DEFINITION's documentation core in CONTEXT.
The documentation core includes all common definition attributes:
  - docstring,
  - package,
  - source location.

Each element is rendered as a table item."
  (render-docstring definition)
  (@tableitem "Package"
    (reference (symbol-package (definition-symbol definition))))
  (render-source definition context))

(defun anchor-and-index (definition)
  "Anchor and index DEFINITION."
  (anchor definition)
  (index definition))

(defmacro render-@defvaroid (kind varoid context)
  "Render VAROID's definition of KIND in CONTEXT."
  (let ((|@defform| (intern (concatenate 'string "@DEF" (symbol-name kind))
			    :com.dvlsoft.declt))
	(the-varoid (gensym "varoid")))
    `(let ((,the-varoid ,varoid))
       (,|@defform| (string-downcase (name ,the-varoid))
	 (anchor-and-index ,the-varoid)
	 (@table ()
	   (render-definition-core ,the-varoid ,context))))))

(defun render-@defconstant (constant context)
  "Render CONSTANT's documentation in CONTEXT."
  (render-@defvaroid :constant constant context))

(defun render-@defspecial (special context)
  "Render SPECIAL variable's documentation in CONTEXT."
  (render-@defvaroid :special special context))

(defmacro render-@defunoid (kind (funcoid &rest funcoids) context &body body)
  "Render FUNCOID's definition of KIND in CONTEXT.
When FUNCOIDS, render their definitions jointly."
  (let ((|@defform| (intern (concatenate 'string "@DEF" (symbol-name kind))
			    :com.dvlsoft.declt))
	(|@defformx| (intern (concatenate 'string
			       "@DEF" (symbol-name kind) "X")
			     :com.dvlsoft.declt))
	(the-funcoid (gensym "funcoid")))
    `(let ((,the-funcoid ,funcoid))
       (,|@defform| (string-downcase (name ,the-funcoid))
		    (lambda-list ,the-funcoid)
	 (anchor-and-index ,the-funcoid)
	 ,@(mapcar (lambda (funcoid)
		     (let ((the-funcoid (gensym "funcoid")))
		       `(let ((,the-funcoid ,funcoid))
			  (,|@defformx|
			   (string-downcase (name ,the-funcoid))
			   (lambda-list ,the-funcoid))
			  (anchor-and-index ,the-funcoid))))
		   funcoids)
	 (@table ()
	   (render-definition-core ,the-funcoid ,context)
	   ,@body)))))

(defun render-@defun (function context)
  "Render FUNCTION's definition in CONTEXT."
  (render-@defunoid :un (function) context))

(defun render-@defunx (reader writer context)
  "Render READER and WRITER's definitions jointly in CONTEXT."
  (render-@defunoid :un (reader writer) context))

(defun render-@defmac (macro context)
  "Render MACRO's definition in CONTEXT."
  (render-@defunoid :mac (macro) context))

(defmacro %render-@defmethod ((method &rest methods) context)
  "Render METHOD's definition in CONTEXT.
When METHODS, render their definitions jointly."
  (let ((the-method (gensym "method")))
    `(let ((,the-method ,method))
       (@defmethod (string-downcase (name ,the-method))
	   (lambda-list ,the-method)
	   (specializers ,the-method)
	   (qualifiers ,the-method)
	 (anchor-and-index ,the-method)
	 ,@(mapcar (lambda (method)
		     (let ((the-method (gensym "method")))
		       `(let ((,the-method ,method))
			  (@defmethodx
			   (string-downcase (name ,the-method))
			   (lambda-list ,the-method)
			   (specializers ,the-method)
			   (qualifiers ,the-method))
			  (anchor-and-index ,the-method))))
		   methods)
	 (@table ()
	   (render-docstring ,the-method)
	   (render-source ,the-method ,context))))))

(defun render-@defmethod (method context)
  "Render METHOD's definition in CONTEXT."
  (%render-@defmethod (method) context))

(defun render-@defmethodx (reader writer context)
  "Render READER and WRITER methods'definitions jointly in CONTEXT."
  (%render-@defmethod (reader writer) context))

(defmacro render-@defgeneric (generic context &body body)
  "Render GENERIC's definition in CONTEXT."
  `(render-@defunoid :generic (,generic) ,context ,@body))

(defmacro render-@defgenericx (reader writer context &body body)
  "Render generic READER and WRITER's definitions jointly in CONTEXT."
  `(render-@defunoid :generic (,reader ,writer) ,context ,@body))

(defmacro render-@defclassoid (kind classoid context)
  "Render CLASSOID's definition of KIND in CONTEXT."
  (let ((|@defform| (intern (concatenate 'string "@DEF" (symbol-name kind))
			    :com.dvlsoft.declt))
	(the-classoid (gensym "classoid")))
    `(let ((,the-classoid ,classoid))
       (,|@defform| (string-downcase (name ,the-classoid))
	 (anchor-and-index ,the-classoid)
	 (@table ()
	   (render-definition-core ,the-classoid ,context)
	   (render-references
	    (classoid-definition-parents ,the-classoid)
	    "Direct superclasses")
	   (render-references
	    (classoid-definition-children ,the-classoid)
	    "Direct subclasses"))))))

(defun render-@defcond (condition context)
  "Render CONDITION's definition in CONTEXT."
  (render-@defclassoid :cond condition context))

(defun render-@defstruct (structure context)
  "Render STRUCTURE's definition in CONTEXT."
  (render-@defclassoid :struct structure context))

(defun render-@defclass (class context)
  "Render CLASS's definition in CONTEXT."
  (render-@defclassoid :class class context))


;;; doc.lisp ends here
