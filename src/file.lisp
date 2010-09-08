;;; file.lisp --- ASDF file documentation

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Aug 25 18:52:11 2010
;; Last Revision: Wed Aug 25 18:52:11 2010

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
;; Rendering Protocols
;; ==========================================================================

;; -----------------
;; Indexing protocol
;; -----------------

(defmethod index ((cl-source-file asdf:cl-source-file) &optional relative-to)
  (format t "@lispfileindex{~A}@c~%"
    (escape (relative-pathname cl-source-file relative-to))))

(defmethod index ((c-source-file asdf:c-source-file) &optional relative-to)
  (format t "@cfileindex{~A}@c~%"
    (escape (relative-pathname c-source-file relative-to))))

(defmethod index
    ((java-source-file asdf:java-source-file) &optional relative-to)
  (format t "@javafileindex{~A}@c~%"
    (escape (relative-pathname java-source-file relative-to))))

(defmethod index ((static-file asdf:static-file) &optional relative-to)
  (format t "@otherfileindex{~A}@c~%"
    (escape (relative-pathname static-file relative-to))))

(defmethod index ((doc-file asdf:doc-file) &optional relative-to)
  (format t "@docfileindex{~A}@c~%"
    (escape (relative-pathname doc-file relative-to))))

(defmethod index ((html-file asdf:html-file) &optional relative-to)
  (format t "@htmlfileindex{~A}@c~%"
    (escape (relative-pathname html-file relative-to))))


;; --------------------
;; Referencing protocol
;; --------------------

(defmethod component-type-name ((cl-source-file asdf:cl-source-file))
  "Lisp file")

(defmethod component-type-name ((c-source-file asdf:c-source-file))
  "C file")

(defmethod component-type-name ((java-source-file asdf:java-source-file))
  "Java file")

(defmethod component-type-name ((static-file asdf:static-file))
  "file")

(defmethod component-type-name ((doc-file asdf:doc-file))
  "doc file")

(defmethod component-type-name ((html-file asdf:html-file))
  "HTML file")



;; ==========================================================================
;; File Nodes
;; ==========================================================================

(defun file-node (file relative-to)
  "Create and return a FILE node."
  (make-node :name (format nil "The ~A file"
		     (escape (relative-pathname file relative-to)))
	     :section-name (format nil "@t{~A}"
			     (escape (relative-pathname file relative-to)))
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
      (make-node :name (format nil "The ~A file" system-base-name)
		 :section-name (format nil "@t{~A}" system-base-name)
		 :before-menu-contents
		 (render-to-string
		   (format t "@lispfileindex{~A}@c~%~
			      @table @strong~%~
			      @item Location~%~
			      ~@[@url{file://~A, ignore, ~]@t{~A}~:[~;}~]~%"
		     system-base-name
		     (when *link-components*
		       (escape (make-pathname
				:name (system-file-name system)
				:type (system-file-type system)
				:directory (pathname-directory
					    (component-pathname system)))))
		     system-base-name
		     *link-components*)
		   (format t "@end table~%")))))
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


;;; file.lisp ends here
