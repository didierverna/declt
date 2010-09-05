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

(defmethod index (stream (cl-source-file asdf:cl-source-file))
  (format stream "@lispfileindex{~A}@c~%" (component-name cl-source-file)))

(defmethod index (stream (c-source-file asdf:c-source-file))
  (format stream "@cfileindex{~A}@c~%" (component-name c-source-file)))

(defmethod index (stream (java-source-file asdf:java-source-file))
  (format stream "@javafileindex{~A}@c~%" (component-name java-source-file)))

(defmethod index (stream (static-file asdf:static-file))
  (format stream "@otherfileindex{~A}@c~%" (component-name static-file)))

(defmethod index (stream (doc-file asdf:doc-file))
  (format stream "@docfileindex{~A}@c~%" (component-name doc-file)))

(defmethod index (stream (html-file asdf:html-file))
  (format stream "@htmlfileindex{~A}@c~%" (component-name html-file)))


;; --------------------
;; Itemization protocol
;; --------------------

(defmethod itemize (stream (cl-source-file asdf:cl-source-file))
  (write-string "Lisp file" stream))

(defmethod itemize (stream (c-source-file asdf:c-source-file))
  (write-string "C file" stream))

(defmethod itemize (stream (java-source-file asdf:java-source-file))
  (write-string "Java file" stream))

(defmethod itemize (stream (static-file asdf:static-file))
  (write-string "file" stream))

(defmethod itemize (stream (doc-file asdf:doc-file))
  (write-string "doc file" stream))

(defmethod itemize (stream (html-file asdf:html-file))
  (write-string "HTML file" stream))



;; ==========================================================================
;; File Nodes
;; ==========================================================================

(defun file-node (file relative-to)
  "Create and return a FILE node."
  (make-node :name (format nil "The ~A file" (component-name file))
	     :section-name (format nil "@t{~A}" (component-name file))
	     :before-menu-contents (with-output-to-string (str)
				     (index str file)
				     (tableize str file relative-to))))

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
		       (add-child node (make-node :name "Files"
						  :synopsis
						  "The system's files"
						  :before-menu-contents
						  (format nil "~
Files are sorted by type and then listed depth-first from the system
components tree."))))
		      (lisp-files-node
		       (add-child files-node
				  (make-node :name "Lisp Files"
					     :section-name "Lisp"))))
  "Add SYSTEM's files node to NODE."
  (add-child lisp-files-node
	     (make-node :name
			(format nil "The ~A file" (system-file-name system))
			:section-name
			(format nil "@t{~A}" (system-file-name system))
			:before-menu-contents
			(with-output-to-string (str)
			  (let ((file (system-base-name system)))
			    (format str "@lispfileindex{~A}@c~%" file)
			    (format str "@table @strong~%")
			    (format str
				"@item Location~%~:[@t{~;@url{file://~]~A}~%"
			      *link-components*
			      (if *link-components*
				  (format nil "~A, ignore, ~A"
				    (make-pathname
				     :name (system-file-name system)
				     :type (system-file-type system)
				     :directory (pathname-directory
						 (component-pathname system)))
				    file)
				file)))
			  (format str "@end table~%"))))
  (dolist (file lisp-files)
    (add-child lisp-files-node (file-node file system-directory)))
  (loop :with other-files-node
    :for files :in other-files
    :for name :in '("C Files" "Java Files" "Doc Files" "HTML Files"
		    "Other Files")
    :for section-name :in '("C" "Java" "Doc" "HTML" "Other")
    :when files
    :do (setq other-files-node
	      (add-child files-node (make-node :name name
					       :section-name section-name)))
    :and :do (dolist (file files)
	       (add-child other-files-node
			  (file-node file system-directory)))))


;;; file.lisp ends here
