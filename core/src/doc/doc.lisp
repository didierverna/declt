;;; doc.lisp --- Items documentation

;; Copyright (C) 2010-2013 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

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



;;; Code:

(in-package :com.dvlsoft.declt)
(in-readtable :com.dvlsoft.declt)


;; ==========================================================================
;; Documentation Contexts
;; ==========================================================================

(defstruct context
  "The documentation context structure."
  systems
  packages
  external-definitions
  internal-definitions
  hyperlinksp)


;; This is used rather often so it is worth a shortcut
(defun context-directory (context)
  "Return CONTEXT's main system directory."
  (system-directory (car (context-systems context))))



;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

(defgeneric title (item &optional relative-to)
  (:documentation "Return ITEM's title."))

(defgeneric index (item &optional relative-to)
  (:documentation "Render ITEM's indexing command."))

(defgeneric reference (item &optional relative-to)
  (:documentation "Render ITEM's reference."))

(defgeneric document (item context &key &allow-other-keys)
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

(defun anchor-and-index (item &optional relative-to)
  "Anchor and index ITEM."
  (anchor item relative-to)
  (index item relative-to))


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

(defun render-source (item context)
  "Render an itemized source line for ITEM in CONTEXT.
Rendering is done on *standard-output*."
  (when-let ((source-pathname (source item)))
    (let* ((systems (context-systems context))
	   (relative-to (context-directory context))
	   ;; Remember that a source can be a system, although systems are not
	   ;; actual cl-source-file's.
	   (source-component
	     (loop :for component
		     :in (append systems (mapcan #'lisp-components systems))
		   :when (equal source-pathname
				(component-pathname component))
		     :return component)))
      (if source-component
	  (@tableitem "Source" (reference source-component relative-to))
	  ;; Otherwise, the source does not belong to the system. This may
	  ;; happen for automatically generated sources (sb-grovel does this
	  ;; for instance). So let's just reference the file itself.
	  (render-location source-pathname context "Source")))))

(defun render-docstring (item)
  "Render ITEM's documentation string.
Rendering is done on *standard-output*."
  (when-let ((docstring (docstring item)))
    (render-text docstring)))

(defun render-references (list title &aux (length (length list)))
  "Render references to a LIST of items.
References are rendered in a table item named TITLE as a list, unless there is
only one item in LIST.

Rendering is done on *standard-output*."
  (unless (zerop length)
    (@tableitem title
      (if (= length 1)
	  (reference (first list))
	(@itemize-list list :renderer #'reference)))))


;;; doc.lisp ends here
