;;; doc.lisp --- Items documentation

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Thu Sep  9 12:31:11 2010
;; Last Revision: Fri Sep 10 13:20:32 2010

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


;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

(defgeneric title (item &optional relative-to)
  (:documentation "Return ITEM's title."))

(defgeneric index (item &optional relative-to)
  (:documentation "Render ITEM's indexing command."))

(defgeneric reference (item &optional relative-to)
  (:documentation "Render ITEM's reference."))

(defgeneric document (item relative-to &key &allow-other-keys)
  (:documentation "Render ITEM's documentation."))



;; ==========================================================================
;; Utilities
;; ==========================================================================

;; Since node references are boring in Texinfo, we prefer to create custom
;; anchors for our items and link to them instead.
(defun anchor-name (item &optional relative-to)
  "Return ITEM's anchor name."
  (format nil "~A anchor" (title item relative-to)))

(defun anchor (item &optional relative-to)
  "Render ITEM's anchor."
  (@anchor (anchor-name item relative-to)))

(defun render-source (object relative-to
		      &aux (source (source object)))
  "Render an itemized source line for OBJECT, RELATIVE-TO.
Rendering is done on *standard-output*."
  (when source
    ;; #### NOTE: Probing the pathname as the virtue of dereferencing
    ;; symlinks. This is good because when the system definition file is
    ;; involved, it is the installed symlink which is seen, whereas we want to
    ;; advertise the original one.
    (setq source (probe-file source))
    (let ((location (escape (enough-namestring source relative-to))))
      (@tableitem "Source"
	;; #### FIXME: somewhat ugly. We fake a cl-source-file anchor name.
	(format t "@ref{The ~A file anchor, , @t{~(~A}~)}~%"
	  location
	  location)))))


;;; doc.lisp ends here
