;;; component.lisp --- ASDF component documentation

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Aug 25 16:06:07 2010
;; Last Revision: Wed Aug 25 16:20:42 2010

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


;;; Code:

(in-package :com.dvlsoft.declt)


;; ==========================================================================
;; The Indexing Protocol
;; ==========================================================================

(defgeneric index (stream component)
  (:documentation "Write an index command for COMPONENT to STREAM.")
  (:method (stream component))
  (:method (stream (cl-source-file asdf:cl-source-file))
    (format stream "@lispfileindex{~A}@c~%"
      (asdf:component-name cl-source-file)))
  (:method (stream (c-source-file asdf:c-source-file))
    (format stream "@cfileindex{~A}@c~%"
      (asdf:component-name c-source-file)))
  (:method (stream (java-source-file asdf:java-source-file))
    (format stream "@javafileindex{~A}@c~%"
      (asdf:component-name java-source-file)))
  (:method (stream (static-file asdf:static-file))
    (format stream "@otherfileindex{~A}@c~%"
      (asdf:component-name static-file)))
  (:method (stream (doc-file asdf:doc-file))
    (format stream "@docfileindex{~A}@c~%"
      (asdf:component-name doc-file)))
  (:method (stream (html-file asdf:html-file))
    (format stream "@htmlfileindex{~A}@c~%"
      (asdf:component-name html-file))))

;; ==========================================================================
;; The Itemization Protocol
;; ==========================================================================

(defgeneric itemize (stream component)
  (:documentation "Write an itemized description of COMPONENT to STREAM.")
  (:method :before (stream component)
    (format stream "@item~%")
    (index stream component)
    (format stream "@t{~A} ("
      (asdf:component-name component)))
  (:method (stream (cl-source-file asdf:cl-source-file))
    (write-string "Lisp source file" stream))
  (:method (stream (c-source-file asdf:c-source-file))
    (write-string "C source file" stream))
  (:method (stream (java-source-file asdf:java-source-file))
    (write-string "Java source file" stream))
  (:method (stream (static-file asdf:static-file))
    (write-string "file" stream))
  (:method (stream (doc-file asdf:doc-file))
    (write-string "doc file" stream))
  (:method (stream (html-file asdf:html-file))
    (write-string "HTML file" stream))
  (:method :after (stream component)
    (format stream "~@[, version ~A~])~%"
      (and (slot-boundp component 'asdf:version )
	   (asdf:component-version component)))))


;;; component.lisp ends here
