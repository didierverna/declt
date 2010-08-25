;;; component.lisp --- ASDF component documentation

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Aug 25 16:06:07 2010
;; Last Revision: Wed Aug 25 16:54:11 2010

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
;; Rendering Protocols
;; ==========================================================================

;; -----------------
;; Indexing protocol
;; -----------------

(defgeneric index (stream component)
  (:documentation "Render an index command for COMPONENT to STREAM."))


;; --------------------
;; Itemization protocol
;; --------------------

(defgeneric itemize (stream component)
  (:documentation "Render an itemized description of COMPONENT to STREAM.")
  (:method :before (stream component)
    (format stream "@item~%")
    (index stream component)
    (format stream "@t{~A} ("
      (asdf:component-name component)))
  (:method :after (stream component)
    (format stream "~@[, version ~A~])~%"
      (and (slot-boundp component 'asdf:version )
	   (asdf:component-version component)))))


;;; component.lisp ends here
