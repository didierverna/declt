;;; system.lisp --- ASDF system documentation

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Aug 24 16:00:04 2010
;; Last Revision: Tue Aug 24 19:54:16 2010

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

;; ---------------------
;; Tableization protocol
;; ---------------------

(defmethod tableize (stream (system asdf:system) &optional relative-to)
  "Also describe SYSTEM's descriptions, author, maintainer and license."
  (declare (ignore relative-to))
  (format stream "@item Name~%@t{~A}~%" (component-name system))
  (when (system-description system)
    (format stream "@item Description~%~A~%"
      (pretty-texify (system-description system))))
  (when (system-long-description system)
    (format stream "@item Long Description~%~A~%"
      (pretty-texify (system-long-description system))))
  (multiple-value-bind (author email)
      (parse-author-string (system-author system))
    (when (or author email)
      (format stream "@item Author~%~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
	author (and author email) (texify email))))
  (multiple-value-bind (maintainer email)
      (parse-author-string (system-maintainer system))
    (when (or maintainer email)
      (format stream "@item Maintainer~%~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
	maintainer (and maintainer email) (texify email))))
  (when (system-license system)
    (format stream "@item License~%~A~%" (system-license system)))
  (call-next-method)
  (format stream "@item Packages~%~A~%"
    (list-to-string (system-packages system)
		    :key (lambda (package)
			   (format nil "@t{~(~A}~)"
			     (package-name package))))))



;; ==========================================================================
;; System Node
;; ==========================================================================

(defun system-node (system)
  "Create and return the SYSTEM node."
  (make-node :name "System"
	     :synopsis "The ASDF system documentation"
	     :before-menu-contents
	     (with-output-to-string (str)
	       (tableize str system (system-directory system)))))

(defun add-system-node (node system)
  "Add SYSTEM's system node to NODE."
  (add-child node (system-node system)))


;;; system.lisp ends here
