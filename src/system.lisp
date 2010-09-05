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

;; ----------------------
;; Documentation protocol
;; ----------------------

(defmethod document ((system asdf:system) &optional relative-to)
  (declare (ignore relative-to))
  (format t "@item Name~%@t{~A}~%" (component-name system))
  (when (system-description system)
    (format t "@item Description~%")
    (render-string (system-description system)))
  (when (system-long-description system)
    (format t "@item Long Description~%")
    (render-string (system-long-description system)))
  (multiple-value-bind (author email)
      (parse-author-string (system-author system))
    (when (or author email)
      (format t "@item Author~%~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
	author (and author email) (escape email))))
  (multiple-value-bind (maintainer email)
      (parse-author-string (system-maintainer system))
    (when (or maintainer email)
      (format t "@item Maintainer~%~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
	maintainer (and maintainer email) (escape email))))
  (format t "~@[@item License~%~A~%~]" (system-license system))
  (call-next-method)
  (format t "@item Packages~%")
  (@itemize-list (system-packages system) :renderer #'reference))



;; ==========================================================================
;; System Node
;; ==========================================================================

(defun system-node (system)
  "Create and return the SYSTEM node."
  (make-node :name "System"
	     :synopsis "The ASDF system documentation"
	     :before-menu-contents
	     (render-to-string (document system (system-directory system)))))

(defun add-system-node (node system)
  "Add SYSTEM's system node to NODE."
  (add-child node (system-node system)))


;;; system.lisp ends here
