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


(defgeneric itemize-component (stream component)
  (:documentation "Write an itemized description of COMPONENT to STREAM.")
  (:method :before (stream component)
    (format stream "@item~%@t{~A} ("
      (asdf:component-name component)))
  (:method (stream (component asdf:module))
    (write-string "module" stream))
  (:method (stream (component asdf:cl-source-file))
    (write-string "Lisp source file" stream))
  (:method (stream (component asdf:c-source-file))
    (write-string "C source file" stream))
  (:method (stream (component asdf:java-source-file))
    (write-string "Java source file" stream))
  (:method (stream (component asdf:static-file))
    (write-string "file" stream))
  (:method (stream (component asdf:doc-file))
    (write-string "doc file" stream))
  (:method (stream (component asdf:html-file))
    (write-string "HTML file" stream))
  (:method :after (stream component)
    (format stream "~@[, version ~A~])~%"
      (and (slot-boundp component 'asdf:version )
	   (asdf:component-version component)))))

(defun system-node (system)
  "Create and return the SYSTEM node."
  (make-node :name "System"
	     :synopsis "The ASDF system documentation"
	     :before-menu-contents
	     (with-output-to-string (str)
	       (format str "@table @strong~%")
	       (format str "@item Name~%@t{~A}~%" (asdf:component-name system))
	       (when (asdf:component-version system)
		 (format str "@item Version~%~A~%"
		   (asdf:component-version system)))
	       (when (asdf:system-description system)
		 (format str "@item Description~%~A~%"
		   (pretty-texify (asdf:system-description system))))
	       (when (asdf:system-long-description system)
		 (format str "@item Long Description~%~A~%"
		   (pretty-texify (asdf:system-long-description system))))
	       (multiple-value-bind (author email)
		   (parse-author-string (asdf:system-author system))
		 (when (or author email)
		   (format str
		       "@item Author~%~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
		     author (and author email) (texify email))))
	       (multiple-value-bind (maintainer email)
		   (parse-author-string (asdf:system-maintainer system))
		 (when (or maintainer email)
		   (format str
		       "@item Maintainer~%~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
		     maintainer (and maintainer email) (texify email))))
	       (when (asdf:system-license system)
		 (format str "@item License~%~A~%"
		   (asdf:system-license system)))
	       ;; #### NOTE: currently, we simply extract all the
	       ;; dependencies regardless of the operations involved.
	       ;; We also assume that dependencies are of the form
	       ;; (OP (OP DEP...) ...), but I'm not sure this is always the
	       ;; case.
	       (let ((in-order-tos (slot-value system 'asdf::in-order-to))
		     dependencies)
		 (when in-order-tos
		   (dolist (in-order-to in-order-tos)
		     (dolist (op-dependency (cdr in-order-to))
		       (dolist (dependency (cdr op-dependency))
			 (pushnew dependency dependencies))))
		   (format str "@item Dependencies~%~A~%"
		     (list-to-string
		      (mapcar (lambda (dep)
				(format nil "@t{~A}" (string-downcase dep)))
			      dependencies)))))
	       (format str "@item Components~%@itemize @bullet~%")
	       (dolist (component (asdf:module-components system))
		 (itemize-component str component))
	       (format str "@end itemize~%@end table"))))


;;; system.lisp ends here
