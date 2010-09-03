;;; symbol.lisp --- Symbols rendering

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Thu Sep  2 23:22:59 2010
;; Last Revision: Thu Sep  2 23:22:59 2010

;; This file is part of Declt.

;; Declt is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3,
;; as published by the Free Software Foundation.

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


(defun render-symbol (stream symbol)
  "Render SYMBOL's documentation on STREAM."
  (when (constantp symbol)
    (format stream "@defvr Constant ~A~%" (string-downcase symbol))
    (when (documentation symbol 'variable)
      (write-string (pretty-texify (documentation symbol 'variable)) stream)
      (fresh-line))
    (format stream "@end defvr~%"))
  (when (eql (sb-int:info :variable :kind symbol) :special)
    (format stream "@defvr {Special Variable} ~A~%" (string-downcase symbol))
    (when (documentation symbol 'variable)
      (write-string (pretty-texify (documentation symbol 'variable)) stream)
      (fresh-line))
    (format stream "@end defvr~%"))
  (let ((class (find-class symbol nil)))
    (when class
      (let ((error-condition-p (subtypep class 'condition))
	    (class-name (string-downcase symbol)))
	(format stream "@deftp {~A} ~A~%"
	  (if error-condition-p
	      "Error Condition"
	    "Class")
	  class-name)
	(format stream "@tpindex @r{~A, }~A~%"
	  (if error-condition-p
	      "Error Conditions"
	    "Classes")
	  class-name)
	(when (documentation symbol 'type)
	  (write-string (pretty-texify (documentation symbol 'type)) stream)
	  (fresh-line))
	(format stream "@end deftp~%")))))


;;; symbol.lisp ends here
