;;; package.lisp --- Package items

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Thu Sep  9 11:33:25 2010
;; Last Revision: Thu Sep  9 11:49:57 2010

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


;; ==========================================================================
;; Rendering Protocols
;; ==========================================================================

(defmethod to-string ((package package))
  "Return PACKAGE's name."
  (package-name package))



;; ==========================================================================
;; Item Protocols
;; ==========================================================================

(defmethod location ((package package))
  ;; #### PORTME.
  (let* ((defsrc (sb-introspect:find-definition-source package)))
    (when defsrc
      (sb-introspect:definition-source-pathname defsrc))))



;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun package-external-symbols (package &aux external-symbols)
  "Return the list of PACKAGE's external symbols."
  (do-external-symbols (symbol package external-symbols)
    (when (eq (symbol-package symbol) package)
      (push symbol external-symbols))))

(defun package-external-definitions (package)
  "Return the list of PACKAGE's external definitions."
  (loop :for symbol :in (package-external-symbols package)
	:when (symbol-definitions symbol)
	:collect :it))

(defun package-internal-symbols
    (package &aux (external-symbols (package-external-symbols package))
		  internal-symbols)
  "Return the list of PACKAGE's internal definitions."
  (do-symbols (symbol package internal-symbols)
    (when (and (not (member symbol external-symbols))
	       (eq (symbol-package symbol) package))
      (push symbol internal-symbols))))

(defun package-internal-definitions (package)
  "Return the list of PACKAGE's internal definitions."
  (loop :for symbol :in (package-internal-symbols package)
	:when (symbol-definitions symbol)
	:collect :it))


;;; package.lisp ends here
