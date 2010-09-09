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

(defun package-external-definitions (package &aux external-definitions)
  "Return the list of PACKAGE's external symbols which need documenting."
  (do-external-symbols (symbol package external-definitions)
    (when (and (eq (symbol-package symbol) package)
	       (symbol-needs-documenting symbol))
      (push symbol external-definitions))))

(defun package-internal-definitions
    (package &aux (external-definitions (package-external-definitions package))
		  internal-definitions)
  "Return the list of PACKAGE's internal symbols which need documenting."
  (do-symbols (symbol package internal-definitions)
    (when (and (not (member symbol external-definitions))
	       (eq (symbol-package symbol) package)
	       (symbol-needs-documenting symbol))
      (push symbol internal-definitions))))


;;; package.lisp ends here
