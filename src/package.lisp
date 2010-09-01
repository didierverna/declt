;;; package.lisp --- Package documentation

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Sep  1 16:04:00 2010
;; Last Revision: Wed Sep  1 17:44:46 2010

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


;;; Code:

(in-package :com.dvlsoft.declt)


;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun file-packages (file)
  "Return the list of all packages involved in FILE."
  )

;; ==========================================================================
;; Package Nodes
;; ==========================================================================

(defun add-packages-node
    (node system &aux (files (collect-components
			      (asdf:module-components system)
			      'asdf:cl-source-file)))
  "Add SYSTEM's packages node to NODE."
  (when files
    (let ((packages-node
	   (add-child node (make-node :name "Packages"
				      :synopsis "The system's packages"
				      :before-menu-contents (format nil "~
Packages are sorted by lexicographic order."))))))))


;;; package.lisp ends here
