;;; item.lisp --- Items subject to documentation

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Sep  8 21:10:37 2010
;; Last Revision: Fri Sep 10 13:24:07 2010

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
;; Item Protocols
;; ==========================================================================

(defgeneric location (item)
  (:documentation "Return ITEM's pathname."))



;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun relative-location (item relative-to)
  "Return ITEM's location RELATIVE-TO."
  (let* ((location (location item))
	 (relative-location (when location
			      (enough-namestring location relative-to))))
    ;; #### HACK ALERT! Some items might end up being located in the *symlink*
    ;; to the system file. In such a case, LOCATION is actually not
    ;; RELATIVE-TO, but we know this is the system file so we just return the
    ;; file name.
    (when (and relative-location
	       (string= relative-location (namestring location)))
      (setq relative-location (file-namestring location)))
    relative-location))


;;; item.lisp ends here
