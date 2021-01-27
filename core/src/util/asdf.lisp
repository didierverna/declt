;;; asdf.lisp --- ASDF Utilities

;; Copyright (C) 2010, 2011, 2013, 2016-2017, 2020, 2021 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Declt.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:



;;; Code:

(in-package :net.didierverna.declt)
(in-readtable :net.didierverna.declt)


;; ==========================================================================
;; Miscellaneous
;; ==========================================================================

(defun relative-location (component relative-to)
  "Return COMPONENT's location RELATIVE-TO."
  (enough-namestring (component-pathname component) relative-to))

(defun system-directory (system)
  "Return ASDF SYSTEM's directory."
  (component-pathname system))

(defun system-base-name (system &aux (file (system-source-file system)))
  "Return the basename part of ASDF SYSTEM's definition file."
  (when file (file-namestring file)))

;; #### NOTE: currently unused.
(defun system-file-name (system &aux (file (system-source-file system)))
  "Return the name part of ASDF SYSTEM's definition file."
  (when file (pathname-name file)))

;; #### NOTE: currently unused.
(defun system-file-type (system &aux (file (system-source-file system)))
  "Return the type part of ASDF SYSTEM's definition file."
  (when file (pathname-type file)))

(defun sub-component-p
    (component directory
     ;; #### FIXME: not sure this is still valid, as we now have a specific
     ;; way of loading UIOP and ASDF.
     ;; #### NOTE: COMPONENT-PATHNAME can return nil when it's impossible to
     ;; locate the component's source. This happens for example with UIOP when
     ;; ASDF is embedded in a Lisp implementation like SBCL. Sabra Crolleton
     ;; fell on this issue when trying to document CL-PROJECT, which
     ;; explicitly depends on UIOP.
     &aux (component-pathname (component-pathname component)))
  "Return T if COMPONENT can be found under DIRECTORY."
  (when component-pathname
    (pathname-match-p component-pathname
		      (make-pathname :name :wild
				     :directory
				     (append (pathname-directory directory)
					     '(:wild-inferiors))))))

(defun reorder-dependency-def (dependency-def)
  "Reorder information in DEPENDENCY-DEF so that the system is always first.
More specifically:
- simple component names are returned as-is,
- :version expressions are returned as (system :version version-specifier),
- :feature expressions are returned as (... :feature feature-expression),
- :require expressions are returned as (system :require).

Note that because a feature expression is defined recursively, the first
element in the reordered list may be another reordered sub-list rather than a
simple component name directly. In any case, the system name will always be
in the deepest first position."
  (typecase dependency-def ;; RTE to the rescue!
    (list (ecase (car dependency-def)
	    (:feature
	     (list (reorder-dependency-def (third dependency-def))
		   :feature (second dependency-def)))
	    (:version
	     (list (second dependency-def)
		   :version (third dependency-def)))
	    (:require
	     (list (second dependency-def) :require))))
    (otherwise dependency-def)))

;;; asdf.lisp ends here
