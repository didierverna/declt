;;; version.lisp --- Declt version management

;; Copyright (C) 2015, 2017, 2019, 2021, 2022, 2024 Didier Verna

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

(in-package :net.didierverna.declt.setup)
(in-readtable :net.didierverna.declt)



;; Copyright years
(defvar *copyright-years* "2010-2013, 2015-2022, 2024, 2025"
  "A string denoting the copyright years for the whole project.")



;; Version specifiers

(defparameter *release-major-level* 4
  "The major level of this release.")

(defparameter *release-minor-level* 0
  "The minor level of this release.")

(defparameter *release-status* :beta
  "The status of this release.")

(defparameter *release-status-level* 3
  "The status level of this release.")

(defparameter *release-name* "William Riker"
  "The name of this release.
The general naming theme for Declt is \"Star Trek characters\".")



;; Internal utilities

(defun release-status-number (release-status)
  (ecase release-status
    (:alpha 0)
    (:beta 1)
    (:rc 2)
    (:patchlevel 3)))

;; #### TODO: I'm sure the format strings can be improved
(defun %version (type major minor status level name)
  (ecase type
    (:number
     (apply #'+
       (* major 10000)
       (* minor 100)
       (when (eq status :patchlevel)
	 (list level))))
    (:short
     (format nil "~S.~S~
		 ~[~
		   a~*~S~;~
		   b~*~S~;~
		   rc~*~S~;~
		   ~:[.~S~;~*~]~
		 ~]"
       major
       minor
       (release-status-number status)
       (zerop level)
       level))
    (:long
     (format nil "~S.~S ~
		 ~[~
		   alpha ~*~S ~;~
		   beta ~*~S ~;~
		   release candidate ~*~S ~;~
		   ~:[patchlevel ~S ~;~*~]~
		 ~]~
		 ~S"
       major
       minor
       (release-status-number status)
       (zerop level)
       level
       name))))



;; Entry point

(defun version (&optional (type :number))
  "Return the current version of Declt.
TYPE can be one of :number, :short or :long.

A version number is computed as major*10000 + minor*100 + patchlevel, leaving
two digits for each level. Alpha, beta and rc status are ignored in version
numbers.

A short version is something like 1.3{a,b,rc}4, or 1.3.4 for patchlevel.
Alpha, beta or rc levels start at 1. Patchlevels start at 0 but are ignored
in the output, so that 1.3.0 appears as just 1.3.

A long version is something like
1.3 {alpha,beta,release candidate,patchlevel} 4 \"James T. Kirk\". As for
the short version, a patchlevel of 0 is ignored in the output."
  (%version type *release-major-level* *release-minor-level*
	    *release-status* *release-status-level*
	    *release-name*))

;;; version.lisp ends here
