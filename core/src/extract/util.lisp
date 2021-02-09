;;; util.lisp --- Utilities

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
;; ASDF
;; ==========================================================================

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

(defun reordered-dependency-def-system (reordered-dependency-def)
  "Extract the system name from REORDERED-DEPENDENCY-DEF.
See `reorder-dependency-def' for more information."
  (typecase reordered-dependency-def
    (list (reordered-dependency-def-system (car reordered-dependency-def)))
    (otherwise reordered-dependency-def)))

;;; util.lisp ends here
