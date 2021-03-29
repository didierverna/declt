;;; package.lisp --- Declt package definition

;; Copyright (C) 2010-2013, 2015, 2017, 2021 Didier Verna

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

(in-package :cl-user)


(defpackage :net.didierverna.declt
  (:documentation "The Declt library's package.")
  (:use :cl :net.didierverna.declt.setup)
  ;; #### PORTME.
  (:import-from #+sbcl :sb-mop
		:generic-function-name
		:generic-function-methods
		:generic-function-method-combination
		:eql-specializer
		:specializer-direct-methods
		:method-generic-function
		:method-lambda-list
		:method-specializers
		:class-direct-superclasses
		:class-direct-subclasses
		:class-direct-slots
		:class-direct-default-initargs
		:slot-definition-name
		:slot-definition-type
		:slot-definition-allocation
		:slot-definition-initform
		:slot-definition-initargs
		:slot-definition-readers
		:slot-definition-writers
		:find-method-combination
		:validate-superclass)
  (:import-from :asdf
    :component-name
    :component-version
    :component-description
    :component-long-description
    :component-parent
    :component-children
    :component-pathname
    :component-relative-pathname
    :component-if-feature
    :component-sideway-dependencies
    :component-find-path
    :find-system
    :resolve-dependency-name
    :system-defsystem-depends-on
    :system-long-name
    :system-author
    :system-maintainer
    :system-mailto
    :system-homepage
    :system-source-control
    :system-bug-tracker
    :system-license
    :system-source-directory
    :system-source-file)
  (:export
    ;; From the :net.didierverna.declt.setup package:
    :*copyright-years*
    :*release-major-level*
    :*release-minor-level*
    :*release-status*
    :*release-status-level*
    :*release-name*
    :version
   ;; From package.lisp (this file):
   :nickname-package
   ;; From src/declt.lisp:
   :declt))


(in-package :net.didierverna.declt)

(defun nickname-package (&optional (nickname :declt))
  "Add NICKNAME (:DECLT by default) to the :NET.DIDIERVERNA.DECLT package."
  (rename-package :net.didierverna.declt
		  (package-name :net.didierverna.declt)
		  (adjoin nickname (package-nicknames :net.didierverna.declt)
			  :test #'string-equal)))

;;; package.lisp ends here
