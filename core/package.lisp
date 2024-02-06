;;; package.lisp --- Declt package definition

;; Copyright (C) 2010-2013, 2015, 2017, 2021, 2022 Didier Verna

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
  (:use :cl :net.didierverna.declt.setup :net.didierverna.declt.assess)
  (:shadow :nickname-package)
  (:export
    ;; From the :net.didierverna.declt.setup package:
    ;; - setup/src/version.lisp:
    :*copyright-years*
    :*release-major-level*
    :*release-minor-level*
    :*release-status*
    :*release-status-level*
    :*release-name*
    :version

    ;; From the :net.didierverna.declt.assess package:
    ;; - assess/src/definition.lisp:
    :definition :uid :source-file :foreignp
    :name :docstring :public-definitions :private-definitions
    ;; - assess/src/symbol.lisp:
    :symbol-definition :definition-symbol :home-package
    :symbol-definition-p :publicp
    :varoid-definition
    :variable-definition :constant-definition :special-definition
    :symbol-macro-definition
    :funcoid-definition :funcoid :setfp :lambda-list
    :setfable-funcoid-definition :expander-for :expanders-to
    :accessor-mixin :target-slot
    :macro-definition :macro
    :compiler-macro-definition :definition-compiler-macro
    :type-definition :expander
    :expander-definition :expander :standalone-reader
    :short-expander-definition :standalone-writer :short-expander-definition-p
    :long-expander-definition
    :function-definition :definition-function
    :ordinary-function-definition
    :ordinary-accessor-definition
    :ordinary-reader-definition :ordinary-writer-definition
    :generic-function-definition :generic :methods
    :combination :combination-options
    :generic-accessor-definition
    :generic-reader-definition :generic-writer-definition
    :combination-definition :combination :clients
    :short-combination-definition :standalone-combinator
    :identity-with-one-argument
    :long-combination-definition
    :method-definition :definition-method :owner :specializers
    :method-definition-p :qualifiers
    :accessor-method-definition
    :reader-method-definition :reader-method-definition-p
    :writer-method-definition :writer-method-definition-p
    :classoid-definition :classoid :direct-slots
    :clos-classoid-mixin :direct-superclassoids :direct-subclassoids
    :direct-methods :direct-default-initargs
    :condition-definition :definition-condition
    :direct-superconditions :direct-subconditions
    :class-definition :definition-class :direct-superclasses :direct-subclasses
    :structure-definition :definition-structure
    :clos-structure-definition :direct-superstructures :direct-substructures
    :typed-structure-definition :structure-type :element-type
    :slot-definition :slot :owner :readers :writers :value-type
    :clos-slot-definition :allocation :initform :initargs
    :typed-structure-slot-definition
    :alias-definition :setfp :referee
    :macro-alias-definition
    :compiler-macro-alias-definition
    :function-alias-definition
    ;; - assess/src/package.lisp:
    :package-definition :definition-package
    :use-list :used-by-list :definitions :nicknames
    :package-definition-p
    ;; - assess/src/asdf.lisp:
    :component-definition :component :location :parent :dependencies
    :component-definition-p
    :description :long-description :definition-version :if-feature
    :file-definition :file :file-definition-p :extension
    :source-file-definition
    :lisp-file-definition :definitions :lisp-file-definition-p
    :c-file-definition
    :java-file-definition
    :static-file-definition
    :doc-file-definition
    :html-file-definition
    :cl-source-file.asd
    :system-file-definition
    :module-definition :module :children :module-definition-p
    :system-definition :system :maintainers :authors :defsystem-dependencies
    :system-definition-p
    :long-name :mailto :homepage :source-control :bug-tracker :license-name
    ;; - assess/src/assess.lisp:
    :report :system-name :library-name :tagline :library-version :contacts
    :copyright-years :license
    :introduction :conclusion
    :definitions
    :assess

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
