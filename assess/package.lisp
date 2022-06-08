;;; package.lisp --- Declt assessment package definition

;; Copyright (C) 2021, 2022 Didier Verna

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

(defpackage :net.didierverna.declt.assess
  (:documentation "The Declt assessment library's package.")
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
    :find-method-combination)
  (:import-from :asdf
    :component-name
    :component-version
    :component-description
    :component-long-description
    :component-parent
    :component-children
    :component-pathname
    :component-if-feature
    :component-sideway-dependencies
    :file-type
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
    ;; From src/definition.lisp:
    :definition :object :uid :source-file :foreignp
    :name :docstring :public-definitions :private-definitions
    ;; From src/symbol.lisp:
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
    ;; From src/package.lisp:
    :package-definition :definition-package
    :use-list :used-by-list :definitions :nicknames
    :package-definition-p
    ;; From src/asdf.lisp:
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
    ;; From src/assess.lisp:
    :report :system-name :library-name :tagline :library-version :contacts
    :copyright-years :license
    :introduction :conclusion
    :definitions
    :assess))

;;; package.lisp ends here
