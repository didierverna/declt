;;; package.lisp --- Declt extract package definition

;; Copyright (C) 2021 Didier Verna

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

(defpackage :net.didierverna.declt.extract
  (:documentation "The Declt extract library's package.")
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
    ;; From ASDF:
    :component-pathname ;; #### FIXME: see render-pathname.
    ;; From src/definition.lisp:
    :definition :source-file :foreignp
    :name :docstring :public-definitions :private-definitions
    ;; From src/symbol.lisp:
    :symbol-definition :definition-symbol :home-package :publicp
    :varoid-definition
    :variable-definition :constant-definition :special-definition
    :symbol-macro-definition
    :funcoid-definition :lambda-list
    :setf-mixin
    :expander-mixin :expander-for :expanders-to
    :accessor-mixin :target-slot
    :macro-definition
    :compiler-macro-definition :setf-compiler-macro-definition
    :type-definition
    :expander-definition :standalone-reader
    :short-expander-definition :standalone-writer
    :long-expander-definition
    :function-definition
    :ordinary-function-definition
    :simple-function-definition :setf-function-definition
    :reader-definition :writer-definition
    :generic-function-definition :methods :combination :combination-options
    :simple-generic-definition :generic-setf-definition
    :generic-reader-definition
    :generic-writer-mixin
    :simple-generic-writer-definition :generic-setf-writer-definition
    :combination-definition :clients
    :short-combination-definition :standalone-combinator
    :identity-with-one-argument
    :long-combination-definition
    :method-definition :owner :specializers :qualifiers
    :simple-method-definition :setf-method-definition
    :reader-method-definition
    :writer-method-definition
    :simple-writer-method-definition :setf-writer-method-definition
    :classoid-definition :direct-slots
    :clos-classoid-mixin :direct-default-initargs
    :direct-superclassoids :direct-subclassoids :direct-methods
    :condition-definition :direct-superconditions :direct-subconditions
    :class-definition :direct-superclasses :direct-subclasses
    :structure-definition
    :clos-structure-definition :direct-superstructures :direct-substructures
    :typed-structure-definition :structure-type :element-type
    :slot-definition :owner :readers :writers :value-type
    :clos-slot-definition :allocation :initform :initargs
    :typed-structure-slot-definition
    :alias-definition :referee
    :macro-alias-definition
    :compiler-macro-alias-definition :setf-compiler-macro-alias-definition
    :function-alias-definition :setf-function-alias-definition
    ;; From src/package.lisp:
    :package-definition :use-list :used-by-list :definitions :nicknames
    :package-definition-p
    ;; From src/asdf.lisp:
    :component-definition :parent :dependencies
    :component ;; #### FIXME: see render-pathname.
    :description :long-description :definition-version :if-feature
    :file-definition :file-definition-p :extension
    :source-file-definition
    :lisp-file-definition :definitions
    :c-file-definition
    :java-file-definition
    :static-file-definition
    :doc-file-definition
    :html-file-definition
    :cl-source-file.asd
    :system-file-definition
    :module-definition :children :module-definition-p
    :system-definition :system-definition-p
    :maintainer-names :maintainer-emails :author-names :author-emails
    :defsystem-dependencies
    :long-name :mailto :homepage :source-control :bug-tracker :license-name
    ;; From src/extract.lisp:
    :extract :library-name :tagline :library-version
    :contact-names :contact-emails :copyright-years :license
    :introduction :conclusion
    :definitions))

;;; package.lisp ends here
