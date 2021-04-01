;;; package.lisp --- Declt setup package definition

;; Copyright (C) 2015, 2017, 2019, 2021 Didier Verna

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

(defpackage :net.didierverna.declt.setup
  (:documentation "The Declt setup library's package.")
  (:use :cl)
  ;; #### PORTME.
  (:import-from #+sbcl :sb-mop
    :validate-superclass)
  (:import-from :named-readtables
    :defreadtable :in-readtable)
  (:export
    :in-readtable
    ;; From src/configuration.lisp:
    :configuration :configure
    ;; From src/version.lisp:
    :*copyright-years*
    :*release-major-level* :*release-minor-level* :*release-status*
    :*release-status-level* :*release-name*
    :version
    ;; From src/util.lisp:
    :while :endpush :retain :find* :when-let :when-let* :mapcat
    :declare-valid-superclass :abstract-class :defabstract
    :non-empty-string-p :non-empty-string))

;;; package.lisp ends here
