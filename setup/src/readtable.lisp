;;; readtable.lisp --- Declt readtable management

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

;; Contents management by FCM version 0.1.


;;; Code:

(in-package :net.didierverna.declt.setup)


(defun clindent (symbol indent)
  "Send SYMBOL's INDENTation information to Emacs.
Emacs will set the 'common-lisp-indent-function property.
If INDENT is a symbol, use its indentation definition. Otherwise, INDENT is
considered as an indentation definition."
  (when (and (member :swank *features*) (configuration :swank-eval-in-emacs))
    ;; #### NOTE: case portability
    (funcall (intern (string :eval-in-emacs) :swank)
      `(put ',symbol 'common-lisp-indent-function
	    ,(if (symbolp indent)
	       `(get ',indent 'common-lisp-indent-function)
	       `',indent))
      t)))

(defmacro defindent (symbol indent)
  "Wrapper around `clindent' to avoid quoting SYMBOL and INDENT."
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (clindent ',symbol ',indent)))

(defun i-reader (stream subchar arg)
  "Construct a call to `defindent' by reading an argument list from STREAM.
This dispatch macro character function is installed on #i in the
NET.DIDIERVERNA.DECLT named readtable."
  (declare (ignore subchar arg))
  (cons 'defindent (read stream)))

(defreadtable :net.didierverna.declt
  (:merge :standard)
  (:dispatch-macro-char #\# #\i #'i-reader))

;;; readtable.lisp ends here
