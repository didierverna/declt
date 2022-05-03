;;; util.lisp --- Utilities

;; Copyright (C) 2010, 2011, 2013, 2016-2017, 2020-2022 Didier Verna

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

(in-package :net.didierverna.declt.assess)
(in-readtable :net.didierverna.declt)


;; ==========================================================================
;; Introspection
;; ==========================================================================

;; #### NOTE: SB-INTROSPECT:FIND-DEFINITION-SOURCES-BY-NAME may return
;; multiple sources (e.g. if we were to ask it for methods) so we take the
;; first one. This is okay because we actually use it only when there can be
;; only one definition source.

;; #### PORTME.
(defun source-by-name (name type)
  "Return NAME's source for TYPE."
  (when-let (sources (sb-introspect:find-definition-sources-by-name name type))
    (sb-introspect:definition-source-pathname (first sources))))

;; #### PORTME.
(defun object-source-pathname (object)
  "Return OBJECT's source pathname."
  (when-let (source (sb-introspect:find-definition-source object))
    (sb-introspect:definition-source-pathname source)))




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




;; ==========================================================================
;; Miscellaneous
;; ==========================================================================

(defun one-liner-p (string)
  "Return T if STRING is non empty and does not span multiple lines."
  (and string (not (or (zerop (length string)) (find #\Newline string)))))


;; #### NOTE: the functions below are relatively robust, without using the
;; whole regexp artillery.
(defun validate-email
    (string
     &aux (string (string-trim '(#\Space #\Tab) string))
	  (@-position (position #\@ string)))
  "Check that STRING is of the form nonblank@nonblank, after trimming.
Return that string, or issue a warning and return NIL."
  (if (and @-position (> @-position 0) (< @-position (1- (length string))))
    string
    (warn "Invalid email address: ~A." string)))

(defun parse-contact-string
    (string &aux (pos-< (position #\< string))
		 (pos-> (position #\> string)))
  "Parse STRING as \"My Name <my@address>\".
Both name and address are optional. If only an address is provided, the angle
brackets may be omitted. Return name and address, as two potentially NIL
values."
  (when (one-liner-p string)
    (cond ((and pos-< pos-> (< pos-< pos->))
	   (let ((name (if (zerop pos-<)
			 ""
			 (string-trim '(#\Space #\Tab)
				      (subseq string 0 (1- pos-<))))))
	     (when (zerop (length name))
	       (setq name nil))
	     (values name (validate-email (subseq string (1+ pos-<) pos->)))))
	  ((position #\@ string)
	   (values nil (validate-email string)))
	  (t
	   (setq string (string-trim '(#\Space #\Tab) string))
	   (values (unless (zerop (length string)) string) nil)))))

(defun |parse-contact(s)| (|contact(s)|)
  "Parse CONTACT(S) as either a contact string, or a list of such.
A contact string is of the form \"My Name <my@address>\", both name and
address being optional.

Return two values: a list of name(s) and a list of address(es). The two lists
maintain correspondence between names and addresses: they are of the same
length and may contain null elements, for contact strings lacking either
one."
  (if (stringp |contact(s)|)
    (multiple-value-bind (name email) (parse-contact-string |contact(s)|)
      (when (or name email)
	(values (list name) (list email))))
    (loop :for contact-string
	    :in (remove-duplicates |contact(s)| :from-end t :test #'string=)
	  :for (name email)
	    := (multiple-value-list (parse-contact-string contact-string))
	  :when (or name email)
	    :collect name :into names :and :collect email :into emails
	  :finally (return (values names emails)))))

;;; util.lisp ends here
