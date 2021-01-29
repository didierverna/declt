;;; doc.lisp --- Items documentation

;; Copyright (C) 2010-2013, 2015-2017, 2019-2021 Didier Verna

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
;; Utilities
;; ==========================================================================

(defparameter *blanks*
  '((#\        . #\⎵)  ;; U+23B5 (Bottom Square Bracket)
    (#\Newline . #\↵)  ;; U+21B5 (Downwards Arrow With Corner Leftwards)
    (#\Tab     . #\⇥)) ;; U+21E5 (Rightwards Arrow To Bar)
  "A list of blank characters and their associated revealed representation.
Each element in this list is of the form (#\BLANK . #\REPLACEMENT).")

;; #### FIXME: there's got to be some portability pitfalls here, working with
;; Unicode strings like that without any precaution.
(defun reveal (string)
  "Return a copy of STRING with blanks revealed.
Each blank character is replaced with a visible Unicode representation.
See `*blanks*' for more information."
  (if (zerop (length string))
    "∅"
    (loop :with revealed := (copy-seq string)
	  :for i :from 0 :upto (1- (length revealed))
	  :for blank := (assoc (aref revealed i) *blanks*)
	  :when blank
	    :do (setf (aref revealed i) (cdr blank))
	  :finally (return revealed))))




;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

(defgeneric safe-name (definition &optional qualified)
  (:documentation "Return DEFINITION's safe name, possibly QUALIFIED.
Safe names have blank characters replaced with visible Unicode symbols.
See `reveal' for more information.")
  ;; #### WARNING: not an around method, because we don't want to reveal
  ;; spaces in a (setf name) form.
  (:method ((definition definition) &optional qualified)
    "Reveal unqualifiable DEFINITION's name. This is the default method."
    (declare (ignore qualified))
    (reveal (name definition))))

(defgeneric type-name (definition)
  (:documentation "Return DEFINITION's type name."))

(defun long-title (definition)
  "Return a long title for DEFINITION.
It is of the form \"The <full safe name> <type name>\"."
  (format nil "The ~A ~A" (safe-name definition t) (type-name definition)))

(defun anchor-name (definition)
  "Return an anchor name for DEFINITION.
It is of the form \"go to the <full safe name> <type name>\"."
  (format nil "go to the ~A ~A"
    (safe-name definition t) (type-name definition)))

(defgeneric index (item) (:documentation "Render ITEM's indexing command."))

(defun reference (definition)
  "Render DEFINITION's reference."
  (@ref (anchor-name definition) (safe-name definition))
  (format t " (~A)" (type-name definition)))

(defgeneric document (item extract &key &allow-other-keys)
  (:documentation "Render ITEM's documentation in EXTRACT."))



;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun anchor (definition)
  "Render DEFINITION's anchor."
  (@anchor (anchor-name definition)))

(defun anchor-and-index (definition)
  "Anchor and index DEFINITION."
  (anchor definition)
  (index definition))


;; #### NOTE: the use of PROBE-FILE below has two purposes:
;; 1/ making sure that the file does exist, so that it can actually be linked
;;    properly,
;; 2/ dereferencing an (ASDF 1) installed system file symlink (what we get) in
;;    order to link the actual file (what we want).
;; #### FIXME: not a Declt bug, but currently, SBCL sets the source file of
;; COPY-<struct> functions to its own target-defstruct.lisp file.
(defun render-location
    (pathname extract
     &optional (title "Location")
     &aux (probed-pathname (probe-file pathname))
	  (relative-to (location extract))
	  (hyperlinkp (and (hyperlinksp extract) probed-pathname)))
  "Render an itemized location line for PATHNAME in EXTRACT.
Rendering is done on *standard-output*."
  (@tableitem title
    (format t "~@[@url{file://~A, ignore, ~]@t{~A}~:[~;}~]~%"
      (when hyperlinkp
	(escape probed-pathname))
      (escape (if probed-pathname
		  (enough-namestring probed-pathname relative-to)
		(concatenate 'string (namestring pathname)
			     " (not found)")))
      hyperlinkp)))

(defun render-source (item extract)
  "Render an itemized source line for ITEM in EXTRACT.
Rendering is done on *standard-output*."
  (when-let (source-pathname (source item))
    (let* ((systems (mapcar #'system (system-definitions extract)))
	   ;; Remember that a source can be a system, although systems are not
	   ;; actual cl-source-file's.
	   (source-component
	     (loop :for component
		     :in (append systems (mapcan #'lisp-components systems))
		   :when (equal source-pathname
				(component-pathname component))
		     :return component)))
      (if source-component
	  (@tableitem "Source" (reference source-component))
	  ;; Otherwise, the source does not belong to the system. This may
	  ;; happen for automatically generated sources (sb-grovel does this
	  ;; for instance). So let's just reference the file itself.
	  (render-location source-pathname extract "Source")))))

(defun render-docstring (item)
  "Render ITEM's documentation string.
Rendering is done on *standard-output*."
  (when-let (docstring (docstring item))
    (render-text docstring)))

;; #### FIXME: concatenate TITLE with ~p and format it to handle length = 1.
(defun render-references (list title &aux (length (length list)))
  "Render references to a LIST of items.
References are rendered in a table item named TITLE as a list, unless there is
only one item in LIST.

Rendering is done on *standard-output*."
  (unless (zerop length)
    (@tableitem title
      (if (= length 1)
	  (reference (first list))
	(@itemize-list list :renderer #'reference)))))


;;; doc.lisp ends here
