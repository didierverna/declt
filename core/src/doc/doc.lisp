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


(defclass context ()
  ((hyperlinks :documentation
	       "Whether to create hyperlinks for ASDF components.
Currently supported values are NIL, and :file-system."
	       :initarg :hyperlinks :accessor hyperlinks))
  (:documentation "The class of rendering contexts."))

(defun make-context ()
  (make-instance 'context))



;; ==========================================================================
;; Utilities
;; ==========================================================================

(defparameter *blanks*
  '((#\        . #\⎵)  ;; U+23B5 (Bottom Square Bracket)
    (#\Newline . #\↵)  ;; U+21B5 (Downwards Arrow With Corner Leftwards)
    (#\Tab     . #\⇥)) ;; U+21E5 (Rightwards Arrow To Bar)
  "A list of blank characters and their associated revealed representation.
Each element in this list is of the form (#\BLANK . #\REPLACEMENT).")

(defun reveal
    ;; #### NOTE: make sure that we can have Unicode in the string.
    (string &aux (string (coerce string '(simple-array character (*)))))
  "Return a copy of STRING with blanks revealed.
Each blank character is replaced with a visible Unicode representation.
See `*blanks*' for more information."
  (if (zerop (length string))
    "∅"
    (loop :for i :from 0 :upto (1- (length string))
	  :for blank := (assoc (aref string i) *blanks*)
	  :when blank
	    :do (setf (aref string i) (cdr blank))
	  :finally (return string))))




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

(defgeneric index-command-name (definition)
  (:documentation "Return DEFINITION's index command name."))

(defun long-title (definition)
  "Return a long title for DEFINITION.
It is of the form \"The <qualified safe name> <type name>\"."
  ;; #### WARNING: casing policy.
  (format nil "The ~(~A~) ~A" (safe-name definition t) (type-name definition)))

(defun anchor-name (definition)
  "Return DEFINITION's anchor name.
It is of the form \"go to the <qualified safe name> <type name>\"."
  (format nil "go to the ~A ~A"
    (safe-name definition t) (type-name definition)))



;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun anchor (definition)
  "Render DEFINITION's anchoring command on *STANDARD-OUTPUT*."
  (@anchor (anchor-name definition)))

(defun index (definition)
  "Render DEFINITION's indexing command on *STANDARD-OUTPUT*."
  ;; #### WARNING: casing policy.
  (format t "@~A{~(~A~)}@c~%"
    (index-command-name definition)
    (escape (safe-name definition))))

(defun anchor-and-index (definition)
  "Render DEFINITION's anchoring and indexing commands on *STANDARD-OUTPUT*."
  (anchor definition)
  (index definition))

(defun reference (definition &optional short (punctuation #\.))
  "Render a possibly SHORT DEFINITION's reference on *STANDARD-OUTPUT*.
Unless SHORT, the DEFINITION type is advertised after the reference
itself. When SHORT, the reference is followed by a PUNCTUATION character (a
dot by default) or NIL."
  ;; #### WARNING: casing policy.
  (@ref (anchor-name definition) (string-downcase (safe-name definition)))
  (if short
    (when punctuation (write-char punctuation))
    ;; #### NOTE: Texinfo adds a comma automatically here.
    (format t " ~A" (type-name definition))))

(defgeneric document (definition context &key &allow-other-keys)
  (:documentation "Render DEFINITION's documentation in CONTEXT."))



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
#i(render-references 1)
(defun render-references (title definitions
			  &optional short
			  &aux (length (length definitions))
			       (renderer (if short
					   (lambda (definition)
					     (reference definition t))
					   #'reference)))
  "Render an enTITLEd list of possibly SHORT references to DEFINITIONS.
See `reference' for the meaning of SHORT. The list is rendered in an itemized
table item, unless there is only one definition in which case it appears
directly as the table item's contents.
Rendering is done on *standard-output*."
  (unless (zerop length)
    (@tableitem title
      (if (= length 1)
	(reference (first definitions) short)
	(@itemize-list definitions :renderer renderer)))))

;;; doc.lisp ends here
