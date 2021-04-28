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

(defun reveal
    ;; #### NOTE: make sure that we can have Unicode in the string.
    ;; Incidentally, this line also turns a NIL string into an empty one.
    (string &aux (string (coerce string '(simple-array character (*)))))
  "Return a copy of STRING with blanks revealed.
If STRING is empty or null, use the empty set symbol. Otherwise, each blank
character is replaced with a visible Unicode representation. See `*blanks*'
for more information."
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

(defgeneric category-name (definition)
  (:documentation "Return DEFINITION's category name."))

(defgeneric index-command-name (definition)
  (:documentation "Return DEFINITION's index command name."))

(defun long-title (definition)
  "Return a long title for DEFINITION.
It is of the form \"The <qualified safe name> <type name>\"."
  ;; #### WARNING: casing policy.
  (format nil "The ~(~A~) ~A"
    (safe-name definition t)
    (category-name definition)))

(defun anchor-name (definition)
  "Return DEFINITION's anchor name, that is, \"(<UID>)\"."
  (format nil "(~A)" (uid definition)))




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
    (format t " ~A" (category-name definition))))

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
    (item (title)
      (if (= length 1)
	(reference (first definitions) short)
	(itemize-list definitions :renderer renderer)))))




;; ==========================================================================
;; Documentation Engine
;; ==========================================================================

(defclass context ()
  ((hyperlinks :documentation
	       "Whether to create hyperlinks to files or directories.
Currently supported values are NIL (the default), and :file-system."
	       :initform nil :initarg :hyperlinks :accessor hyperlinks))
  (:documentation "The class of rendering contexts."))

(defun make-context (&rest keys &key hyperlinks)
  "Make a new rendering context.
The following keys are available.
- HYPERLINKS: whether to create hyperlinks to files or directories.
  Currently supported values are NIL (the default), and :file-system."
  (declare (ignore hyperlinks))
  (apply #'make-instance 'context keys))


(define-method-combination document ()
  ((around (:around))
   (open (:open))
   (body () :order :most-specific-last)
   (close (:close)))
  "The documentation protocol's method combination.
This method combination provides the following four method groups:
- around methods (optional, :around qualifier),
- opening methods (optional, :open qualifier),
- body methods (no qualifier),
- closing methods (optional, :close qualifier).

Around methods behave like those of the standard method combination. They can
be used to conditionalize the actual rendering of documentation, for example
in order to filter out definitions that are merged with others.

The main methods block behaves as follows.
- The most specific opening method, if any, is executed.
- All body methods (if any) are executed sequentially in most specific last
  order.
- Finally, the most specific closing method, if any, is executed.

No method group requires the existence of an applicable method, but for each
generic call, there must of course be at least one applicable method,
regardless of the group."
  (let ((form `(multiple-value-prog1
		   (progn ,(when open `(call-method ,(first open)))
			  (progn ,@(mapcar
				       (lambda (method) `(call-method ,method))
				     body)))
		 ,(when close `(call-method ,(first close))))))
    (if around
      `(call-method ,(first around) (,@(rest around) (make-method ,form)))
      form)))

(defgeneric document (definition context &key &allow-other-keys)
  (:documentation "Render DEFINITION's documentation in CONTEXT.")
  (:method-combination document))

;;; doc.lisp ends here
