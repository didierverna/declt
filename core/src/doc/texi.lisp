;;; texi.lisp --- Texinfo format utilities

;; Copyright (C) 2010-2013, 2015-2017, 2019 Didier Verna

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

;; #### NOTE: Texinfo has different contexts in which the set of characters to
;; escape varies. Since the escaping commands can be used (nearly?) anywhere,
;; even when it's not actually needed, it's simpler for us to use them all,
;; all the time. Also, in many cases, it's preferable to use the alphabetic
;; commands rather than shortcuts such as @@ etc. (see Section 12 of the
;; Texinfo manual). For simplicity, we use those systematically.

(defvar *special-characters*
  '((#\@ . "atchar")
    (#\{ . "lbracechar")
    (#\} . "rbracechar")
    (#\, . "comma")
    (#\\ . "backslashchar")
    (#\# . "hashchar")
    (#\& . "ampchar"))
  "An association list of Texinfo special characters.
Elements are the form (CHAR . COMMAND) where CHAR is the special character and
COMMAND is the name of the corresponding Texinfo alphabetic command.")

;; #### FIXME: I don't like much ESCAPE accepting NULL strings, but it saves
;; us some conditionals in format calls.
(defun escape (string)
  "When STRING, escape it for Texinfo."
  (when string
    (apply #'concatenate 'string
	   (loop :for char :across string
		 :for special := (assoc char *special-characters*)
		 :if special
		   :collect (concatenate 'string "@" (cdr special) "{}")
		 :else
		   :collect (string char)))))

;; #### NOTE: Parenthesis are less restricted than the other characters (see
;; Section 4.4 of the Texinfo manual), but for simplicity, we systematically
;; avoid them. The ampersand character is not advertised as problematic, but
;; in fact it is in PDF output (not in Info or HTML).
(defvar *fragile-characters*
  '((#\. . #\․) ;; U+2024 (One Dot Leader)
    (#\, . #\‚) ;; U+201A (Single Low-9 Quotation Mark)
    (#\: . #\∶) ;; U+2236 (Ratio)
    (#\( . #\❨) ;; U+2768 (Medium Left Parenthesis Ornament)
    (#\) . #\❩) ;; U+2769 (Medium Right Parenthesis Ornament)
    (#\& . #\⅋) ;; U+214B (Turned Ampersand)
    (#\\ . #\∖));; U+2216 (Set Minus)
  "An association list of Texinfo fragile (anchor) characters.
Elements are the form (CHAR . ALT) where CHAR is the fragile (anchor)
character and ALT is an alternative Unicode character.")

(defun escape-anchor (string)
  "Escape STRING for use as a Texinfo anchor name.
In addition to regular escaping, periods, commas, colons, and parenthesis are
replaced with alternative Unicode characters."
  (escape (apply #'concatenate 'string
		 (loop :for char :across string
		       :for fragile := (assoc char *fragile-characters*)
		       :if fragile
			 :collect (string (cdr fragile))
		       :else
			 :collect (string char)))))

;; #### NOTE: Anchor labels have less restrictions than anchor names, but this
;; is not well documented. Dots seem to be allowed which is a relief.
(defun escape-label (string)
  "Escape STRING for use as a Texinfo anchor label.
In addition to regular escaping, colons are replaced with alternative Unicode
characters."
  (escape
   (apply #'concatenate 'string
	  (loop :for char :across string
		:if (member char '(#\:))
		  :collect (string (cdr (assoc char *fragile-characters*)))
		:else
		  :collect (string char)))))

(defun first-word-length (string)
  "Return the length of the first word in STRING.
Initial whitespace characters are skipped."
  (or (position-if (lambda (char) (member char '(#\space #\tab #\newline)))
		   (string-trim '(#\space #\tab #\newline) string))
      (length string)))

(defun read-next-line (stream)
  "Read one line from STREAM.
Return a list of two values:
- the line itself, or STREAM,
- whether a newline character is missing at the end of the line."
  (multiple-value-list (read-line stream nil stream)))

(defun render-text (text)
  "Render TEXT for Texinfo.
Rendering is done on *standard-output*.
The rendering takes care of escaping the text for Texinfo, and attempts to
embellish the output by detecting potential paragraphs from standalone lines."
  (when text
    (destructuring-bind (lines width missing-newline-p)
	(with-input-from-string (stream text)
	  (loop :with width := 0
		:with missing-newline-p
		:for (line mnlp) := (read-next-line stream)
		:until (eq line stream)
		:collect line :into lines
		:do (setq missing-newline-p mnlp)
		:do (setq width (max width (length line)))
		:finally (return (list lines width missing-newline-p))))
      ;; Let's give ourselves some additional margin before deciding we should
      ;; go next line. This accounts for an additional space if the word
      ;; beginning next line were to be put at the end of the current one,
      ;; plus 3 more characters. This is because I noticed that some people
      ;; like to align their docstrings with the first line, which is usually
      ;; indented by 2 spaces, plus the opening ".
      (decf width 4)
      (loop :for remaining-lines :on lines
	    :for line := (car remaining-lines)
	    :for last-line-p := (null (cdr remaining-lines))
	    :if (zerop (length line))
	      :do (terpri)
	    :else
	      :if last-line-p
		:do (format t "~A~@[~%~]" (escape line) (not missing-newline-p))
	    :else
	      :if (> (- width (length line))
		     (first-word-length (cadr remaining-lines)))
		:do (format t "~A@*~%" (escape line))
	    :else
	      :do (format t "~A~%" (escape line))))))

(defun @anchor (anchor)
  "Render ANCHOR as an @anchor.
ANCHOR is escaped for Texinfo prior to rendering.
Rendering is done on *standard-output*."
  (format t "@anchor{~A}@c~%" (escape-anchor anchor)))

(defun @ref (anchor label)
  "Render ANCHOR as an @ref with online and printed LABEL.
Both ANCHOR and LABEL are escaped for Texinfo prior to rendering.
LABEL is rendered in teletype.
Rendering is done on *standard-output*."
  (format t "@ref{~A, , @t{~A}}"
    (escape-anchor anchor) (escape-label label)))

(defmacro @tableitem (title &body body)
  "Execute BODY within a table @item TITLE.
BODY should render on *standard-output*."
  `(progn
     (format t "~&@item ~A~%" ,title)
     ,@body))

(defmacro @table ((&optional (kind :@strong)) &body body)
  "Execute BODY within a @table KIND environment.
BODY should render on *standard-output*."
  `(progn
     (format t "~&@table ~(~A~)~%" ,kind)
     ,@body
     (format t "~&@end table~%")))

(defmacro @multitable ((&rest fractions) &body body)
  "Execute BODY within a @multitable environment.
FRACTIONS is the list of column fractions to use.
BODY should render on *standard-output*."
  `(progn
     (format t "~&@multitable @columnfractions~{ ~S~}~%" ',fractions)
     ,@body
     (format t "~&@end multitable~%")))

(defmacro @item (&body body)
  "Execute BODY within an itemize @item.
BODY should render on *standard-output*."
  `(progn
     (format t "~&@item~%")
     ,@body))

(defmacro @itemize ((&optional (kind :@bullet)) &body body)
  "Execute BODY within an @itemize KIND environment.
BODY should render on *standard-output*."
  `(progn
     (format t "~&@itemize ~(~A~)~%" ,kind)
     ,@body
     (format t "~&@end itemize~%")))

#i(itemize-list 1)
(defun @itemize-list
    (list &key renderer (kind :@bullet) (format "~A") (key #'identity))
  "Render a LIST of items within an @itemize KIND environment.
If RENDERER is non-nil, it must be a function of one argument (every LIST
element) that performs the rendering on *standard-output* directly. Otherwise,
the rendering is done by calling format, as explained below.

- FORMAT is the format string to use for every LIST element.
- KEY is a function of one argument (every LIST element) used to provide
  the necessary arguments to the FORMAT string. If multiple arguments are
  needed, they should be returned by KEY as multiple values."
  (@itemize (kind)
    (dolist (elt list)
      (@item
	(if renderer
	    (funcall renderer elt)
	  (apply #'format t format
		 (multiple-value-list (funcall key elt))))))))

(defmacro @defvr (category name &body body)
  "Execute BODY within a @defvr {CATEGORY} NAME environment.
CATEGORY and NAME are escaped for Texinfo prior to rendering.
BODY should render on *standard-output*."
  `(progn
     (format t "~&@defvr {~A} ~A~%" (escape ,category) (escape ,name))
     ,@body
     (format t "~&@end defvr~%")))


(defun escape-lambda-list (lambda-list)
  "Escape safe LAMBDA-LIST for Texinfo.
This function expects a value from `safe-lambda-list', or
`safe-specializers', which see. It returns a string properly escaped for
Texinfo, apart from &-constructs which retain their original form, and @ref's
which are already properly set."
  (if lambda-list
    (loop :for rest :on lambda-list
	  :for element := (car rest)
	  :if (listp element)
	    :collect (escape-lambda-list element) :into escaped-lambda-list
	  :else :if (or (member element '("&optional" "&rest" "&key"
					  "&allow-other-keys" "&aux"
					  "&environment" "&whole" "&body")
				:test #'string-equal) ;; case-insensitive test
			;; #### WARNING: I'll be damned in we ever fall on a
			;; specifier called @ref{... !
			(when-let (pos (search "@ref{" element))
			  (zerop pos)))
		  :collect element :into escaped-lambda-list
	  :else :collect (escape element) :into escaped-lambda-list
	  :finally (progn (when rest ;; dotted list
			    (setf (cdr (last escaped-lambda-list))
				  (escape rest))) ;; cannot be a &-construct
			  (return (princ-to-string escaped-lambda-list))))
    "()"))

(defmacro @deffn ((category name lambda-list &optional qualifiers) &body body)
  "Execute BODY under @deffn CATEGORY NAME [QUALIFIERS] LAMBDA-LIST.
CATEGORY, NAME, QUALIFIERS, and LAMBDA-LIST are escaped for Texinfo prior to
rendering. LAMBDA-LIST should be provided by `safe-lambda-list', which see.
BODY should render on *standard-output*."
  (let ((the-qualifiers (gensym "qualifiers")))
    `(let ((,the-qualifiers ,qualifiers))
       (format t "~&@deffn {~A} {~A}~@[ ~A~] ~A~%"
	 (escape ,category)
	 (escape ,name)
	 (when ,the-qualifiers (escape ,the-qualifiers))
	 (escape-lambda-list ,lambda-list))
       ,@body
       (format t "~&@end deffn~%"))))

(defun @deffnx (category name lambda-list &optional qualifiers)
  "Render @deffnx CATEGORY NAME [QUALIFIERS] LAMBDA-LIST on *standard-output*.
CATEGORY, NAME, QUALIFIERS, and LAMBDA-LIST are escaped for Texinfo prior to
rendering. LAMBDA-LIST should be provided by `safe-lambda-list', which see."
  (format t "~&@deffnx {~A} {~A}~@[ ~A~] ~A~%"
    (escape category)
    (escape name)
    (when qualifiers (escape qualifiers))
    (escape-lambda-list lambda-list)))

(defmacro @defmethod (category name qualifiers lambda-list &body body)
  "Execute BODY under @deffn CATEGORY NAME QUALIFIERS LAMBDA-LIST.
CATEGORY, NAME, and LAMBDA-LIST are escaped for Texinfo prior to rendering.
LAMBDA-LIST should be provided by `safe-lambda-list', which see.
BODY should render on *standard-output*."
  `(@deffn (,category ,name ,lambda-list ,qualifiers)
     ,@body))

(defun @defmethodx (category name qualifiers lambda-list)
  "Render @deffnx CATEGORY NAME QUALIFIERS LAMBDA-LIST on *standard-output*.
CATEGORY, NAME, QUALIFIERS, and LAMBDA-LIST are escaped for Texinfo prior to
rendering. LAMBDA-LIST should be provided by `safe-lambda-list', which see."
  (@deffnx category name lambda-list qualifiers))

(defmacro @defcombination (name lambda-list &body body)
  "Execute BODY within a @deftp {KIND Method Combination} NAME environment.
NAME is escaped for Texinfo prior to rendering.
BODY should render on *standard-output*."
  `(@deffn ("Method Combination" ,name ,lambda-list) ,@body))

(defmacro @deftp
    ((category name
      &optional (lambda-list nil lambda-list-p))
     &body body)
  "Execute BODY within a @deftp CATEGORY NAME [LAMBDA-LIST] environment.
CATEGORY, NAME, and LAMBDA-LIST are escaped for Texinfo prior to rendering.
LAMBDA-LIST should be provided by `safe-lambda-list', which see.
BODY should render on *standard-output*."
  `(progn
     (format t "~&@deftp {~A} {~A}~@[ ~A~]~%"
       (escape ,category)
       (escape ,name)
       (when ,lambda-list-p (escape-lambda-list ,lambda-list)))
     ,@body
     (format t "~&@end deftp~%")))

(defmacro @deftype (name lambda-list &body body)
  "Execute BODY within a @deftp Type NAME LAMBDA-LIST environment.
NAME and LAMBDA-LIST are escaped for Texinfo prior to rendering.
BODY should render on *standard-output*."
  `(@deftp ("Type" ,name ,lambda-list) ,@body))

(defmacro render-to-string (&body body)
  "Execute BODY with *standard-output* redirected to a string.
Return that string."
  `(with-output-to-string (*standard-output*)
     ,@body))



;; ==========================================================================
;; Node Implementation
;; ==========================================================================

(defparameter *section-names*
  '((:numbered   nil
     "chapter"    "section"       "subsection"       "subsubsection")
    (:unnumbered "top"
     "unnumbered" "unnumberedsec" "unnumberedsubsec" "unnumberedsubsubsec")
    (:appendix   nil
     "appendix"   "appendixsec"   "appendixsubsec"   "appendixsubsubsec" ))
  "The numbered, unumbered and appendix section names sorted by level.")

;; #### NOTE: all contents in the NODE structure must be already escaped for
;; Texinfo.  #### WARNING: not any more. The node names are not escaped any
;; more. RENDER-NODE does it now.
(defstruct node
  "The NODE structure.
This structure holds Texinfo nodes."
  name                      ;; node name
  synopsis                  ;; node menu description
  (section-type :numbered)  ;; see *section-names*
  section-name              ;; alternate name for section
  next                      ;; next node
  previous                  ;; previous node (or up)
  up                        ;; parent node
  children                  ;; list of sub-nodes
  before-menu-contents      ;; contents before the menu
  after-menu-contents)      ;; contents after the menu

#i(add-child 1)
(defun add-child (parent child
		  &aux (previous (car (last (node-children parent)))))
  "Add CHILD node to PARENT node and return CHILD."
  (cond (previous
	 (setf (node-next previous) child)
	 (endpush child (node-children parent)))
	(t
	 (setf (node-children parent) (list child))))
  (setf (node-previous child) previous)
  (setf (node-up child) parent)
  child)

(defun render-node (node level
		    &aux (node-name (node-name node))
			 (safe-node-name (escape-anchor node-name)))
  "Render NODE at LEVEL and all its children at LEVEL+1."
  (cond ((<= level 1)
	 (format t "


@c ====================================================================
@c ~A
@c ====================================================================~%"
	   node-name))
	((= level 2)
	 (let ((separator (make-string (length node-name)
			    :initial-element #\-)))
	   (format t
		   "

@c ~A
@c ~A
@c ~A~%"
	     separator node-name separator)))
	(t (terpri)))
  (when (= level 0)
    (format t "@ifnottex~%"))
  (format t "@node ~A, ~@[~A~], ~@[~A~], ~A~%"
    safe-node-name
    (or (when (= level 0)
	  (escape-anchor (node-name (first (node-children node)))))
	(when (node-next node)
	  (escape-anchor (node-name (node-next node)))))
    (or (when (= level 0)
	  "(dir)")
	(when (node-previous node)
	  (escape-anchor (node-name (node-previous node))))
	(escape-anchor (node-name (node-up node))))
    (if (= level 0)
	"(dir)"
      (escape-anchor (node-name (node-up node)))))
  (format t "@~A ~A~%"
    (nth level (cdr (assoc (node-section-type node) *section-names*)))
    (or (node-section-name node) (node-name node)))
  (when (node-before-menu-contents node)
    (write-string (node-before-menu-contents node))
    (fresh-line))
  (when (node-children node)
    (when (node-before-menu-contents node)
      (terpri))
    (format t "@menu~%")
    (dolist (child (node-children node))
      (let* ((node-name (node-name child))
	     (safe-node-name (escape-anchor node-name)))
      ;; #### FIXME: this could be improved with proper alignment of synopsis.
	(format t "* ~:[~A: ~A.~;~A~*::~]~@[ ~A~]~%"
	  (string= node-name safe-node-name)
	  node-name
	  safe-node-name
	  (node-synopsis child))))
    (format t "@end menu~%"))
  (when (node-after-menu-contents node)
    (when (or (node-children node) (node-before-menu-contents node))
      (terpri))
    (write-string (node-after-menu-contents node))
    (fresh-line))
  (when (= level 0)
    (format t "@end ifnottex~%"))
  (dolist (child (node-children node))
    (render-node child (1+ level))))

(defun render-top-node (node)
  "Render the whole nodes hierarchy starting at toplevel NODE."
  (render-node node 0)
  (values))

;;; texi.lisp ends here
