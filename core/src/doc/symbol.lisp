;;; symbol.lisp --- Symbol based documentation

;; Copyright (C) 2010-2013, 2015-2017, 2020 Didier Verna

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
;; Rendering protocols
;; ==========================================================================

(defmethod safe-name
    ((definition symbol-definition)
     &optional qualified
     &aux (name (reveal (princ-to-string (name definition)))))
  "Reveal symbol DEFINITION's name, possibly QUALIFIED.
A QUALIFIED name is of the form \"package:[:]symbol\"."
  (when qualified
    (setq name (concatenate 'string
		 (reveal (name (package-definition definition)))
		 (if (publicp definition) ":" "::")
		 name)))
  name)

;; #### NOTE: spaces in symbol names are revealed (see above), but not the
;; ones between SETF and the symbol in a setf name, because that would look
;; rather weird in the output. Consequently, Declt must expect to get names
;; with unescaped spaces. @DEFFN, @DEFFNX, AND @DEFTP take care of protecting
;; their NAME argument with braces because of that.
(defmethod safe-name
    ((definition setf-mixin)
     &optional qualified
     &aux (name (reveal (princ-to-string (second (name definition))))))
  "Reveal setf DEFINITION's name, possibly QUALIFIED.
A QUALIFIED name is of the form \"(setf package:[:]symbol)\"."
  (when qualified
    (setq name (concatenate 'string
		 (reveal (name (package-definition definition)))
		 (if (publicp definition) ":" "::")
		 name)))
  ;; Hack for future case-preserving implementation.
  (format nil "(~A ~A)" 'setf name))



;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun render-definition-core (definition context)
  "Render DEFINITION's documentation core in CONTEXT.
The documentation core includes all common definition attributes:
  - package,
  - source location.

Each element is rendered as a table item."
  (@tableitem "Package"
    (reference (package-definition definition)))
  (when-let (source (source-file definition))
    (@tableitem "Source" (reference source))))

(defgeneric headline-function (definition)
  (:documentation "Return a suitable headline function for DEFINITION.")
  (:method ((function ordinary-function-definition))
    "Return #'@DEFUNX."
    #'@defunx)
  (:method ((generic generic-function-definition))
    "Return #'@DEFGENERICX."
    #'@defgenericx)
  (:method ((expander expander-definition))
    "Return #'@DEFSETFX."
    #'@defsetfx))

(defun render-headline (definition)
  "Render a headline for DEFINITION. Also anchor and index it."
  (funcall (headline-function definition)
	   (string-downcase (safe-name definition)) (lambda-list definition))
  (anchor-and-index definition))




;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

;; #### NOTE: all the indexing methods below perform sub-indexing only,
;; because the main index entries are created automatically in Texinfo by the
;; @defXXX routines.

;; -------
;; Varoids
;; -------

(defmacro render-varoid (definition context
			 &body body
			 &aux (the-definition (gensym "definition"))
			      (the-context (gensym "context")))
  "Execute BODY within a varoid DEFINITION documentation in CONTEXT.
BODY is executed within a @table environement."
  `(let ((,the-definition ,definition)
	 (,the-context ,context))
     (@defvr (type-name ,the-definition)
	 ;; #### WARNING: casing policy.
	 (string-downcase (safe-name ,the-definition))
       (anchor-and-index ,the-definition)
       (render-docstring ,the-definition)
       (@table () ,@body))))

(defmethod document ((definition varoid-definition) context &key)
  "Render varoid DEFINITION's documentation in CONTEXT.
This is the default method used for simple varoids,
providing only basic information."
  (render-varoid definition context
    (render-definition-core definition context)))



;; Constants
(defmethod type-name ((definition constant-definition))
  "Return \"Constant\"."
  "Constant")

(defmethod index-command-name ((definition constant-definition))
  "Return \"constantsubindex\"."
  "constantsubindex")



;; Special variables
(defmethod type-name ((definition special-definition))
  "Return \"Special Variable\"."
  "Special Variable")

(defmethod index-command-name ((definition special-definition))
  "Return \"specialsubindex\"."
  "specialsubindex")



;; Symbol macros
(defmethod type-name ((definition symbol-macro-definition))
  "Return \"Symbol Macro\"."
  "Symbol Macro")

(defmethod index-command-name ((definition symbol-macro-definition))
  "Return \"symbolmacrosubindex\"."
  "symbolmacrosubindex")


;; Slots
(defmethod safe-name
    ((definition slot-definition)
     &optional qualified
     &aux (safe-name (call-next-method)))
  "When QUALIFIED, prepend slot DEFINITION's classoid safe name."
  (if qualified
    (concatenate 'string
      (safe-name (classoid-definition definition) t)
      "->"
      safe-name)
    safe-name))

(defmethod type-name ((definition slot-definition))
  "Return \"Slot\"."
  "Slot")

(defmethod index-command-name ((definition slot-definition))
  "Return \"slotsubindex\"."
  "slotsubindex")

;; #### PORTME.
(defun slot-property (slot property)
  "Return SLOT's PROPERTY value."
  (funcall (intern (concatenate 'string "SLOT-DEFINITION-"
				(symbol-name property))
		   :sb-mop)
    slot))

;; #### FIXME: not rendering standard / default values should be a context
;; choice.
#i(render-slot-property 2)
(defun render-slot-property
    (slot property
     &key (renderer (lambda (value)
		      (format t "@t{~A}~%"
			;; #### WARNING: casing policy.
			(escape (format nil "~(~S~)" value)))))
     &aux (value (slot-property slot property)))
  "Render SLOT's PROPERTY value as a table item on *standard-output*."
  (when (and value
	     (not (and (eq property :type) (eq value t)))
	     (not (and (eq property :allocation) (eq value :instance))))
    (@tableitem (format nil "~@(~A~)" property)
      (funcall renderer value))))

(defmethod document
    ((definition slot-definition) context &key &aux (slot (slot definition)))
  "Render slot DEFINITION's documentation in CONTEXT.
- The source file is not documented at all, since it is lexically the same as
  that of the parent classoid.
- The package is not documented, unless it differs from that of the parent
  classoid."
  (render-varoid definition context
    (unless (eq (package-definition definition)
		(package-definition (classoid-definition definition)))
      (@tableitem "Package" (reference (package-definition definition))))
    (@table ()
      (render-slot-property slot :type)
      (render-slot-property slot :allocation)
      (render-slot-property slot :initform)
      (render-slot-property slot :initargs
	;; #### FIXME: format mess. There's gotta be a better way.
	:renderer (lambda (value)
		    (let ((values (mapcar (lambda (val)
					    ;; #### WARNING: casing policy.
					    (escape (format nil "~(~S~)" val)))
				    value)))
		      (format t "@t{~A}~{, @t{~A}~}"
			(first values)
			(rest values)))))
      (render-references (reader-definitions definition) "Readers")
      (render-references (writer-definitions definition) "Writers"))))



;; --------
;; Funcoids
;; --------

;; #### TODO: there's the question of offering the option to qualify symbols.
;; #### FIXME: specializers code not updated yet.
;; Based on Edi Weitz's write-lambda-list* from documentation-template.
(defun safe-lambda-list (lambda-list &optional specializers)
  "Return a safe string for LAMBDA-LIST, possibly with SPECIALIZERS.
Safe lambda list tokens have blank characters replaced with visible Unicode
symbols. See `reveal' for more information."
  ;; #### NOTE: we cannot use DOLIST here because some lambda lists may be
  ;; improper (e.g. in the case of macros).
  (with-output-to-string (s)
    (write-char #\( s)
    (do ((firstp t nil)
	 (part (car lambda-list))
	 (next (cdr lambda-list))
	 after-required-args-p
	 stop)
	(stop)
      (when (and (consp part) after-required-args-p) (setq part (first part)))
      (unless firstp (write-char #\Space s))
      (cond ((listp part)
	     (write-char #\( s)
	     (when (consp part) (write-string (safe-lambda-list part) s))
	     (write-char #\) s))
	    ((member part '(&optional &rest &key &allow-other-keys
			    &aux &environment &whole &body))
	     (setq after-required-args-p t)
	     ;; #### NOTE: PART is not escaped below, which is fine because
	     ;; Texinfo recognizes &stuff (#### FIXME: does it really
	     ;; recognize all &stuff, or just Emacs Lisp ones?) and processes
	     ;; them in a special way as part of definition commands. This
	     ;; should be exactly what we want.
	     (write-string (string part) s))
	  (t
	   ;; #### WARNING: we don't ask to qualify the specializers here
	   ;; because that would completely clutter the display. There are
	   ;; some cases however (like MCClim) which have specializers on the
	   ;; same symbol but from different packages (e.g. defclass). These
	   ;; won't show in the output unfortunately.
	   (let ((specializer nil #+() (pop specializers)))
	     (if (and specializer (not (eq specializer (find-class t))))
	       (write-string (format nil "(~A @t{~A})"
			       (reveal (string part))
			       (safe-name specializer))
			     s)
	       (write-string (reveal (string part)) s)))))
      (cond ((not next)
	     (setq stop t))
	    ((consp next)
	     (setq part (car next)
		   next (cdr next)))
	    (t
	     (write-char #\  s)
	     (write-char #\. s)
	     (setq  part next
		    next nil))))
    (write-char #\) s)))

(defmacro render-funcoid
    (|definition(s)| context
     &body body
     &aux (the-definition (gensym "definition"))
	  (the-context (gensym "context")))
  "Render funcoid DEFINITION(S) documentation in CONTEXT."
  `(let ((,the-definition ,(if (consp |definition(s)|)
			     (car |definition(s)|)
			     |definition(s)|))
	 (,the-context ,context))
     (@deffn ((type-name ,the-definition)
	      ;; #### WARNING: casing policy.
	      (string-downcase (safe-name ,the-definition))
	      (safe-lambda-list (lambda-list ,the-definition)))
	 (anchor-and-index ,the-definition)
       ,@(mapcar #'render-headline
	   (when (consp |definition(s)|) (cdr |definition(s)|)))
       (render-docstring ,the-definition)
       (@table ()
	 (render-definition-core ,the-definition ,the-context)
	 ,@body))))

(defmethod document ((definition funcoid-definition) context &key)
  "Render funcoid DEFINITION's documentation in CONTEXT.
This is the default method used for simple funcoids,
providing only basic information."
  (render-funcoid definition context))



;; Macros
(defmethod type-name ((definition macro-definition))
  "Return \"Macro\"."
  "Macro")

(defmethod index-command-name ((definition macro-definition))
  "Return \"macrosubindex\"."
  "macrosubindex")

;; #### FIXME: rethink the possibilities of merging with the expander-for.
(defmethod document ((definition macro-definition) context &key)
  "Render macro DEFINITION's documentation in CONTEXT."
  (render-funcoid definition context
    (when-let (expander-for (expander-for definition))
      (@tableitem "Setf expander for this macro"
	(reference expander-for)))
    (when-let (expanders-to (expanders-to definition))
      (render-references expanders-to "Setf expanders to this macro"))))



;; Compiler macros
(defmethod type-name ((definition compiler-macro-definition))
  "Return \"Compiler Macro\"."
  "Compiler Macro")

(defmethod index-command-name ((definition compiler-macro-definition))
  "Return \"compilermacrosubindex\"."
  "compilermacrosubindex")



;; Types
(defmethod type-name ((definition type-definition))
  "Return \"Type\"."
  "Type")

(defmethod index-command-name ((definition type-definition))
  "Return \"typesubindex\"."
  "typesubindex")

;; #### WARNING: a type is a funcoid because it has a lambda list, but it's
;; really a @deftp, not a @deffn.
(defmethod document ((definition type-definition) context &key)
  "Render type DEFINITION's documentation in CONTEXT."
  ;; #### WARNING: casing policy.
  (@deftype (string-downcase (safe-name definition))
      (safe-lambda-list (lambda-list definition))
    (anchor-and-index definition)
    (render-docstring definition)
    (@table ()
      (render-definition-core definition context))))



;; Setf expanders
(defmethod type-name ((expander expander-definition))
  "Return \"Setf Expander\"."
  "Setf Expander")

(defmethod index-command-name ((expander expander-definition))
  "Return \"expandersubindex\"."
  "expandersubindex")

(defmethod document ((definition short-expander-definition) context &key)
  "Render short setf expander DEFINITION's documentation in CONTEXT."
  (render-funcoid definition context
    (when-let (access-definition (access-definition definition))
      (@tableitem "Corresponding Reader"
	(reference access-definition)))
    (@tableitem "Corresponding Writer"
      (reference (update-definition definition)))))

(defmethod document ((definition long-expander-definition) context &key)
  "Render long setf expander DEFINITION's documentation in CONTEXT."
  (render-funcoid definition context
    (when-let (access-definition (access-definition definition))
      (@tableitem "Corresponding Reader"
	(reference access-definition)))))



;; Method combinations
(defmethod type-name ((definition combination-definition))
  "Return \"Method Combination\"."
  "Method Combination")

(defmethod index-command-name ((definition combination-definition))
  "Return \"combinationsubindex\"."
  "combinationsubindex")

(defmacro render-combination (definition context &body body)
  "Render method combination DEFINITION's documentation in CONTEXT."
  (let ((the-definition (gensym "definition"))
	(the-context (gensym "context")))
    `(let ((,the-definition ,definition)
	   (,the-context ,context))
       ;; #### WARNING: casing policy.
       (@defcombination (string-downcase (safe-name ,the-definition))
	 (anchor-and-index ,the-definition)
	(render-docstring ,the-definition)
	(@table ()
	  (render-definition-core ,the-definition ,context)
	  ,@body)))))

;; #### FIXME: rethink the lambda-list problem (at least for long forms).
(defmethod document ((definition combination-definition) context &key)
  "Render standard method combination DEFINITION's documentation in CONTEXT."
  (render-combination definition context
    (render-references (user-definitions definition) "Users")))

;; #### PORTME.
(defmethod document ((definition short-combination-definition) context &key)
  "Render short method combination DEFINITION's documentation in CONTEXT."
  (render-combination definition context
    (when-let (operator-definition (operator-definition definition))
      (@tableitem "Operator"
	(reference operator-definition)))
    (@tableitem "Indentity with one argument"
      (format t "@t{~(~A~)}"
	(sb-pcl::short-combination-identity-with-one-argument
	 (combination definition))))
    (render-references (user-definitions definition) "Users")))



;; Methods
(defmethod safe-name
    ((definition method-definition)
     &optional qualified
     &aux (safe-name (call-next-method)))
  "When QUALIFIED, append method DEFINITION's qualifiers and specializers."
  (if qualified
    (concatenate 'string
      safe-name
      ;; #### NOTE: I'm using an S for qualifiers, assuming they'll always be
      ;; symbols, in order to distinguish keywords from the rest.
      (format nil "~{ ~S~} (~{~A~^ ~})"
	(method-qualifiers (definition-method definition))
	(mapcar (lambda (specializer)
		  ;; #### PORTME.
		  (typecase specializer
		    (sb-mop:eql-specializer specializer)
		    (otherwise (safe-name specializer t))))
	  (specializers definition))))
    safe-name))

(defmethod type-name ((definition method-definition))
  "Return \"Method\"."
  "Method")

(defmethod type-name ((definition reader-method-definition))
  "Return \"Reader Method\"."
  "Reader Method")

(defmethod type-name ((definition writer-method-definition))
  "Return \"Writer Method\"."
  "Writer Method")

(defmethod index-command-name ((definition method-definition))
  "Return \"methodsubindex\"."
  "methodsubindex")

(defmacro render-method
    (|definition(s)| context &aux (the-definition (gensym "definition")))
  "Render method DEFINITION(S) in CONTEXT."
  `(let ((,the-definition ,(if (consp |definition(s)|)
			     (car |definition(s)|)
			     |definition(s)|)))
     ;; #### WARNING: casing policy.
     (@defmethod (type-name ,the-definition)
	 (string-downcase (safe-name ,the-definition))
	 #+()(lambda-list ,the-definition) ""
       #+()(specializers ,the-definition) ""
       #+()(qualifiers ,the-definition) ""
       (anchor-and-index ,the-definition)
       ,@(mapcar (lambda (definition)
		   (let ((the-definition (gensym "definition")))
		     `(let ((,the-definition ,definition))
			(@defmethodx
			    ;; #### WARNING: casing policy.
			    (string-downcase (safe-name ,the-definition))
			    (lambda-list ,the-definition)
			  (specializers ,the-definition)
			  (qualifiers ,the-definition))
			(anchor-and-index ,the-definition))))
	   (when (consp |definition(s)|) (cdr |definition(s)|)))
       (render-docstring ,the-definition)
       (when-let (source-file (source-file ,the-definition))
	 (unless (equal source-file
			(source-file (generic-definition ,the-definition)))
	   (@table ()
	     (@tableitem "Source" (reference source-file))))))))

(defmethod document ((definition method-definition) context &key)
  "Render METHOD's documentation in CONTEXT."
  (render-method definition context))

;; #### FIXME: Implement reader and writer methods.



;; Ordinary functions
(defmethod type-name ((definition ordinary-function-definition))
  "Return \"Function\"."
  "Function")

(defmethod index-command-name ((definition ordinary-function-definition))
  "Return \"functionsubindex\"."
  "functionsubindex")

(defmethod document ((definition simple-function-definition) context &key)
  "Render simple function DEFINITION's documentation in CONTEXT."
  (render-funcoid definition context
    (when-let (expander-for (expander-for definition))
      (@tableitem "Setf expander for this function"
	(reference expander-for)))
    (when-let (expanders-to (expanders-to definition))
      (render-references expanders-to "Setf expanders to this function"))))


(defmethod type-name ((definition reader-definition))
  "Return \"Reader\"."
  "Reader")

(defmethod document ((definition reader-definition) context &key)
  "Render function DEFINITION's documentation in CONTEXT."
  (render-funcoid definition context
    (@tableitem "Corresponding Slot"
      (reference (slot-definition definition)))))


(defmethod type-name ((definition writer-definition))
  "Return \"Writer\"."
  "Writer")

;; #### FIXME: same as above.
(defmethod document ((definition writer-definition) context &key)
  "Render writer DEFINITION's documentation in CONTEXT."
  (render-funcoid definition context
    (@tableitem "Corresponding Slot"
      (reference (slot-definition definition)))))



;; Generic functions
(defmethod type-name ((definition generic-function-definition))
  "Return \"Generic Function\"."
  "Generic Function")

(defmethod index-command-name ((definition generic-function-definition))
  "Return \"genericsubindex\"."
  "genericsubindex")

;; #### PORTME.
(defun render-method-combination (definition)
  "Render generic function DEFINITION's method combination documentation."
  (@tableitem "Method Combination"
    (reference (combination-definition definition))
    (terpri)
    (when-let (options (mapcar (lambda (option)
				 (escape (format nil "~(~S~)" option)))
			 (sb-pcl::method-combination-options
			  (sb-mop:generic-function-method-combination
			   (generic definition)))))
      (format t "@b{Options:} @t{~A}~{, @t{~A}~}"
	(first options)
	(rest options)))))

(defmethod document ((definition simple-generic-definition) context &key)
  "Render simple generic function DEFINITION's documentation in CONTEXT."
  (render-funcoid definition context
    (when-let (expander-for (expander-for definition))
      (@tableitem "Setf expander for this function"
	(reference expander-for)))
    (when-let (expanders-to (expanders-to definition))
      (render-references expanders-to "Setf expanders to this function"))
    (render-method-combination definition)
    (when-let ((methods (method-definitions definition)))
      (@tableitem "Methods"
	(dolist (method methods)
	  (document method context))))))

(defmethod document ((definition generic-setf-definition) context &key)
  "Render generic setf DEFINITION's documentation in CONTEXT."
  (render-funcoid definition context
    (render-method-combination definition)
    (when-let ((methods (method-definitions definition)))
      (@tableitem "Methods"
	(dolist (method methods)
	  (document method context))))))



;; ---------
;; Classoids
;; ---------

;; #### PORTME.
(defun render-initargs (definition context)
  "Render classoid DEFINITION's direct default initargs in CONTEXT."
  (when-let (initargs
	     (sb-mop:class-direct-default-initargs (classoid definition)))
    (@tableitem "Direct Default Initargs"
      ;; #### FIXME: we should rather compute the longest initarg name and use
      ;; that as a template size for the @headitem specification.
      (@multitable (.3f0 .5f0)
	(format t "@headitem Initarg @tab Value~%")
	(dolist (initarg initargs)
	  (format t "@item @t{~A}~%@tab @t{~A}~%"
	    ;; #### WARNING: casing policy.
	    ;; The ~S is to preserve the appearance of keywords.
	    (escape (format nil "~(~S~)" (first initarg)))
	    (escape (format nil "~(~A~)" (second initarg)))))))))

(defmacro render-classoid (definition context &body body)
  "Execute BODY within a classoid DEFINITION documentation in CONTEXT."
  (let ((the-definition (gensym "definition"))
	(the-context (gensym "context")))
    `(let ((,the-definition ,definition)
	   (,the-context ,context))
       (@deftp ((type-name ,the-definition)
		;; #### WARNING: casing policy.
		(string-downcase (safe-name ,the-definition)))
	   (anchor-and-index ,the-definition)
	 (render-docstring ,the-definition)
	 (@table ()
	   (render-definition-core ,the-definition ,the-context)
	   (render-references
	    (superclassoid-definitions ,the-definition)
	    "Direct superclasses")
	   (render-references
	    (subclassoid-definitions ,the-definition)
	    "Direct subclasses")
	   (render-references
	    (method-definitions ,the-definition)
	    "Direct methods")
	   (when-let (slot-definitions (slot-definitions ,the-definition))
	     (@tableitem "Direct slots"
	       (dolist (slot-definition slot-definitions)
		 (document slot-definition ,the-context))))
	   ,@body)))))

(defmethod document ((definition classoid-definition) context &key)
  "Render classoid DEFINITION's documentation in CONTEXT.
This is the default method used for conditions and classes,
which also documents direct default initargs."
  (render-classoid definition context
    (render-initargs definition context)))



;; Structures
(defmethod type-name ((definition structure-definition))
  "Return \"Structure\"."
  "Structure")

(defmethod index-command-name ((definition structure-definition))
  "Return \"structuresubindex\"."
  "structuresubindex")

(defmethod document ((definition structure-definition) context &key)
  "Render structure DEFINITION's documentation in CONTEXT."
  (render-classoid definition context))



;; Conditions
(defmethod type-name ((definition condition-definition))
  "Return \"Condition\"."
  "Condition")

(defmethod index-command-name ((definition condition-definition))
  "Return \"conditionsubindex\"."
  "conditionsubindex")



;; Classes
(defmethod type-name ((definition class-definition))
  "Return \"Class\"."
  "Class")

(defmethod index-command-name ((definition class-definition))
  "Return \"classsubindex\"."
  "classsubindex")


#+()(defmethod document
    ((accessor accessor-definition) context
     &key
     &aux (access-expander (access-expander-definition accessor))
	  (update-expander (update-expander-definition accessor))
	  (writer (writer-definition accessor))
	  (merge-expander
	   (and access-expander
		(functionp (update access-expander))
		(equal (source accessor) (source access-expander))
		(equal (docstring accessor) (docstring access-expander))))
	  (merge-writer
	   (and (writer-definition-p writer)
		(equal (source accessor) (source writer))
		(equal (docstring accessor) (docstring writer))))
	  (merge-setters
	   (and access-expander
		(functionp (update access-expander))
		(writer-definition-p writer)
		(equal (source access-expander) (source writer))
		(equal (docstring access-expander) (docstring writer)))))
  "Render ACCESSOR's documentation in CONTEXT."
  (cond ((and merge-writer merge-expander)
	 (render-funcoid :un (accessor access-expander writer) context
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))))
	(merge-setters
	 (render-funcoid :un accessor context
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))
	   (@tableitem "Setf Expander" (reference access-expander))
	   (@tableitem "Writer" (reference writer)))
	 (render-funcoid :setf (access-expander writer) context
	   (@tableitem "Reader"
	     (reference (access-definition access-expander)))))
	(merge-expander
	 (render-funcoid :un (accessor access-expander) context
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))
	   (when writer
	     (@tableitem "Writer"
	       (reference writer))))
	 (when (writer-definition-p writer)
	   (document writer context)))
	(merge-writer
	 (render-funcoid :un (accessor writer) context
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))
	   (when access-expander
	     (@tableitem "Setf Expander"
	       (reference access-expander))))
	 (when access-expander
	   (document access-expander context)))
	(t
	 (render-funcoid :un accessor context
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))
	   (when access-expander
	     (@tableitem "Setf Expander"
	       (reference access-expander)))
	   (when writer
	     (@tableitem "Writer"
	       (reference writer))))
	 (when access-expander
	   (document access-expander context))
	 (when (writer-definition-p writer)
	   (document writer context)))))

#+()(defmethod document ((method accessor-method-definition) context
		     &key (document-writers t) generic-source)
  "Render accessor METHOD's documentation in CONTEXT."
  (cond ((and (equal (source method)
		     (source (writer-definition method)))
	      (equal (docstring method)
		     (docstring (writer-definition method)))
	      document-writers)
	 (render-method (method (writer-definition method))
			context
			generic-source))
	(t
	 (call-next-method)
	 (when document-writers
	   ;; #### NOTE: if DOCUMENT-WRITERS, it means that we're merging the
	   ;; defintions for the reader and the writer, and hence the generic
	   ;; sources are the same. It's thus ok to use GENERIC-SOURCE here.
	   (document (writer-definition method) context
		     :generic-source generic-source)))))

#+()(defmethod document
    ((writer generic-writer-definition) context &key additional-methods)
  "Render generic WRITER's documentation in CONTEXT."
  (render-funcoid :generic writer context
    (when-let (reader (reader-definition writer))
      (@tableitem "Reader"
	(reference reader)))
    (render-method-combination writer)
    (when-let (methods (append additional-methods
			       (method-definitions writer)))
      (@tableitem "Methods"
	(dolist (method methods)
	  (document method context :generic-source (source writer)))))))

#+()(defmethod document
    ((accessor generic-accessor-definition) context
     &key
     &aux (access-expander (access-expander-definition accessor))
	  (update-expander (update-expander-definition accessor))
	  (writer (writer-definition accessor)))
  "Render generic ACCESSOR's documentation in CONTEXT."
  ;; #### NOTE: contrary to the case of ordinary functions, setf expanders can
  ;; never be merged with generic definitions. The reason is that even if a
  ;; setf expander is in long form, the corresponding lambda function is not
  ;; generic, and we don't mix heterogeneous definitions. One consequence of
  ;; this is that such long form setf expanders will be listed below the
  ;; corresponding generic accessor definition, that is, ni the "Generic
  ;; Functions" section whereas they are ordinary ones. But I still think it's
  ;; better to put them here. Besides, these heterogeneous cases should be
  ;; extremely rare anyway.
  (cond ((and (generic-writer-definition-p writer)
	      (equal (source accessor) (source writer))
		(equal (docstring accessor) (docstring writer))
		(eq (definition-symbol (combination-definition accessor))
		    (definition-symbol (combination-definition writer)))
		(equal (sb-pcl::method-combination-options
			(sb-mop:generic-function-method-combination
			 (generic accessor)))
		       (sb-pcl::method-combination-options
			(sb-mop:generic-function-method-combination
			 (generic writer)))))
	 (render-funcoid :generic (accessor writer) context
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))
	   (when access-expander
	     (@tableitem "Setf Expander"
	       (reference access-expander)))
	   (render-method-combination accessor)
	   (let ((accessor-and-reader-methods (method-definitions accessor))
		 (writer-methods (method-definitions writer)))
	     (when (or accessor-and-reader-methods writer-methods)
	       (@tableitem "Methods"
		 (dolist (method accessor-and-reader-methods)
		   (document method context
		     :generic-source (source accessor)))
		 (dolist (method writer-methods)
		   (document method context
		     :generic-source (source accessor)))))))
	 (when access-expander
	   (document access-expander context)))
	(t
	 (render-funcoid :generic accessor context
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))
	   (when access-expander
	     (@tableitem "Setf Expander"
	       (reference access-expander)))
	   (when writer
	     (@tableitem "Writer"
	       (reference writer)))
	   (render-method-combination accessor)
	   (when-let (methods (method-definitions accessor))
	     (@tableitem "Methods"
		(dolist (method methods)
		  (document method context
		    :document-writers nil
		    :generic-source (source accessor))))))
	 (when access-expander
	   (document access-expander context))
	 (when (generic-writer-definition-p writer)
	   (document writer context
		     :additional-methods
		     (mapcar #'writer-definition
			     (remove-if-not
			      #'accessor-method-definition-p
			      (method-definitions accessor))))))))

#+()(defmethod document ((expander setf-expander-definition) context &key)
  "Render setf EXPANDER's documentation in CONTEXT."
  (render-funcoid :setf expander context
    (@tableitem "Reader"
      (reference (access-definition expander)))
    (when (symbol-definition-p (update expander))
      (@tableitem "Writer"
	(reference (update expander))))))




;; ==========================================================================
;; Definition Nodes
;; ==========================================================================

;; ----------
;; Categories
;; ----------

;; #### NOTE: the order in *CATEGORIES* is important (see
;; ADD-CATEGORIES-NODE). It conditions the order of appearance of the
;; definitions in the generated manual.

(defparameter *categories*
  '((constant-definition          "constants")
    (special-definition           "special variables")
    (symbol-macro-definition      "symbol macros")
    (macro-definition             "macros")
    (compiler-macro-definition    "compiler macros")
    (expander-definition          "setf expanders")
    (ordinary-function-definition "ordinary functions")
    (generic-function-definition  "generic functions")
    (combination-definition       "method combinations")
    (condition-definition         "conditions")
    (structure-definition         "structures")
    (class-definition             "classes")
    (type-definition              "types"))
  "The list of definition categories.
Each category is of type (TYPE DESCRIPTION-STRING).")

(defun add-category-node (parent context status category definitions)
  "Add the STATUS CATEGORY node to PARENT for DEFINITIONS in CONTEXT."
  (add-child parent
    (make-node :name (format nil "~@(~A ~A~)" status category)
	       :section-name (format nil "~@(~A~)" category)
	       :before-menu-contents
	       (render-to-string
		 (dolist (definition (sort definitions #'string-lessp
					   :key #'definition-symbol))
		   (document definition context))))))

(defun add-categories-node (parent context status definitions)
  "Add the STATUS DEFINITIONS categories nodes to PARENT in CONTEXT."
  (dolist (category *categories*)
    (when-let (type-definitions
	       (remove-if-not (lambda (definition)
				(typep definition (first category)))
		   definitions))
      (add-category-node parent context status (second category)
			 type-definitions))))

(defun add-definitions-node
    (parent extract context
     &aux (public-definitions (public-definitions extract))
	  (public-definitions-number (length public-definitions))
	  (private-definitions (private-definitions extract))
	  (private-definitions-number (length private-definitions)))
  "Add EXTRACT's definitions node to PARENT in CONTEXT."
  (unless (zerop (+ public-definitions-number private-definitions-number))
    (let ((definitions-node
	    (add-child parent
	      (make-node :name "Definitions"
			 :synopsis "The symbols documentation"
			 :before-menu-contents(format nil "~
Definitions are sorted by export status, category, package, and then by
lexicographic order.")))))
      (unless (zerop public-definitions-number)
	(let ((node (add-child definitions-node
		      (make-node :name "Public Interface"))))
	  (add-categories-node node context "public" public-definitions)))
      (unless (zerop private-definitions-number)
	(let ((node (add-child definitions-node
		      (make-node :name "Internals"))))
	  (add-categories-node node context "private" private-definitions))))))

;;; symbol.lisp ends here
