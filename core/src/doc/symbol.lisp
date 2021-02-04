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

(defmacro render-varoid
    (kind varoid context
     &body body
     &aux (the-varoid (gensym "varoid"))
	  (defcmd (intern (concatenate 'string "@DEF" (symbol-name kind))
			  :net.didierverna.declt)))
  "Render VAROID definition of KIND in CONTEXT."
  `(let ((,the-varoid ,varoid))
     ;; #### WARNING: casing policy.
     (,defcmd (string-downcase (safe-name ,the-varoid))
       (anchor-and-index ,the-varoid)
       (render-docstring ,the-varoid)
       (@table ()
	 (render-definition-core ,the-varoid ,context)
	 ,@body))))

(defmacro render-funcoid
    (kind |funcoid(s)| context
     &body body
     &aux (the-funcoid (gensym "funcoid"))
	  (defcmd (intern (concatenate 'string "@DEF" (symbol-name kind))
			  :net.didierverna.declt)))
  "Render FUNCOID(S) definition of KIND in CONTEXT."
  `(let ((,the-funcoid ,(if (consp |funcoid(s)|)
			  (car |funcoid(s)|)
			  |funcoid(s)|)))
     ;; #### WARNING: casing policy.
     (,defcmd (string-downcase (safe-name ,the-funcoid))
	 (lambda-list ,the-funcoid)
       (anchor-and-index ,the-funcoid)
       ,@(mapcar (lambda (funcoid) `(render-headline ,funcoid))
	   (when (consp |funcoid(s)|) (cdr |funcoid(s)|)))
       (render-docstring ,the-funcoid)
       (@table ()
	 (render-definition-core ,the-funcoid ,context)
	 ,@body))))

(defun render-slots (classoid context)
  "Render CLASSOID's direct slots documentation."
  (when-let (slot-definitions (slot-definitions classoid))
    (@tableitem "Direct slots"
      (dolist (slot-definition slot-definitions)
	(document slot-definition context)))))

(defmacro render-classoid (kind classoid context &body body)
  "Render CLASSOID's definition of KIND in CONTEXT."
  (let ((|@defform| (intern (concatenate 'string "@DEF" (symbol-name kind))
			    :net.didierverna.declt))
	(the-classoid (gensym "classoid")))
    `(let ((,the-classoid ,classoid))
       (,|@defform| (string-downcase (safe-name ,the-classoid))
	 (anchor-and-index ,the-classoid)
	 (render-docstring ,the-classoid)
	 (@table ()
	   (render-definition-core ,the-classoid ,context)
	   (render-references
	    (superclassoid-definitions ,the-classoid)
	    "Direct superclasses")
	   (render-references
	    (subclassoid-definitions ,the-classoid)
	    "Direct subclasses")
	   (render-references
	    (method-definitions ,the-classoid)
	    "Direct methods")
	   (render-slots ,the-classoid context)
	   ,@body)))))

;; #### PORTME.
(defun render-initargs (classoid)
  "Render CLASSOID's direct default initargs."
  (when-let (initargs (sb-mop:class-direct-default-initargs
		       (find-class (definition-symbol classoid))))
    (@tableitem "Direct Default Initargs"
      ;; #### FIXME: we should rather compute the longest initarg name and use
      ;; that as a template size for the @headitem specification.
      (@multitable (.3f0 .5f0)
	(format t "@headitem Initarg @tab Value~%")
	(dolist (initarg initargs)
	  (format t "@item @t{~A}~%@tab @t{~A}~%"
	    (escape (format nil "~(~S~)" (first initarg)))
	    (escape (format nil "~(~S~)" (second initarg)))))))))

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

;; Constants
(defmethod type-name ((definition constant-definition))
  "Return \"constant\"."
  "constant")

(defmethod index-command-name ((definition constant-definition))
  "Return \"constantsubindex\"."
  "constantsubindex")

(defmethod document ((definition constant-definition) context &key)
  "Render constant DEFINITION's documentation in CONTEXT."
  (render-varoid :constant definition context))



;; Special variables
(defmethod type-name ((definition special-definition))
  "Return \"special variable\"."
  "special variable")

(defmethod index-command-name ((definition special-definition))
  "Return \"specialsubindex\"."
  "specialsubindex")

(defmethod document ((definition special-definition) context &key)
  "Render special variable DEFINITION's documentation in CONTEXT."
  (render-varoid :special definition context))



;; Symbol macros
(defmethod type-name ((definition symbol-macro-definition))
  "Return \"symbol macro\"."
  "symbol macro")

(defmethod index-command-name ((definition symbol-macro-definition))
  "Return \"symbolmacrosubindex\"."
  "symbolmacrosubindex")

(defmethod document ((definition symbol-macro-definition) context &key)
  "Render symbol macro definition's documentation in CONTEXT."
    (render-varoid :symbolmacro definition context))


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
  "Return \"slot\"."
  "slot")

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

;; #### NOTE: even though slots are varoids, they get a special treatment
;; because they appear as part of a classoid documentation. In particular, we
;; don't advertise their source file, as it is the same as the parent
;; classoid, and we only advertise their package if it's different from that
;; of the parent classoid.
(defmethod document
    ((definition slot-definition) context &key &aux (slot (slot definition)))
  "Render slot DEFINITION's documentation in CONTEXT."
  ;; #### WARNING: casing policy.
  (@defslot (string-downcase (safe-name definition))
    (anchor-and-index definition)
    (render-docstring definition)
    (unless (eq (package-definition definition)
		(package-definition (classoid-definition definition)))
      (@tableitem "Package"
	(reference (package-definition definition))))
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

;; Macros
(defmethod type-name ((definition macro-definition))
  "Return \"macro\"."
  "macro")

(defmethod index-command-name ((definition macro-definition))
  "Return \"macrosubindex\"."
  "macrosubindex")

;; #### FIXME: rethink the possibilities of merging with the expander-for.
(defmethod document ((definition macro-definition) context &key)
  "Render macro DEFINITION's documentation in CONTEXT."
  (render-funcoid :macro definition context
    (when-let (expander-for (expander-for definition))
      (@tableitem "Setf expander for this macro"
	(reference expander-for)))
    (when-let (expanders-to (expanders-to definition))
      (render-references expanders-to "Setf expanders to this macro"))))



;; Compiler macros
(defmethod type-name ((definition compiler-macro-definition))
  "Return \"compiler macro\"."
  "compiler macro")

(defmethod index-command-name ((definition compiler-macro-definition))
  "Return \"compilermacrosubindex\"."
  "compilermacrosubindex")

(defmethod document ((definition compiler-macro-definition) context &key)
  "Render compiler macro DEFINITION's documentation in CONTEXT."
  (render-funcoid :compilermacro definition context))



;; Types
(defmethod type-name ((definition type-definition))
  "Return \"type\"."
  "type")

(defmethod index-command-name ((definition type-definition))
  "Return \"typesubindex\"."
  "typesubindex")

(defmethod document ((definition type-definition) context &key)
  "Render type DEFINITION's documentation in CONTEXT."
  ;; #### WARNING: casing policy.
  (@deftype ((string-downcase (safe-name definition)) (lambda-list definition))
      (anchor-and-index definition)
    (render-docstring definition)
    (@table ()
      (render-definition-core definition context))))




;; Setf expanders
(defmethod type-name ((expander expander-definition))
  "Return \"setf expander\"."
  "setf expander")

(defmethod index-command-name ((expander expander-definition))
  "Return \"expandersubindex\"."
  "expandersubindex")

(defmethod document ((definition short-expander-definition) context &key)
  "Render short setf expander DEFINITION's documentation in CONTEXT."
  (render-funcoid :setf definition context
    (when-let (access-definition (access-definition definition))
      (@tableitem "Corresponding Reader"
	(reference access-definition)))
    (@tableitem "Corresponding Writer"
      (reference (update-definition definition)))))

(defmethod document ((definition long-expander-definition) context &key)
  "Render long setf expander DEFINITION's documentation in CONTEXT."
  (render-funcoid :setf definition context
    (when-let (access-definition (access-definition definition))
      (@tableitem "Corresponding Reader"
	(reference access-definition)))))



;; Method combinations
(defmethod type-name ((definition combination-definition))
  "Return \"method combination\"."
  "method combination")

(defmethod index-command-name ((definition combination-definition))
  "Return \"combinationsubindex\"."
  "combinationsubindex")

(defmacro render-combination (kind definition context &body body)
  "Render KIND method combination DEFINITION's documentation in CONTEXT."
  (let ((the-definition (gensym "definition")))
    `(let ((,the-definition ,definition))
       ;; #### WARNING: casing policy.
       (@defcombination (string-downcase (safe-name ,the-definition)) ,kind
	 (anchor-and-index ,the-definition)
	 (render-docstring ,the-definition)
	 (@table ()
	   (render-definition-core ,the-definition ,context)
	   ,@body)))))

(defmethod document ((definition combination-definition) context &key)
  "Render standard method combination DEFINITION's documentation in CONTEXT."
  (render-combination :standard definition context
    (render-references (user-definitions definition) "Users")))

;; #### PORTME.
(defmethod document ((definition short-combination-definition) context &key)
  "Render short method combination DEFINITION's documentation in CONTEXT."
  (render-combination :short definition context
    (when-let (operator-definition (operator-definition definition))
      (@tableitem "Operator"
	(reference operator-definition)))
    (@tableitem "Indentity with one argument"
      (format t "@t{~(~A~)}"
	(sb-pcl::short-combination-identity-with-one-argument
	 (combination definition))))
    (render-references (user-definitions definition) "Users")))

(defmethod document ((definition long-combination-definition) context &key)
  "Render long method combination DEFINITION's documentation in CONTEXT."
  (render-combination :long definition context
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
  "Return \"method\"."
  "method")

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
     (@defmethod (string-downcase (safe-name ,the-definition))
	 (lambda-list ,the-definition)
       (specializers ,the-definition)
       (qualifiers ,the-definition)
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
  "Return \"function\"."
  "function")

(defmethod index-command-name ((definition ordinary-function-definition))
  "Return \"functionsubindex\"."
  "functionsubindex")

(defmethod document ((definition simple-function-definition) context &key)
  "Render simple function DEFINITION's documentation in CONTEXT."
  (render-funcoid :un definition context
    (when-let (expander-for (expander-for definition))
      (@tableitem "Setf expander for this function"
	(reference expander-for)))
    (when-let (expanders-to (expanders-to definition))
      (render-references expanders-to "Setf expanders to this function"))))

(defmethod document ((definition setf-function-definition) context &key)
  "Render setf function DEFINITION's documentation in CONTEXT."
  (render-funcoid :un definition context))

;; #### FIXME: should be other kinds of @def.
(defmethod document ((definition reader-definition) context &key)
  "Render function DEFINITION's documentation in CONTEXT."
  (render-funcoid :un definition context
    (@tableitem "Corresponding Slot"
      (reference (slot-definition definition)))))

;; #### FIXME: should be other kinds of @def.
(defmethod document ((definition writer-definition) context &key)
  "Render writer DEFINITION's documentation in CONTEXT."
  (render-funcoid :un definition context
    (@tableitem "Corresponding Slot"
      (reference (slot-definition definition)))))



;; Generic functions
(defmethod type-name ((definition generic-function-definition))
  "Return \"generic function\"."
  "generic function")

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
  (render-funcoid :generic definition context
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
  (render-funcoid :generic definition context
    (render-method-combination definition)
    (when-let ((methods (method-definitions definition)))
      (@tableitem "Methods"
	(dolist (method methods)
	  (document method context))))))



;; ---------
;; Classoids
;; ---------

;; Conditions
(defmethod type-name ((definition condition-definition))
  "Return \"condition\"."
  "condition")

(defmethod index-command-name ((definition condition-definition))
  "Return \"conditionsubindex\"."
  "conditionsubindex")

(defmethod document ((definition condition-definition) context &key)
  "Render condition DEFINITION's documentation in CONTEXT."
  (render-classoid :cond definition context
    (render-initargs definition)))



;; Structures
(defmethod type-name ((definition structure-definition))
  "Return \"structure\"."
  "structure")

(defmethod index-command-name ((definition structure-definition))
  "Return \"structuresubindex\"."
  "structuresubindex")

(defmethod document ((definition structure-definition) context &key)
  "Render structure DEFINITION's documentation in CONTEXT."
  (render-classoid :struct definition context))



;; Classes
(defmethod type-name ((definition class-definition))
  "Return \"class\"."
  "class")

(defmethod index-command-name ((definition class-definition))
  "Return \"classsubindex\"."
  "classsubindex")

(defmethod document ((definition class-definition) context &key)
  "Render class DEFINITION's documentation in CONTEXT."
  (render-classoid :class definition context
    (render-initargs definition)))


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
