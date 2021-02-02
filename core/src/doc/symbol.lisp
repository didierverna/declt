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

;; #### FIXME: restore this properly.
#+()(defmethod anchor-name ((method method-definition))
  "Return METHOD's qualified symbol name, specializers and qualifiers ."
  (format nil "~A::~A~{ ~A~^~}~{ ~A~^~}"
    (definition-package-name method)
    (pretty-name method)
    (mapcar (lambda (specializer) (pretty-specializer specializer t))
	    (specializers method))
    (qualifiers method)))



;; ------------------
;; Type name protocol
;; ------------------

(defmethod type-name ((constant constant-definition))
  "Return \"constant\""
  "constant")

(defmethod type-name ((special special-definition))
  "Return \"special variable\""
  "special variable")

(defmethod type-name ((symbol-macro symbol-macro-definition))
  "Return \"symbol macro\""
  "symbol macro")

(defmethod type-name ((macro macro-definition))
  "Return \"macro\""
  "macro")

(defmethod type-name ((compiler-macro compiler-macro-definition))
  "Return \"compiler macro\""
  "compiler macro")

(defmethod type-name ((function ordinary-function-definition))
  "Return \"function\""
  "ordinary function")

(defmethod type-name ((generic generic-function-definition))
  "Return \"generic function\""
  "generic function")

(defmethod type-name ((method method-definition))
  "Return \"method\""
  "method")

(defmethod type-name ((expander expander-definition))
  "Return \"setf expander\""
  "setf expander")

(defmethod type-name ((definition slot-definition))
  "Return \"slot\""
  "slot")

(defmethod type-name ((combination combination-definition))
  "Return \"standard method combination\"."
  "standard method combination")

(defmethod type-name ((combination short-combination-definition))
  "Return \"short method combination\"."
  "short method combination")

(defmethod type-name ((combination long-combination-definition))
  "Return \"long method combination\"."
  "long method combination")

(defmethod type-name ((condition condition-definition))
  "Return \"condition\""
  "condition")

(defmethod type-name ((structure structure-definition))
  "Return \"structure\""
  "structure")

(defmethod type-name ((class class-definition))
  "Return \"class\""
  "class")

(defmethod type-name ((type type-definition))
  "Return \"type\""
  "type")



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
    (reference (definition-package definition)))
  (render-source definition context))

(defmacro render-varoid
    (kind varoid context &body body
     &aux (the-varoid (gensym "varoid"))
	  (defcmd (intern (concatenate 'string "@DEF" (symbol-name kind))
			  :net.didierverna.declt)))
  "Render VAROID definition of KIND in CONTEXT."
  `(let ((,the-varoid ,varoid))
     (,defcmd (string-downcase (pretty-name ,the-varoid))
       (anchor-and-index ,the-varoid)
       (render-docstring ,the-varoid)
       (@table ()
	 (render-definition-core ,the-varoid ,context)
	 ,@body))))

(defmacro render-funcoid
    (kind |funcoid(s)| context &body body
     &aux (the-funcoid (gensym "funcoid"))
	  (defcmd (intern (concatenate 'string "@DEF" (symbol-name kind))
			  :net.didierverna.declt)))
  "Render FUNCOID(S) definition of KIND in CONTEXT."
  `(let ((,the-funcoid ,(if (consp |funcoid(s)|)
			    (car |funcoid(s)|)
			    |funcoid(s)|)))
     (,defcmd (string-downcase (pretty-name ,the-funcoid)) (lambda-list ,the-funcoid)
       (anchor-and-index ,the-funcoid)
       ,@(mapcar (lambda (funcoid)
		   `(render-headline ,funcoid))
	   (when (consp |funcoid(s)|) (cdr |funcoid(s)|)))
       (render-docstring ,the-funcoid)
       (@table ()
	 (render-definition-core ,the-funcoid ,context)
	 ,@body))))

(defmacro render-method
    (|method(s)| context generic-source &aux (the-method (gensym "method")))
  "Render METHOD(S) definition in CONTEXT.
GENERIC-SOURCE is the source of the generic function. METHOD(S) sources are
not advertised if they are the same as GENERIC-SOURCE."
  `(let ((,the-method ,(if (consp |method(s)|)
			   (car |method(s)|)
			   |method(s)|)))
     (@defmethod (string-downcase (pretty-name ,the-method))
	 (lambda-list ,the-method)
	 (specializers ,the-method)
	 (qualifiers ,the-method)
       (anchor-and-index ,the-method)
       ,@(mapcar (lambda (method)
		   (let ((the-method (gensym "method")))
		     `(let ((,the-method ,method))
			(@defmethodx
			    (string-downcase (pretty-name ,the-method))
			    (lambda-list ,the-method)
			    (specializers ,the-method)
			    (qualifiers ,the-method))
			(anchor-and-index ,the-method))))
		 (when (consp |method(s)|) (cdr |method(s)|)))
       (render-docstring ,the-method)
       (unless (equal (source ,the-method) ,generic-source)
	 (@table ()
	   (render-source ,the-method ,context))))))

(defun render-slot-property
    (slot property
	  &key (renderer (lambda (value)
			   (format t "@t{~A}~%"
			     (escape (format nil "~(~S~)" value)))))
	  &aux (value (slot-property (slot slot) property)))
  "Render SLOT definition's PROPERTY value as a table item."
  (when (and value
	     (not (and (eq value t) (eq property :type)))
	     (not (and (eq value :instance) (eq property :allocation))))
    (@tableitem (format nil "~@(~A~)" (symbol-name property))
      (funcall renderer value))))

(defun render-slot (slot)
  "Render SLOT's documentation."
  (@defslot (string-downcase (pretty-name slot))
    (index slot)
    (render-docstring slot)
    (@table ()
      (render-slot-property slot :type)
      (render-slot-property slot :allocation)
      (render-slot-property slot :initargs
	:renderer (lambda (value)
		    (let ((values (mapcar (lambda (val)
					    (escape
					     (format nil "~(~S~)" val)))
					  value)))
		      (format t "@t{~A}~{, @t{~A}~}"
			(first values)
			(rest values)))))
      (render-slot-property slot :initform)
      (render-references (reader-definitions slot) "Readers")
      (render-references (writer-definitions slot) "Writers"))))

(defun render-slots (classoid)
  "Render CLASSOID's direct slots documentation."
  (when-let (slots (slot-definitions classoid))
    (@tableitem "Direct slots"
      (dolist (slot slots)
	(render-slot slot)))))

(defmacro render-combination (kind combination context &body body)
  "Render method COMBINATION's definition of KIND in CONTEXT."
  (let ((the-combination (gensym "combination")))
    `(let ((,the-combination ,combination))
       (@defcombination (string-downcase (pretty-name ,the-combination)) ,kind
	 (anchor-and-index ,the-combination)
	 (render-docstring ,the-combination)
	 (@table ()
	   (render-definition-core ,the-combination ,context)
	   ,@body)))))

(defmacro render-classoid (kind classoid context &body body)
  "Render CLASSOID's definition of KIND in CONTEXT."
  (let ((|@defform| (intern (concatenate 'string "@DEF" (symbol-name kind))
			    :net.didierverna.declt))
	(the-classoid (gensym "classoid")))
    `(let ((,the-classoid ,classoid))
       (,|@defform| (string-downcase (pretty-name ,the-classoid))
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
	   (render-slots ,the-classoid)
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
	   (string-downcase (pretty-name definition)) (lambda-list definition))
  (anchor-and-index definition))



;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

;; #### NOTE: the INDEX methods below only perform sub-indexing because the
;; main index entries are created automatically in Texinfo by the @defXXX
;; routines.

(defmethod index ((constant constant-definition))
  "Render CONSTANT's indexing command."
  (format t "@constantsubindex{~(~A~)}@c~%" (escape constant)))

(defmethod index ((special special-definition))
  "Render SPECIAL's indexing command."
  (format t "@specialsubindex{~(~A~)}@c~%" (escape special)))

(defmethod index ((symbol-macro symbol-macro-definition))
  "Render SYMBOL-MACRO's indexing command."
  (format t "@symbolmacrosubindex{~(~A~)}@c~%" (escape symbol-macro)))

(defmethod index ((macro macro-definition))
  "Render MACRO's indexing command."
  (format t "@macrosubindex{~(~A~)}@c~%" (escape macro)))

(defmethod index ((compiler-macro compiler-macro-definition))
  "Render COMPILER-MACRO's indexing command."
  (format t "@compilermacrosubindex{~(~A~)}@c~%" (escape compiler-macro)))

(defmethod index ((function ordinary-function-definition))
  "Render FUNCTION's indexing command."
  (format t "@functionsubindex{~(~A~)}@c~%" (escape function)))

(defmethod index ((method method-definition))
  "Render METHOD's indexing command."
  (format t "@methodsubindex{~(~A~)}@c~%" (escape method)))

(defmethod index ((generic generic-function-definition))
  "Render GENERIC's indexing command."
  (format t "@genericsubindex{~(~A~)}@c~%" (escape generic)))

(defmethod index ((expander expander-definition))
  "Render setf EXPANDER's indexing command."
  (format t "@setfexpandersubindex{~(~A~)}@c~%" (escape expander)))

(defmethod index ((slot slot-definition))
  "Render SLOT's indexing command."
  (format t "@slotsubindex{~(~A~)}@c~%" (escape slot)))

(defmethod index ((combination short-combination-definition))
  "Render short method COMBINATION's indexing command."
  (format t "@shortcombinationsubindex{~(~A~)}@c~%" (escape combination)))

(defmethod index ((combination long-combination-definition))
  "Render long method COMBINATION's indexing command."
  (format t "@longcombinationsubindex{~(~A~)}@c~%" (escape combination)))

(defmethod index ((condition condition-definition))
  "Render CONDITION's indexing command."
  (format t "@conditionsubindex{~(~A~)}@c~%" (escape condition)))

(defmethod index ((structure structure-definition))
  "Render STRUCTURE's indexing command."
  (format t "@structuresubindex{~(~A~)}@c~%" (escape structure)))

(defmethod index ((class class-definition))
  "Render CLASS's indexing command."
  (format t "@classsubindex{~(~A~)}@c~%" (escape class)))

(defmethod index ((type type-definition))
  "Render TYPE's indexing command."
  (format t "@typesubindex{~(~A~)}@c~%" (escape type)))

(defmethod document ((constant constant-definition) context &key)
  "Render CONSTANT's documentation in CONTEXT."
  (render-varoid :constant constant context))

(defmethod document ((special special-definition) context &key)
  "Render SPECIAL variable's documentation in CONTEXT."
  (render-varoid :special special context))

;; #### FIXME: there's no reason to show the expansion of a symbol macro, and
;; not, say, that of a regular macro, or even a functional value.
(defmethod document ((symbol-macro symbol-macro-definition) context &key)
  "Render SYMBOL-MACRO's documentation in CONTEXT."
    (render-varoid :symbolmacro symbol-macro context
      (@tableitem "Expansion"
	(format t "@t{~(~A~)}~%"
	  (escape
	   (format nil "~S"
		   (macroexpand-1 (definition-symbol symbol-macro))))))))

(defmethod document
    ((macro macro-definition) context
     &key
     &aux (access-expander (access-expander-definition macro))
	  (update-expander (update-expander-definition macro))
	  (merge (and access-expander
		      (functionp (update access-expander))
		      (equal (source macro) (source access-expander))
		      (equal (docstring macro) (docstring access-expander)))))
  "Render MACRO's documentation in CONTEXT."
  (cond (merge
	 (render-funcoid :macro (macro access-expander) context
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))))
	(t
	 (render-funcoid :macro macro context
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))
	   (when access-expander
	     (@tableitem "Setf Expander"
	       (reference access-expander))))
	 (when access-expander
	   (document access-expander context)))))

(defmethod document ((compiler-macro compiler-macro-definition) context &key)
  "Render COMPILER-MACRO's documentation in CONTEXT."
  (render-funcoid :compilermacro compiler-macro context))

(defmethod document ((function ordinary-function-definition) context &key)
  "Render FUNCTION's documentation in CONTEXT."
  (render-funcoid :un function context
    (when-let (expander (update-expander-definition function))
      (@tableitem "Setf Expander"
	(reference expander)))))

(defmethod document ((writer writer-definition) context &key)
  "Render WRITER's documentation in CONTEXT."
  (render-funcoid :un writer context
    (when-let (reader (reader-definition writer))
      (@tableitem "Reader"
	(reference reader)))))

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

(defmethod document ((method method-definition) context &key generic-source)
  "Render METHOD's documentation in CONTEXT.
GENERIC-SOURCE is the source of METHOD's generic function."
  (render-method method context generic-source))

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

;; #### PORTME.
(defun render-method-combination
    (generic &aux (combination (combination-definition generic)))
  "Render GENERIC definition's method combination documentation.
The standard method combination is not rendered."
  (unless (eq (definition-symbol combination) 'standard)
    (@tableitem "Method Combination"
      (reference combination)
      (terpri)
      (when-let (options (mapcar (lambda (option)
				   (escape (format nil "~(~S~)" option)))
			   (sb-pcl::method-combination-options
			    (sb-mop:generic-function-method-combination
			     (generic generic)))))
	(format t "@b{Options:} @t{~A}~{, @t{~A}~}"
	  (first options)
	  (rest options))))))

(defmethod document ((generic generic-function-definition) context &key)
  "Render GENERIC's documentation in CONTEXT."
  (render-funcoid :generic generic context
    (when-let (expander (update-expander-definition generic))
      (@tableitem "Setf Expander"
	(reference expander)))
    (render-method-combination generic)
    (when-let ((methods (method-definitions generic)))
      (@tableitem "Methods"
	(dolist (method methods)
	  (document method context :generic-source (source generic)))))))

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

;; #### NOTE: no DOCUMENT method for SLOT-DEFINITION

;; #### PORTME.
(defmethod document ((combination short-combination-definition) contexy &key)
  "Render short method COMBINATION's documentation in CONTEXT."
  (render-combination :short combination context
    (@tableitem "Operator"
      (reference (operator-definition combination)))
    (@tableitem "Indentity with one argument"
      (format t "@t{~(~A~)}"
	(sb-pcl::short-combination-identity-with-one-argument
	 (combination combination))))
    (render-references (users combination) "Users")))

(defmethod document ((combination long-combination-definition) context &key)
  "Render long method COMBINATION's documentation in CONTEXT."
  (render-combination :long combination context
    (render-references (users combination) "Users")))

(defmethod document ((condition condition-definition) context &key)
  "Render CONDITION's documentation in CONTEXT."
  (render-classoid :cond condition context
    (render-initargs condition)))

(defmethod document ((structure structure-definition) context &key)
  "Render STRUCTURE's documentation in CONTEXT."
  (render-classoid :struct structure context))

(defmethod document ((class class-definition) context &key)
  "Render CLASS's documentation in CONTEXT."
  (render-classoid :class class context
    (render-initargs class)))

(defmethod document ((type type-definition) context &key)
  "Render TYPE's documentation in CONTEXT."
  (@deftype ((string-downcase (pretty-name type)) (lambda-list type))
    (anchor-and-index type)
    (render-docstring type)
    (@table ()
      (render-definition-core type context))))



;; ==========================================================================
;; Definition Nodes
;; ==========================================================================

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
    (when-let (type-definitions (type-definitions (first category) definitions))
      (add-category-node parent context status (second category)
			 type-definitions))))

(defun add-status-definitions-node (parent context status definitions)
  "Add the STATUS DEFINITIONS node to PARENT in CONTEXT."
  (let ((node (add-child parent
		(make-node :name (format nil "~@(~A~) definitions" status)))))
    (add-categories-node node context status definitions)))

(defun add-definitions-node
    (parent context
     &aux (external-definitions (external-definitions context))
	  (external-definitions-number (length external-definitions))
	  (internal-definitions (internal-definitions context))
	  (internal-definitions-number (length internal-definitions)))
  "Add the definitions node to PARENT in CONTEXT."
  (unless (zerop (+ external-definitions-number internal-definitions-number))
    (let ((definitions-node
	    (add-child parent
	      (make-node :name "Definitions"
			 :synopsis "The symbols documentation"
			 :before-menu-contents(format nil "~
Definitions are sorted by export status, category, package, and then by
lexicographic order.")))))
      (unless (zerop external-definitions-number)
	(add-status-definitions-node definitions-node context "exported"
				     external-definitions))
      (unless (zerop internal-definitions-number)
	(add-status-definitions-node definitions-node context "internal"
				     internal-definitions)))))

;;; symbol.lisp ends here
