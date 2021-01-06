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

(defmethod name ((definition symbol-definition))
  "Return DEFINITION's symbol name."
  (name (definition-symbol definition)))

;; #### NOTE: all of these methods are in fact equivalent. That's the drawback
;; of using structures instead of classes, which limits the inheritance
;; expressiveness (otherwise I could have used a writer mixin or something).

;; #### NOTE: spaces in symbol names are "revealed", but not the ones below
;; (between SETF and the symbol) because that would look rather weird in the
;; output. Consequently, Declt must expect to get names with unescaped
;; spaces. @DEFFN, @DEFFNX, AND @DEFTP take care of protecting their NAME
;; argument with braces because of that.
(defmethod name ((writer writer-definition))
  "Return WRITER's name, that is (setf <name>)."
  (format nil "(SETF ~A)" (name (definition-symbol writer))))

(defmethod name ((writer-method writer-method-definition))
  "Return WRITER-METHOD's name, that is (setf <name>)."
  (format nil "(SETF ~A)"
    (name (definition-symbol writer-method))))

(defmethod name ((generic-writer generic-writer-definition))
  "Return GENERIC-WRITER's name, that is (setf <name>)."
  (format nil "(SETF ~A)"
    (name (definition-symbol generic-writer))))

(defmethod name ((expander setf-expander-definition))
  "Return setf EXPANDER's name, that is (setf <name>)."
  (format nil "(SETF ~A)" (name (definition-symbol expander))))



;; ==========================================================================
;; Utilities
;; ==========================================================================

(defun definition-package-name (definition)
  "Return DEFINITION's symbol home package name."
  (name (definition-package definition)))

(defun render-internal-definitions-references (definitions)
  "Render references to a list of internal DEFINITIONS."
  (render-references definitions "Internal Definitions"))

(defun render-external-definitions-references (definitions)
  "Render references to a list of external DEFINITIONS."
  (render-references definitions "Exported Definitions"))

(defun render-definition-core (definition extract)
  "Render DEFINITION's documentation core in EXTRACT.
The documentation core includes all common definition attributes:
  - package,
  - source location.

Each element is rendered as a table item."
  (@tableitem "Package"
    (reference (definition-package definition)))
  (render-source definition extract))

(defmacro render-varoid
    (kind varoid extract &body body
     &aux (the-varoid (gensym "varoid"))
	  (defcmd (intern (concatenate 'string "@DEF" (symbol-name kind))
			  :net.didierverna.declt)))
  "Render VAROID definition of KIND in EXTRACT."
  `(let ((,the-varoid ,varoid))
     (,defcmd (string-downcase (name ,the-varoid))
       (anchor-and-index ,the-varoid)
       (render-docstring ,the-varoid)
       (@table ()
	 (render-definition-core ,the-varoid ,extract)
	 ,@body))))

(defmacro render-funcoid
    (kind |funcoid(s)| extract &body body
     &aux (the-funcoid (gensym "funcoid"))
	  (defcmd (intern (concatenate 'string "@DEF" (symbol-name kind))
			  :net.didierverna.declt)))
  "Render FUNCOID(S) definition of KIND in EXTRACT."
  `(let ((,the-funcoid ,(if (consp |funcoid(s)|)
			    (car |funcoid(s)|)
			    |funcoid(s)|)))
     (,defcmd (string-downcase (name ,the-funcoid)) (lambda-list ,the-funcoid)
       (anchor-and-index ,the-funcoid)
       ,@(mapcar (lambda (funcoid)
		   `(render-headline ,funcoid))
	   (when (consp |funcoid(s)|) (cdr |funcoid(s)|)))
       (render-docstring ,the-funcoid)
       (@table ()
	 (render-definition-core ,the-funcoid ,extract)
	 ,@body))))

(defmacro render-method
    (|method(s)| extract generic-source &aux (the-method (gensym "method")))
  "Render METHOD(S) definition in EXTRACT.
GENERIC-SOURCE is the source of the generic function. METHOD(S) sources are
not advertised if they are the same as GENERIC-SOURCE."
  `(let ((,the-method ,(if (consp |method(s)|)
			   (car |method(s)|)
			   |method(s)|)))
     (@defmethod (string-downcase (name ,the-method))
	 (lambda-list ,the-method)
	 (specializers ,the-method)
	 (qualifiers ,the-method)
       (anchor-and-index ,the-method)
       ,@(mapcar (lambda (method)
		   (let ((the-method (gensym "method")))
		     `(let ((,the-method ,method))
			(@defmethodx
			    (string-downcase (name ,the-method))
			    (lambda-list ,the-method)
			    (specializers ,the-method)
			    (qualifiers ,the-method))
			(anchor-and-index ,the-method))))
		 (when (consp |method(s)|) (cdr |method(s)|)))
       (render-docstring ,the-method)
       (unless (equal (source ,the-method) ,generic-source)
	 (@table ()
	   (render-source ,the-method ,extract))))))

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
  (@defslot (string-downcase (name slot))
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
  (when-let ((slots (slot-definitions classoid)))
    (@tableitem "Direct slots"
      (dolist (slot slots)
	(render-slot slot)))))

(defmacro render-combination (kind combination extract &body body)
  "Render method COMBINATION's definition of KIND in EXTRACT."
  (let ((the-combination (gensym "combination")))
    `(let ((,the-combination ,combination))
       (@defcombination (string-downcase (name ,the-combination)) ,kind
	 (anchor-and-index ,the-combination)
	 (render-docstring ,the-combination)
	 (@table ()
	   (render-definition-core ,the-combination ,extract)
	   ,@body)))))

(defmacro render-classoid (kind classoid extract &body body)
  "Render CLASSOID's definition of KIND in EXTRACT."
  (let ((|@defform| (intern (concatenate 'string "@DEF" (symbol-name kind))
			    :net.didierverna.declt))
	(the-classoid (gensym "classoid")))
    `(let ((,the-classoid ,classoid))
       (,|@defform| (string-downcase (name ,the-classoid))
	 (anchor-and-index ,the-classoid)
	 (render-docstring ,the-classoid)
	 (@table ()
	   (render-definition-core ,the-classoid ,extract)
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
  (when-let ((initargs (sb-mop:class-direct-default-initargs
			(find-class (definition-symbol classoid)))))
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
  (:method ((function function-definition))
    "Return #'@DEFUNX."
    #'@defunx)
  (:method ((generic generic-definition))
    "Return #'@DEFGENERICX."
    #'@defgenericx)
  (:method ((expander setf-expander-definition))
    "Return #'@DEFSETFX."
    #'@defsetfx))

(defun render-headline (definition)
  "Render a headline for DEFINITION. Also anchor and index it."
  (funcall (headline-function definition)
	   (string-downcase (name definition)) (lambda-list definition))
  (anchor-and-index definition))



;; ==========================================================================
;; Documentation Protocols
;; ==========================================================================

(defmethod anchor-name (definition)
  "Return DEFINITION's qualified symbol name.
This is the default method for most definitions."
  (format nil "~A::~A"
    (definition-package-name definition)
    (name definition)))

(defmethod anchor-name ((method method-definition))
  "Return METHOD's qualified symbol name, specializers and qualifiers ."
  (format nil "~A::~A~{ ~A~^~}~{ ~A~^~}"
    (definition-package-name method)
    (name method)
    (mapcar (lambda (specializer) (pretty-specializer specializer t))
	    (specializers method))
    (qualifiers method)))

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

(defmethod index ((function function-definition))
  "Render FUNCTION's indexing command."
  (format t "@functionsubindex{~(~A~)}@c~%" (escape function)))

(defmethod index ((method method-definition))
  "Render METHOD's indexing command."
  (format t "@methodsubindex{~(~A~)}@c~%" (escape method)))

(defmethod index ((generic generic-definition))
  "Render GENERIC's indexing command."
  (format t "@genericsubindex{~(~A~)}@c~%" (escape generic)))

(defmethod index ((expander setf-expander-definition))
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

(defmethod reference ((definition symbol-definition))
  "Render DEFINITION's reference."
  (cond ((foreignp definition)
	 (format t "@t{~(~A}~) (~A)~%"
	   (escape definition)
	   (type-name definition)))
	(t
	 (@ref (anchor-name definition) definition)
	 (format t " (~A)~%" (type-name definition)))))

(defmethod document ((constant constant-definition) extract &key)
  "Render CONSTANT's documentation in EXTRACT."
  (render-varoid :constant constant extract))

(defmethod document ((special special-definition) extract &key)
  "Render SPECIAL variable's documentation in EXTRACT."
  (render-varoid :special special extract))

(defmethod document ((symbol-macro symbol-macro-definition) extract &key)
  "Render SYMBOL-MACRO's documentation in EXTRACT."
    (render-varoid :symbolmacro symbol-macro extract
      (@tableitem "Expansion"
	(format t "@t{~(~A~)}~%"
	  (escape
	   (format nil "~S"
		   (macroexpand-1 (definition-symbol symbol-macro))))))))

(defmethod document
    ((macro macro-definition) extract
     &key
     &aux (access-expander (access-expander-definition macro))
	  (update-expander (update-expander-definition macro))
	  (merge (and access-expander
		      (functionp (update access-expander))
		      (equal (source macro) (source access-expander))
		      (equal (docstring macro) (docstring access-expander)))))
  "Render MACRO's documentation in EXTRACT."
  (cond (merge
	 (render-funcoid :macro (macro access-expander) extract
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))))
	(t
	 (render-funcoid :macro macro extract
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))
	   (when access-expander
	     (@tableitem "Setf Expander"
	       (reference access-expander))))
	 (when access-expander
	   (document access-expander extract)))))

(defmethod document ((compiler-macro compiler-macro-definition) extract &key)
  "Render COMPILER-MACRO's documentation in EXTRACT."
  (render-funcoid :compilermacro compiler-macro extract))

(defmethod document ((function function-definition) extract &key)
  "Render FUNCTION's documentation in EXTRACT."
  (render-funcoid :un function extract
    (when-let ((expander (update-expander-definition function)))
      (@tableitem "Setf Expander"
	(reference expander)))))

(defmethod document ((writer writer-definition) extract &key)
  "Render WRITER's documentation in EXTRACT."
  (render-funcoid :un writer extract
    (when-let ((reader (reader-definition writer)))
      (@tableitem "Reader"
	(reference reader)))))

(defmethod document
    ((accessor accessor-definition) extract
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
  "Render ACCESSOR's documentation in EXTRACT."
  (cond ((and merge-writer merge-expander)
	 (render-funcoid :un (accessor access-expander writer) extract
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))))
	(merge-setters
	 (render-funcoid :un accessor extract
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))
	   (@tableitem "Setf Expander" (reference access-expander))
	   (@tableitem "Writer" (reference writer)))
	 (render-funcoid :setf (access-expander writer) extract
	   (@tableitem "Reader"
	     (reference (access-definition access-expander)))))
	(merge-expander
	 (render-funcoid :un (accessor access-expander) extract
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))
	   (when writer
	     (@tableitem "Writer"
	       (reference writer))))
	 (when (writer-definition-p writer)
	   (document writer extract)))
	(merge-writer
	 (render-funcoid :un (accessor writer) extract
	   (when update-expander
	     (@tableitem "Setf Expander"
	       (reference update-expander)))
	   (when access-expander
	     (@tableitem "Setf Expander"
	       (reference access-expander))))
	 (when access-expander
	   (document access-expander extract)))
	(t
	 (render-funcoid :un accessor extract
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
	   (document access-expander extract))
	 (when (writer-definition-p writer)
	   (document writer extract)))))

(defmethod document ((method method-definition) extract &key generic-source)
  "Render METHOD's documentation in EXTRACT.
GENERIC-SOURCE is the source of METHOD's generic function."
  (render-method method extract generic-source))

(defmethod document ((method accessor-method-definition) extract
		     &key (document-writers t) generic-source)
  "Render accessor METHOD's documentation in EXTRACT."
  (cond ((and (equal (source method)
		     (source (writer-definition method)))
	      (equal (docstring method)
		     (docstring (writer-definition method)))
	      document-writers)
	 (render-method (method (writer-definition method))
			extract
			generic-source))
	(t
	 (call-next-method)
	 (when document-writers
	   ;; #### NOTE: if DOCUMENT-WRITERS, it means that we're merging the
	   ;; defintions for the reader and the writer, and hence the generic
	   ;; sources are the same. It's thus ok to use GENERIC-SOURCE here.
	   (document (writer-definition method) extract
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
      (when-let ((options (mapcar (lambda (option)
				    (escape (format nil "~(~S~)" option)))
				  (sb-pcl::method-combination-options
				   (sb-mop:generic-function-method-combination
				    (generic generic))))))
	(format t "@b{Options:} @t{~A}~{, @t{~A}~}"
	  (first options)
	  (rest options))))))

(defmethod document ((generic generic-definition) extract &key)
  "Render GENERIC's documentation in EXTRACT."
  (render-funcoid :generic generic extract
    (when-let ((expander (update-expander-definition generic)))
      (@tableitem "Setf Expander"
	(reference expander)))
    (render-method-combination generic)
    (when-let ((methods (method-definitions generic)))
      (@tableitem "Methods"
	(dolist (method methods)
	  (document method extract :generic-source (source generic)))))))

(defmethod document
    ((writer generic-writer-definition) extract &key additional-methods)
  "Render generic WRITER's documentation in EXTRACT."
  (render-funcoid :generic writer extract
    (when-let ((reader (reader-definition writer)))
      (@tableitem "Reader"
	(reference reader)))
    (render-method-combination writer)
    (when-let ((methods (append additional-methods
				(method-definitions writer))))
      (@tableitem "Methods"
	(dolist (method methods)
	  (document method extract :generic-source (source writer)))))))

(defmethod document
    ((accessor generic-accessor-definition) extract
     &key
     &aux (access-expander (access-expander-definition accessor))
	  (update-expander (update-expander-definition accessor))
	  (writer (writer-definition accessor)))
  "Render generic ACCESSOR's documentation in EXTRACT."
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
	 (render-funcoid :generic (accessor writer) extract
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
		   (document method extract
		     :generic-source (source accessor)))
		 (dolist (method writer-methods)
		   (document method extract
		     :generic-source (source accessor)))))))
	 (when access-expander
	   (document access-expander extract)))
	(t
	 (render-funcoid :generic accessor extract
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
	   (when-let ((methods (method-definitions accessor)))
	     (@tableitem "Methods"
		(dolist (method methods)
		  (document method extract
		    :document-writers nil
		    :generic-source (source accessor))))))
	 (when access-expander
	   (document access-expander extract))
	 (when (generic-writer-definition-p writer)
	   (document writer extract
		     :additional-methods
		     (mapcar #'writer-definition
			     (remove-if-not
			      #'accessor-method-definition-p
			      (method-definitions accessor))))))))

(defmethod document ((expander setf-expander-definition) extract &key)
  "Render setf EXPANDER's documentation in EXTRACT."
  (render-funcoid :setf expander extract
    (@tableitem "Reader"
      (reference (access-definition expander)))
    (when (symbol-definition-p (update expander))
      (@tableitem "Writer"
	(reference (update expander))))))

;; #### NOTE: no DOCUMENT method for SLOT-DEFINITION

;; #### PORTME.
(defmethod document ((combination short-combination-definition) extract &key)
  "Render short method COMBINATION's documentation in EXTRACT."
  (render-combination :short combination extract
    (@tableitem "Operator"
      (reference (operator-definition combination)))
    (@tableitem "Indentity with one argument"
      (format t "@t{~(~A~)}"
	(sb-pcl::short-combination-identity-with-one-argument
	 (combination combination))))
    (render-references (users combination) "Users")))

(defmethod document ((combination long-combination-definition) extract &key)
  "Render long method COMBINATION's documentation in EXTRACT."
  (render-combination :long combination extract
    (render-references (users combination) "Users")))

(defmethod document ((condition condition-definition) extract &key)
  "Render CONDITION's documentation in EXTRACT."
  (render-classoid :cond condition extract
    (render-initargs condition)))

(defmethod document ((structure structure-definition) extract &key)
  "Render STRUCTURE's documentation in EXTRACT."
  (render-classoid :struct structure extract))

(defmethod document ((class class-definition) extract &key)
  "Render CLASS's documentation in EXTRACT."
  (render-classoid :class class extract
    (render-initargs class)))

(defmethod document ((type type-definition) extract &key)
  "Render TYPE's documentation in EXTRACT."
  (@deftype ((string-downcase (name type)) (lambda-list type))
    (anchor-and-index type)
    (render-docstring type)
    (@table ()
      (render-definition-core type extract))))



;; ==========================================================================
;; Definition Nodes
;; ==========================================================================

(defun add-category-node (parent extract status category definitions)
  "Add the STATUS CATEGORY node to PARENT for DEFINITIONS in EXTRACT."
  (add-child parent
    (make-node :name (format nil "~@(~A ~A~)" status category)
	       :section-name (format nil "~@(~A~)" category)
	       :before-menu-contents
	       (render-to-string
		 (dolist (definition (sort definitions #'string-lessp
					   :key #'definition-symbol))
		   (document definition extract))))))

(defun add-categories-node (parent extract status definitions)
  "Add the STATUS DEFINITIONS categories nodes to PARENT in EXTRACT."
  (dolist (category *categories*)
    (when-let ((category-definitions
		(category-definitions (first category) definitions)))
      (add-category-node parent extract status (second category)
			 category-definitions))))

(defun add-status-definitions-node (parent extract status definitions)
  "Add the STATUS DEFINITIONS node to PARENT in EXTRACT."
  (let ((node (add-child parent
		(make-node :name (format nil "~@(~A~) definitions" status)))))
    (add-categories-node node extract status definitions)))

(defun add-definitions-node
    (parent extract
     &aux (external-definitions (external-definitions extract))
       (external-definitions-number
	(definitions-pool-size external-definitions))
       (internal-definitions (internal-definitions extract))
       (internal-definitions-number
	(definitions-pool-size internal-definitions)))
  "Add the definitions node to PARENT in EXTRACT."
  (unless (zerop (+ external-definitions-number internal-definitions-number))
    (let ((definitions-node
	    (add-child parent
	      (make-node :name "Definitions"
			 :synopsis "The symbols documentation"
			 :before-menu-contents(format nil "~
Definitions are sorted by export status, category, package, and then by
lexicographic order.")))))
      (unless (zerop external-definitions-number)
	(add-status-definitions-node definitions-node extract "exported"
				     external-definitions))
      (unless (zerop internal-definitions-number)
	(add-status-definitions-node definitions-node extract "internal"
				     internal-definitions)))))

;;; symbol.lisp ends here
