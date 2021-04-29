;;; declt.lisp --- Entry points

;; Copyright (C) 2010-2013, 2015-2021 Didier Verna

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


#i(select-keys 1)
(defun select-keys (keys &rest selected)
  "Return a new property list from KEYS with only SELECTED ones."
  (loop :for key :in keys :by #'cddr
	:for val :in (cdr keys) :by #'cddr
	:when (member key selected)
	  :nconc (list key val)))

(defun render-header
    (extract file-name info-name declt-notice current-time-string)
  "Render the header of the Texinfo file."
  (format t "\\input texinfo~2%@c ~A.texi --- Reference manual~2%" file-name)

  (when (copyright-years extract)
    (mapc (lambda (name)
	    (format t "@c Copyright (C) ~A ~A~%"
	      (copyright-years extract) name))
      ;; #### NOTE: we already removed the duplicates in the original contact
      ;; list, but there may still be duplicates in the names, for instance if
      ;; somebody used his name several times, with a different email
      ;; address.
      (remove-duplicates (remove-if #'null (contact-names extract))
	:from-end t :test #'string=))
    (terpri))

  (format t "@c This file is part of ~A.~2%" (library-name extract))

  (when (license extract)
    (with-input-from-string (str (caddr (license extract)))
      (loop :for line := (read-line str nil str)
	    :until (eq line str)
	    :do (format t "@c ~A~%" line))))

  (terpri)
  (terpri)

  (format t "@c Commentary:~2%~
	     @c Generated automatically by Declt version ~A~%~
	     @c on ~A.~3%"
    (version :long) current-time-string)

  (format t "~
@c ====================================================================
@c Header
@c ====================================================================
@c %**start of header
@setfilename ~A.info
@settitle The ~A Reference Manual
@afourpaper
@documentencoding UTF-8
@c %**end of header~4%"
    (escape info-name) (escape (library-name extract)))

  (format t "~
@c ====================================================================
@c Format Specific Tweaks
@c ====================================================================
@tex
%% Declt uses several Unicode characters to \"reveal\" blanks. This
%% works fine in HTML or Info output, but TeX will have problems with
%% these. The code below translates those characters to something that
%% TeX can handle.

%% U+23B5 (Bottom Square Bracket), used to reveal white spaces, is
%% translated to its Computer Modern teletype version.
\\DeclareUnicodeCharacter{23B5}{{\\tt\\char'040}}

%% U+21B5 (Downwards Arrow With Corner Leftwards), used to reveal
%% carriage returns, is translated to \\hookleftarrow in math mode.
\\DeclareUnicodeCharacter{21B5}{\\ensuremath\\hookleftarrow}

%% U+21E5 (Rightwards Arrow To Bar), used to reveal tabs, is
%% translated to something that looks similar, based on a rightarrow
%% and a vertical bar from the math extension font.
\\DeclareUnicodeCharacter{21E5}{%
  \\ensuremath{\\rightarrow\\kern-.5em\\mathchar\\\"130C}}


%% Declt uses several Unicode characters to replace \"fragile\" ones in
%% anchor names and references. These characters are chosen to resemble
%% the original ones, without interfering with Info syntax. In TeX
%% however, we can switch them back to the original versions, because
%% cross-references are done differently. In theory, I think we could do
%% something similar for HTML output (again, only the Info syntax poses
%% problems), but I don't know how to do something similar to what's
%% below.

%% U+2024 (One Dot Leader) replaces periods.
\\DeclareUnicodeCharacter{2024}{.}

%% U+2236 (Ratio) replaces colons.
\\DeclareUnicodeCharacter{2236}{:}

%% U+2768 (Medium Left Parenthesis Ornament) replaces left parenthesis.
\\DeclareUnicodeCharacter{2768}{(}

%% U+2769 (Medium Right Parenthesis Ornament) replaces right parenthesis.
\\DeclareUnicodeCharacter{2769}{)}

%% U+214B (Turned Ampersand) replaces ampersands.
\\DeclareUnicodeCharacter{214B}{&}

%% U+2216 (Set Minus) replaces backslashes.
\\DeclareUnicodeCharacter{2216}{\\char\"5C}

%% The following ones are already defined in texinfo.tex so we have nothing
%% more to do:
%% U+201A (Single Low-9 Quotation Mark) replaces commas.
%% U+2205 (Empty Set) replaces empty symbol names.

@end tex~4%")

  (format t "~
@c ====================================================================
@c Settings
@c ====================================================================
@setchapternewpage odd
@documentdescription
The ~A Reference Manual~@[, version ~A~].
@end documentdescription~4%"
    (escape (library-name extract))
    (escape (library-version extract)))

  (format t "~
@c ====================================================================
@c New Commands
@c ====================================================================

@c ---------------
@c Indexing macros
@c ---------------

@c Packages
@macro packageindex{name}
@tpindex \\name\\
@tpindex @r{Package, }\\name\\
@end macro

@c Systems
@macro systemindex{name}
@tpindex \\name\\
@tpindex @r{System, }\\name\\
@end macro

@c Modules
@macro moduleindex{name}
@tpindex @t{\\name\\}
@tpindex Module, @t{\\name\\}
@end macro

@c Files
@macro fileindex{name}
@tpindex @t{\\name\\}
@tpindex File, @t{\\name\\}
@end macro

@c The following macros are meant to be used within @defxxx environments.
@c Texinfo performs half the indexing job and we do the other half.

@c Constants
@macro constantsubindex{name}
@vindex @r{Constant, }\\name\\
@end macro

@c Special variables
@macro specialsubindex{name}
@vindex @r{Special Variable, }\\name\\
@end macro

@c Symbol macros
@macro symbolmacrosubindex{name}
@vindex @r{Symbol Macro, }\\name\\
@end macro

@c Slots
@macro slotsubindex{name}
@vindex @r{Slot, }\\name\\
@end macro

@c Macros
@macro macrosubindex{name}
@findex @r{Macro, }\\name\\
@end macro

@c Compiler Macros
@macro compilermacrosubindex{name}
@findex @r{Compiler Macro, }\\name\\
@end macro

@c Functions
@macro functionsubindex{name}
@findex @r{Function, }\\name\\
@end macro

@c Methods
@macro methodsubindex{name}
@findex @r{Method, }\\name\\
@end macro

@c Generic Functions
@macro genericsubindex{name}
@findex @r{Generic Function, }\\name\\
@end macro

@c Setf Expanders
@macro expandersubindex{name}
@findex @r{Setf Expander, }\\name\\
@end macro

@c Method Combinations
@macro combinationsubindex{name}
@findex @r{Method Combination, }\\name\\
@end macro

@c Conditions
@macro conditionsubindex{name}
@tpindex @r{Condition, }\\name\\
@end macro

@c Structures
@macro structuresubindex{name}
@tpindex @r{Structure, }\\name\\
@end macro

@c Types
@macro typesubindex{name}
@tpindex @r{Type, }\\name\\
@end macro

@c Classes
@macro classsubindex{name}
@tpindex @r{Class, }\\name\\
@end macro~4%")

  (format t "~
@c ====================================================================
@c Info Category and Directory
@c ====================================================================
@dircategory Common Lisp
@direntry
* ~A Reference: (~A). The ~A Reference Manual.
@end direntry~4%"
    (escape (library-name extract))
    (escape info-name)
    (escape (library-name extract)))

  (when (license extract)
    (format t "~
@c ====================================================================
@c Copying
@c ====================================================================
@copying
@quotation~%")

    (when (copyright-years extract)
      (mapc (lambda (name)
	      (format t "Copyright @copyright{} ~A ~A~%"
		(escape (copyright-years extract))
		(escape name)))
	;; #### NOTE: we already removed the duplicates in the original
	;; contact list, but there may still be duplicates in the names, for
	;; instance if somebody used his name several times, with a different
	;; email address.
	(remove-duplicates (remove-if #'null (contact-names extract))
	  :from-end t :test #'string=))
      (terpri))
    (format t "~
Permission is granted to make and distribute verbatim copies of this
manual provided the copyright notice and this permission notice are
preserved on all copies.

@ignore
Permission is granted to process this file through TeX and print the
results, provided the printed document carries a copying permission
notice identical to this one except for the removal of this paragraph
(this paragraph not being relevant to the printed manual).

@end ignore
Permission is granted to copy and distribute modified versions of this
manual under the conditions for verbatim copying, provided also that the
section entitled ``Copying'' is included exactly as in the original.

Permission is granted to copy and distribute translations of this manual
into another language, under the above conditions for modified versions,
except that this permission notice may be translated as well.
@end quotation
@end copying~4%"))

  (format t "~
@c ====================================================================
@c Title Page
@c ====================================================================
@titlepage
@title The ~A Reference Manual
~A~%"
    (escape (library-name extract))
    (if (or (tagline extract) (library-version extract))
	(format nil "@subtitle ~@[~A~]~:[~;, ~]~@[version ~A~]~%"
	  (escape (tagline extract))
	  (and (tagline extract) (library-version extract))
	  (escape (library-version extract)))
	""))
  (mapc (lambda (name email)
	  (format t "@author ~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
	    (escape name) email (escape email)))
    (contact-names extract) (contact-emails extract))
  (terpri)
  (when (or declt-notice (license extract))
    (format t "@page~%"))

  (when declt-notice
    (format t "~
@quotation
This manual was generated automatically by Declt ~A~@[ on ~A~].
@end quotation~%"
    (escape (version declt-notice))
    (when (eq declt-notice :long) (escape current-time-string))))

  (when (license extract)
    (format t "@vskip 0pt plus 1filll~%@insertcopying~%"))

  (format t "@end titlepage~4%")

  (format t "~
@c ====================================================================
@c Table of Contents
@c ====================================================================
@contents~%"))

(defun declt (system-name
	      &rest keys
	      &key library-name tagline library-version contact
		   copyright-years license
		   introduction conclusion

		   locations

		   (output-directory #p"./")
		   (file-name (if (stringp system-name)
				system-name
				(string-downcase system-name)))
		   (info-name file-name)
		   (declt-notice :long)

	      &aux (current-time-string (current-time-string))
		   (extract (apply #'extract system-name
				   (select-keys keys
				     :library-name :tagline :library-version
				     :contact :copyright-years :license
				     :introduction :conclusion)))
		   (context (apply #'make-context
			      (select-keys keys :locations))))
  "Generate a reference manual for ASDF SYSTEM-NAME.
The reference manual is currently generated in Texinfo format.

For a description of SYSTEM-NAME, LIBRARY-NAME, TAGLINE, LIBRARY-VERSION,
CONTACT, COPYRIGHT-YEARS, LICENSE, INTRODUCTION, and CONCLUSION, see
`extract'.

For a description of LOCATIONS, see `make-context'.

The following keyword parameters are also available.
- OUTPUT-DIRECTORY: output directory for the generated reference manual.
  Defaults to the current directory.
- FILE-NAME: base name for the generated reference manual, sans extension.
  Defaults to the system name.
- INFO-NAME: base name for the subsequent Info file, sans extension (this
  name appears in the Texinfo file). Defaults to FILE-NAME.
- DECLT-NOTICE: small credit paragraph to Declt, or NIL. Defaults to
  :long. Also accepts :short."
  (declare (ignore library-name tagline library-version contact
		   copyright-years license
		   introduction conclusion

		   locations))

  ;; Construct the nodes hierarchy.
  (with-standard-io-syntax
    (let ((*print-readably* nil)
	  (top-node
	    (make-node :name "Top"
		       :section-name
		       (format nil "The ~A Reference Manual"
			 (escape (library-name extract)))
		       :section-type :unnumbered
		       :before-menu-contents
		       (format nil "~

This is the ~A Reference Manual~@[, version ~A~]~@[,
generated automatically by Declt version ~A~@[
on ~A~]~]."
			 (escape (library-name extract))
			 (escape (library-version extract))
			 (when declt-notice
			   (escape
			    (version declt-notice)))
			 (when (eq declt-notice :long)
			   (escape current-time-string)))
		       :after-menu-contents
		       (when (license extract) "@insertcopying"))))
      (when (license extract)
	(add-child top-node
	  (make-node :name "Copying"
		     :synopsis (cadr (license extract))
		     :section-type :unnumbered
		     :before-menu-contents
		     (format nil "@quotation~@
				  ~A~@
				  @end quotation"
		       (escape (caddr (license extract)))))))
      (when (introduction extract)
	(add-child top-node
	  (make-node :name "Introduction"
		     :synopsis (format nil "What ~A is all about"
				 (escape (library-name extract)))
		     :before-menu-contents (introduction extract))))
      (add-systems-node     top-node extract context)
      (add-modules-node     top-node extract context)
      (add-files-node       top-node extract context)
      (add-packages-node    top-node extract context)
      (add-definitions-node top-node extract context)
      (when (conclusion extract)
	(add-child top-node
	  (make-node :name "Conclusion"
		     :synopsis "Time to go"
		     :before-menu-contents (conclusion extract))))
      (let ((indexes-node (add-child top-node
			    (make-node :name "Indexes"
				       :synopsis (format nil "~
Concepts, functions, variables and data types")
				       :section-type :appendix))))
	(add-child indexes-node
	  (make-node :name "Concept index"
		     :section-type :appendix
		     :section-name "Concepts"
		     :before-menu-contents "@printindex cp"
		     :after-menu-contents "@page"))
	(add-child indexes-node
	  (make-node :name "Function index"
		     :section-type :appendix
		     :section-name "Functions"
		     :before-menu-contents "@printindex fn"
		     :after-menu-contents "@page"))
	(add-child indexes-node
	  (make-node :name "Variable index"
		     :section-type :appendix
		     :section-name "Variables"
		     :before-menu-contents "@printindex vr"
		     :after-menu-contents "@page"))
	(add-child indexes-node
	  (make-node :name "Data type index"
		     :section-type :appendix
		     :section-name "Data types"
		     :before-menu-contents "@printindex tp")))
      (with-open-file (*standard-output*
		       (merge-pathnames (make-pathname :name file-name
						       :type "texi")
					output-directory)
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create
		       :external-format :utf8)
	(render-header extract file-name info-name declt-notice
		       current-time-string)
	(render-top-node top-node)
	(format t "~%@bye~%~%@c ~A.texi ends here~%" file-name))))
  (values))

;;; declt.lisp ends here
