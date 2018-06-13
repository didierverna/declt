;;; declt.lisp --- Entry points

;; Copyright (C) 2010-2013, 2016 Didier Verna

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


;; #### FIXME: when we make this a customizable variable instead of a
;; constant, we need to decide on whether this should be raw Texinfo contents,
;; or whether to escape the notices before using them.
(defparameter *licenses*
  '((:mit
     "The MIT License"
     "Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THIS SOFTWARE IS PROVIDED \"AS IS\" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.")
    (:bsd
     "The BSD License"
     "Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THIS SOFTWARE IS PROVIDED \"AS IS\" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.")
    (:gpl
     "The GNU GPL License"
     "This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.")
      (:lgpl
       "The GNU LGPL License"
       "This program is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.")))

(defun render-header (library tagline version contact-names contact-emails
		      copyright license
		      texi-name info-file declt-notice
		      current-time-string)
  "Render the header of the Texinfo file."
  (format t "\\input texinfo~2%@c ~A --- Reference manual~2%" texi-name)

  (when copyright
    (mapc (lambda (name)
	    (format t "@c Copyright (C) ~A ~A~%" copyright name))
      ;; #### NOTE: we already removed the duplicates in the original contact
      ;; list, but there may still be duplicates in the names, for instance if
      ;; somebody used his name several times, with a different email
      ;; address.
      (remove-duplicates contact-names :from-end t :test #'string=))
    (terpri))

  (format t "@c This file is part of ~A.~2%" library)

  (when license
    (with-input-from-string (str (caddr license))
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
    info-file (escape library))

  (format t "~
@c ====================================================================
@c Settings
@c ====================================================================
@setchapternewpage odd
@documentdescription
The ~A Reference Manual~@[, version ~A~].
@end documentdescription~4%"
    (escape library) version)

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
@cindex @t{\\name\\}
@cindex Module, @t{\\name\\}
@end macro

@c Lisp files
@macro lispfileindex{name}
@cindex @t{\\name\\}
@cindex Lisp File, @t{\\name\\}
@cindex File, Lisp, @t{\\name\\}
@end macro

@c C files
@macro cfileindex{name}
@cindex @t{\\name\\}
@cindex C File, @t{\\name\\}
@cindex File, C, @t{\\name\\}
@end macro

@c Java files
@macro javafileindex{name}
@cindex @t{\\name\\}
@cindex Java File, @t{\\name\\}
@cindex File, Java, @t{\\name\\}
@end macro

@c Other files
@macro otherfileindex{name}
@cindex @t{\\name\\}
@cindex Other File, @t{\\name\\}
@cindex File, other, @t{\\name\\}
@end macro

@c Doc files
@macro docfileindex{name}
@cindex @t{\\name\\}
@cindex Doc File, @t{\\name\\}
@cindex File, doc, @t{\\name\\}
@end macro

@c HTML files
@macro htmlfileindex{name}
@cindex @t{\\name\\}
@cindex HTML File, @t{\\name\\}
@cindex File, html, @t{\\name\\}
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
@macro setfexpandersubindex{name}
@findex @r{Setf Expander, }\\name\\
@end macro

@c Method Combinations
@macro shortcombinationsubindex{name}
@tpindex @r{Short Method Combination, }\\name\\
@tpindex @r{Method Combination, Short, }\\name\\
@end macro

@macro longcombinationsubindex{name}
@tpindex @r{Long Method Combination, }\\name\\
@tpindex @r{Method Combination, Long, }\\name\\
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
    library info-file library)

  (when license
    (format t "~
@c ====================================================================
@c Copying
@c ====================================================================
@copying
@quotation~%")

    (when copyright
      (mapc (lambda (name)
	      (format t "Copyright @copyright{} ~A ~A~%"
		(escape copyright) (escape name)))
	;; #### NOTE: we already removed the duplicates in the original
	;; contact list, but there may still be duplicates in the names, for
	;; instance if somebody used his name several times, with a different
	;; email address.
	(remove-duplicates contact-names :from-end t :test #'string=))
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
    (escape library)
    (if (or tagline version)
	(format nil "@subtitle ~@[~A~]~:[~;, ~]~@[version ~A~]~%"
	  tagline (and tagline version) version)
	""))
  (mapc (lambda (name email)
	  (format t "@author ~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
	    (escape name) email (escape email)))
    contact-names contact-emails)
  (terpri)
  (when (or declt-notice license)
    (format t "@page~%"))

  (when declt-notice
    (format t "~
@quotation
This manual was generated automatically by Declt ~A on ~A.
@end quotation~%"
    (escape (version declt-notice))
    (escape current-time-string)))

  (when license
    (format t "@vskip 0pt plus 1filll~%@insertcopying~%"))

  (format t "@end titlepage~4%")

  (format t "~
@c ====================================================================
@c Table of Contents
@c ====================================================================
@contents~%"))

(defun add-packages (context)
  "Add all package definitions to CONTEXT."
  (setf (context-packages context)
	;; #### NOTE: several subsystems may share the same packages (because
	;; they would share files defining them) so we need to filter
	;; potential duplicates out.
	(remove-duplicates
	 (mapcan #'system-packages (context-systems context)))))

(defun add-external-definitions (context)
  "Add all external definitions to CONTEXT."
  (dolist (symbol (mapcan #'system-external-symbols (context-systems context)))
    (add-symbol-definitions symbol (context-external-definitions context))))

(defun add-internal-definitions (context)
  "Add all internal definitions to CONTEXT."
  (dolist (symbol (mapcan #'system-internal-symbols (context-systems context)))
    (add-symbol-definitions symbol (context-internal-definitions context))))

(defun add-definitions (context)
  "Add all definitions to CONTEXT."
  (add-external-definitions context)
  (add-internal-definitions context)
  (finalize-definitions
   (context-external-definitions context)
   (context-internal-definitions context)))

(defun declt (system-name
	      &key (library (if (stringp system-name)
				system-name
			      (string-downcase (symbol-name system-name))))
		   (tagline nil taglinep)
		   (version nil versionp)
		   (contact nil contactp)

		   copyright
		   license
		   introduction
		   conclusion

		   (texi-file (format nil "~A.texi" library))
		   (info-file (pathname-name texi-file))
		   hyperlinks
		   (declt-notice :long)

	      &aux (system (find-system system-name))
		   (texi-name (file-namestring texi-file))
		   (current-time-string (current-time-string))
		   contact-names contact-emails)
  "Generate a reference manual in Texinfo format for ASDF SYSTEM-NAME.
SYSTEM-NAME is an ASDF system designator.

The following keyword arguments are available.
- LIBRARY: name of the library being documented (defaults to the system
  name).
- TAGLINE: small text to be used as the manual's subtitle (defaults to the
  system's long name or description).
- VERSION: version information (defaults to the system version).
- CONTACT: contact information (defaults to the system's maintainer(s) and
  author(s)). If the library doesn't provide that information, \"John Doe\" is
  used. If you provide it by hand, use an author string or a list of such. An
  author string contains a name, optionally followed by an <email@ddress>.

- COPYRIGHT: copyright years information (defaults to the current
  year). Possible values are NIL or any string.
- LICENSE: license information (defaults to NIL). Possible other values are
  :mit, :bsd, :gpl, and :lgpl).
- INTRODUCTION: contents for an optional introduction chapter.
- CONCLUSION: contents for an optional conclusion chapter.

- TEXI-FILE: path to the Texinfo file (defaults to LIBRARY.texi).
- INFO-FILE: info file basename sans extension (defaults to the relevant part
- of TEXI-FILE).
- HYPERLINKS: whether to create hyperlinks to files or directories in the
  reference manual. Note that those links being specific to the machine on
  which the manual was generated, it is preferable to keep it to NIL for
  creating reference manuals meant to be put online.
- DECLT-NOTICE: small paragraph about automatic manual generation by Declt
  (defaults to :long). Possible other values are nil and :short.

Both the INTRODUCTION and the CONCLUSION may contain Texinfo directives (no
post-processing will occur). All other textual material is considered raw text
and will be properly escaped for Texinfo."

  ;; First load the target system. If this fails, there's no point in working
  ;; hard on the rest.
  (asdf:load-system system-name)

  ;; Next, post-process some parameters.
  ;; #### NOTE: some Texinfo contents is escaped once and for all below, but
  ;; not everything. The exceptions are the pieces that we also use in
  ;; comments (in which case we don't want to escape them).
  (unless taglinep
    (setq tagline (or (system-long-name system)
		      (component-description system))))
  (when (and tagline (zerop (length tagline)))
    (setq tagline nil))
  (when tagline
    (when (char= (aref tagline (1- (length tagline))) #\.)
      (setq tagline (subseq tagline 0 (1- (length tagline)))))
    (setq tagline (escape tagline)))
  (unless versionp
    (setq version (component-version system)))
  (when version
    (setq version (escape version)))
  (unless contact
    (setq contact (system-author system))
    (when (stringp contact) (setq contact (list contact)))
    (cond ((stringp (system-maintainer system))
	   (push (system-maintainer system) contact))
	  ((consp (system-maintainer system))
	   (setq contact (append (system-maintainer system) contact))))
    (unless contact (setq contact (list "John Doe"))))
  (setq contact (remove-duplicates contact :from-end t :test #'string=))
  (multiple-value-bind (names emails) (|parse-contact(s)| contact)
    (setq contact-names names
	  contact-emails emails))
  (when (and (= (length contact-names) 1)
	     (not contactp)
	     (null (car contact-emails))
	     (system-mailto system))
    (setq contact-emails (list (system-mailto system))))

  (setq copyright
	(or copyright
	    (multiple-value-bind (second minute hour date month year)
		(get-decoded-time)
	      (declare (ignore second minute hour date month))
	      (format nil "~A" year))))
  (when license
    (setq license (assoc license *licenses*))
    (unless license
      (error "License not found.")))

  (setq info-file (escape info-file))

  ;; Construct the nodes hierarchy.
  (with-standard-io-syntax
    (let ((context (make-context
		    :systems
		    (cons system
			  (remove-duplicates
			   (subsystems system (system-directory system))))
		    :external-definitions (make-definitions-pool)
		    :internal-definitions (make-definitions-pool)
		    :hyperlinksp hyperlinks))
	  (top-node
	    (make-node :name "Top"
		       :section-name (format nil "The ~A Reference Manual"
				       (escape library))
		       :section-type :unnumbered
		       :before-menu-contents (format nil "~
This is the ~A Reference Manual~@[, version ~A~],
generated automatically by Declt version ~A
on ~A."
					       (escape library)
					       version
					       (escape (version :long))
					       (escape current-time-string))
		       :after-menu-contents (when license "@insertcopying"))))
      (add-packages context)
      (add-definitions context)
      (when license
	(add-child top-node
	  (make-node :name "Copying"
		     :synopsis (cadr license)
		     :section-type :unnumbered
		     :before-menu-contents (format nil "@quotation~@
						    ~A~@
						    @end quotation"
					     (escape (caddr license))))))
      (when introduction
	(add-child top-node
	  (make-node :name "Introduction"
		     :synopsis (format nil "What ~A is all about" library)
		     :before-menu-contents introduction)))
      (add-systems-node     top-node context)
      (add-modules-node     top-node context)
      (add-files-node       top-node context)
      (add-packages-node    top-node context)
      (add-definitions-node top-node context)
      (when conclusion
	(add-child top-node
	  (make-node :name "Conclusion"
		     :synopsis "Time to go"
		     :before-menu-contents conclusion)))
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
      (with-open-file (*standard-output* texi-file
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create
		       :external-format :utf8)
	(render-header library tagline version contact-names contact-emails
		       copyright license
		       texi-name info-file declt-notice
		       current-time-string)
	(render-top-node top-node)
	(format t "~%@bye~%~%@c ~A ends here~%" texi-name))))
  (values))


;;; declt.lisp ends here
