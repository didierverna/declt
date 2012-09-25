;;; declt.lisp --- Entry points

;; Copyright (C) 2010, 2011 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

;; This file is part of Declt.

;; Declt is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Declt is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(in-package :com.dvlsoft.declt)
(in-readtable :com.dvlsoft.declt)


(define-constant +licenses+
    '((:bsd
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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.")))

(defun render-header (library-name texi-name info-file subtitle version
		      author email license declt-notice
		      copyright-date current-time-string)
  "Render the header of the Texinfo file."
  (format t "\\input texinfo~2%@c ~A --- Reference manual~2%" texi-name)

  (when license
    (format t "@c Copyright (C) ~A ~A~2%@c This file is part of ~A.~2%"
      copyright-date author library-name)
    (with-input-from-string (str (caddr license))
      (loop :for line := (read-line str nil :eof)
	    :until (eq line :eof)
	    :do (format t "@c ~A~%" line)))
    (terpri)
    (terpri))

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
@c %**end of header~4%"
    info-file library-name)

  (format t "~
@c ====================================================================
@c Settings
@c ====================================================================
@setchapternewpage odd
@setcontentsaftertitlepage
@documentdescription
The ~A Reference Manual~@[, version ~A~].
@end documentdescription~4%"
    library-name version)

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

@c Macros
@macro macrosubindex{name}
@findex @r{Macro, }\\name\\
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

@c Conditions
@macro conditionsubindex{name}
@tpindex @r{Condition, }\\name\\
@end macro

@c Structures
@macro structuresubindex{name}
@tpindex @r{Structure, }\\name\\
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
    library-name info-file library-name)

  (when license
    (format t "~
@c ====================================================================
@c Copying
@c ====================================================================
@copying
@quotation
Copyright @copyright{} ~A ~A

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
@end copying~4%"
    copyright-date author))

  (format t "~
@c ====================================================================
@c Title Page
@c ====================================================================
@titlepage
@title The ~A Reference Manual
~A~A~%"
    library-name
    (if (or subtitle version)
	(format nil "@subtitle ~@[~A~]~:[~;, ~]~@[version ~A~]~%"
	  subtitle (and subtitle version) version)
      "")
    (format nil "@author ~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
       author email email))

  (when (or declt-notice license)
    (format t "@page~%"))

  (when declt-notice
    (format t "~
@quotation
This manual was generated automatically by Declt ~A on ~A.
@end quotation~%"
    (version declt-notice)
    current-time-string))

  (when license
    (format t "@vskip 0pt plus 1filll~%@insertcopying~%"))

  (format t "@end titlepage~4%")

  (format t "~
@c ====================================================================
@c Table of Contents
@c ====================================================================
@contents~%"))

(defun declt (system-name
	      &key (library-name (string-downcase (symbol-name system-name)))
		   (texi-file (format nil "~A.texi" library-name))
		   (info-file (pathname-name texi-file))
		   introduction
		   (subtitle nil subtitlep)
		   (version nil versionp)
		   author
		   (email nil emailp)
		   license
		   (declt-notice :long)
		   copyright-date
		   conclusion
		   (link-files *link-files*)
	      &aux (system (find-system system-name))
		   (texi-name (escape (file-namestring texi-file)))
		   (current-time-string (escape (current-time-string))))
  "Generate a reference manual in Texinfo format for ASDF SYSTEM-NAME.
- LIBRARY-NAME defaults to SYSTEM-NAME.
- TEXI-FILE is the full path to the Texinfo file.
  Defaults to LIBRARY-NAME.texi.
- INFO-FILE is the info file basename sans extension.
  Defaults is built from TEXI-FILE.
- INTRODUCTION is a potential contents for an introduction chapter.
- SUBTITLE defaults to the system description.
- VERSION defaults to the system version.
- AUTHOR and EMAIL defaults are extracted from the system author.
- LICENSE defaults to nil (possible values are: :BSD and :GPL).
- DECLT-NOTICE is a small paragraph about automatic manual generatiopn by
  Declt. Possible values are nil, :short and :long (the default).
- COPYRIGHT-DATE defaults to the current year.
- CONCLUSION is a potential contents for a conclusion chapter.

See also the special variable *LINK-FILES* for the meaning of LINK-FILES."

  ;; First load the target system. If this fails, there's no point in working
  ;; hard on the rest.
  (asdf:operate 'asdf:load-op system-name)

  ;; Next, post-process some parameters.
  (setq library-name (escape library-name))
  (setq info-file (escape info-file))
  (unless subtitlep
    (setq subtitle (system-description system)))
  (when subtitle
    (when (char= (aref subtitle (1- (length subtitle))) #\.)
      (setq subtitle (subseq subtitle 0 (1- (length subtitle)))))
    (setq subtitle (escape subtitle)))
  (unless versionp
    (setq version (component-version system)))
  (when version
    (setq version (escape version)))
  (multiple-value-bind (system-author system-email)
      (parse-author-string (system-author system))
    (unless author
      (setq author system-author))
    (if author
	(setq author (escape author))
      (error "Author must be provided."))
    (setq email (if emailp email system-email)))
  (when email
    (setq email (escape email)))
  (when license
    (setq license (assoc license +licenses+))
    (unless license
      (error "License not found.")))
  (setq copyright-date
	(escape (or copyright-date
		    (multiple-value-bind (second minute hour date month year)
			(get-decoded-time)
		      (declare (ignore second minute hour date month))
		      (format nil "~A" year)))))
  (setq *top-node*
	(make-node :name "Top"
		   :section-name (format nil "The ~A Reference Manual"
				   library-name)
		   :section-type :unnumbered
		   :before-menu-contents (format nil "~
This is the ~A Reference Manual~@[, version ~A~],
generated automatically by Declt version ~A
on ~A."
					   library-name
					   version
					   (escape (version :long))
					   current-time-string)
		   :after-menu-contents (when license "@insertcopying")))
  (when license
    (add-child *top-node*
      (make-node :name "Copying"
		 :synopsis (cadr license)
		 :section-type :unnumbered
		 :before-menu-contents (format nil "@quotation~@
						    ~A~@
						    @end quotation"
					 (escape (caddr license))))))
  (when introduction
    (add-child *top-node*
      (make-node :name "Introduction"
		 :synopsis (format nil "What ~A is all about" library-name)
		 :before-menu-contents introduction)))
  (let ((*link-files* link-files))
    (add-system-node      *top-node* system)
    (add-modules-node     *top-node* system)
    (add-files-node       *top-node* system)
    (add-packages-node    *top-node* system)
    (add-definitions-node *top-node* system))
  (when conclusion
    (add-child *top-node*
      (make-node :name "Conclusion"
		 :synopsis "Time to go"
		 :before-menu-contents
		 (render-to-string (render-text conclusion)))))
  (let ((indexes-node (add-child *top-node*
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
				     :if-does-not-exist :create)
    (render-header library-name texi-name info-file subtitle version author
		   email license declt-notice
		   copyright-date current-time-string)
    (render-nodes)
    (format t "~%@bye~%~%@c ~A ends here~%" texi-name))
  (values))


;;; declt.lisp ends here
