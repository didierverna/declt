;;; declt.lisp --- Entry points

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Mon Aug 23 17:47:05 2010
;; Last Revision: Mon Aug 23 17:47:05 2010

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


(defun render-header (library-name texi-name info-file subtitle version
		      author email copyright-date current-time-string)
  "Render the header of the Texinfo file."
  (format t "\\input texinfo

@c ~A --- Reference manual

~A@c This file is part of ~A.

@c ~A is free software; you can redistribute it and/or modify
@c it under the terms of the GNU General Public License version 3,
@c as published by the Free Software Foundation.

@c ~A is distributed in the hope that it will be useful,
@c but WITHOUT ANY WARRANTY; without even the implied warranty of
@c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
@c GNU General Public License for more details.

@c You should have received a copy of the GNU General Public License
@c along with this program; if not, write to the Free Software
@c Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


@c Commentary:

@c Generated automatically by Declt version ~A
@c on ~A.


@c ====================================================================
@c Header
@c ====================================================================
@c %**start of header
@setfilename ~A.info
@settitle The ~A Reference Manual
@afourpaper
@c %**end of header



@c ====================================================================
@c Settings
@c ====================================================================
@setchapternewpage odd
@setcontentsaftertitlepage
@documentdescription
The ~A Reference Manual~@[, version ~A~].
@end documentdescription



@c ====================================================================
@c New Commands
@c ====================================================================

@c ---------------
@c Indexing macros
@c ---------------

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



@c ====================================================================
@c Info Category and Directory
@c ====================================================================
@dircategory Common Lisp
@direntry
* ~A Reference: (~A). The ~A Reference Manual.
@end direntry



@c ====================================================================
@c Copying
@c ====================================================================
@copying
@quotation
~APermission is granted to make and distribute verbatim copies of this
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
sections entitled ``Copying'' and ``GNU General Public License'' are
included exactly as in the original, and provided that the entire
resulting derived work is distributed under the terms of a permission
notice identical to this one.

Permission is granted to copy and distribute translations of this manual
into another language, under the above conditions for modified versions,
except that this permission notice may be stated in a translation
approved by the Free Software Foundation.
@end quotation
@end copying



@c ====================================================================
@c Title Page
@c ====================================================================
@titlepage
@title The ~A Reference Manual
~A~A
@page
@quotation
This manual was generated automatically by Declt
version ~A on ~A.
@end quotation
@vskip 0pt plus 1filll
@insertcopying
@end titlepage



@c ====================================================================
@c Table of Contents
@c ====================================================================
@contents
"
    texi-name ;; @c ~A --- Reference manual
    (if (or copyright-date author) ;; ~A
	(format nil "@c Copyright (C) ~@[~A~]~:[~; ~]~@[~A~]~%~%"
	  copyright-date (and copyright-date author) author))
    library-name ;; @c This file is part of ~A.
    library-name ;; @c ~A is free software...
    library-name ;; @c ~A is distributed...
    (version :long) current-time-string ;; Generated automatically...
    info-file ;; @setfilename ~A.info
    library-name ;; @settitle...
    library-name version ;; @documentdescription...
    library-name info-file library-name ;; @direntry...
    (if (or copyright-date author) ;; @quotation...
	(format nil "Copyright @copyright{} ~@[~A~]~:[~; ~]~@[~A~].~%~%"
	  copyright-date (and copyright-date author) author)
      "")
    library-name ;; @title...
    (if (or subtitle version) ;; ~A
	(format nil "@subtitle ~@[~A~]~:[~;, ~]~@[version ~A~]~%"
	  subtitle (and subtitle version) version)
      "")
    (if (or author email) ;; ~A
	(format nil "@author ~@[~A~]~:[~; ~]~@[<@email{~A}>~]~%"
	  author (and author email) email)
      "")
    (version :long) current-time-string)) ;; This manual was...

(defun declt (system-name
	      &key (library-name (string-downcase (symbol-name system-name)))
		   (texi-file (format nil "~A.texi" library-name))
		   (info-file (pathname-name texi-file))
		   (subtitle nil subtitlep)
		   (version nil versionp)
		   (author nil authorp)
		   (email nil emailp)
		   (copyright-date nil copyright-date-p)
		   (link-components *link-components*)
	      &aux (system (asdf:find-system system-name))
		   (texi-name (make-pathname :name (pathname-name texi-file)
					     :type (pathname-type texi-file)))
		   (current-time-string (current-time-string)))
  "Generate a reference manual in Texinfo format for ASDF SYSTEM-NAME.
- LIBRARY-NAME defaults to SYSTEM-NAME.
- TEXI-FILE is the full path to the Texinfo file.
  Defaults to LIBRARY-NAME.texi.
- INFO-FILE is the info file basename sans extension.
  Defaults is built from TEXI-FILE.
- SUBTITLE defaults to the system description.
- VERSION defaults to the system version.
- AUTHOR and EMAIL are extracted from the system author.
- COPYRIGHT-DATE defaults to the current year."
  (unless subtitlep
    (setq subtitle (system-description system)))
  (when subtitle
    (when (char= (aref subtitle (1- (length subtitle))) #\.)
      (setq subtitle (subseq subtitle 0 (1- (length subtitle)))))
    (setq subtitle (texify subtitle)))
  (unless versionp
    (setq version (component-version system)))
  (multiple-value-bind (system-author system-email)
      (parse-author-string (system-author system))
    (unless authorp
      (setq author system-author))
    (setq email (texify (if emailp email system-email))))
  (unless copyright-date-p
    (setq copyright-date
	  (multiple-value-bind (second minute hour date month year)
	      (get-decoded-time)
	    (declare (ignore second minute hour date month))
	    year)))
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
					   (version :long)
					   current-time-string)
		   :after-menu-contents "@insertcopying"))
  (add-child *top-node*
	     (make-node :name "Copying"
			:synopsis "The GNU General Public License"
			:section-type :unnumbered
			:before-menu-contents (format nil "@quotation
~A is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License version 3,
as published by the Software Foundation.

~A is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
675 Mass Ave, Cambridge, MA 02139, USA.
@end quotation"
						library-name
						library-name)))
  (when (system-long-description system)
    (add-child *top-node*
	       (make-node :name "Introduction"
			  :synopsis (format nil "What ~A is all about"
				      library-name)
			  :before-menu-contents
			  (pretty-texify
			   (asdf:system-long-description system)))))
  (let ((*link-components* link-components))
    (add-system-node   *top-node* system)
    (add-modules-node  *top-node* system)
    (add-files-node    *top-node* system)
    (add-packages-node *top-node* system))
  (let ((indexes-node (add-child *top-node*
				 (make-node :name "Indexes"
					    :section-type :appendix))))
    (add-child indexes-node
	       (make-node :name "Concept Index"
			  :section-type :appendix
			  :section-name "Concepts"
			  :before-menu-contents "@printindex cp"
			  :after-menu-contents "@page"))
    (add-child indexes-node
	       (make-node :name "Function Index"
			  :section-type :appendix
			  :section-name "Functions"
			  :before-menu-contents "@printindex fn"
			  :after-menu-contents "@page"))
    (add-child indexes-node
	       (make-node :name "Variable Index"
			  :section-type :appendix
			  :section-name "Variables"
			  :before-menu-contents "@printindex vr"
			  :after-menu-contents "@page"))
    (add-child indexes-node
	       (make-node :name "Data Type Index"
			  :section-type :appendix
			  :section-name "Data Types"
			  :before-menu-contents "@printindex tp")))
  (with-open-file (*standard-output* texi-file
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create)
    (render-header library-name texi-name info-file subtitle version author
		   email copyright-date current-time-string)
    (render-nodes)
    (format t "~%@bye~%~%@c ~A ends here~%" texi-name))
  (values))


;;; declt.lisp ends here
