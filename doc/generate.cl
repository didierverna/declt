;;; generate.cl --- Declt reference manual generation script

;; Copyright (C) 2010-2013 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Declt.

;; Declt is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3,
;; as published by the Free Software Foundation.

;; Declt is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:



;;; Code:

(require :asdf)
(asdf:load-system :asdf)

(asdf:load-system :com.dvlsoft.declt)
(com.dvlsoft.declt:nickname-package)

(if (and (second sb-ext:*posix-argv*)
	 (string= (second sb-ext:*posix-argv*) "--web"))
    (declt:declt :com.dvlsoft.declt
		 :library-name "Declt"
		 :texi-file "webreference.texi"
		 :info-file "declt-webreference" ; but we don't care
		 :license :gpl
		 :copyright-date "2010--2013")
    (declt:declt :com.dvlsoft.declt
		 :library-name "Declt"
		 :texi-file "reference.texi"
		 :info-file "declt-reference"
		 :license :gpl
		 :copyright-date "2010--2013"
		 :hyperlinks t))

(sb-ext:quit)


;;; generate.cl ends here
