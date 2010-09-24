#! /usr/local/bin/sbcl --script

;;; generate.sh --- Declt reference manual generation script

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sun Sep 19 21:32:07 2010
;; Last Revision: Fri Sep 24 15:14:53 2010

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

(require :asdf)

#-asdf2 (setf asdf:*central-registry*
	      (list* (merge-pathnames "share/common-lisp/systems/"
				      (user-homedir-pathname))
		     #p"/usr/local/share/common-lisp/systems/"
		     #p"/usr/share/common-lisp/systems/"
		     asdf:*central-registry*))
#+asdf2 (asdf:initialize-source-registry
	 `(:source-registry
	   (:directory ,(merge-pathnames "share/common-lisp/systems/"
					 (user-homedir-pathname)))
	   (:directory "/usr/local/share/common-lisp/systems")
	   (:directory "/usr/share/common-lisp/systems")
	   :inherit-configuration))

#-asdf2 (ignore-errors (asdf:operate 'asdf:load-op :asdf-binary-locations))

(asdf:operate 'asdf:load-op :com.dvlsoft.declt)
(com.dvlsoft.declt:declt :com.dvlsoft.declt
			 :library-name "Declt" :texi-file "declt.texi")
(com.dvlsoft.declt:declt :com.dvlsoft.declt
			 :library-name "Declt" :texi-file "decltweb.texi"
			 :link-files nil)



;;; Local Variables:
;;; mode: lisp
;;; End:

;;; generate.sh ends here
