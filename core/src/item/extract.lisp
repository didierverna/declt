;;; extract.lisp --- Documentation information extraction

;; Copyright (C) 2020 Didier Verna

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
;; Documentation Contexts
;; ==========================================================================

(defstruct (extract (:conc-name))
  "The documentation extract structure."
  library-name
  tagline
  ;; Yuck. To avoid collision with the VERSION function.
  library-version
  contact-names
  contact-emails
  copyright-years
  license
  systems
  packages
  external-definitions
  internal-definitions
  hyperlinksp)

;; This is used rather often (in fact, not so much! ;-)) so it is worth a
;; shortcut.
(defun location (extract)
  "Return EXTRACT's main system location."
  (system-directory (car (systems extract))))



;; ==========================================================================
;; Documentation Information Extraction
;; ==========================================================================

(defun add-external-definitions (extract)
  "Add all external definitions to EXTRACT."
  (dolist (symbol (mapcan #'system-external-symbols (systems extract)))
    (add-symbol-definitions symbol (external-definitions extract))))

(defun add-internal-definitions (extract)
  "Add all internal definitions to EXTRACT."
  (dolist (symbol (mapcan #'system-internal-symbols (systems extract)))
    (add-symbol-definitions symbol (internal-definitions extract))))

(defun add-definitions (extract)
  "Add all definitions to EXTRACT."
  (add-external-definitions extract)
  (add-internal-definitions extract)
  (finalize-definitions
   (external-definitions extract)
   (internal-definitions extract)))

(defun add-packages (extract)
  "Add all package definitions to EXTRACT."
  (setf (packages extract)
	;; #### NOTE: several subsystems may share the same packages (because
	;; they would share files defining them) so we need to filter
	;; potential duplicates out.
	(remove-duplicates
	 (mapcan #'system-packages (systems extract)))))

(defun add-systems (extract system)
  "Add all system definitions to EXTRACT.
This includes SYSTEM and its subsystems."
  (setf (systems extract)
	(cons system
	      (remove-duplicates
	       (subsystems system (system-directory system))))))

(defun extract
    (system-name
     &key (library-name (if (stringp system-name)
			  system-name
			  (string-downcase system-name)))
	  (tagline nil taglinep)
	  (version nil versionp)
	  (contact nil contactp)
	  copyright-years
	  license
     &allow-other-keys ;; lazy calling from DECLT
     &aux (system (load-system system-name))
	  contact-names contact-emails
	  (extract (make-extract
		    :external-definitions (make-definitions-pool)
		    :internal-definitions (make-definitions-pool))))
  "Extract and return documentation information for ASDF SYSTEM-NAME.
The documentation information is returned in a EXTRACT structure, which see.

SYSTEM-NAME is an ASDF system designator. The following keyword parameters
allow to specify or override some bits of information.
- LIBRARY-NAME: name of the library being documented. Defaults to the system
  name.
- TAGLINE: small text to be used as the manual's subtitle, or NIL.
  Defaults to the system long name or description.
- VERSION: version information, or NIL. Defaults to the system version.
- CONTACT: contact information, or NIL. Defaults to the system maintainer(s)
  and author(s). Accepts a contact string, or a list of such. See
  `parse-contact-string' for more information.
- COPYRIGHT-YEARS: copyright years information or NIL. Defaults to the current
  year.
- LICENSE: license information. Defaults to NIL. Also accepts :mit, :boost,
  :bsd, :gpl, and :lgpl."

  (check-type library-name non-empty-string)
  (setf (library-name extract) library-name)
  (unless taglinep
    (setq tagline (or (system-long-name system)
		      (component-description system))))
  (unless (one-liner-p tagline)
    (setq tagline nil))
  (when (and tagline (char= (aref tagline (1- (length tagline))) #\.))
    (setq tagline (subseq tagline 0 (1- (length tagline)))))
  (setf (tagline extract) tagline)
  (unless versionp
    (setq version (component-version system)))
  (unless (one-liner-p version)
    (setq version nil))
  (setf (library-version extract) version)
  (unless contactp
    (setq contact (system-author system))
    (when (stringp contact) (setq contact (list contact)))
    (cond ((stringp (system-maintainer system))
	   (push (system-maintainer system) contact))
	  ((consp (system-maintainer system))
	   (setq contact (append (system-maintainer system) contact)))))
  (multiple-value-bind (names emails) (|parse-contact(s)| contact)
    (setq contact-names names
	  contact-emails emails))
  (when (and (= (length contact-names) 1)
	     (not contactp)
	     (null (car contact-emails))
	     (one-liner-p (system-mailto system)))
    (setq contact-emails (list (system-mailto system))))
  (setf (contact-names extract) contact-names)
  (setf (contact-emails extract) contact-emails)
  (setq copyright-years
	(or copyright-years
	    (multiple-value-bind (second minute hour date month year)
		(get-decoded-time)
	      (declare (ignore second minute hour date month))
	      (format nil "~A" year))))
  (unless (one-liner-p copyright-years)
    (setq copyright-years nil))
  (setf (copyright-years extract) copyright-years)
  (when license
    (setq license (assoc license *licenses*))
    (unless license
      (error "License not found.")))
  (setf (license extract) license)

  (add-systems extract system)
  (add-packages extract)
  (add-definitions extract)

  extract)

;;; extract.lisp ends here
