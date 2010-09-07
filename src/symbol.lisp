;;; symbol.lisp --- Symbols rendering

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Thu Sep  2 23:22:59 2010
;; Last Revision: Thu Sep  2 23:22:59 2010

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

;; Contents management by FCM version 0.1.


;;; Code:

(in-package :com.dvlsoft.declt)


;; ==========================================================================
;; Rendering Protocols
;; ==========================================================================

(defun render-constant (symbol)
  "Render SYMBOL as a constant."
  (when (constant-definition-p symbol)
    (@defconstant (string-downcase symbol)
      (render-documentation symbol 'variable))))

(defun render-special (symbol)
  "Render SYMBOL as a special variable."
  (when (special-definition-p symbol)
    (@defspecial (string-downcase symbol)
      (render-documentation symbol 'variable))))

(defun render-macro (symbol)
  "Render SYMBOL as a macro."
  (when (macro-definition-p symbol)
    (@defmac (string-downcase symbol)
	;; #### PORTME.
	(sb-introspect:function-lambda-list symbol)
      (render-documentation symbol 'function))))

(defun render-function (symbol)
  "Render SYMBOL as an ordinary function."
  (when (function-definition-p symbol)
    (@defun (string-downcase symbol)
	;; #### PORTME.
	(sb-introspect:function-lambda-list symbol))
    (render-documentation symbol 'function)))

(defun render-method (method)
  "Render METHOD."
  (@defmethod
      ;; #### PORTME:
      (string-downcase (sb-mop:generic-function-name
			(sb-mop:method-generic-function method)))
      ;; #### PORTME.
      (sb-mop:method-lambda-list method)
      ;; #### PORTME.
      (sb-mop:method-specializers method)
      ;; #### PORTME.
      (sb-mop:method-qualifiers method)
    (render-documentation method 't)))

(defun render-generic (symbol)
  "Render SYMBOL as a generic function."
  (when (generic-definition-p symbol)
    (@defgeneric (string-downcase symbol)
	;; #### PORTME.
	(sb-introspect:function-lambda-list symbol)
      (render-documentation symbol 'function)))
  ;; #### PORTME.
  (dolist (method (sb-mop:generic-function-methods (fdefinition symbol)))
    (render-method method)))

(defun render-condition (symbol)
  "Render SYMBOL as a condition."
  (when (condition-definition-p symbol)
    (@defcond (string-downcase symbol)
      (render-documentation symbol 'type))))

(defun render-structure (symbol)
  "Render SYMBOL as a structure."
  (when (structure-definition-p symbol)
    (@defstruct (string-downcase symbol)
      (render-documentation symbol 'type))))

(defun render-class (symbol)
  "Render SYMBOL as an ordinary class."
  (when (class-definition-p symbol)
    (@defclass (string-downcase symbol)
      (render-documentation symbol 'type))))


;;; symbol.lisp ends here
