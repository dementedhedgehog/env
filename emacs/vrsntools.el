;;; vrsntools.el --- version conditionalization tools for Emacs Lisp

;; Copyright (C) 1993 Hans Chalupsky

;; Author: Hans Chalupsky <hans@cs.buffalo.edu>
;; Created: 30 Nov 1993
;; Version: $Id: vrsntools.el,v 1.1 2003/05/23 00:43:04 blaize Exp $
;; Keywords: lisp, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to hans@cs.buffalo.edu) or from
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;; 02139, USA.


;;; Commentary:

;; This package suggests a uniform way of dealing with version 
;; conditionalization in Emacs Lisp code, so that not every Emacs Lisp
;; programmer has to write his/her own version handling.

;; Various macros for compile-time conditionalization are provided.
;; The conditionalization is based on the compile-time value of the global
;; variable `version-description'. This variable typically contains a
;; description of the current Emacs instance, but any list of features can
;; be used as its value. Version descriptions are totally ordered by the
;; function `version-compare' (which see), which allows one to specify
;; ranges of versions for easy conditionalization.

;; The macros can also be used in interpreted code, e.g., to conditionalize
;; a `~/.emacs' file.

;; Examples:

;; (version-case
;;  ((GNU 18)
;;   (v18-specific-fn)
;;   (v18-specific-fn2))
;;  (((Lucid 19 6) (Lucid 19 8))
;;   (lemacs-19.6-19.8-specific-fn))
;;  ((nil 19)
;;   (any-v19-specific-fn))
;;  ((Epoch)
;;   (epoch-specific-fn))
;;  (t (default-fn)))

;; (version-when (>= (Lucid 19 8))
;;   (lemacs-19.8-and-later-fn)
;;   (lemacs-19.8-and-later-fn2))
  
;; ...or rewriting part of `sc-ask' in Supercite 3.1:

;; (version-case
;;  ((nil 18) (setq event (read-char)))
;;  ((Lucid) (next-command-event event))
;;  (t (setq event (read-event))))

;; The version tests are evaluated at compile time, hence, the compiled
;; code only contains the code necessary for the specified version (range).
;; Unnecessary compiler warnings due to undefined fns, etc., are avoided.
;; Using a package in a different Emacs environment requires recompilation.

;; If you just want to use this package in Emacs-19's then you can load it
;; at compile time by putting

;; (eval-when-compile (require 'vrsntools))

;; at the beginning of your file. If you also want to use it in Emacs-18
;; then you have to either unconditionally require it with

;; (require 'vrsntools)

;; because there is no `eval-when-compile' in Emacs-18, or you use the
;; following slightly verbose sequence of forms which will work in Emacs-18's
;; as well as Emacs-19's and will avoid the loading of `vrnstools.elc'
;; when you load your compiled file:

;; (provide 'vrsntools-preload)
;; (require 'vrsntools-preload)
;; (if (not (fboundp 'version-case))
;;     (setq features (delq 'vrsntools-preload features)))

;; Note that in Emacs-18 top-level applications of `version-case' and
;; `version-when' will not get compiled, because the v18 byte-compiler
;; does not expand top-level macros. Only applications in the bodies of
;; `defun's and `defmacro's will be compiled. If you need to use top-level
;; conditionalization in Emacs-18 you have to unconditionally require
;; `vrsntools'.


;;; Code:

;; The list format was inspired by Supercite 3.1:

(defun version-description-of-emacs ()
  "Returns a list `(FLAVOR MAJOR MINOR)' describing this Emacs instance.
FLAVOR can be `GNU', `Lucid' or `Epoch', MAJOR and MINOR are the major
and minor release numbers."
  (let* ((flavor
          (cond ((boundp 'epoch::version)
                 'Epoch)
                ((string-match "Lucid" emacs-version)
                 'Lucid)
                (t 'GNU)))
         (emacs-version
	  (if (eq flavor 'Epoch) "18.59" emacs-version))
         (major-version
	  (if (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)" emacs-version)
	      (car (read-from-string 
		    emacs-version
		    (match-beginning 1)
		    (match-end 1)))))
         (minor-version
	  (if major-version
	      (car (read-from-string 
		    emacs-version
		    (match-beginning 2)
		    (match-end 2))))))
    (list flavor major-version minor-version)))

(defvar version-description (version-description-of-emacs)
  "*Global version description used in `version-when' and `version-case'.
A version description must be a list of symbols/strings/numbers.")

(defun version-compare (version-a version-b)
  "Returns -1/0/1 depending on whether VERSION-A is </=/> VERSION-B.
Every version has to be a list of symbols/strings/numbers. A version list
is considered to be a list of digits starting with the most significant
digit. Missing digits to the right of the shorter version will be padded
with NIL's. Symbol and string digits will be compared with `string<',
number digits will be compared with `<'. NIL digits will be considered to
be equal to the corresponding digit in the other version, i.e., they are
treated as ``don't cares''.
Example: `(version-compare '(10 nil \"a\" b) '(10 20 a))' returns 0."
  (let ((result 0)
	version-a-digit version-b-digit)
    (while (and version-a version-b (zerop result))
      (setq version-a-digit (car version-a))
      (setq version-b-digit (car version-b))
      (setq version-a (cdr version-a))
      (setq version-b (cdr version-b))
      (setq result
	    (cond ((or (null version-a-digit)
		       (null version-b-digit))
		   0)
		  ((numberp version-a-digit)
		   (if (< version-a-digit version-b-digit)
		       -1
		     (if (= version-a-digit version-b-digit)
			 0
		       1)))
		  (t (if (symbolp version-a-digit)
			 (setq version-a-digit (symbol-name version-a-digit)))
		     (if (symbolp version-b-digit)
			 (setq version-b-digit (symbol-name version-b-digit)))
		     (if (string< version-a-digit version-b-digit)
			 -1
		       (if (string= version-a-digit version-b-digit)
			   0
			 1))))))
    result))

(defmacro version-when (test &rest body)
  "Expands into BODY if the version TEST succeeds at compile time.
TEST has to be a list `(RELATION VERSION)' where RELATION is one of
`<', `<=', `=', `>=' or `>' and VERSION is a version list. TEST succeeds
if the global `version-description' is in RELATION with VERSION."
  (if (funcall (car test)
	       (version-compare version-description (car (cdr test)))
	       0)
      (if (<= (length body) 1)
	  (car body)
	(cons 'progn body))))

(defmacro version-case (&rest clauses)
  "Expands into the clause body forms whose TEST succeeds at compile time.
Every clause of CLAUSES has to be of the form `(TEST BODY...)' where TEST
has to be one of `VERSION', `(VERSION-A VERSION-B)', t, `otherwise', or
`default'. TEST succeeds if the global `version-description' is either
= to VERSION, or in the inclusive range [VERSION-A .. VERSION-B]. The
`t/otherwise/default' tests always succeed."
  (let (test body)
    (while clauses
      (setq test (car (car clauses)))
      (setq body (cdr (car clauses)))
      (if (cond ((memq test '(t otherwise default)))
		((consp (car test))
		 ;; we have a version range:
		 (and (<= (version-compare
			   (car test) version-description)
			  0)
		      (>= (version-compare
			   (car (cdr test)) version-description)
			  0)))
		(t;; we have a normal version
		 (= (version-compare test version-description) 0)))
	  (setq clauses nil)
	(setq body nil)
	(setq clauses (cdr clauses))))
    (if (<= (length body) 1)
	(car body)
      (cons 'progn body))))

(provide 'vrsntools-preload)
(provide 'vrsntools)

;;; vrsntools.el ends here
