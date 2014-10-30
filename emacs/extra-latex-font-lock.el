;; @(#) extra-latex-font-lock.el -- extra highlighting for latex
;; @(#) $Id: extra-latex-font-lock.el,v 1.1 2003/09/22 03:04:46 blaize Exp $

;; This file is not part of Emacs

;; Copyright (C) 1998, 1999 by David Ponce
;; Author:       David Ponce david.ponce@wanadoo.fr
;; Maintainer:   David Ponce david.ponce@wanadoo.fr
;; Created:      September 28 1998

;; LCD Archive Entry:
;; extra-latex-font-lock|David Ponce|david.ponce@wanadoo.fr|
;; extra highlighting for latex|
;; $Date: 2003/09/22 03:04:46 $|$Revision: 1.1 $|~/misc/extra-latex-font-lock.el|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Description:
;;
;;  Adds some extra highlighting to `latex-mode'

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add following into your
;;  ~/.emacs startup file
;;
;;     (cond ((fboundp 'global-font-lock-mode)
;;           (require 'extra-latex-font-lock)
;;           (setq font-lock-maximum-decoration t)
;;           (global-font-lock-mode t)
;;           ))

;;; Usage:
;;

;;; Customization:
;;

;;; Support:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to David Ponce at david.ponce@wanadoo.fr.
;;
;;  This version of jpack was developed with NTEmacs 20.3.1 under MS Windows
;;  NT 4 WKS SP3 and also tested with Emacs 20.3 under Sun Solaris 2.5.
;;  Please, let me know if it works with other OS and versions of Emacs.

;;; Code:

;; latex-font-lock20a.el --- patch font-lock for Latex in Gnu Emacs20
;;(load "latex-font-lock20a")

;; creates a specific face for numbers
(defface extra-latex-font-lock-number-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Font Lock mode face used to highlight numbers."
  :group 'font-lock-highlighting-faces)

;; creates a specific face for links
(defface extra-latex-font-lock-link-face
  '((t (:foreground "blue" :italic nil :underline t)))
  "Font Lock mode face used to highlight links."
  :group 'font-lock-highlighting-faces)

; creates a specific face for paragraph summary environments
(defface extra-latex-font-lock-psum-face
  '((t (:foreground "white" :italic nil :underline nil)))
  "Font Lock mode face used to highlight our home-grown latex paragraph summaries."
  :group 'font-lock-highlighting-faces)


;; defines the extra font lock faces
(defvar extra-latex-font-lock-number-face    'extra-latex-font-lock-number-face)
(defvar extra-latex-font-lock-link-face      'extra-latex-font-lock-link-face)
(defvar extra-latex-font-lock-psum-face      'extra-latex-font-lock-psum-face)
(defvar extra-latex-font-lock-bold-face      'bold)
(defvar extra-latex-font-lock-italic-face    'italic)
(defvar extra-latex-font-lock-underline-face 'underline)
(defvar extra-latex-font-lock-pre-face       'default)
(defvar extra-latex-font-lock-code-face      'font-lock-builtin-face)

;; extra fontification regexps
(defvar extra-latex-font-lock-keywords
  (eval-when-compile
    (list
     
     ;; Use a different face for modifiers
     ;(cons (concat "\\<\\("
     ;(regexp-opt '("abstract" "const" "final" "synchronized"
     ;"transient" "static" "volatile" "public"
     ; "private" "protected" "native"))
     ;"\\)\\>")
     ;'font-lock-builtin-face)
       
     '("\\b\\(0[xX][0-9a-fA-F]+[lL]?\\|[0-9]+\\.?[0-9]*\\([eE][-+]?[0-9]+\\)?\\([lL]\\|[fF]\\|[dD]\\)?\\)\\b" . extra-latex-font-lock-number-face)
     
     '("\\b\\(\\.[0-9]+\\([eE][-+]?[0-9]+\\)?\\([lL]\\|[fF]\\|[dD]\\)?\\)\\b" . extra-latex-font-lock-number-face)
       
     ;; Fontify capitalised identifiers as constant
     ;'("\\b[A-Z_]+[A-Z0-9_]*\\b" . font-lock-constant-face)

     ;; Fontify text between `' in comments
     ;'("`\\(.*\\)'"
     ;  1 font-lock-constant-face             prepend)
       
     ;; Latexdoc tags within comments.
     ;'("@\\(deprecated\\|since\\|serial\\|serialData\\|serialField\\|throws\\)\\>"
     ;  1 font-lock-constant-face            prepend)
     ;'("{@\\(docRoot\\)}"
     ;  1 font-lock-constant-face            prepend)
;;;     '("{@\\(link\\)\\>[^}\n\r]*}?" ;; no newlines in link tag
     ;'("{@\\(link\\)\\>\\s-+\\(.*\\)\\s-*}"
     ;  (1 font-lock-constant-face            prepend)
     ;  (2 extra-latex-font-lock-link-face     prepend))
     
     ;; Basic HTML highlighting in latexdoc comments
     ;; Fontify the text of a HREF anchor.
     ;'("<[Aa]\\s-+[Hh][Rr][Ee][Ff][^>]*>\\([^>]+\\)</[Aa]>"
     ;  1 extra-latex-font-lock-link-face      t)

     ; BLAIZE
     ;'("\\begin\{psum\}\\([^}]+\\)\\end\psum\}>" ;;;;;;;;;;;;;;;;;
     '("begin{psum}\\([^>]+\\)end{psum}"
       0 extra-latex-font-lock-psum-face      t)

     ;; Fontify <b>, <strong>, <i>, <u>, <code> and <pre> tags when no tags inside
     ;'("<[Ss][Tt][Rr][Oo][Nn][Gg]>\\([^<]*\\)</[Ss][Tt][Rr][Oo][Nn][Gg]>"
     ;  1 extra-latex-font-lock-bold-face      t)
     ;'("<[Bb]>\\([^<]*\\)</[Bb]>"
     ;  1 extra-latex-font-lock-bold-face      t)
     ;'("<[Ii]>\\([^<]*\\)</[Ii]>"
     ;  1 extra-latex-font-lock-italic-face    t)
     ;'("<[Uu]>\\([^<]*\\)</[Uu]>"
     ;  1 extra-latex-font-lock-underline-face t)
     ;'("<[Cc][Oo][Dd][Ee]>\\([^<]*\\)</[Cc][Oo][Dd][Ee]>"
     ;  1 extra-latex-font-lock-code-face      t)
     ;'("<[Pp][Rr][Ee]>\\([^<]*\\)</[Pp][Rr][Ee]>"
     ;  1 extra-latex-font-lock-pre-face       t)

     )
    )
  )

;; adds the extra fontification to latex-mode and jde-mode
(font-lock-add-keywords 'latex-mode extra-latex-font-lock-keywords)

(provide 'extra-latex-font-lock)

;;; Change History:

;;
;; $Log: extra-latex-font-lock.el,v $
;; Revision 1.1  2003/09/22 03:04:46  blaize
;; added some emacs functions
;; added some web-page links
;;
;; Revision 1.5  2000/05/22 08:07:46  david_ponce
;; Added syntax highlighting for JDK 1.2/1.3 latexdoc tags.
;; New customizable face to highlight latexdoc/HTML links.
;;
;; Revision 1.4  1999/08/24 08:31:51  ebat311
;; New regexp to highlight capitalised identifiers as constants.
;;
;; Revision 1.3  1999-04-23 00:05:10+02  ebat311
;; FIXED: capitalised identifiers with leading underscore
;;             were not highlighted.
;;
;; Revision 1.2  1999-03-05 14:13:17+01  ebat311
;; Improved regexps.
;;
;; Revision 1.1  1999-02-01 12:25:17+01  ebat311
;; Initial revision
;;
;;

;;; extra-latex-font-lock.el ends here.













