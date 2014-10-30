;
;  Blaizes Emacs File
;
(message "loading emacs.el")



;;; psvn broken with svn version 1.7 (no .svn dir in each dir)
;(require 'psvn)
; use dsvn instead
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

; 
;(server-start)

;(set-border-color "lightblue")
;(set-border-color "blue")
;(set-cursor-color "blue")
;(set-foreground-color "green")
;(set-background-color "black")
;(set-mouse-color "lightgreen");

;(dos-codepage-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set up Emacs load library path
; 
;;(setq load-path (cons (concat (getenv "HOME") "\\emacs") load-path))
;;(insert (concat (getenv "HOME") "\\emacs"))
;;;(insert  "~/emacs")

;(setq load-path (cons (concat (getenv "HOME") "/emacs") load-path))
;(insert (concat (getenv "HOME") "/emacs"))


(setq load-path (cons (concat (getenv "HOME") "/.emacs.d") load-path))
;(insert (concat (getenv "HOME") "/emacs"))

;; customize the emacs file path
;(add-to-list 'load-path "~/emacs.d")

;(insert  "~/emacs")


; The versioning package
(load-library "vrsntools")
(version-case
 ((GNU 20 2 1)o
    (setq load-path (cons (expand-file-name "~/env/emacs/20.21") load-path)))
 ((GNU 20 3 1)
    (setq load-path (cons (expand-file-name "~/env/emacs/20.31") load-path)))
 ((GNU 22 1 1)
    (setq load-path (cons (expand-file-name "~/env/emacs/22.1") load-path))))
; ((GNU 23 2 1)
;    (setq load-path (cons (expand-file-name "~/env/emacs/23.2") load-path))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tramp (ange over ssh) 
; 
;(add-to-list 'load-path "~/env/emacs/tramp/lisp/")
;(setq tramp-default-method "ssh")
(setq tramp-default-method "C:/cygwin/bin/ssh.exe")
(require 'tramp)

;(add-to-list 'Info-default-directory-list "~/env/emacs/tramp/info/")

;(setq tramp-default-user "blaize" tramp-default-host "remote.itee.uq.edu.au")

(setq tramp-verbose 10)
(setq tramp-debug-buffer t)



 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Python Mode 
; 

; use spaces for tabs...
(setq-default indent-tabs-mode nil);     
(load-library "python-mode")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Markdown Mode 
; 
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
  (setq auto-mode-alist
		(cons '("\\.mdml$" . markdown-mode) auto-mode-alist))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Dos Cmd Mode 
; 
;;(require 'dos-mode)

(autoload 'cmd-mode "cmd-mode" "CMD mode." t)
(setq auto-mode-alist (append '(("\\.\\(cmd\\|bat\\)$" . cmd-mode))
      auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; php Mode 
; 
(require 'php-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dos batch Mode 
; 
(require 'batch-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Haxe Mode 
; 
(require 'haxe-mode)
(defconst my-haxe-style
  '("java" (c-offsets-alist . ((case-label . +)
			       (arglist-intro . +)
			       (arglist-close . 0)
			       (cpp-macro . 0))))
  "My haXe Programming Style")
(add-hook 'haxe-mode-hook
	  (function (lambda () (c-add-style "haxe" my-haxe-style t))))
(add-hook 'haxe-mode-hook
	  (function
	   (lambda ()
	     (setq tab-width 4)
	     (setq indent-tabs-mode t)
	     (setq fill-column 80)
	     (local-set-key [(return)] 'newline-and-indent))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PHP Mode 
; 
;(autoload 'php-mode "php-mode" "PHP editing mode" t)
;(add-to-list 'auto-mode-alist '("\\.php3\\'" . php-mode))
;(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
;(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ActionScript Mode 
; 
(load-library "actionscript-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; C Mode 
; 
(require 'cc-mode)

(setq default-tab-width 4);
(setq indent-tabs-mode nil);

(setq c-default-style "k&r"
      c-basic-offset 4)

;(setq c-mode-hook
;    (function (lambda ()
;                (setq indent-tabs-mode nil)
;                (setq c-indent-level 4))))
;(setq c++-mode-hook
;    (function (lambda ()
;                (setq indent-tabs-mode nil)
;                (setq c-indent-level 4))))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Insert Local Templates using: C-x t
; 
(load-library "local-templates")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RST (DocUtils) Mode 
; 
(load "rst")
(add-hook 'text-mode-hook 'rst-text-mode-bindings)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; List-Buffers/Processes in the current frame
; 
(load-library "list-stuff-here")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; Show path/filename in titlebar and icon popup text 
;  
(setq frame-title-format "Emacs - %f")
(setq icon-title-format "Emacs - %b")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Some Miscellaneous Options 
;
(message "miscellaneous options")

; hide the startup message
(setq inhibit-startup-message t)

; Ensure $ORGANISATION is set
(if (not (getenv "ORGANISATION")) 
    (setenv "ORGANISATION" "ITEE Univesity of Queensland"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Calendar/Diary Preferences
;
(message "calendar prefs")

; use a local diary file
(setq diary-file "~/env/emacs/diary")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Oooh - Pretty Colours
;
(message "pretty colours")
(require 'extra-latex-font-lock)
(setq font-lock-maximum-decoration t)

; turn on pretty colours
;(global-font-lock-mode 1 "global font lock on")
(global-font-lock-mode 1)


(message "----1")

; fn for setting pretty frame colours
(defun set-frame-options ()
  (set-foreground-color "yellow")
  (set-background-color "black")
  (set-cursor-color "Blue")
  (message "set frame options")
)

(message "----2")

; set the frame options for this frame using the fn above
(set-frame-options)
(defun set-any-other-frame-options (frame)
  (select-frame frame)
  (set-frame-options)
)

(message "----3")

; and any other frames we create..
(setq after-make-frame-functions 'set-any-other-frame-options)


(message "----4")

; set faces at the end to make sure they don't get clobbered
(cond ((and t window-system)
       (message "setting faces")
       (custom-set-faces
        '(font-lock-comment-face ((t (:foreground "green"))))
		  '(font-lock-string-face ((t (:foreground "wheat"))))
		  '(font-lock-keyword-face ((t (:foreground "orangered"))))
		  '(show-paren-mismatch-face ((t (:background "red"))))
		  '(font-lock-type-face ((t (:foreground "MediumSpringGreen"))))
		  '(modeline ((t (:foreground "gold" :background "navy"))))
		  '(show-paren-match-face ((t (:background "blue"))))
		  '(font-lock-function-name-face ((t (:foreground "orange"))))
		  )
       )
      )

(message "----5")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Define some File Extension to Emacs Mode Mappings
;
(message "map extensions to emacs modes")

; use makefile-mode for *.win files
(setq auto-mode-alist (cons '("\\.win\\'" . makefile-mode) auto-mode-alist))

; use python-mode for *.py files
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))

; use elvin-c-mode for .c and .h files
(setq auto-mode-alist (cons '("\.h$" . c-mode) auto-mode-alist)) 
(setq auto-mode-alist (cons '("\.c$" . c-mode) auto-mode-alist))

; use xml for rule files
(setq auto-mode-alist (cons '("\.rules$" . xml-mode) auto-mode-alist))

; www modes
(setq auto-mode-alist (cons '("\.asp$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.jsp$" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.jsp_c$" . html-mode) auto-mode-alist))
; these file extensions are used by my home-page publishing program
(setq auto-mode-alist (cons '("\.hins$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.hsrc$" . html-mode) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Revert (Undo all changes since last save) 
; from a file - no questions asked!
;
(defun dv-revert-buffer ()
   "Revert current buffer.  Ignore auto-save."
     (interactive)
     (revert-buffer t t)
)
(global-set-key (quote [f8]) 'dv-revert-buffer)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Uncomment Region
;
(defun uncomment-region (beg end)
  "Undo a comment-region."
  (interactive "r")
  (comment-region beg end -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Some Key Bindings 
;


; F1 -> Man-page Lookup
(global-set-key  [(f1)]  (lambda () (interactive) 
			   (manual-entry (current-word))))

(global-set-key "\C-xl" 'make-symbolic-link)
(global-set-key (quote [f1]) (quote describe-function))               
(global-set-key (quote [\C-f1]) (quote find-function-at-point))       
(global-set-key (quote [f4]) (quote isearch-repeat-forward))
(global-set-key (quote [\C-f4]) (quote isearch-forward))


(global-set-key (quote [f12]) (quote eval-buffer))               


(global-set-key (quote [\M-f4]) (quote query-replace))
(global-set-key (quote [f5]) (quote shell))

;; F6 -> goto-line
(global-set-key (quote [f6]) (quote goto-line))  
(global-set-key (quote [f8]) (quote other-frame))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Use Windows-Arrow keys to navigate Buffers and Windows
; 

; defines prev-buffer and next-buffer
(load-library "traverse_buffers")

; navigate around a 2x2 window layout in one frame
(defun win-up ()
  (interactive)
  (other-window -1)
  (message "goto previous window")
)

(defun win-down ()
  (interactive)
  (other-window 1)
  (message "goto next window")
)

; Win & Up-Arrow ==> Prev-Window
(global-set-key (quote [s-up]) (quote win-up))
; Win & Down-Arrow ==> Next-Window
(global-set-key (quote [s-down]) (quote win-down))
; Win & Left-Arrow ==> Prev-Buffer
(global-set-key (quote [s-left]) (quote prev-buffer))
; Win & Right-Arrow ==> Next-Buffer
(global-set-key (quote [s-right]) (quote next-buffer))
; Win & Delete ==> Kill-This-Buffer
(global-set-key (quote [s-delete]) (quote kill-this-buffer))
; Win & Insert ==> List-Buffer-Here 
; (from whence you can select the buffer you want)
(global-set-key (quote [s-insert]) (quote list-buffers-here))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Look and Feel
; 

; use a readable font
;(set-default-font  "-adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1") 
;(set-default-font  "-adobe-courier-bold-r-normal--*-40-*-*-*-*-*") 

; save space by hidding the menu and tool bar
(menu-bar-mode nil)
(tool-bar-mode nil)


;set the frames up the way I like it
;; (progn 
;;   (set-frame-size (selected-frame) 222 101)
;;    first column
;;   (split-window-horizontally 106)
;;   (other-window 1)
;;    second column
;;   (split-window-horizontally 106)
;;   (split-window-vertically)
;;   (other-window 2)
;; )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FUCK OFF "Buffer foo does not end in newline. Add one?" 
; 
(setq require-final-newline nil) 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; load nsis mode (windows installer scripts)
; 
(autoload 'nsis-mode "nsis-mode" "NSIS mode" t)
(setq auto-mode-alist (append '(("\\.[Nn][Ss][Ii]\$" . nsis-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.[Nn][Ss][Hh]\$" . nsis-mode)) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Y/N Before Exiting Emacs
; Don't worry about active processes
; 


(defun els-exit ()
  "Function to ask for \"y/n\" before exiting.  This ensures we do not
   accidently exit."
  (interactive)
  (if (y-or-n-p "Exit? ")
      (save-buffers-kill-emacs t))
)

;; Change the binding of C-x C-c, which normally exits emacs.
;; It's easy to hit this by mistake, and that can be annoying.
(global-set-key "\C-x\C-c" 'els-exit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Putty fix.. ugly bad but it works!
;;(define-key global-map "\M-[1~" 'beginning-of-line)
;;(define-key global-map [select] 'end-of-line)


;; These need to go near the end.. 
;; there's some sort of a race condition at startup where the menus etc get reenabled

; turn off the tool bar
(message "turn off the toolbar")
(tool-bar-mode -1)

; turn off the menu bar
(message "turn off the menubar")
(menu-bar-mode -1)

; close all buffers
(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


;; WikiCreole mode (wiki-mode)
;; Thanks to Alex Schroeder of www.emacswiki.org 
;; And Jason Blevins for his inspiring Markdown Mode
;; http://jblevins.org/projects/markdown-mode/
(define-generic-mode 'wiki-mode 
  nil ; comments 
  nil; keywords 
  '(("^\\(= \\)\\(.*?\\)\\($\\| =$\\)" . 'info-title-1)
    ("^\\(== \\)\\(.*?\\)\\($\\| ==$\\)" . 'info-title-2)
    ("^\\(=== \\)\\(.*?\\)\\($\\| ===$\\)" . 'info-title-3)
    ("^\\(====+ \\)\\(.*?\\)\\($\\| ====+$\\)" . 'info-title-4) 
    ("\\[\\[.*?\\]\\]" . 'link)
    ("\\[.*\\]" . 'link)
    ("\\[b\\].*?\\[/b\\]" . 'bold)
    ("\\[i\\].*?\\[/i\\]" . 'italic)
    ("\\*\\*.*?\\*\\*" . 'bold)
    ("\\*.*?\\*" . 'bold)
    ("\\_<//.*?//" . 'italic)
    ("\\_</.*?/" . 'italic)
    ("__.*?__" . 'italic)
    ("_.*?_" . 'underline)
    ("|+=?" . font-lock-string-face)
    ("\\\\\\\\[ \t]+" . font-lock-warning-face)) ; font-lock list
  '(".wiki\\'"); auto-mode-alist
  '((lambda () (require 'info) (require 'goto-addr))); function-list
  "Wiki stuff including Creole Markup and BBCode.")

