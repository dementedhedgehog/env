;
; Emacs mode for POS tagged files
;
;
(require 'font-lock)

;(defvar postag-mode-hook nil)

(defconst postag-mode-version "0.0"
  "POS tag mode version number.")

;(defvar postag-mode-map
;  (let ((postag-mode-map (make-keymap)))
;    (define-key postag-mode-map "\C-j" 'newline-and-indent)
;    postag-mode-map)
;  "Keymap for POS tag  major mode")

(defconst postag-font-lock-keywords nil)
;(defconst postag-font-lock-keywords 
;  (list
;   '("\(/NNP\(?:\$?/\)\)" . font-lock-builtin-face)
;   '("\(/DT/\)" . font-lock-variable-name-face))
;  "Minimal highlighting expressions for POS tag mode")


(define-derived-mode postag-mode fundamental-mode "postag"
  "Major mode for viewing POS tagged files"
  (interactive)
  (kill-all-local-variables)
;  (set-syntax-table postag-mode-syntax-table)
;  (use-local-map postag-mode-map)
  (set (make-local-variable 'font-lock-defaults) 
       '(postag-font-lock-keywords))
;  (set (make-local-variable 'indent-line-function) 'postag-indent-line)  
  (setq major-mode 'postag-mode)
  (setq mode-name "postag")
  (run-hooks 'postag-mode-hook))

(provide 'postag-mode)
