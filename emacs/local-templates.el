;
;  Inserts local templates using: C-x t
;
;

; setup local template insertion
(defun local-templates ()
   "Insert a template from ~/env/emacs/templates:"
   (interactive)
   (insert-file (read-file-name "Template File: " "~/env/emacs/templates/" nil t))) 
; bind to keys: Ctrl-x t
(global-set-key "\C-xt" 'local-templates)
