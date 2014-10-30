
(defun next-buffer()
  "switches to next buffer on buffer list."
  (interactive) 
  (bury-buffer)
  (switch-to-buffer (car (buffer-list))))

(defun prev-buffer()
  "switches to previous buffer"
  (interactive)
  (switch-to-buffer (car (reverse (buffer-list)))))

