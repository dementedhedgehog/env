;;------------------------------------------------------------------------------
;;   LIST BUFFERS USING CURRENT WINDOW *AND* MOVE POINT INTO THAT BUFFER
;;------------------------------------------------------------------------------
(defun list-buffers-here (files-only)
  "Display a list of names of existing buffers.
Inserts it in buffer *Buffer List* and displays that using the current
window.  Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

The M column contains a * for buffers that are modified.
The R column contains a % for buffers that are read-only."
  (interactive "P")
  (save-window-excursion
    (list-buffers files-only))
  (switch-to-buffer (get-buffer "*Buffer List*") 'norecord)
  (goto-line 3)
)


;;------------------------------------------------------------------------------
;;   LIST Processes here  *AND* MOVE POINT INTO THAT BUFFER
;;------------------------------------------------------------------------------
(defun list-processes-here (files-only)
  "Display a list of processes
Inserts it in buffer *Buffer List* and displays that using the current
window.  Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

The M column contains a * for buffers that are modified.
The R column contains a % for buffers that are read-only."
  (interactive "P")
  (save-window-excursion
    (list-processes) ; files-only))
  (switch-to-buffer (get-buffer "*Buffer List*") 'norecord)
  (goto-line 3)
)

