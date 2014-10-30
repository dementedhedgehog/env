(require 'font-lock)
(require 'cc-mode)
(eval-when-compile
  (require 'regexp-opt))

(defconst actionscript-mode-version "2.1"
  "Actionscript Mode version number.")

(defgroup actionscript nil
  "Major mode for editing Actionscript code."
  :group 'languages
  :prefix "actionscript-")

(defcustom actionscript-mode-hook nil
  "Hook for customizing `actionscript-mode'."
  :group 'actionscript
  :type 'hook)

(defvar actionscript-mode-map (c-make-inherited-keymap)
  "Keymap used in `actionscript-mode' buffers.")

;;;###autoload
(define-derived-mode actionscript-mode java-mode "Actionscript"
  "Major mode for editing Actionscript code.

This mode is derived from `java-mode'; see its documentation for further
information.

\\{actionscript-mode-map}"
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((;; comment out the lines below to adjust
           ;; syntax highlighting gaudiness
           ;actionscript-font-lock-keywords-1
           ;actionscript-font-lock-keywords-2
           actionscript-font-lock-keywords-3
           )
          nil nil ((?_ . "w") (?$ . "w")) nil))

  (define-key actionscript-mode-map [f5] 'as-print-func-info)
  (define-key actionscript-mode-map "\C-t" 'as-insert-trace)
  (define-key actionscript-mode-map [f4] 'as-flash-compile)
  (define-key actionscript-mode-map "\C-c\C-v" 'as-insert-flash-var)
  (define-key actionscript-mode-map "\C-c\C-f" 'as-insert-flash-func)
  (define-key actionscript-mode-map [(control meta a)] 'as-beginning-of-defun)
  (define-key actionscript-mode-map [(control meta e)] 'as-end-of-defun)
  (define-key actionscript-mode-map [(control meta h)] 'as-mark-defun)

  (easy-menu-define c-actionscript-menu actionscript-mode-map
    "Actionscript Mode Commands" (c-mode-menu "Actionscript")))

(defvar actionscript-font-lock-default-face 'actionscript-font-lock-default-face)

(defconst actionscript-font-lock-keywords-1
  (append
   java-font-lock-keywords-1
   (list

    '("\\<\\(function\\)\\>\\(?:\\s-+\\(\\sw+\\)\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))

    ;; need to fix this to handle: var a, b;
    '("\\<\\(var\\)\\>\\(?:\\s-+\\(\\sw+\\)\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face nil t))
    ))
  "Subdued level highlighting for Actionscript mode.")

(defconst actionscript-font-lock-keywords-2
  (append
   java-font-lock-keywords-2
   actionscript-font-lock-keywords-1
   (list

    "\\<\\(debugger\\|delete\\|export\\|in\\|typeof\\|with\\)\\>"

    (list (concat
           "\\<\\("
           (mapconcat 'identity java-font-lock-extra-types nil)
           "\\)\\>\\.")
          '(1 font-lock-type-face nil t))

    ;; In Java, `void' is a type. In Actionscript, it is an operator.
    ;; This overrides the inherited notion of keyword `void'.
    '("\\<\\(void\\)\\>\\(?:\\s-+\\(\\sw+\\)\\)?"
      (1 font-lock-keyword-face t)
      (2 actionscript-font-lock-default-face t t))

    ;; Value properties of the global object
    '("\\<\\(Infinity\\|NaN\\|undefined\\)\\>" 0 font-lock-constant-face t)

    ;; Properties of the Number constructor
    (list (concat
           "\\<Number\\."
           (regexp-opt
            '("MAX_VALUE" "MIN_VALUE" "NaN" "NEGATIVE_INFINITY"
              "POSITIVE_INFINITY") t)
           "\\>")
          '(1 font-lock-constant-face))

    ;; Value properties of the Math object
    (list (concat
           "\\<Math\\."
           (regexp-opt
            '("E" "LN10" "LN2" "LOG2E" "LOG10E" "PI" "SQRT1_2" "SQRT2") t)
           "\\>")
          '(1 font-lock-constant-face))
    ))
  "Medium level highlighting for Actionscript mode.")

(defconst actionscript-font-lock-keywords-3
  (append
   java-font-lock-keywords-3
   actionscript-font-lock-keywords-2
   (list

    ;; Properties of the Date constructor
    '("\\<Date\\.\\(parse\\|UTC\\)\\>" 1 font-lock-builtin-face)

    ;; Function properties of the Math object
    (list (concat
           "\\<Math\\."
           (regexp-opt
            '("abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "exp" "floor"
              "log" "max" "min" "pow" "random" "round" "sin" "sqrt" "tan") t)
           "\\>")
          '(1 font-lock-builtin-face))

    (list (regexp-opt
           '(;; URI handling function properties
             "decodeURI" "decodeURIComponent" "encodeURI" "encodeURIComponent"
             ;; Function properties of the global object
             "trace" "eval" "isFinite" "isNaN" "parseFloat" "parseInt") 'words)
          '(0 font-lock-builtin-face))

    (list (concat
           "\\."
           (regexp-opt
            '(;; Properties of the Object prototype object
              "hasOwnProperty" "isPrototypeOf" "propertyIsEnumerable"
              "toLocaleString" "toString" "valueOf"
              ;; Properties of the Function prototype object
              "apply" "call"
              ;; Properties of the Array prototype object
              "concat" "join" "pop" "push" "reverse" "shift" "slice" "sort"
              "splice" "unshift"
              ;; Properties of the String prototype object
              "charAt" "charCodeAt" "fromCharCode" "indexOf" "lastIndexOf"
              "localeCompare" "match" "replace" "search" "split" "substring"
              "toLocaleLowerCase" "toLocaleUpperCase" "toLowerCase"
              "toUpperCase"
              ;; Properties of the Number prototype object
              "toExponential" "toFixed" "toPrecision"
              ;; Properties of the Date prototype object
              "getDate" "getDay" "getFullYear" "getHours" "getMilliseconds"
              "getMinutes" "getMonth" "getSeconds" "getTime"
              "getTimezoneOffset" "getUTCDate" "getUTCDay" "getUTCFullYear"
              "getUTCHours" "getUTCMilliseconds" "getUTCMinutes" "getUTCMonth"
              "getUTCSeconds" "setDate" "setFullYear" "setHours"
              "setMilliseconds" "setMinutes" "setMonth" "setSeconds" "setTime"
              "setUTCDate" "setUTCFullYear" "setUTCHours" "setUTCMilliseconds"
              "setUTCMinutes" "setUTCMonth" "setUTCSeconds" "toDateString"
              "toLocaleDateString" "toLocaleString" "toLocaleTimeString"
              "toTimeString" "toUTCString"
              ;; Properties of the RegExp prototype object
              "exec" "test"
              ) t)
           "\\>")
          '(1 font-lock-builtin-face))
    ))
  "Gaudy level highlighting for Actionscript mode.")

(defun update-etags(source-folder)
  (interactive
   (list
		(get-or-ask 'actionscript-mode 'source-folder '(read-file-name "Which is the source folder?" "src"))))
  (save-some-buffers)
	(let ((wd (getenv "PWD")))
		(shell-command (concat "find " source-folder " -name '*.as' -print | etags --language=none --regex='/[ \t]*\\(?:static \\)?\\(?:private \\)?\\(?:public \\)?\\(?:static \\)?function \\([a-zA-Z0-9_]+\\)/\\1/' -o " wd "/TAGS -"))
		(visit-tags-table (concat wd "/TAGS"))))

(defconst as-function-re "\\(^[ \t\n]*\\)\\(static \\)?\\(?:private \\)?\\(?:public \\)?\\(static \\)?function \\([a-zA-Z0-9_$]*\\)([ \t\n]*\\(.*?\\)[ \t\n]*)[ \t\n]*\\(:[a-zA-Z_0-9$]*\\)?[ \t\n]*{" 
	"A regexp that matches a function signature in Actionscript.")

(defun as-print-func-info()
  "Insert a print statement immediately after the nearest function definition before point."
  (interactive)
  (save-excursion
		(re-search-backward as-function-re)
		(goto-char (match-end 0))
		(let ((function (match-string 4))
					(args (match-string 5))
					(static1 (match-string 2))
					(static2 (match-string 3))
					(arg-string "")
					(debug-msg "")
					(first-part "\"")
					(arg-trace "\""))

			(when (> (length args) 0)
				;;parse args
				(setf arg-string (mapconcat (function (lambda (x)
																								;; chop off any type info
																								(car (split-string x ":"))))
																		(split-string args ",")
																		" +\", \"+ ")))

			;; See if we should add "this."
			(if (not (or (stringp static1) (stringp static2)))
					(setf first-part "this + \"."))

			;; Check if there are any args to trace
			(when (and (> (length args) 0)
								 (not (equal arg-string "Void")))
				(setf arg-trace (concat " : \" + " arg-string)))

			;;now, print out our debug statement after the function start
			(setf debug-msg (concat "trace" "(" first-part function "(" args ")" arg-trace ");"))
			(insert (concat "\n\n" debug-msg))
			(indent-according-to-mode)
			(message debug-msg))))

(defun as-insert-trace ()
  "Insert an empty trace call at point. If we are over a word, then trace that word on the next line"
  (interactive)

  (let ((cw (current-word)))
		(cond
		 ((not (equal cw ""))
			;; goto the next line
			(end-of-line)
			(insert (format "\n%s(\"%s: \" + %s);" "trace" cw cw))
			(indent-according-to-mode))
		 (t
			(indent-according-to-mode)
			(insert (format "%s(\"\");" "trace"))
			(backward-char 3)))))

(defun get-all-files(file_regexp lst)
  (if (null lst)
	  nil
	(let* ((file (car lst))
		   (basename (file-name-nondirectory file)))
	  ;; if the first item is a dir
	  (if (and (file-directory-p file)
			   (not (equal basename "."))
			   (not (equal basename "..")))
		  ;; get all the files in this dir
		  (let ((new_list (get-all-files file_regexp (directory-files file t nil t))))
			;; if there were files in it...
			(if new_list
				;; Add the dir name as the first element of the new list, and add
				;; that as the first element to whatever is left.
				(cons (cons basename new_list) (get-all-files file_regexp (cdr lst)))
			  nil))
		;; not a dir. Check if it matches the regexp.
		(if (string-match file_regexp basename)
			;; Match. Add on to whatever is left.
			(cons basename (get-all-files file_regexp (cdr lst)))
		  ;; No match. Move on the the rest of the list.
		  (get-all-files file_regexp (cdr lst)))))))

(defun as-print(msg)
	(princ msg)
	(terpri))

(defun as-preprocess(src_folder build_folder file_regexp cpp-args)

	;; Make sure that src_folder exists.
	;; If it doesn't, exit.
  (if (not (file-directory-p src_folder))
			(let ((msg (format "as-preprocess: Source folder: %s does not exist!!!" src_folder)))
				(as-print msg)
				(message msg))
		;; Recursively travel the directory structure.
		(let ((files (directory-files src_folder t nil t)))
			(dolist (f files)
				(let ((basename (file-name-nondirectory f)))
					;; If this is a directory, and is not "." or ".."
					(if (and (file-directory-p f)
									 (not (equal basename "."))
									 (not (equal basename "..")))
							(as-preprocess f (concat build_folder "/" basename) file_regexp cpp-args)
						;; f is not a directory. Check if it matches
						;; the regexp.
						(when (string-match file_regexp basename)
							;; Match
							;;(as-print (format "processing: %s\n" basename))
							;; Create build folder, if it doesn't exist.
							(when (not (file-directory-p build_folder))
								(as-print (format "Build folder does not exist. Creating %s" build-folder))
								(make-directory build_folder t))
							(let ((build-file (concat build_folder "/" basename)))
								;; Check if src is newer than build
								;; *** We aren't performing this check right now because the resolution
								;;     on the timer is only to the minute.
								;;(if (file-newer-than-file-p f build-file)
								(progn
									(as-print (format "Processing: %s" basename))
									(as-print (shell-command-to-string (format "cpp %s -P %s %s" cpp-args f build-file))))))))))))

(defun get-or-ask(s p q)
	(if (memq p (symbol-plist s))
			(get s p)
		(eval q)))

(defun launch-flash-tracer()
	(interactive)
  (if (get-buffer "*flash-tracer*")
			(progn
				;; Clear the buffer.
				(save-current-buffer
					(set-buffer "*flash-tracer*")
					(erase-buffer))
				(if (not (get-process "flash-tracer"))
						;; Buffer exists, but process doesn't.
						(progn
							(message "Starting Flash tracer...")
							(start-process "flash-tracer" "*flash-tracer*" "python" "/home/astro/projects/applications/flash_tracer_socket/main.py" "&")
							(message "Done."))))
		;; Neither buffer nor process exist.
    (progn
			(message "Starting Flash tracer...")
			(start-process "flash-tracer" "*flash-tracer*" "python" "/home/astro/projects/applications/flash_tracer_socket/main.py" "&")
      (message "Done."))))

(defun show-dev-screens ()
;	(let ((cb "*flash-tracer*"))
	(let ((cb "*slime-repl sbcl*"))
		(switch-to-buffer-other-window "*as_build*")
		(goto-char (point-max))
		(switch-to-buffer-other-window cb)))

(defun as-mtasc(main-file build-folder swf-name mtasc-frame swf-width swf-height swf-frame-rate custom-trace use-existing-swf)
	"Compile a swf using mtasc."

	(let ((compile-string (format "mtasc -main -cp ~/Software/Classes -mx -cp ~/Software/mtasc/ocaml/mtasc/std -cp %sas -swf %s%s -frame %s -trace %s" build-folder build-folder swf-name mtasc-frame custom-trace)))

		(unless use-existing-swf
			(setq compile-string (format "%s -header %s:%s:%s" compile-string swf-width swf-height swf-frame-rate)))

		(when using-flash8
			(setq compile-string (format "%s -version 8 -cp ~/Software/mtasc/ocaml/mtasc/std8" compile-string)))

		(setq compile-string (format "%s %s" compile-string main-file))

		(as-print (format "Compile command: %s" compile-string))

		;; Run MTASC. Return the output.
		(shell-command-to-string compile-string)))

(defun find-mtasc-error(mtasc-output build-folder source-folder)

	(let ((error-regexp (concat "^" (expand-file-name build-folder) "\\([\\/_[:alnum:]]*\.as\\):\\([0-9]*\\): characters \\([0-9]*\\)-\\([0-9]*\\) : \\(.*\\)")))

		(as-print "Parsing error output...")
		(as-print mtasc-output)

		;; if there was a match...
		(if (string-match error-regexp mtasc-output)
				(let ((error-path (match-string 1 mtasc-output))
							(error-line (string-to-number (match-string 2 mtasc-output)))
							(error-start-char (string-to-number (match-string 3 mtasc-output)))
							(error-end-char (string-to-number (match-string 4 mtasc-output)))
							(error-msg (match-string 5 mtasc-output)))

					(as-print error-path)

					;;;; Because of the preprocessor, the build and source files
					;;;; may differ. Therefore, we execute the following steps:

					(let ((source-buffer nil)
								(build-buffer nil)
								(offending-text nil)
								(offending-start-pos nil)
								(offending-end-pos nil)
								(match-list nil))

						;;; 1. Find the error in the build file.
						(setq build-buffer (find-file-read-only (concat build-folder error-path)))
						(goto-line error-line)
						(forward-char error-start-char)

						;;; 2. Record the offending text listed in the error.
						(setq offending-start-pos (point))
						(setq offending-end-pos (+ (point) (- error-end-char error-start-char)))
						(setq offending-text (buffer-substring-no-properties offending-start-pos offending-end-pos))

						(as-print (format "Offending text: %s" offending-text))
						(as-print (format "line: %s" error-line))

						;;; 3. Open the source file
						(setq source-buffer (find-file (concat source-folder error-path)))
						(set-mark (point))
						(beginning-of-buffer)

						;;; 4. Search for matching text in the source file.

						;; Collect all the potential matches.
						(let ((case-fold-search nil))
							(while (search-forward offending-text nil t)
								(push `((match-start-pos . ,(- (point) (- error-end-char error-start-char)))
												(match-end-pos . ,(point)))
											match-list)))

						(setq match-list (reverse match-list))

						(if match-list
								(let ((best-match nil)
											(best-distance (point-max)))

									(if (> (length match-list) 1)
											;; We need to find the correct match.
											;; Try to find the match with the closest starting position to the
											;; build version. ***This could be more sophisticated/accurate.***
											(dolist (elt match-list best-match)
												(let ((distance (abs ( - offending-start-pos (cdr (assoc 'match-start-pos elt))))))
													(when (< distance best-distance)
														(setq best-match elt)
														(setq best-distance distance))))
										;; There was only one match, so this must be it.
										(setq best-match (car match-list)))

									(as-print "Found error in source file.")

												;;; 5a. Highlight the text.
									(let ((best-match-start-pos (cdr (assoc 'match-start-pos best-match)))
												(best-match-end-pos (cdr (assoc 'match-end-pos best-match))))

										(as-print (format "start pos: %s" best-match-start-pos))
										(as-print (format "end pos: %s" best-match-end-pos))

										(setq mtasc-error-overlay (make-overlay best-match-start-pos best-match-end-pos))
										(overlay-put mtasc-error-overlay 'face '(background-color . "#6a0000"))

										;; put point at the start of the error
										(goto-char best-match-start-pos)

										;; Show block if hidden by hideshow.
										(save-excursion
											(hs-show-block))

										;; close the build file
										(kill-buffer build-buffer)))

										;;; 5b. If we cannot find a match in the source file, the
										;;;     the error probably occurs in a macro expansion. In
										;;;     this case, we will highlight the error in the build
										;;;     file.
							(as-print "Could not find error in source file.")
							(switch-to-buffer build-buffer)
							(setq mtasc-error-overlay (make-overlay offending-start-pos offending-end-pos))
							(overlay-put mtasc-error-overlay 'face '(background-color . "#3a0000"))))

					(as-print "\n")
					(as-print error-msg))

			;; There was an error, but we couldn't parse the output.
			(as-print "\n")
			(as-print (format "UNKNOWN ERROR: %s" mtasc-output))
			(message mtasc-output))

		(let ((cb (current-buffer)))
			(switch-to-buffer-other-window "*as_build*")
			(goto-char (point-max))
			(switch-to-buffer-other-window cb))))

(defun as-flash-compile(source-folder build-folder swf-name swf-width swf-height swf-frame-rate main-file custom-trace use-existing-swf using-flash8 server-path copy-command launch-browser cpp-args browser-target mtasc-frame)
  "Compile Flash movie. Reload in browser. Check for errors."

  (interactive
   (list
		(get-or-ask 'actionscript-mode 'source-folder '(read-file-name "Which is the source folder?" "src"))
		(get-or-ask 'actionscript-mode 'build-folder '(read-file-name "Which is the build folder?" "src"))
		(get-or-ask 'actionscript-mode 'swf-name '(read-string "What is the swf's name?" "fireworks.swf"))
		(get-or-ask 'actionscript-mode 'swf-width '(read-string "[mtasc] Width?" "360"))
		(get-or-ask 'actionscript-mode 'swf-height '(read-string "[mtasc] Height?" "558"))
		(get-or-ask 'actionscript-mode 'swf-frame-rate '(read-string "[mtasc] Frame Rate?" "24"))
		(get-or-ask 'actionscript-mode 'main-file '(read-string "[mtasc] Main file?"))
		(get-or-ask 'actionscript-mode 'custom-trace '(read-string "[mtasc] Custom trace method?"))
		(get-or-ask 'actionscript-mode 'use-existing-swf '(y-or-n-p "[mtasc] Use existing swf?"))
		(get-or-ask 'actionscript-mode 'using-flash8 '(y-or-n-p "[mtasc] Using Flash 8?"))
		(get-or-ask 'actionscript-mode 'server-path '(read-string "What is the path to the server?" "local"))
		(get-or-ask 'actionscript-mode 'copy-command '(read-string "Copy command"))
		(get-or-ask 'actionscript-mode 'launch-browser '(y-or-n-p "Launch Browser?"))
		(get-or-ask 'actionscript-mode 'cpp-args '(read-string "cpp args?"))
		(get-or-ask 'actionscript-mode 'browser-target '(read-string "browser target"))
		(get-or-ask 'actionscript-mode 'mtasc-frame '(read-string "[mtasc] frame?"))))

  (save-some-buffers)

 	(let ((standard-output (get-buffer-create "*as_build*")))

		(let ((wd (getenv "PWD")))

			(with-current-buffer (get-buffer "*as_build*")
				(goto-char (point-max)))

			(as-print "\n---Starting Build---\n")

			;; run the preprocessor
			(as-print "Running preprocessor...")
			(as-print "-----------------------")
			(as-preprocess source-folder build-folder "^.*\.as$" cpp-args)

			;; copy a clean swf over
			;; (only if use-existing-swf)
			(when use-existing-swf
				(as-print (format "\nCopying swf: %s" swf-name))
				(as-print "--------------")
				(let ((original-swf-name (get 'actionscript-mode 'swf-original-name)))
					(copy-file (concat source-folder original-swf-name) (concat build-folder swf-name) t)))

			;; copy any additional files over, if newer than build versions.
			(as-print "\nCopying additional files...")
			(as-print "---------------------------")
			(as-print (shell-command-to-string (format "rsync --update --cvs-exclude --exclude \"*.as\" --archive --verbose %s %s" source-folder build-folder)))

			;; create the html page, if necessary.
			(let ((html-page (concat build-folder "index.html")))
				(unless (file-exists-p html-page)
					(with-temp-file html-page
						(insert-file-contents "/home/astro/.emacs.d/site-lisp/swf_html_template")
						(perform-replace "#TITLE#" swf-name nil nil nil nil nil (point-min) (point-max))
						(perform-replace "#WIDTH#" swf-width nil nil nil nil nil (point-min) (point-max))
						(perform-replace "#HEIGHT#" swf-height nil nil nil nil nil (point-min) (point-max))
						(perform-replace "#SWFNAME#" swf-name nil nil nil nil nil (point-min) (point-max)))))

			;; run mtasc
			(as-print "\nRunning mtasc...")
			(as-print "----------------")
			(let ((mtasc-output (as-mtasc main-file build-folder swf-name mtasc-frame swf-width swf-height swf-frame-rate custom-trace use-existing-swf)))

				;; delete any error overlays
				(when (boundp 'mtasc-error-overlay)
					(delete-overlay mtasc-error-overlay))

				(if (equal mtasc-output "")
						;; everything must have gone ok.
						(progn
							;; Catch debug messages

							;; Python version
							;(launch-flash-tracer)

							;; SBCL version
							(if (get-buffer "*slime-repl sbcl*")
									(progn
										(set-buffer "*slime-repl sbcl*")
										(slime-repl-clear-buffer))
								(slime)
								;; Need to wait until it's loaded
								(add-hook 'slime-connected-hook (lambda ()
																									(slime-load-system "flash-tracer"))))

							(unless (equal server-path "local")
								;; copy this file over to our staging
								(as-print "\nCopying files to staging server...")
								(as-print "----------------------------------")
								(as-print (format "[%s]" server-path))
								(let ((copy-output (shell-command-to-string (concat copy-command build-folder "* " server-path))))
									(as-print (format "%s" copy-output))))

							;; Launch browser (if required).
							(when launch-browser
								(if (equal server-path "local")
										(browse-url (concat "file://" (expand-file-name build-folder) "index.html"))
									(browse-url browser-target)))

							(as-print (current-time-string))
							(as-print "Success!")
							(message "Success!")

							;(sleep-for 2)	; if we don't do this, then no debug messages will show up

							;; Show the trace output in one window, and
							;; show the build messages in the other.
							;; TODO: Simplify this.
							(show-dev-screens)

							(when (memq 'on-finish-hook (symbol-plist 'actionscript-mode))
								(funcall (get 'actionscript-mode 'on-finish-hook))))

					;; Had output. Must be error.
					;; Parse error message.
					(find-mtasc-error mtasc-output build-folder source-folder))))))

(defun as-path-to-current-file ()

  (let* ((wholePath buffer-file-name)
				 (filename (file-name-nondirectory wholePath))
				 (className (file-name-sans-extension filename)))

		;; We use a regexp to grab everything in the path between the as (root) folder and this file.
		(let ((package-regexp (concat "^.*?as/\\(.*?\\)" filename)))

			;; We'll break this apart to get a list of package names.
			(if (string-match package-regexp wholePath)
					(let ((package-path (match-string 1 wholePath)))
						;; Parse the path into package names.
						(let ((package-list (split-string package-path "/"))
									(package-string ""))

							;; combine the package names into a string
							(dolist (elt package-list)
								(setf package-string (concat package-string elt ".")))
							(concat package-string className)))
				nil))))

(defun as-quick-compile (source-folder custom-trace cpp-args)
	"Compile the current buffer into a temporary swf and launch the browser. This function
is intended for running quick tests of code."

  (interactive
   (list
		(get-or-ask 'actionscript-mode 'source-folder '(read-file-name "Which is the source folder?" "src"))
		(get-or-ask 'actionscript-mode 'custom-trace '(read-string "[mtasc] Custom trace method?"))
		(get-or-ask 'actionscript-mode 'cpp-args '(read-string "cpp args?"))))

	(save-buffer)

 	(let ((standard-output (get-buffer-create "*as_build*")))

		(with-current-buffer (get-buffer "*as_build*")
			(goto-char (point-max)))

		(as-print "\n---Starting Build---\n")

		(let ((main-file "/tmp/as-quick-compile/Quick_Compile_Main.as")
					(build-folder "/tmp/as-quick-compile/")
					(swf-name "temp.swf")
					(mtasc-frame 1)
					(swf-width 100)
					(swf-height 100)
					(swf-frame-rate 24)
					(use-existing-swf nil)
					(using-flash8 t))

			;; run the preprocessor
			(as-print "Running preprocessor...")
			(as-print "-----------------------")
			(as-preprocess source-folder build-folder "^.*\.as$" cpp-args)
			(as-print "\n")

			;;;; Create the generic main class file.
			;; Are we compiling the whole class, or just some "free" code?

			;; We need to get the path to this file.
			(let ((code-to-inject (concat (as-path-to-current-file) ".test()")))
				(with-temp-file (concat build-folder "Quick_Compile_Main.as")
					(insert-file-contents "/home/astro/.emacs.d/site-lisp/Quick_Compile_Template")
					(perform-replace "#CODE#" code-to-inject nil nil nil nil nil (point-min) (point-max))))

			;; run mtasc
			(as-print "\nRunning mtasc...")
			(as-print "----------------")
			(let ((mtasc-output (as-mtasc main-file build-folder swf-name mtasc-frame swf-width swf-height swf-frame-rate custom-trace use-existing-swf)))

				;; delete any error overlays
				(when (boundp 'mtasc-error-overlay)
					(delete-overlay mtasc-error-overlay))

				(if (equal mtasc-output "")
						;; everything must have gone ok.
						(progn
							;; Catch debug messages
							(launch-flash-tracer)

							;; Launch browser.
							(browse-url (concat "file://" (expand-file-name build-folder) swf-name))

							(as-print "Success!")
							(message "Success!")

							(sleep-for 2)	; if we don't do this, then no debug messages will show up

							;; Show the trace output in one window, and
							;; show the build messages in the other.
							;; TODO: Simplify this.
							(show-dev-screens))

					;; Had output. Must be error.
				;; Parse error message.
					(find-mtasc-error mtasc-output build-folder source-folder))))))

(defun as-insert-flash-func()
  (interactive)
  (let ((isStatic (y-or-n-p "Static? "))
		(isPublic (y-or-n-p "Public? "))
		(f-string ""))

	(when isStatic
	  (setf f-string "static "))

	(if isPublic
		(setf f-string (concat f-string "public function "))
	  (setf f-string (concat f-string "private function ")))

	(indent-according-to-mode)
	(insert f-string)

	(save-excursion
	  (insert "(){")
	  (insert "\n\n")
	  (indent-according-to-mode)
	  (insert "}"))))

(defun as-insert-flash-var()
  (interactive)
  (let ((isStatic (y-or-n-p "Static? "))
		(isPublic (y-or-n-p "Public? "))
		(f-string ""))

	(when isStatic
	  (setf f-string "static "))

	(if isPublic
		(setf f-string (concat f-string "public "))
	  (setf f-string (concat f-string "private ")))

	(indent-according-to-mode)
	(insert (concat f-string "var "))))

(defun as-inside-defun?()
	(let ((cur (point))
				(start (as-get-beginning-of-defun))
				(end (as-get-end-of-defun)))
		(and start
				 end
				 (> cur start)
				 (< cur end))))

(defun as-get-beginning-of-defun()
	;; Returns the position.
	(save-excursion
		(when	(re-search-backward as-function-re nil t)
			(match-end 1))))

(defun as-get-end-of-defun()
	;; This only works if we are inside a defun.
	(save-excursion
		(when	(re-search-backward as-function-re nil t)
			(goto-char (match-end 0))
			;; Move back a char, so that point is right on
			;; the opening brace.
			(backward-char)
			(forward-list)
			(point))))

(defun as-get-end-of-defun2()
	;; This should work if we are not inside any defuns.
	(save-excursion
		(beginning-of-line) ; hack, in case point is currently inside a function sig.
		(when	(re-search-forward as-function-re nil t)
			(goto-char (match-end 0))
			;; Move back a char, so that point is right on
			;; the opening brace.
			(backward-char)
			(forward-list)
			(point))))

(defun as-beginning-of-defun()
	(interactive)
	(let ((pos (as-get-beginning-of-defun)))
		(if pos
				(goto-char pos)
			(message "Can't find any functions."))))

(defun as-end-of-defun()
	(interactive)
	(if (as-inside-defun?)
			(goto-char (as-get-end-of-defun))
		(let ((pos (as-get-end-of-defun2)))
			(if pos
					(goto-char pos)
				(message "Can't find any functions.")))))

(defun as-mark-defun()
	(interactive)
	(let ((start (as-get-beginning-of-defun))
				(end (as-get-end-of-defun)))
		(if (not (or start end))
				(message "Can't find any functions.")
			(set-mark end)
			(goto-char start)
			(beginning-of-line))))

(defun toggle-build-source (source-folder build-folder)
	"If we are visiting a file in the source folder, open the corresponding file
in the build folder, and vice-versa."
  (interactive
   (list
		(get-or-ask 'actionscript-mode 'source-folder '(read-file-name "Which is the source folder?" "src"))
		(get-or-ask 'actionscript-mode 'build-folder '(read-file-name "Which is the build folder?" "src"))))

	;; get the path to the current buffer
  (let ((wholePath buffer-file-name)
				(build-regexp (concat "^" (expand-file-name build-folder) "\\(.*\\)"))
				(source-regexp (concat "^" (expand-file-name source-folder) "\\(.*\\)")))

		;; check if it is within the build or the source folder
		(if (string-match source-regexp wholePath)
				; We are in the source directory
				;;(message (concat "source: " (match-string 1 wholePath)))
				(find-file (concat build-folder (match-string 1 wholePath)))
			(if (string-match build-regexp wholePath)
					; we are in the build directory
					(find-file (concat source-folder (match-string 1 wholePath)))
				(message "Not in build or source folder.")))))

;;(when running-on-x
;;		(setq special-display-buffer-names
;;					'("*as_build*")))

(provide 'actionscript-mode)
