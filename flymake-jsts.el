;;; flymake-jsts.el --- A Flymake backend for Javascript and Typescript  -*- lexical-binding: t; -*-

;; Version: 1.0.0
;; Author: Dan Orzechowski
;; URL: https://github.com/orzechowskid/flymake-jsts
;; Package-Requires: ((emacs "29") (pfuture "1.10.3"))
;; Keywords: languages, tools, lint, javascript, typescript

;;; Commentary:

;; A backend for Flymake which supports a variety of linters found in the
;; Javascript/Typescript ecosystem.

;;; License: GPLv3

;;; Code:


(require 'flymake)
(require 'seq)

(require 'pfuture)
(when (featurep 'project)
	(require 'project))


(defgroup flymake-jsts nil
	"Flymake checker for Javascript/Typescript."
	:group 'programming
	:prefix "flymake-jsts-")


(defcustom flymake-jsts-executable-name-alist
	'((eslint . "eslint")
		(oxlint . "oxlint"))
	"Mapping of linters to binary names."
	:type '(alist :key-type (symbol :tag "mode")
								:value-type (string :tag "binary name"))
	:group 'flymake-jsts)

(defcustom flymake-jsts-project-markers-alist
	'((eslint . ("eslint.config.js" "eslint.config.mjs" "eslint.config.cjs" "package.json"))
		(oxlint . (".oxlintrc.json" "package.json")))
	"Mapping of linters to 'project markers': files which denote the root of a
project tree to which linting is applied."
	:type '(alist :key-type (symbol :tag "mode")
								:value-type (repeat string))
	:group 'flymake-jsts)

(defcustom flymake-jsts-show-rule-name t
	"Non-nil to append rule name to diagnostic message, or nil to suppress."
	:type 'boolean
	:group 'flymake-jsts)

(defcustom flymake-jsts-show-extended-info t
	"Non-nil to append extra information to each diagnostic message (when provided
by your linter), or nil to supporess."
	:type 'boolean
	:group 'flymake-jsts)


(defvar flymake-jsts/debug nil
	"Internal variable.  Set to non-nil to enable debug logging.")


(defun flymake-jsts/message (&rest args)
	"Internal function.  Conditionally emit debug messages."
	(when flymake-jsts/debug
		(apply #'message args)))

(defun flymake-jsts/get-error-diag (source-buffer)
	"Internal function.  Wraps buffer contents in a fake linter message in the
case of linter crash or malfunction."
	(flymake-jsts/message "get error diags")
	(flymake-make-diagnostic source-buffer
													 0
													 1
													 :error
													 (with-current-buffer source-buffer
														 (buffer-substring-no-properties (point-min) (point-max)))))

(defun flymake-jsts/get-pos-from-line-and-column (line column source-buffer)
	"Internal function.  Returns the postion in SOURCE-BUFFER pointed to by LINE
and COLUMN."
	(with-current-buffer source-buffer
		(save-excursion
			(goto-char (point-min))
			(forward-line (1- line))
			(forward-char (1- column))
			(point))))

(defun flymake-jsts/eslint-json-to-diags (source-buffer lint-buffer)
	"Internal function.  Parses eslint output (expressed in JSON) and returns a
list of Flymake diagnostic messages.

SOURCE-BUFFER is the buffer containing the user's source code; LINT-BUFFER is
the buffer containing eslint's own output."
	(flymake-jsts/message "eslint json-to-diags")
	;; the JSON in an eslint buffer looks like this:
	;;
	;; [{
	;;   messages: [{
	;;     ruleId: string;
	;;     severity: number;
	;;     message: string;
	;;     line: number;
	;;     column: number;
	;;     endLint: number;
	;;     endColumn: number;
	;;   }];
	;; }]
	;;
	;; (there's other stuff in there too but we currently don't use it)
	(seq-map (lambda (el)
						 (flymake-jsts/message "current message: %s" el)
						 (let* ((start-column (gethash "column"
																					 el))
										(end-column (gethash "endColumn"
																				 el))
										(start-line (gethash "line"
																				 el))
										(end-line (gethash "endLine"
																			 el))
										(raw-message (gethash "message"
																					el))
										(rule-id (gethash "ruleId"
																			el))
										(message (if flymake-jsts-show-rule-name
																 (format "%s [%s]"
																				 raw-message
																				 rule-id)
															 raw-message))
										(severity (if (equal (gethash "severity"
																									el)
																				 1)
																	:warning
																:error))
										(start-pos (flymake-jsts/get-pos-from-line-and-column start-line
																																					start-column
																																					source-buffer))
										(end-pos (if (and end-line
																			end-column)
																 (flymake-jsts/get-pos-from-line-and-column end-line
																																						end-column
																																						source-buffer)
															 ;; no end-position info found; let Flymake itself
															 ;; take a guess
															 (save-match-data
																 (cdr (flymake-diag-region source-buffer
																													 start-line
																													 start-column))))))
										(flymake-make-diagnostic source-buffer
																						 start-pos
																						 end-pos
																						 severity
																						 message
																						 (list :rule-name
																									 rule-id))))
					 ;; map over eslint rules if present or our own fake message if an error
					 ;; was caught
					 (condition-case nil
							 (gethash "messages"
												(elt (with-current-buffer lint-buffer
															 (progn
																 (goto-char (point-min))
																 (json-parse-buffer)))
														 0))
						 ('(debug json-parse-error) (flymake-jsts/get-error-diags source-buffer)))))

(defun flymake-jsts/oxlint-json-to-diags (source-buffer lint-buffer)
	"Internal function.  Parses oxlint output (expressed in JSON) and returns a
list of Flymake diagnostic messages.

SOURCE-BUFFER is the buffer containing the user's source code; LINT-BUFFER is
the buffer containing oxlint's own output."
	(flymake-jsts/message "oxlint json-to-diags")
	;; the JSON in an oxlint buffer looks like this:
	;;
	;; {
	;;   diagnostics: [{
	;;     message: string;
	;;     code: string;  // rule id
	;;     severity: string;
	;;     help: string;
	;;     labels: [{
	;;       label: string;  // context-sensitive help string
	;;       span: {
	;;         offset: number;
	;;         length: number;
	;;       };
	;;     }];
	;;   }];
	;; }
	;;
	;; (there's other stuff in there too but we currently don't use it)
	(condition-case nil
			(seq-reduce (lambda (acc el-d)
										(flymake-jsts/message "current diagnostic: %s" el-d)
										(let* ((raw-message (gethash "message"
																								 el-d))
													 (rule-id (gethash "code"
																						 el-d))
													 (message-with-rule (if t;;flymake-jsts-show-rule-name
																									(format "[%s] %s"
																													rule-id
																													raw-message)
																								message))
													 (help-message (gethash "help"
																									el-d))
													 (message-with-help (if (and t;;flymake-jsts-show-extended-info
																											 help-message)
																									(format "%s %s"
																													message-with-rule
																													help-message)
																								main-message))
													 (severity (if (string= (gethash "severity"
																													 el-d)
																									"warning")
																				 :warning
																			 :error))
													 (labels (gethash "labels"
																						el-d)))
											(append acc
															(if (or (not labels)
																			(= (length labels)
																				 0))
																	;; if no labels then no position information
																	;; but we still want to display the reported
																	;; diagnostic somewhere
																	(list (flymake-make-diagnostic source-buffer
																																 1
																																 2
																																 severity
																																 message-with-help))
																;; this diagnostic comes with labels; turn each
																;; one into its own flymake diagnostic message
																(seq-map (lambda (el-l)
																					 (let* ((pos-info (gethash "span"
																																		 el-l))
																									(start-pos (1+ (gethash "offset"
																																					pos-info)))
																									(end-pos (+ (gethash "length"
																																			 pos-info)
																															start-pos)))
																						 (flymake-make-diagnostic source-buffer
																																			start-pos
																																			end-pos
																																			severity
																																			message-with-help
																																			(list :rule-name
																																						rule-id))))
																				 labels)))))
									(gethash "diagnostics"
													 (with-current-buffer lint-buffer
														 (progn
															 (goto-char (point-min))
															 (json-parse-buffer))))
									'())
		('(debug json-parse-error) (list (flymake-jsts/get-error-diag source-buffer)))))

(defun flymake-jsts/eslint-get-command (source-buffer)
	"Internal function.  Returns a list of strings which represents a shell
command for running eslint.  SOURCE-BUFFER is a buffer containing code to lint."
	`(,(cdr (assoc 'eslint flymake-jsts-executable-name-alist))
		"--no-color"
		"--no-ignore"
		"--format"
		"json"
		"--stdin"
		"--stdin-filename"
		,(or (buffer-file-name source-buffer) (buffer-name source-buffer))))

(defun flymake-jsts/oxlint-get-command (file-name source-buffer)
	"Internal function.  Returns a list of strings which represents a shell
command for running oxlint.  SOURCE-BUFFER is a buffer containing code to lint."
	`(,(cdr (assoc 'oxlint flymake-jsts-executable-name-alist))
		"-f"
		"json"
		,file-name))

(defun flymake-jsts/get-process-cwd (lint-mode source-buffer)
	"Internal function.  Finds the directory from which the current lint process
should be invoked.  LINT-MODE is the current lint mode; SOURCE-BUFFER is the
buffer containing the user's source code."
	(with-current-buffer source-buffer
		;; TODO: see if this works for files not yet written to disk
		(locate-dominating-file default-directory
														(lambda (directory)
															(seq-find (lambda (project-marker)
																					(file-exists-p (expand-file-name project-marker
																																					 directory)))
																				(cdr (assoc lint-mode
																										flymake-jsts-project-markers-alist)))))))
															 

(defun flymake-jsts/eslint-create-process (source-buffer callback)
	"Internal function.  Runs eslint on the contents of SOURCE-BUFFER then invokes
CALLBACK.

CALLBACK should be a function which takes a single argument, LINT-BUFFER,
containing the output from the lint process."
	(let* ((args (flymake-jsts/eslint-get-command source-buffer))
				 (success-callback (lambda (process status buffer)
														 (flymake-jsts/message "eslint-create-process: no diags")
														 (funcall callback buffer)))
				 (error-callback (lambda (process status buffer)
													 (funcall callback buffer)))
				 (cwd (flymake-jsts/get-process-cwd 'eslint
																						source-buffer))
				 (proc (pfuture-callback args
								 :connection-type 'pipe
								 :directory cwd
								 :on-success success-callback
								 :on-error error-callback)))
		(flymake-jsts/message "eslint-create-process\n args: %s\n directory: %s"
													args
													cwd)
		(progn
			(process-send-string proc (with-current-buffer source-buffer
																	(buffer-string)))
			(process-send-eof proc)
			proc)))

(defun flymake-jsts/oxlint-create-process (source-buffer callback)
	"Internal function.  Runs oxlint on the contents of SOURCE-BUFFER then invokes
CALLBACK when complete.

CALLBACK should be a function which takes a single argument, LINT-BUFFER,
containing the output from the lint process."
	;; oxlint does not currently support reading from stdin, so we need to
	;; (possibly) create a temporary file for it to read then delete it when we're
	;; done with it
	(let* ((file-name (if (file-exists-p (buffer-file-name source-buffer))
												buffer-file-name
											(make-temp-file nil nil nil
																			(with-current-buffer source-buffer
																				(buffer-string)))))
				 (args (flymake-jsts/oxlint-get-command file-name source-buffer))
				 (our-callback (lambda (buffer)
												 (unless (file-exists-p (buffer-file-name source-buffer))
													 (flymake-jsts/message "deleting oxlint temp file")
													 (delete-file temp-file-name))
												 (funcall callback buffer)))
				 (success-callback (lambda (process status buffer)
														 (funcall our-callback buffer)))
				 (error-callback (lambda (process status buffer)
													 (funcall our-callback buffer)))
				 (proc (pfuture-callback args
								 :directory (flymake-jsts/get-process-cwd 'oxlint
																													source-buffer)
								 :on-success success-callback
								 :on-error error-callback)))
		(flymake-jsts/message "oxlint-create-process\n args: %s\n directory: %s"
													args
													(flymake-jsts/get-process-cwd 'oxlint source-buffer))
		proc))

(defun flymake-jsts/check-and-report (source-buffer
																			get-process-fn
																			report-diags-fn
																			report-fn)
	"Internal function.  A generic function which spawns an external process then
generates Flymake diagnostics based on its output.  This is done by delegating
to, and coordinating the output of, its helper functions.

SOURCE-BUFFER should be a buffer containing the source code to lint.

GET-PROCESS-FN should be a function which takes two arguments (a source buffer
and a callback function) and spawns an external process which passes a lint
buffer to the callback function when terminated.  The return value of
GET-PROCESS-FN is currently ignored.

GENERATE-DIAGS-FN should be a function which takes two arguments (a source
buffer and a linter-output buffer) and returns one value (a list of Flymake
diagnostic messages).

REPORT-FN is Flymake's own `report-fn`."
	(if (not (string-empty-p (with-current-buffer source-buffer
														 (buffer-string))))
			;; ask GET-PROCESS-FN to spawn a process
			(funcall get-process-fn
							 source-buffer
							 ;; callback invoked when 
							 (lambda (lint-buffer)
								 (funcall report-fn
													;; ask REPORT-DIAGS-FN to create a list of messages
													(funcall report-diags-fn
																	 source-buffer
																	 lint-buffer))))
		(flymake-jsts/message "buffer is empty")
		(funcall report-fn
						 (list))))

(defun flymake-jsts-eslint-check-and-report (report-fn &rest _ignored)
	"Generates Flymake diagnostics based on eslint output.  Can be used in
`flymake-diagnostic-functions'."
	(flymake-jsts/check-and-report (current-buffer)
																 #'flymake-jsts/eslint-create-process
																 #'flymake-jsts/eslint-json-to-diags
																 report-fn))

(defun flymake-jsts-oxlint-check-and-report (report-fn &rest _ignored)
	"Generates Flymake diagnostics based on oxlint output.  Can be used in
`flymake-diagnostic-functions'."
	(flymake-jsts/check-and-report (current-buffer)
																 #'flymake-jsts/oxlint-create-process
																 #'flymake-jsts/oxlint-json-to-diags
																 report-fn))

(defun flymake-jsts-eslint-enable ()
	"Convenience function to register eslint as a creator of Flymake diagnostics."
	(interactive)
	(add-hook 'flymake-diagnostic-functions
						#'flymake-jsts-eslint-check-and-report))

(defun flymake-jsts-eslint-disable ()
	"Unregisters eslint as a creator of Flymake diagnostics."
	(interactive)
	(remove-hook 'flymake-diagnostic-functions
							 #'flymake-jsts-eslint-check-and-report))

(defun flymake-jsts-oxlint-enable ()
	"Convenience function to register oxlint as a creator of Flymake diagnostics."
	(interactive)
	(add-hook 'flymake-diagnostic-functions
						#'flymake-jsts-oxlint-check-and-report))

(defun flymake-jsts-oxlint-disable ()
	"Unregisters oxlint as a creator of Flymake diagnostics."
	(interactive)
	(remove-hook 'flymake-diagnostic-functions
							 #'flymake-jsts-oxlint-check-and-report))


(provide 'flymake-jsts)
;;; flymake-eslint.el ends here
