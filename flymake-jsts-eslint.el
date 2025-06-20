;;; flymake-jsts-eslint.el --- eslint support for flymake-jsts -*- lexical-binding: t; -*-

;; Author: Dan Orzechowski

;;; Commentary:

;; This file, like all other adapters in this package, contains the following
;; functions:
;;
;; - a function which is suitable for use as a Flymake backend
;; - a function which generates a shell command to launch a lniter
;; - a function which actually creates the subprocess
;; - a function which turns linter output into Flymake diagnostic messages

;;; Code:


(require 'flymake-jsts-utils)


(defun flymake-jsts/eslint-get-command (source-buffer)
	"Internal function.  Returns a list of strings which represents a shell
command for running eslint.  SOURCE-BUFFER is a buffer containing code to lint."
	(list (cdr (assoc 'eslint
										flymake-jsts-executable-name-alist))
				"--no-color"
				"--no-ignore"
				"--format"
				"json"
				"--stdin"
				"--stdin-filename"
				(or (buffer-file-name source-buffer) (buffer-name source-buffer))))

(defun flymake-jsts/eslint-report-diags (source-buffer lint-buffer)
	"Internal function.  Parses eslint output (expressed in JSON) and returns a
list of Flymake diagnostic messages.

SOURCE-BUFFER is the buffer containing the user's source code; LINT-BUFFER is
the buffer containing eslint's own output."
	(flymake-jsts/message "eslint report-diags")
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

(defun flymake-jsts/eslint-create-process (source-buffer callback)
	"Internal function.  Runs eslint on the contents of SOURCE-BUFFER then invokes
CALLBACK.

CALLBACK should be a function which takes a single argument, LINT-BUFFER,
containing the output from the lint process."
	(let* ((args (flymake-jsts/eslint-get-command source-buffer))
				 (success-callback (lambda (process status buffer)
														 (message "cb")
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


;;;###autoload
(defun flymake-jsts-eslint-check-and-report (report-fn &rest _ignored)
	"Generates Flymake diagnostics based on eslint output.  Can be used in
`flymake-diagnostic-functions'."
	(flymake-jsts/check-and-report (current-buffer)
																 #'flymake-jsts/eslint-create-process
																 #'flymake-jsts/eslint-report-diags
																 report-fn))


(provide 'flymake-jsts-eslint)
;;; flymake-jsts-eslint.el ends here
