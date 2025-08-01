;;; flymake-jsts-oxlint.el --- oxlint support for flymake-jsts -*- lexical-binding: t; -*-

;; Author: Dan Orzechowski

;;; Commentary:

;; This file, like all other adapters in this package, contains the following
;; functions:
;;
;; - a function which is suitable for use as a Flymake backend
;; - a function which generates a shell command to launch a linter
;; - a function which actually creates the subprocess
;; - a function which turns linter output into Flymake diagnostic messages

;;; Code:


(require 'pfuture)

(require 'flymake-jsts-utils)


(defun flymake-jsts/oxlint-report-diags (source-buffer lint-buffer)
	"Internal function.  Parses oxlint output (expressed in JSON) and returns a
list of Flymake diagnostic messages.

SOURCE-BUFFER is the buffer containing the user's source code; LINT-BUFFER is
the buffer containing oxlint's own output."
	(flymake-jsts/message "oxlint report-diags")
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
													 (message-with-rule (if flymake-jsts-show-rule-name
																									(format "[%s] %s"
																													rule-id
																													raw-message)
																								message))
													 (help-message (gethash "help"
																									el-d))
													 (message-with-help (if (and flymake-jsts-show-extended-info
																											 help-message)
																									(format "%s %s"
																													message-with-rule
																													help-message)
																								main-message))
													 (severity (if (string= (gethash "severity"
																													 el-d)
																									"warning")
																				 flymake-jsts-warning-type
																			 flymake-jsts-error-type))
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

(defun flymake-jsts/oxlint-get-command (file-name source-buffer)
	"Internal function.  Returns a list of strings which represents a shell
command for running oxlint.  SOURCE-BUFFER is a buffer containing code to lint."
	(list (cdr (assoc 'oxlint flymake-jsts-executable-name-alist))
				"-f"
				"json"
				file-name))

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

;;;###autoload
(defun flymake-jsts-oxlint-check-and-report (report-fn &rest _ignored)
	"Generates Flymake diagnostics based on oxlint output.  Can be used in
`flymake-diagnostic-functions'."
	(flymake-jsts/check-and-report (current-buffer)
																 #'flymake-jsts/oxlint-create-process
																 #'flymake-jsts/oxlint-report-diags
																 report-fn))


(provide 'flymake-jsts-oxlint)
;;; flymake-jsts-oxlint.el ends here
