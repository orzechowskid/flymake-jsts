;;; flymake-jsts-biome.el --- biome support for flymake-jsts -*- lexical-binding: t; -*-

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


(defun flymake-jsts/biome-get-help-message (diagnostic)
	"Internal function.  Extracts a help string (if present) from a biome
diagnostic message in JSON form."
	(when-let* ((advices (gethash "advices"
													 diagnostic))
							(actual-advices (gethash "advices"
																			 advices))
							(first-advice (when (length> actual-advices
																					 0)
															(elt actual-advices
																	 0)))
							(advice-log (gethash "log"
																	 first-advice))
							(content-array (when (length> advice-log
																						0)
															 (elt advice-log
																		1)))
							(first-content-object (when (length> content-array
																									 0)
																		(elt content-array
																				 0))))
		(gethash "content"
						 first-content-object)))

(defun flymake-jsts/biome-report-diags (source-buffer lint-buffer)
	"Internal function.  Parses biome output (expressed in JSON) and returns a
list of Flymake diagnostic messages.

SOURCE-BUFFER is the buffer containing the user's source code; LINT-BUFFER is
the buffer containing biome's own output."
	(flymake-jsts/message "biome report-diags %s %s" source-buffer lint-buffer)
	;; the JSON in an biome buffer currently looks like this:
	;;
	;; {
	;;   diagnostics: [{
	;;     category: string;  // rule id
	;;     message: [{
	;;       content: string;
	;;     }];
	;;     severity: string;
	;;     location: {
	;;       span: [
	;;         number, number // start pos, end pos
	;;       ];
	;;     };
	;;     advices: {
	;;       advices: [{
	;;         log: [ string, [{
	;;           "content": string;
	;;         }]];
	;;       }];
	;;     };
	;;   }];
	;; }
	;;
	;; (there's other stuff in there too but we currently don't use it)
	;; the shape of biome's JSON output is subject to change, as its JSON
	;; interface is still marked as experimental.
	(condition-case nil
			(seq-reduce (lambda (acc el-d)
										(flymake-jsts/message "current diagnostic: %s" el-d)
										(let* ((raw-message (gethash "content"
																								 (elt (gethash "message"
																															 el-d)
																											0)))
													 (rule-id (gethash "category"
																						 el-d))
													 (message-with-rule (if flymake-jsts-show-rule-name
																									(format "[%s] %s"
																													rule-id
																													raw-message)
																								raw-message))
													 (severity (if (string= (gethash "severity"
																													 el-d)
																									"error")
																				 flymake-jsts-error-type
																			 flymake-jsts-warning-type))
													 (start-pos (elt (gethash "span"
																										(gethash "location"
																														 el-d))
																					 0))
													 (end-pos (elt (gethash "span"
																									(gethash "location"
																													 el-d))
																				 1))
													 (help-message (flymake-jsts/biome-get-help-message el-d))
													 (message-with-help (if (and flymake-jsts-show-extended-info
																											 help-message)
																									(format "%s %s"
																													message-with-rule
																													help-message)
																								message-with-rule)))
											(append acc
															(list (flymake-make-diagnostic source-buffer
																														 (1+ start-pos)
																														 (1+ end-pos)
																														 severity
																														 message-with-help
																														 (list :rule-name
																																	 rule-id))))))
									(gethash "diagnostics"
													 (with-current-buffer lint-buffer
														 (json-parse-string (buffer-substring (save-excursion
																																		(goto-char (point-min))
																																		(next-line)
																																		(point))
																																	(save-excursion
																																		(goto-char (point-max))
																																		(search-backward "}")
																																		(forward-char)
																																		(point))))))
									'())
		('(debug json-parse-error) (list (flymake-jsts/get-error-diag source-buffer)))))

(defun flymake-jsts/biome-get-command (file-name source-buffer)
	"Internal function.  Returns a list of strings which represents a shell
command for running biome.  SOURCE-BUFFER is a buffer containing code to lint."
	(list (cdr (assoc 'biome flymake-jsts-executable-name-alist))
				"lint"
				"--reporter"
				"json"
				file-name))

(defun flymake-jsts/biome-create-process (source-buffer callback)
	"Internal function.  Runs biome on the contents of SOURCE-BUFFER then invokes
CALLBACK.

CALLBACK should be a function which takes a single argument, LINT-BUFFER,
containing the output from the lint process."
	(flymake-jsts/message "biome linting: >%s<" source-buffer)
	;; biome deliberately does not write diagnostics to stdout or stderr when
	;; using --stdin, so we have to write a temp file somewhere and check that
	;; instead (sigh): https://github.com/biomejs/biome/pull/3726
	(let* ((file-name (if (file-exists-p (buffer-file-name source-buffer))
												buffer-file-name
											(make-temp-file nil nil nil
																			(with-current-buffer source-buffer
																				(buffer-string)))))
				 (args (flymake-jsts/biome-get-command file-name source-buffer))
				 (success-callback (lambda (process status buffer)
														 (flymake-jsts/message "biome-create-process: no diags")
														 (funcall callback buffer)))
				 (error-callback (lambda (process status buffer)
													 (flymake-jsts/message "biome-create-process: error")
													 (funcall callback buffer)))
				 (cwd (flymake-jsts/get-process-cwd 'biome
																						source-buffer))
				 (proc (pfuture-callback args
								 :connection-type 'pipe
								 :directory cwd
								 :on-success success-callback
								 :on-error error-callback)))
		(progn
			(process-send-string proc (with-current-buffer source-buffer
																	(buffer-string)))
			(process-send-eof proc)
			proc)))

;;;###autoload
(defun flymake-jsts-biome-check-and-report (report-fn &rest _ignored)
	"Generates Flymake diagnostics based on biome output.  Can be used in
`flymake-diagnostic-functions'."
	(flymake-jsts/message "linting with biome")
	(flymake-jsts/check-and-report (current-buffer)
																 #'flymake-jsts/biome-create-process
																 #'flymake-jsts/biome-report-diags
																 report-fn))


(provide 'flymake-jsts-biome)
;;; flymake-jsts-biome.el ends here
