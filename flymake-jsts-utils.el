;;; flymake-jsts-eslint.el --- eslint support for flymake-jsts -*- lexical-binding: t; -*-

;; Author: Dan Orzechowski

;;; Code:


(defun flymake-jsts/message (&rest args)
	"Internal function.  Conditionally emit debug messages."
	(when flymake-jsts/debug
		(apply #'message args)))

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
															 
(defun flymake-jsts/get-error-diag (source-buffer)
	"Internal function.  Wraps buffer contents in a fake linter message in the
case of linter crash or malfunction."
	(flymake-jsts/message "get error diags")
	(flymake-make-diagnostic source-buffer
													 0
													 1
													 flymake-jsts-error-type
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

(defun flymake-jsts/check-and-report (source-buffer
																			get-process-fn
																			report-diags-fn
																			report-fn)
	"Internal function.  A generic function which spawns an external process then
generates Flymake diagnostics based on its output.  This is done by delegating
to, and coordinating the output of, the helper functions provided as arguments.

SOURCE-BUFFER should be a buffer containing the source code to lint.

GET-PROCESS-FN should be a function which takes two arguments (a source buffer
and a callback function) and spawns the child process.  Its return value is
currently ignored.

GENERATE-DIAGS-FN should be a function which takes two arguments (a source
buffer and a linter-output buffer) and generates a list of Flymake diagnostics.
Its return value should be that list.

REPORT-FN is Flymake's own report-fn."
	(if (string-empty-p (with-current-buffer source-buffer
												(buffer-string)))
			;; nothing to do; report an empty list
			(progn
				(flymake-jsts/message "buffer is empty")
				(funcall report-fn
								 (list)))
		(setq-local flymake-jsts/current-report-fn report-fn)
		;; ask GET-PROCESS-FN to spawn a process
		(funcall get-process-fn
						 source-buffer
						 (lambda (lint-buffer)
							 ;; ensure we're using a non-destroyed buffer, as well as the
							 ;; report-fn belonging to the most recent Flymake request
							 (when (and (buffer-live-p source-buffer)
													(eq (buffer-local-value 'flymake-jsts/current-report-fn
																									source-buffer)
															report-fn))
								 (funcall report-fn
													;; ask REPORT-DIAGS-FN to create a list
													;; of messages
													(funcall report-diags-fn
																	 source-buffer
																	 lint-buffer)))))
		(flymake-jsts/message "buffer is empty")
		(funcall report-fn
						 (list))))


(provide 'flymake-jsts-utils)
;;; flymake-jsts-utils.el ends here
