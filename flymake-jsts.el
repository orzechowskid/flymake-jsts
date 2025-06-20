;;; flymake-jsts.el --- A Flymake backend for Javascript and Typescript  -*- lexical-binding: t; -*-

;; Version: 1.1.0
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

(when (featurep 'project)
	(require 'project))


(add-to-list 'load-path
						 (file-name-directory (buffer-file-name)))
(require 'flymake-jsts-utils)
(require 'flymake-jsts-eslint)
(require 'flymake-jsts-oxlint)
(require 'flymake-jsts-biome)


(defgroup flymake-jsts nil
	"Flymake checker for Javascript/Typescript."
	:group 'programming
	:prefix "flymake-jsts-")


(defcustom flymake-jsts-executable-name-alist
	'((eslint . "eslint")
		(oxlint . "oxlint")
		(biome . "biome"))
	"Mapping of linters to binary names."
	:type '(alist :key-type (symbol :tag "linter")
								:value-type (string :tag "binary name"))
	:group 'flymake-jsts)

(defcustom flymake-jsts-project-markers-alist
	'((eslint . ("eslint.config.js" "eslint.config.mjs" "eslint.config.cjs" "package.json"))
		(oxlint . (".oxlintrc.json" "package.json"))
		(biome . ("biome.json" "biome.jsonc" "package.json")))
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


;;;###autoload
(defun flymake-jsts-eslint-enable ()
	"Convenience function to register eslint as a creator of Flymake diagnostics."
	(interactive)
	(add-hook 'flymake-diagnostic-functions
						#'flymake-jsts-eslint-check-and-report
						nil
						t))

;;;###autoload
(defun flymake-jsts-eslint-disable ()
	"Unregisters eslint as a creator of Flymake diagnostics."
	(interactive)
	(remove-hook 'flymake-diagnostic-functions
							 #'flymake-jsts-eslint-check-and-report))

;;;###autoload
(defun flymake-jsts-oxlint-enable ()
	"Convenience function to register oxlint as a creator of Flymake diagnostics."
	(interactive)
	(add-hook 'flymake-diagnostic-functions
						#'flymake-jsts-oxlint-check-and-report
						nil
						t))

;;;###autoload
(defun flymake-jsts-oxlint-disable ()
	"Unregisters oxlint as a creator of Flymake diagnostics."
	(interactive)
	(remove-hook 'flymake-diagnostic-functions
							 #'flymake-jsts-oxlint-check-and-report))

;;;###autoload
(defun flymake-jsts-biome-enable ()
	"Convenience function to register biome as a creator of Flymake diagnostics."
	(interactive)
	(add-hook 'flymake-diagnostic-functions
						#'flymake-jsts-biome-check-and-report
						nil
						t))

;;;###autoload
(defun flymake-jsts-biome-disable ()
	"Unregisters biome as a creator of Flymake diagnostics."
	(interactive)
	(remove-hook 'flymake-diagnostic-functions
							 #'flymake-jsts-biome-check-and-report))


(provide 'flymake-jsts)
;;; flymake-jsts.el ends here
