# flymake-jsts: A Flymake backend for Javascript/Typescript

## Supported Linters

- eslint
- oxlint
- biome

## Requirements

- emacs 29+
- [pfuture](https://github.com/Alexander-Miller/pfuture)

## Installation

```
(use-package flymake-jsts
  :straight '(flymake-jsts :type git :host github :repo "orzechowskid/flymake-jsts" :branch "main"))
```

Then enable the linter(s) of your choice:

- `M-x flymake-jsts-eslint-enable` (or `-oxlint-enable`, etc.)
- `M-x flymake-mode`

## Configuration

useful variables are members of the `flymake-jsts` group and can be viewed and modified with the command `M-x customize-group [RET] flymake-jsts [RET]`.

### `flymake-jsts-executable-name-alist`

Mapping of linters to binary names.  Useful if you want to use `eslint_d` instead of `eslint`, for instance.

### `flymake-jsts-project-markers-alist`

Mapping of linters to 'project markers': files which denote the root of a project tree for which linting is applied.  Useful if you have a config file with a weird name, so it can be found and its directory can be used as `$PWD` for the linter process.

### `flymake-jsts-show-rule-name`

Non-nil to show the name of a lint rule as part of the Flymake diagnostic message, or nil to suppress.

### `flymake-jsts-show-extended-info`

Non-nil to show extra information for each lint rule (when provided), or nil to suppress.

### `flymake-jsts-error-type`
### `flymake-jsts-warning-type`

Type symbols to use when reporting errors.  Useful to customize if you want to distinguish Flymake errors from other sources of errors (e.g. LSP).

### Others

#### `flymake-jsts/debug`

Non-nil to write tracing info to the `*Messages*` buffer.  It sometimes comes in handy when things aren't working as expected for you.

## Comparison with `flymake-eslint`

this is a from-the-ground-up rewrite of `flymake-eslint`, which was the first elisp package I ever published; I've learned a lot since then!  it should be easier to add support for other linters like stylelint, for instance, and it should be a little easier to customize too.

## Playbooks

### Usage with eglot

> [!TIP]
> [tsx-mode.el](https://github.com/orzechowskid/tsx-mode.el) can configure this (and lots more) for you.

eglot is pretty aggressive in taking over your Flymake configuration; it assumes that your language server is the only source of diagnostic messages, and completely overwrites the value of `flymake-diagnostic-functions` to suit its own needs.  One workaround you can try is to add a `flymake-jsts` backend function after eglot has been enabled:

```lisp
(add-hook 'eglot-managed-mode-hook
  (lambda ()
    (flymake-jsts-eslint-enable))
  nil t)
```

Alternately, you can tell eglot to stay out of your Flymake configuration and then add eglot's backend yourself:

```lisp
(add-to-list 'eglot-stay-out-of 'flymake)
(add-hook 'flymake-mode-hook
  (lambda ()
    (add-hook 'flymake-diagnostic-functions
	  #'eglot-flymake-backend
	  nil
	  t)))
```

## License

GPLv3
