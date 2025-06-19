# flymake-jsts: A Flymake backend for Javascript/Typescript

## Supported Linters

- eslint
- oxlint
- biome (coming soon)

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

## Comparison with `flymake-eslint`

this is a from-the-ground-up rewrite of `flymake-eslint`, which was the first elisp package I ever published; I've learned a lot since then!  it should be easier to add support for other linters like biome or stylelint, for instance, and it should be a little easier to customize too.

## License

GPLv3
