# jemdoc-mode for Emacs

`jemdoc-mode` is a major mode for editting [jemdoc](http://jemdoc.jaboc.net) files.

# Installation:

#### Via MELPA (recommended)

If you have a recent version of `package.el` you can install `jemdoc-mode` from
the [MELPA](http://melpa.org) package repository.

#### Manually

Ensure `jemdoc-mode.el` is in a directory on your load-path, and add `(require 'jemdoc-mode)`
to your ~/.emacs or ~/.emacs.d/init.el:

# Note:
1. By default `jit-lock` is **not** used as a `font-lock` support mode.
   It could be turned on using

   ```
   (setq-local jemdoc-font-lock-support-mode 'jit-lock)
   ```

2. If `font-lock+` is installed, it is used to ignore (i.e., not fortify) code-block regions
   (i.e., tilde blocks with `{...}{...}` arguments)
