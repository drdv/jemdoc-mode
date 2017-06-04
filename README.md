# jemdoc-mode for Emacs

`jemdoc-mode` is a major mode for editing [jemdoc](http://jemdoc.jaboc.net) files.

# Installation:

#### Via MELPA (recommended)

If you have a recent version of `package.el` you can install `jemdoc-mode` from
the [MELPA](http://melpa.org) package repository.

#### Manually

Ensure `jemdoc-mode.el` is in a directory on your load-path, and add `(require 'jemdoc-mode)`
to your `~/.emacs` or `~/.emacs.d/init.el`.

# Note:
1. If `font-lock+` is installed, it is used to ignore (i.e., not fortify) code-block regions
   (i.e., tilde blocks with `{...}{...}` arguments)

2. There is a simple option for editing code-blocks with proper syntax highlighting in an "edit-buffer"
   (see `jemdoc-mode-edit-code-block`, and the associated keybinding `C-x n r`).
   The edited text can be inserted back in the jemdoc buffer (using the menu at the top of the "edit-buffer").

3. By default `jit-lock` is **not** used as a `font-lock` support mode.
   It could be turned on by setting `jemdoc-font-lock-support-mode` to `'jit-lock`

   ```
   (setq jemdoc-font-lock-support-mode 'jit-lock)
   ```
