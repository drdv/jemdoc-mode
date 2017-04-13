# jemdoc-mode
Major mode for editting [jemdoc](http://jemdoc.jaboc.net) files.

# Quick start:
Simply add ```(require 'jemdoc-mode)``` to your ```.emacs```

#Note:
1. By default ```jit-lock``` is **not** used as a ```font-lock``` support mode. It could be turned on using ```(setq-local jemdoc-font-lock-support-mode 'jit-lock)```.
2. If ```font-lock+``` is installed, it is used to ignore (i.e., not fortify) code-block regions (i.e., tilde blocks with {...}{...} arguments)
