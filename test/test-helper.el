;; Don't load old byte-compiled versions!
(setq load-prefer-newer t)

;; Load jemdoc-mode
(require 'f)
(add-to-list 'load-path (f-parent (f-dirname load-file-name)))
(require 'jemdoc-mode)
