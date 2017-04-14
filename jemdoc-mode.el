;;; jemdoc-mode.el --- Major mode for editting jemdoc files

;; Copyright (C) 2017 Dimitar Dimitrov

;; Author: Dimitar Dimitrov <mail.mitko@gmail.com>
;; URL: https://github.com/drdv/jemdoc-mode
;; Package-Version: 20170413.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience, usability

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Pakage tested on:
;; GNU Emacs 25.1.1 (x86_64-apple-darwin16.1.0)
;;
;; jemdoc is a light text-based markup language designed
;; for creating websites. For more information see
;; http://jemdoc.jaboc.net

;; Quick start:
;;
;; add (require 'jemdoc-mode) to your .emacs

;;; Code:



;; Terminology I use in the code:
;;
;; a "tilde block" is a block of the form
;; ~~~
;; ARGUMENTS
;; this is a tilde block
;; ~~~
;; for all possible ARGUMENTS, see http://jemdoc.jaboc.net
;;
;; I call a "code block" a tilde block with ARGUMENTS
;; {optional block name}{optional programming language name}



(defface jemdoc-face-monospace
    '((t . (:inherit font-lock-type-face)))
  "Face for +monospace+."
  :group 'jemdoc-faces)

(defface jemdoc-face-bold
    '((t . (:inherit 'bold)))
  "Face for *bold*."
  :group 'jemdoc-faces)

(defface jemdoc-face-italics
    '((t . (:inherit 'shadow)))
  "Face for /italics/."
  :group 'jemdoc-faces)

(defface jemdoc-face-tilde-block-delimeters
    '((t . (:inherit 'success)))
  "Face for tilde block delimeters."
  :group 'jemdoc-faces)

(defface jemdoc-face-equation
    '((t . (:inherit font-lock-keyword-face)))
  "Face for $inline equation$ or \(equation\)."
  :group 'jemdoc-faces)

(defface jemdoc-face-special-keywords
    '((t . (:inherit font-lock-variable-name-face)))
  "Face for special keywords.
For example, keywords in comments or the definition construct : {}."
  :group 'jemdoc-faces)

(defface jemdoc-face-special-keywords-name
    '((t . (:inherit font-lock-function-name-face)))
  "Face for names in special keywords.
For example the definition construct : {A name}
or #include{name of file}."
  :group 'jemdoc-faces)

(defface jemdoc-face-monospace-html
    '((t . (:inherit font-lock-function-name-face)))
  "Face for +{{monospace html}}+ which is equivalent to %monospace html%."
  :group 'jemdoc-faces)

(defface jemdoc-face-other
    '((t . (:inherit font-lock-constant-face)))
  "Face for \\n \\A \\C \\R \\M etc."
  :group 'jemdoc-faces)

(defface jemdoc-face-http-mail
    '((t . (:inherit font-lock-constant-face
	    :weight bold)))
  "Face for [http/mail block]."
  :group 'jemdoc-faces)

(defface jemdoc-face-dashes-2
    '((t . (:foreground "sienna"
	    :weight bold)))
  "Face for --."
  :group 'jemdoc-faces)

(defface jemdoc-face-dashes-3
    '((t . (:foreground "Blue1"
	    :weight bold)))
  "Face for ---."
  :group 'jemdoc-faces)

(defface jemdoc-face-ellipsis
    '((t . (:foreground "Blue1"
	    :weight bold)))
  "Face for ellipsis (...)."
  :group 'jemdoc-faces)

(defface jemdoc-face-html-text
    '((t . (:foreground "color-105")))
  "Face for {{html text}}."
  :group 'jemdoc-faces)

(defface jemdoc-face-bullet
    '((t .(:foreground "red"
	   :weight bold)))
  "Face for bullets."
  :group 'jemdoc-faces)

(defface jemdoc-face-bullet-warning
    '((t . (:foreground "white"
	    :background "red"
	    :weight bold)))
  "Warning face for bullets."
  :group 'jemdoc-faces)

(defface jemdoc-face-title-1
    '((t . (:foreground "color-18"
	    :weight bold)))
  "Face for title with one \"=\"."
  :group 'jemdoc-faces)

(defface jemdoc-face-title-2
    '((t . (:foreground "color-21"
	    :weight bold)))
  "Face for title with two \"==\"."
  :group 'jemdoc-faces)

(defface jemdoc-face-title-3
    '((t . (:foreground "color-27"
	    :weight bold)))
  "Face for title with three \"===\"."
  :group 'jemdoc-faces)

(defface jemdoc-face-title-4
    '((t . (:foreground "color-33"
	    :weight bold)))
  "Face for title with four \"====\"."
  :group 'jemdoc-faces)



(defvar jemdoc-debug-messages nil
  "Set to non-nil to output debug messages.")
(make-local-variable 'jemdoc-debug-messages)

(defvar jemdoc-mode-map
  (let ((map (make-sparse-keymap)))
    ;; define your key bindings
    ;;(define-key map (kbd ...) '...)
    map)
  "Keymap for jemdoc major mode.")
(make-local-variable 'jemdoc-mode-map)

(defvar jemdoc-region-extended-already nil
  "`jemdoc-extend-region' should change the region only once per iteration.

After each font-lock iteration, it is set back to nil in
`jemdoc-extend-region-initialize', which is registerd in
`font-lock-extend-after-change-region-function'.")
(make-local-variable 'jemdoc-region-extended-already)

(defvar jemdoc-font-lock-syntax-table
  (let ((st (make-syntax-table)))
    ;; I use the "b" below because otherwise the single-line comments
    ;; interfere with other comments that I might want to define.
    (modify-syntax-entry ?#  "< b" st)
    (modify-syntax-entry ?\n ">#b" st)
    st)
  "Syntax table for `jemdoc-mode'.")
(make-local-variable 'jemdoc-font-lock-syntax-table)

(defvar jemdoc-font-lock-support-mode nil
  "Specify the support mode for jemdoc.")
(make-local-variable 'jemdoc-font-lock-support-mode)

(defvar font-lock-beg nil
  "To suppress warning during byte-compilation.")
(make-local-variable 'font-lock-beg)

(defvar font-lock-end nil
  "To suppress warning during byte-compilation.")
(make-local-variable 'font-lock-end)



(defun jemdoc-syntax-propertize-function (start end)
  "Assign text properties from START to END.

Text properties:
  jemdoc-keywords-in-comments-property: delimits keywords in comments
  font-lock-ignore: used by font-lock+ to ignore region (and not fontify it)."
  (let ((case-fold-search nil))
    (goto-char start)

    (remove-text-properties start end '(jemdoc-keywords-in-comments-property))
    (remove-text-properties start end '(font-lock-ignore t))

    (funcall
     (syntax-propertize-rules
      ;; keywords in comments
      ((regexp-opt '("jemdoc" "menu" "nofooter" "nodate" "notime"
		     "fwtitle" "showsource" "nodefaultcss" "addcss"
		     "addjs" "addpackage" "addtex" "analytics" "title"
		     "noeqs" "noeqcache" "eqsize" "eqdir")
		   'words)
       (0 (ignore (jemdoc-property-assign))))
      ;; regions to be ignored
      ("^~~~ *$"
       (0 (ignore (jemdoc-ignore-region))))
      )
     start end)))

(defun jemdoc-property-assign ()
  "Assign text properties in keywords in comments."
  (let* ((beg (match-beginning 0))
	 (str-line (thing-at-point 'line t))
	 (jemdoc-line-start-p (if (> (length str-line) 9)
				  (equal (substring str-line 0 9) "# jemdoc:")
				nil))
	 (context (save-excursion
		    (save-match-data (syntax-ppss beg)))))
    (when jemdoc-line-start-p
      (put-text-property beg (1+ beg)
			 'jemdoc-keywords-in-comments-property
			 (cons (nth 4 context) (match-data))))))

(defun jemdoc-property-retrieve (limit)
  "Highlight text with jemdoc-keywords-in-comments-property until LIMIT."
  (let ((pos (next-single-char-property-change (point)
					       'jemdoc-keywords-in-comments-property
                                               nil limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let ((value (get-text-property pos 'jemdoc-keywords-in-comments-property)))
        (if (eq (car value) t)
            (progn
              (set-match-data (cdr value))
              t)
          (jemdoc-property-retrieve limit))))))



(defun jemdoc-highlight-curly-brackets-tilde-block (limit)
  "Highlight curly brackets in the preamble of a tilde block (until LIMIT).

- pictures format is handled using: {}{.*?}{.*?}{.*?}{.*?}{.*?}{.*?}."
  (when (re-search-forward "^ *\\({}{.*?}{.*?}{.*?}{.*?}{.*?}{.*?}\\|{.*?} *{.*?}\\|{.*?}\\)" limit t)
    (save-excursion
      (save-match-data
	(goto-char (line-beginning-position 0))
	;; If the match is not preceded by a "^~~~ *$" I don't want to return nil
	;; because the search for this keyword would terminate (and there might be
	;; more matches below). Instead, I return true but with empty match data.
	(unless (looking-at "^~~~ *$")
	  (set-match-data nil))))
    t))

(defun jemdoc-highlight-monospace-html-text (limit)
  "Highlight monospace html text (until LIMIT).

There are two formats: +{{format 1}}+ and %format 2%."
  (when (re-search-forward "\\(\\+{{.*?}}\\+\\|%.*?%\\)" limit t)
    t))

(defun jemdoc-highlight-curly-brackets-html-text (limit)
  "Highlight curly brackets with html text (until LIMIT)."
  (when (re-search-forward "{{.*?}}" limit t)
    t))



(defun jemdoc-extend-tilde-region ()
  "Extend region to contain encolsing tilde block.

This function is called in `jemdoc-extend-region' which is registered
in `font-lock-extend-region-functions' and is called by font-lock during
fontification. The variables `font-lock-beg' and `font-lock-end' in the code
refer to dynamically bound variables used by font-lock."
  (let ((region-beg)
	(region-end))
    (save-excursion
      (goto-char font-lock-beg)
      (setq region-beg (jemdoc-in-tilde-block-internal 'general-block)))
    (save-excursion
      (goto-char font-lock-end)
      (setq region-end (jemdoc-in-tilde-block-internal 'general-block)))
    (when (and region-beg
	       (< (car region-beg) font-lock-beg)) ;; don't shorten the region
      (setq font-lock-beg (car region-beg)))
    (when (and region-end
	       (> (cdr region-end) font-lock-end)) ;; don't shorten the region
      (setq font-lock-end (cdr region-end)))
    ))

(defun jemdoc-extend-bullet-region ()
  "Extend region to contain encolsing bullet block.

This function is called in `jemdoc-extend-region' which is registered
in `font-lock-extend-region-functions' and is called by font-lock during
fontification. The variables `font-lock-beg' and `font-lock-end' in the code
refer to dynamically bound variables used by font-lock."
  (save-excursion
    (when jemdoc-debug-messages
      (message "----------------------------------------------------")
      (message "[initial]: font-lock-beg = %d, font-lock-end = %d, point = %d" font-lock-beg font-lock-end (point)))
    (save-excursion
      (goto-char (line-beginning-position 2))
      (when jemdoc-debug-messages
	(message "[move down]: point = %d" (point)))
      ;; search for
      ;; ^ *- +   (bullet with one dash)
      ;; ^ *\\. + (bullet with one dot)
      ;; ^ *$     (empty line)
      ;; \\'      (end of buffer)
      ;; ^~~~ *$  (beginning or end of a tilde block)
      (if (re-search-forward "\\(^ *- +\\|^ *\\. +\\|^ *$\\|\\'\\|^~~~ *$\\)" nil t)
	  (progn
	    (when jemdoc-debug-messages
	      (message "[after first search]: point = %d" (point)))
	    ;; Here I don't include the last line, because if it is the
	    ;; beginning of a tilde block, later on it would be included
	    ;; as well because the end of the region would be inside it
	    ;; (which is not what I want). On the other hand if the last
	    ;; line is the end of a tilde region it is included in the region
	    ;; in `jemdoc-extend-tilde-region'.
	    (goto-char (line-end-position 0))
	    (when (> (point) font-lock-end) ;; don't shorten the region
	      (setq font-lock-end (point))))
	(setq font-lock-end (point-max))))
    ;; Here I go up because when point is at the beginning of an empty line
    ;; (re-search-backward "^ *$") matches it and doesn't move point, which
    ;; I think is quite a bit counter-intuitive.
    (when (looking-at "^ *$")
      (goto-char (line-beginning-position 0)))
    ;; the only difference with the regex above is that here I have
    ;; \\` (i.e., beginning of buffer) instead of \\' (i.e., end of buffer)
    (if (re-search-backward "\\(^ *- +\\|^ *\\. +\\|^ *$\\|\\`\\|^~~~ *$\\)" nil t)
	(progn
	  (when jemdoc-debug-messages
	    (message "[after second search]: point = %d" (point)))
	  (when (< (match-beginning 0) font-lock-beg) ;; don't shorten the region
	    (setq font-lock-beg (match-beginning 0))))
      (setq font-lock-beg (point-min)))
    )
  (when jemdoc-debug-messages
    (message "[extend]: font-lock-beg = %d, font-lock-end = %d, point = %d" font-lock-beg font-lock-end (point)))
  nil)

(defun jemdoc-extend-region-initialize (beg end &optional len)
  "Reset `jemdoc-region-extended-already'.

BEG, END and LEN are the standard arguments provided to `after-change-functions'."
  (setq jemdoc-region-extended-already nil)
  nil)

(defun jemdoc-extend-region()
  "Extend the font-lock region.

registered in `font-lock-extend-region-functions'."
  (unless jemdoc-region-extended-already
    (setq jemdoc-region-extended-already t)
    (jemdoc-extend-bullet-region)
    (jemdoc-extend-tilde-region))
  (when jemdoc-debug-messages
    (message "[while]: font-lock-beg = %d, font-lock-end = %d" font-lock-beg font-lock-end))
  ;; this function is executed first among the functions in
  ;; `font-lock-extend-region-functions' and there is no problem
  ;; to always return true even if we haven't changed the region
  ;; becasue this wouldn't cause the start of other iterations
  ;; in the while loop of `font-lock-default-fontify-region'
  t)



(defun jemdoc-concat-string (str n)
  "Concatenate a string STR, N times."
  (let ((out-str))
    (while (> n 0)
      (setq out-str (concat out-str str))
      (setq n (1- n)))
    out-str))

(defun jemdoc-end-of-block (str n)
  "Return position of next delimeter.

Delimeters can be: empty line, end of buffer, or line starting with
STR appearing N or less times in a row."
  (save-excursion
    ;; find an empty line ("^$"), end of buffer ("\\'") or a line starting with 1, ..., n str
    (re-search-forward (let ((S "\\(\\'\\|^ *$"))
			 (while (> n 0)
			   (setq S (concat S (format "\\|^ *%s +" (jemdoc-concat-string str n))))
			   (setq n (1- n)))
			 (setq S (concat S "\\)")))
		       nil t)))



(defun jemdoc-in-tilde-block-internal (tilde-block-type)
  "Check whether point is inside a tilde block.

If point is inside a tilde block with type TILDE-BLOCK-TYPE,
return a cell array with its beginning and end. If not, return nil.

TILDE-BLOCK-TYPE can be 'code-block, 'general-block."
  (save-excursion
    (let ((p (point))
	  (regexp (if (eq tilde-block-type 'code-block)
		      ;; code block
		      "^ *{.*?} *{.*?} *$"
		    ;; general-block
		    "^ *\\({}{.*?}{.*?}{.*?}{.*?}{.*?}{.*?}\\|{.*?} *{.*?}\\|{.*?}\\)"))
	  beg
	  end)
      (catch 'drdv-return

	(beginning-of-line)
	(if (and (re-search-forward "^~~~ *$" nil t)
		 (save-excursion
		   (goto-char (line-beginning-position 2))
		   (not (looking-at regexp))))
	    (setq end (match-end 0))
	  (throw 'drdv-return nil))

	(beginning-of-line)
	(if (re-search-backward "^~~~ *$" nil t)
	    (setq beg (match-beginning 0))
	  (throw 'drdv-return nil))

	(goto-char (line-beginning-position 2))
	(if (looking-at regexp)
	    `(,beg . ,end)
	  nil)
	))))

(defun jemdoc-ignore-region ()
  "Assigne text property 'font-lock-ignore to code-blocks."
  (interactive)
  (let ((region (jemdoc-in-tilde-block-internal 'code-block))
	start
	end)
    (when region

      ;; leave the highlightling of the oppening ~~~\n{}{...}
      (setq start (save-excursion
		    (goto-char (car region))
		    (line-beginning-position 3)))

      ;; leave the highlightling of the closing ~~~
      (setq end (save-excursion
		  (goto-char (cdr region))
		  (line-end-position 0)))
      (put-text-property start end  'font-lock-ignore t)
      (remove-text-properties start end '(face nil))

      ;; move point after the block
      (goto-char (save-excursion
		   (goto-char (cdr region))
		   (line-beginning-position 2)))
      )))



(defvar jemdoc-font-lock-keywords
  (list

   ;; ---------------------------------------------------------

   '("^ *\\(-\\) +" (1 'jemdoc-face-bullet)
     ("^ *\\(--\\) +"
      (jemdoc-end-of-block "-" 1)
      nil
      (1 'jemdoc-face-bullet t)))

   '("^ *\\(--\\) +" (1 'jemdoc-face-bullet-warning)
     ("^ *\\(---\\) +"
      (jemdoc-end-of-block "-" 2)
      nil
      (1 'jemdoc-face-bullet t)))

   '("^ *\\(---\\) +" (1 'jemdoc-face-bullet-warning)
     ("^ *\\(----\\) +"
      (jemdoc-end-of-block "-" 3)
      nil
      (1 'jemdoc-face-bullet t)))

   '("^ *\\(----\\) +" (1 'jemdoc-face-bullet-warning)
     ("^ *\\(-----\\) +"
      (jemdoc-end-of-block "-" 4)
      nil
      (1 'jemdoc-face-bullet t)))

   ;; ---------------------------------------------------------

   '("^ *\\(\\.\\) +" (1 'jemdoc-face-bullet)
     ("^ *\\(\\.\\.\\) +"
      (jemdoc-end-of-block "\\." 1)
      nil
      (1 'jemdoc-face-bullet t)))

   '("^ *\\(\\.\\.\\) +" (1 'jemdoc-face-bullet-warning)
     ("^ *\\(\\.\\.\\.\\) +"
      (jemdoc-end-of-block "\\." 2)
      nil
      (1 'jemdoc-face-bullet t)))

   '("^ *\\(\\.\\.\\.\\) +" (1 'jemdoc-face-bullet-warning)
     ("^ *\\(\\.\\.\\.\\.\\) +"
      (jemdoc-end-of-block "\\." 3)
      nil
      (1 'jemdoc-face-bullet t)))

   '("^ *\\(\\.\\.\\.\\.\\) +" (1 'jemdoc-face-bullet-warning)
     ("^ *\\(\\.\\.\\.\\.\\.\\) +"
      (jemdoc-end-of-block "\\." 4)
      nil
      (1 'jemdoc-face-bullet t)))

   ;; ---------------------------------------------------------

   ;; handle `singly quoted text'
   '("\`.*?\'"  . 'emdoc-face-other)

   ;; titles
   '("^ *= +.*"    . 'jemdoc-face-title-1)
   '("^ *== +.*"   . 'jemdoc-face-title-2)
   '("^ *=== +.*"  . 'jemdoc-face-title-3)
   '("^ *==== +.*" . 'jemdoc-face-title-4)

   ;; ---------------------------------------------------------

   ;; definition
   '("^ *\\(: *{\\)\\(.*?\\)\\(}\\)"
     (1 'jemdoc-face-special-keywords)  ;; : {
     (2 'jemdoc-face-special-keywords-name)  ;; definition name
     (3 'jemdoc-face-special-keywords)) ;; }

   ;; ---------------------------------------------------------

   ;; monospace
   ;; (unfortunately I cannot use negative look-behind assertions in emacs)
   ;; the following (rx ...) generates "\\(?:^\\|[^\\]\\)\\(\\+.*?[^\\]\\+\\)"
   `(,(rx (or line-start
	      (not (any "\\")))
	  (group
	   "+"
	   (minimal-match (zero-or-more not-newline))
	   (not (any "\\"))
	   "+"))
      1 'jemdoc-face-monospace prepend)

   ;; bold
   '("\\(?:^\\|[^\\]\\)\\(\\*.*?[^\\]\\*\\)"  1 'jemdoc-face-bold prepend)

   ;; italics
   '("\\(?:^\\|[^\\]\\)\\(/.*?[^\\]/\\)"  1 'jemdoc-face-italics prepend)

   ;; tilde blocks
   '("^~~~" . 'jemdoc-face-tilde-block-delimeters)
   '(jemdoc-highlight-curly-brackets-tilde-block . 'jemdoc-face-tilde-block-delimeters)

   ;; {{html text}}
   '(jemdoc-highlight-curly-brackets-html-text 0 'jemdoc-face-html-text t)

   ;; +{{monospace html text}}+ or %monospace html text%
   '(jemdoc-highlight-monospace-html-text 0 'jemdoc-face-monospace-html t)

   ;; inline $equations$
   '("\\$.*?\\$" . 'jemdoc-face-equation)

   ;; \(equations\)
   '("^ *\\\\(.*\\\\)" 0 'jemdoc-face-equation t)

   ;; [http/mail ...]
   '("\\[\\(http\\|mail\\).*\\]" 0 'jemdoc-face-http-mail prepend)

   ;; syntax-table stuff
   '(jemdoc-property-retrieve 0 'jemdoc-face-special-keywords t)

   ;; #include{...} and #includeraw{...}
   ;; since I use "t" as a third argument I can directly nest \\(.*?\\)
   ;; in the first group
   '("\\(?:^ *# *\\)\\(include\\(?:raw\\)?{\\(.*?\\)}\\)"
     (1 'jemdoc-face-special-keywords t)
     (2 'jemdoc-face-special-keywords-name t))

   ;; 2--3 different-sized dashes
   ;; I have to put this after the ^-, ^--, ^---, ...
   '("[^-\\]\\(-\\{3,3\\}\\)[^-]" 1 'jemdoc-face-dashes-3)
   '("[^-\\]\\(-\\{2,2\\}\\)[^-]" 1 'jemdoc-face-dashes-2)

   ;; ... ellipsis
   ;; I have to put this after the ^\\., ^\\.\\., ^\\.\\.\\., ...
   '("[^-\\]\\(\\.\\{3,3\\}\\)[^-]" 1 'jemdoc-face-ellipsis)

   ;; ---------------------------------------------------------

   ;; other
   `(,(regexp-opt '("\\n" "\\A" "\\C" "\\R" "\\M" "\\\#" "\\`" "\\\'" "\\\"")) 0 'jemdoc-face-other)

   )
  "Keywords to highlight in jemdoc mode.")
(make-local-variable 'jemdoc-font-lock-keywords)



;;;###autoload
(define-derived-mode jemdoc-mode prog-mode "jemdoc"
		     "Major mode for editing jemdoc files."
		     (setq-local syntax-propertize-function 'jemdoc-syntax-propertize-function)
		     (setq-local font-lock-defaults '(jemdoc-font-lock-keywords
						      nil
						      nil))
		     ;; (setq-local font-lock-multiline t) ;; I don't need it
		     (set-syntax-table jemdoc-font-lock-syntax-table)
		     )



(add-hook 'jemdoc-mode-hook
	  (lambda ()
	    (setq-local font-lock-extend-after-change-region-function 'jemdoc-extend-region-initialize)
	    (add-hook 'font-lock-extend-region-functions 'jemdoc-extend-region)

	    ;; sometimes the region used in jit-lock doesn't contain the whole block
	    ;; so I prefer to not use it
	    (setq-local font-lock-support-mode jemdoc-font-lock-support-mode)

	    ;; used to not fontify code blocks
	    ;; (see the font-lock-ignore text property)
	    (when (package-installed-p 'font-lock+)
	      (require 'font-lock+))
	    ))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jemdoc\\'" . jemdoc-mode))

(provide 'jemdoc-mode)

;;; jemdoc-mode.el ends here
