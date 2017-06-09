;; -----------------------------------------------------------------------------
;; NOTE
;; -----------------------------------------------------------------------------
;; font-lock is disabled by default in temp buffers
;; https://stackoverflow.com/questions/23568779/elisp-font-lock-in-temp-buffer
;; that's why I have to call (font-lock-ensure) which is apparently better than
;; using (font-lock-fontify-buffer)
;; -----------------------------------------------------------------------------

(defvar jemdoc-mode-tilde-block-string
  "# jemdoc: menu{MENU}{test.html}, fwtitle, nofooter

== test text-properties of tilde-blocks
1. untitled-simple-block (general-block)
~~~
Save the file as +index.jemdoc+, say, and simply call +jemdoc index+ (after
[download.html downloading jemdoc], of course).
~~~
2. simple-block (general-block)
~~~
{Simple block}
This is a simple text block, with a title. Notice how the previous line has only
one set of braces (\{\}).
~~~
3. code-block
~~~
{Optionally empty title}{Optionally empty highlight mode}
Code block with monospaced text.
~~~
4. raw-block (code-block)
~~~
{}{raw}
Any text here will be copied straight to the output file without processing.
~~~
5. figure-block (general-block)
~~~
{}{img_left}{FILENAME.IMG}{alt text}{WIDTHpx}{HEIGHTpx}{IMGLINKTARGET}
Ordinary jemdoc markup goes here.
~~~
")

(defvar jemdoc-mode-keywords-in-comments
  "# jemdoc: menu{MENU}{test.html}, fwtitle, nofooter, nodate, notime, fwtitle,
# jemdoc: showsource, nodefaultcss, addcss, fake, addjs, addpackage, addtex,
# jemdoc: analytics, title, noeqs, noeqcache, eqsize, eqdir
")

(ert-deftest test-jemdoc-mode-in-tilde-block-internal ()
  "Test `jemdoc-mode-in-tilde-block-internal'."
  (with-temp-buffer
    (insert jemdoc-mode-tilde-block-string)
    (jemdoc-mode)
    (font-lock-ensure)
    (sleep-for 0.1)
    (let ((test-cases '(;; general-block
			(general-block  93 nil)
			(general-block 134 (134 . 265))
			(general-block 160 (134 . 265))
			(general-block 265 (134 . 265))
			(general-block 266 nil)
			(general-block 298 (298 . 425))
			(general-block 330 (298 . 425))
			(general-block 425 (298 . 425))
			(general-block 426 nil)
			(general-block 440 (440 . 538))
			(general-block 500 (440 . 538))
			(general-block 538 (440 . 538))
			(general-block 539 nil)
			(general-block 565 (565 . 657))
			(general-block 583 (565 . 657))
			(general-block 657 (565 . 657))
			(general-block 658 nil)
			(general-block 690 (690 . 802))
			(general-block 730 (690 . 802))
			(general-block 802 (690 . 802))
			;; code-block
			(code-block  93 nil)
			(code-block 134 nil)
			(code-block 160 nil)
			(code-block 265 nil)
			(code-block 266 nil)
			(code-block 298 nil)
			(code-block 330 nil)
			(code-block 425 nil)
			(code-block 426 nil)
			(code-block 440 (440 . 538))
			(code-block 500 (440 . 538))
			(code-block 538 (440 . 538))
			(code-block 539 nil)
			(code-block 565 (565 . 657))
			(code-block 583 (565 . 657))
			(code-block 657 (565 . 657))
			(code-block 658 nil)
			(code-block 690 nil)
			(code-block 730 nil)
			(code-block 802 nil)
			)))
      (dolist (test-case test-cases)
	(goto-char (nth 1 test-case))
	(should (equal (jemdoc-mode-in-tilde-block-internal (nth 0 test-case)) (nth 2 test-case)))))))

(ert-deftest test-jemdoc-mode-tilde-block-text-properties ()
  "Test `jemdoc-mode-tilde-block-text-properties'."
  (with-temp-buffer
    (insert jemdoc-mode-tilde-block-string)
    (jemdoc-mode)
    (font-lock-ensure)
    (sleep-for 0.1)
    (let ((test-pairs '(( 93 . nil)
			(134 . start)
			(160 . nil)
			(262 . end)
			(266 . nil)
			(298 . start)
			(330 . nil)
			(422 . end)
			(426 . nil)
			(440 . start)
			(500 . nil)
			(535 . end)
			(539 . nil)
			(565 . start)
			(583 . nil)
			(654 . end)
			(658 . nil)
			(690 . start)
			(730 . nil)
			(799 . end))))
      (dolist (pair test-pairs)
	(should (equal (get-text-property (car pair) 'tilde-block-delimiter) (cdr pair)))))))

(ert-deftest test-jemdoc-mode-tilde-block-text-properties-ignore ()
  "Test `jemdoc-mode-tilde-block-text-properties.'
Check whether the content of code-blocks is ignored
i.e., it has the the text property `font-lock-ignore'."
  (with-temp-buffer
    (insert jemdoc-mode-tilde-block-string)
    (jemdoc-mode)
    (font-lock-ensure)
    (sleep-for 0.1)
    ;; there should be no text with a 'font-lock-ignore property t
    (should (equal (text-property-any 138 261 'font-lock-ignore t) nil))
    (should (equal (text-property-any 317 421 'font-lock-ignore t) nil))
    (should (equal (text-property-any 765 798 'font-lock-ignore t) nil))
    ;; there should be no text without a 'font-lock-ignore property t
    (should (equal (text-property-not-all 502 534 'font-lock-ignore t) nil))
    (should (equal (text-property-not-all 577 653 'font-lock-ignore t) nil))))

(ert-deftest test-jemdoc-mode-tilde-block-text-properties-warning ()
  "Test `jemdoc-mode-tilde-block-text-properties.'
Check whether jemdoc-mode-tilde-block-delimiter-last-value == 'start."
  (with-temp-buffer
    (insert jemdoc-mode-tilde-block-string)
    (jemdoc-mode)
    ;; disable warnings
    ;; this avoids "Warning: wrong delimiters of tilde blocks."
    (setq jemdoc-mode-warning-messages nil)
    (font-lock-ensure)
    (sleep-for 0.1)
    (should (equal jemdoc-mode-tilde-block-delimiter-last-value 'end))
    (goto-char 439)
    ;; add an opening tilde-block delimiter without a closing one
    (insert "\n~~~\n\n")
    ;; force buffer fontification
    (font-lock-ensure)
    (should (equal jemdoc-mode-tilde-block-delimiter-last-value 'start))))


(ert-deftest test-jemdoc-mode-keywords-in-comments-property ()
  "Test `jemdoc-mode-keywords-in-comments-property-*'."
  (with-temp-buffer
    (insert jemdoc-mode-keywords-in-comments)
    (jemdoc-mode)
    (font-lock-ensure)
    (sleep-for 0.1)
    ;; there should be no text without a 'face property 'jemdoc-keywords-in-comments-property
    (let ((test-pairs '((   3 .   8)
			(  11 .  14)
			(  34 .  40)
			(  43 .  50)
			(  53 .  58)
			(  61 .  66)
			(  69 .  75)
			(  80 .  85)
			(  88 .  97)
			( 100 . 111)
			( 114 . 119)
			( 128 . 132)
			( 135 . 144)
			( 147 . 152)
			( 157 . 162)
			( 165 . 173)
			( 176 . 180)
			( 183 . 187)
			( 190 . 198)
			( 201 . 206)
			( 209 . 213))))
      (dolist (pair test-pairs)
	(should (equal (text-property-not-all  (car pair)  (cdr pair) 'face 'jemdoc-mode-face-special-keywords) nil))))
    ;; there should be no text with a 'face property 'jemdoc-keywords-in-comments-property
    (let ((test-pairs '((  15 .   32)
			( 120 .  126))))
      (dolist (pair test-pairs)
	(should (equal (text-property-any (car pair)  (cdr pair) 'face 'jemdoc-mode-face-special-keywords) nil))))))

(ert-deftest test-jemdoc-mode-concat-string ()
  "Test `jemdoc-mode-concat-string'."
  (should (equal (jemdoc-mode-concat-string "alo-" 4) "alo-alo-alo-alo-")))

(ert-deftest test-jemdoc-mode-end-of-block-delimiter ()
  "Test `jemdoc-mode-end-of-block-delimiter'."
  (should
   (equal (jemdoc-mode-end-of-block-delimiter "alo-" 3)
	  "\\(\\'\\|^ *$\\|^ *alo-alo-alo- +\\|^ *alo-alo- +\\|^ *alo- +\\)")))
