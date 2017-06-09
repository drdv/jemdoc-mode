(defvar jemdoc-mode-test-string
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

(ert-deftest test-jemdoc-mode-in-tilde-block-internal ()
  "Test `jemdoc-mode-in-tilde-block-internal'."
  (with-temp-buffer
    (insert jemdoc-mode-test-string)
    (jemdoc-mode)
    (write-file "drdv.jemdoc")
    ;; font-lock is disabled by default in temp buffers
    ;; https://stackoverflow.com/questions/23568779/elisp-font-lock-in-temp-buffer
    (font-lock-ensure) ;; apparently better than using (font-lock-fontify-buffer)
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

(ert-deftest test-face ()
  "Test `jemdoc-mode-in-tilde-block-internal'."
  (with-temp-buffer
    (insert jemdoc-mode-test-string)
    (jemdoc-mode)
    ;; font-lock is disabled by default in temp buffers
    ;; https://stackoverflow.com/questions/23568779/elisp-font-lock-in-temp-buffer
    (font-lock-ensure) ;; apparently better than using (font-lock-fontify-buffer)
    (sleep-for 0.1)
    (should (equal (get-text-property 1 'face) 'font-lock-comment-face))
    ))
