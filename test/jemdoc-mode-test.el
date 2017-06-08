(defvar jemdoc-mode-test-string
  "# jemdoc: menu{MENU}{test.html}, fwtitle, nofooter

== test various tilde-blocks

~~~
{}{img_left}{FILENAME.IMG}{alt text}{WIDTHpx}{HEIGHTpx}{IMGLINKTARGET}
Ordinary jemdoc markup goes here.
~~~
1
2
3
~~~
Save the file as +index.jemdoc+, say, and simply call +jemdoc index+ (after
[download.html downloading jemdoc], of course).
~~~
1
2
3
~~~
{Simple block}
This is a simple text block, with a title. Notice how the previous line has only
one set of braces (\{\}).
~~~
1
2
3
~~~
{Optionally empty title}{Optionally empty highlight mode}
Code block with monospaced text.
~~~
1
2
3
~~~
{}{raw}
Any text here will be copied straight to the output file without processing.
~~~
")

(ert-deftest test-jemdoc-mode-in-tilde-block-internal ()
  "Test `jemdoc-mode-in-tilde-block-internal'."
  (with-temp-buffer
    (insert jemdoc-mode-test-string)
    (jemdoc-mode)
    (sit-for 0.1)
    (let ((test-cases '((general-block  72 nil)
			(general-block  73 (69 . 207))
			(general-block  80 (69 . 207))
			(general-block 117 (69 . 207))
			(general-block 140 (69 . 207))
			(general-block 207 (69 . 207))
			(general-block 208 nil))))
      (dolist (test-case test-cases)
	(goto-char (nth 1 test-case))
	(should (equal (jemdoc-mode-in-tilde-block-internal (nth 0 test-case)) (nth 2 test-case)))))))

(jemdoc-mode-in-tilde-block-internal 'general-block)
