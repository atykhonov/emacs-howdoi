Howdoi.el -- instant coding answers via Emacs.
============

Do you find yourself constantly Googling for how to do basic
programing tasks? Suppose you want to know how to format a date in
bash. Why open your browser and read through blogs when you can just
M-x howdoi-query RET format date bash RET

Howdoi.el is a way to query Stack Overflow directly from the Emacs and
get back the most upvoted answer to the first question that comes up
for that query.

Installation
============

Copy howdoi.el file to the ~/.emacs.d/ folder and put the line

    (load-file "~/.emacs.d/howdoi.el")

into the ~/.emacs configuration file.

Usage
============

The following two commands show an answer in a pop up buffer:

     M-x howdoi-query RET <your-query> RET ;; e.g.: M-x howdoi-query RET format date bash RET
     M-x howdoi-query-line-at-point ;; takes a query from a line at point and shows an answer in a pop up buffer.

To get an answer containing only code snippet you could use:

     M-x howdoi-query-line-at-point-replace-by-code-snippet

this command replaces current line with a code snippet parsed from an answer.

With that command you could get situation when it returns not good
enough code snippet. Or may be after that command you would like to
get more details which relates to the original query. Then you could
use the following command:

    M-x howdoi-show-current-question

This one will show (in a pop up buffer) full answer which contains
recently inserted code snippet. This command may help sometimes to
avoid additional googling when original query is a little bit
ambiguous.

By default pop up buffer displays only answers. You could change
`howdoi-display-question` custom variable to show also a question.

In the mentioned pop up buffer enables HowDoI major-mode. There are
such key bindings are available:

* n - `howdoi-show-next-question`
* p - `howdoi-show-previous-question`
* b - `howdoi-browse-current-question`
* &lt; - `beginning-of-buffer`
* &gt; - `end-of-buffer`
* q - `quit window`

There is also howdoi-minor-mode available with a list of key bindings:

* C-c o n - `howdoi-show-next-question`
* C-c o p - `howdoi-show-previous-question`
* C-c o c - `howdoi-show-current-question`
* C-c o b - `howdoi-browse-current-question`
* C-c o q - `howdoi-query`
* C-c o l - `howdoi-query-line-at-point`
* C-c o r - `howdoi-query-line-at-point-replace-by-code-snippet`


Example
============

For example there is the line in the *scratch* buffer:

    python file exists

point is located somewhere on this line.

After `M-x howdoi-query-line-at-point-replace-by-code-snippet` this
line will be replaced by something like the following:

    try:
       with open('filename'):
           process()
       except IOError:
           print 'Oh dear.'

Other example: there is the line in the *scratch* buffer:

    elisp split string

After `M-x howdoi-query-line-at-point-replace-by-code-snippet` this
line will be replaced by:

(split-string "1.2.3" "\\.")

You may be wondered: why "." is escaped with two '\'? You could
execute then `M-x howdoi-show-current-question` and the full answer
will be shown so you'll be able to get an explanation.

And last example: just execute `M-x howdoi-query RET howdoi RET` and
it will return an answer in a pop up buffer.

In a pop up buffer you could press "n" to view next question, "p" to
view previous question and "b" to browse question in a www browser.

Author
============

Andrey Tykhonov (atykhonov at gmail.com; @atykhonov)

Notes
============

Please contribute if you like too! :) Otherwise write down your suggestions, comments, feature requests etc etc etc.

Thank you! And Enjoy!

Contributors
============
[Graydon Hoare](https://github.com/graydon)
