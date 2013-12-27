Emacs interface to the `howdoi` command line tool.
============

howdoi - instant coding answers via the command line (https://github.com/gleitz/howdoi).

emacs-howdoi.el - interface to this tool.

This package allows to use `howdoi` command line utility from
whithin GNU Emacs. This utility needs to be installed and needs
to be located somewhere in the PATH (so GNU Emacs will be able
to find and use it).

Installation
============

First of all `howdoi` command line utility must be installed.

The utility and more information about usage and installation
you can find here: https://github.com/gleitz/howdoi

Then copy howdoi.el file to the ~/.emacs.d/ folder and put the line

    (load-file "~/.emacs.d/howdoi.el")

into the ~/.emacs configuration file.

Usage
============

This package provides several functions to deal with the utility:

    M-x howdoi-query-line-at-point

Takes a line at the point and makes `howdoi` query. Pop ups a buffer displaying the answer.

    M-x howdoi-query-line-at-point-replace

Takes a line at the point, makes `howdoi` query and replaces the line by the answer.

    M-x howdoi-query

Asks to put a query in the minibuffer and makes `howdoi` query. Pop ups a buffer displaying the answer.

Example
============

For example there is the line in the *scratch* buffer:

    python open file

point is located somewhere on this line.

After `M-x howdoi-query-line-at-point-replace` this line will be replaced by something like the following:

    fname = "C:\\Python32\\getty.txt"
    infile = open(fname, 'r')
    data = infile.read()
    print(data)

Author
============

Andrey Tykhonov (@atykhonov)

Notes
============

Please contribute if you like too! :) Otherwise write down your suggestions, comments, feature requests etc etc etc.

Thank you! And Enjoy!

TODO
============
* implement minor mode which will be activated in pop up buffer. The main purpose of this minor mode is to provide key bindings by means of which it will be possible to `goto` to the next/previous answers;
* make it so that howdoi.el will be able to guess the major mode for a pop up buffer.
* don't block Emacs when show the answer in pop up buffer.
