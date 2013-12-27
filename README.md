Emacs interface to the `howdoi` command line tool.
============

howdoi - instant coding answers via the command line.

emacs-howdoi.el - interface to this tool.

This package allows to use `howdoi` command line utility from
whithin GNU Emacs. This utility needs to be installed and needs
to be located somewhere in the PATH (so GNU Emacs will be able
to find and use it).

The utility and more information about usage and installation
you can find here: https://github.com/gleitz/howdoi


This package provides several functions to deal with the utility:

    M-x howdoi-query-line-at-point

Takes a line at the point and makes `howdoi` query. Pop ups a buffer displaying the answer.

    M-x howdoi-query-line-at-point-replace

Takes a line at the point, makes `howdoi` query and replaces the line by the answer.

    M-x howdoi-query

Asks to put a query in the minibuffer and makes `howdoi` query. Pop ups a buffer displaying the answer.

For example there is the line in the *scratch* buffer:

    python open file

point is located somewhere on this line.

After `M-x howdoi-query-line-at-point-replace` this line will be replaced by:

    fname = "C:\\Python32\\getty.txt"
    infile = open(fname, 'r')
    data = infile.read()
    print(data)
