;;; howdoi.el --- Emacs interface to the `howdoi` command line tool.

;; Copyright (C) 2013 Andrey Tykhonov
;; Free Software Foundation, Inc.

;; Author: Andrey Tykhonov <atykhonov at gmail.com>
;; Version: 0.1.0
;; Keywords: howdoi

;; This file is NOT part of GNU Emacs.

;; howdoi.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; howdoi.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows to use howdoi command line utility from
;; whithin GNU Emacs. This utility needs to be installed and needs
;; to be located somewhere in the PATH (so GNU Emacs will be able
;; to find and use it).
;;
;; The utility and more information about usage and installation
;; you can find here: https://github.com/gleitz/howdoi

;;; Code:

(defcustom howdoi-display-full-answer nil
  "Whether to display the full text of the answer.
When non-nil, full text is printed."
  :type 'boolean
  :group 'howdoi)

(defcustom howdoi-number-of-answers 1
  "Controls how many answers to return."
  :type 'integer
  :group 'howdoi)

(defcustom howdoi-display-only-answer-link nil
  "Whether to display only the answer link.
When non-nil, only the answer link is printed."
  :type 'boolean
  :group 'howdoi)

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
  White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" ""
                            (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun howdoi-query-line-at-point ()
  "Take line at point and make howdoi query.
Pop up a buffer displaying the answer."
  (interactive)
  (let ((query (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
    (howdoi-pop-answer-to-buffer
     (howdoi-shell-command-to-string query))))

(defun howdoi-query-line-at-point-replace ()
  "Take the line at the point, make howdoi query
and replace the line by the answer."
  (interactive)
  (let* ((query (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
        (buffer (current-buffer))
        (answer (howdoi-shell-command-to-string query)))
    (with-current-buffer buffer
      (move-beginning-of-line nil)
      (kill-line nil)
      (insert answer))))

(defun howdoi-shell-command-to-string (query)
  "Execute howdoi shell command and return answer as the string."
  (let* ((display-full-answer-arg
          (if howdoi-display-full-answer "-a " ""))
         (display-only-answer-link-arg
          (if howdoi-display-only-answer-link "-l " ""))
         (number-of-answers-arg
          (if howdoi-number-of-answers
              (format "-n %d " howdoi-number-of-answers) ""))
         (shell-command (format "howdoi %s%s%s%s"
                                display-full-answer-arg
                                display-only-answer-link-arg
                                number-of-answers-arg
                                query)))
    (trim-string
     (shell-command-to-string shell-command))))

(defun howdoi-pop-answer-to-buffer (answer)
  "Pop up a buffer named *How do I* displaying the answer."
  (let ((howdoi-buffer (get-buffer-create "*How do I*")))
    (save-selected-window
      (with-current-buffer howdoi-buffer
        (erase-buffer)
        (insert answer))
      (pop-to-buffer howdoi-buffer))))

(defun howdoi-query (query)
  "Prompts for the query and makes howdoi query.
Pop up a buffer displaying the answer."
  (interactive "sQuery: ")
  (howdoi-pop-answer-to-buffer
   (howdoi-shell-command-to-string query)))

(provide 'howdoi)

;;; howdoi.el ends here
