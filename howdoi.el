;;; howdoi.el --- instant coding answers via Emacs.

;; Copyright (C) 2014 Andrey Tykhonov
;; Free Software Foundation, Inc.

;; Author: Andrey Tykhonov <atykhonov at gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/emacs-howdoi/
;; Version: 0.4.3
;; Keywords: howdoi, convenience

;; Contributors:
;;   Graydon Hoare (https://github.com/graydon)
;;   Łukasz Jędrzejewski (https://github.com/jedrz)

;; This file is NOT part of GNU Emacs.

;; howdoi.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; howdoi.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with howdoi.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Do you find yourself constantly Googling for how to do basic
;; programing tasks? Suppose you want to know how to format a date in
;; bash. Why open your browser and read through blogs when you can
;; just M-x howdoi-query RET format date bash
;;
;; This package was inspired by Tom (adatgyujto at gmail.com). It was
;; his idea to make a port of command line tool such as python's
;; `howdoi`: https://github.com/gleitz/howdoi
;;
;; Thank you, Tom!
;;
;; Commands:
;;
;; The following two commands show an answer in a pop up buffer:
;; M-x howdoi-query RET <your-query>
;; M-x howdoi-query-line-at-point ;; takes a query from a line at point
;;
;; To get an answer containing only code snippet you could use:
;; M-x howdoi-query-line-at-point-replace-by-code-snippet
;;     this command replaces current line with a code snippet
;;     parsed from an answer.
;;
;; In case of last command you could get situation when it returns not
;; good enough code snippet. Or may be after that command you would
;; like to get more details which relates to the original query. Then
;; you could use the following command:
;;
;; M-x howdoi-show-current-question
;;
;; This one will show (in a pop up buffer) full answer which contains
;; recently inserted code snippet. This command may help sometimes to
;; avoid additional googling when original query is a little bit
;; ambiguous.
;;
;; By default pop up buffer displays only answers. You could change
;; `howdoi-display-question` custom variable to show also a question.
;;
;; In the mentioned pop up buffer enables HowDoI major-mode. There are
;; such key bindings are available:
;;
;; n - howdoi-show-next-question
;; p - howdoi-show-previous-question
;; b - howdoi-browse-current-question
;; u - howdoi-query
;; < - beginning-of-buffer
;; > - end-of-buffer
;; q - quit window
;;
;; There is also howdoi-minor-mode available with a list of key
;; bindings:
;;
;; C-c C-o n - howdoi-show-next-question
;; C-c C-o p - howdoi-show-previous-question
;; C-c C-o c - howdoi-show-current-question
;; C-c C-o b - howdoi-browse-current-question
;; C-c C-o u - howdoi-query
;; C-c C-o l - howdoi-query-line-at-point
;; C-c C-o r - howdoi-query-line-at-point-replace-by-code-snippet
;; C-c C-o i - howdoi-query-insert-code-snippet-at-point
;;
;;

;;; Code:

(defgroup howdoi nil
  "Instant coding answers via Emacs."
  :group 'extensions
  :group 'convenience
  :version "24.3"
  :link '(emacs-commentary-link "howdoi.el"))

(defcustom howdoi-display-question nil
  "Whether to display the question in a pop up buffer.
When non-nil, question is printed."
  :type 'boolean
  :group 'howdoi)

(defvar howdoi-current-stackoverflow-url nil
  "Temporal variable which holds stackoverflow url for current query.")

(defvar howdoi-question-urls '()
  "Temporal variable which holds urls parsed from Google.")

(defvar howdoi-current-question-num 0
  "Current number of the question which is currently displayed
in a pop up buffer.")

(defvar howdoi-display-callback nil
  "Temporal variable which is used to keep display callback
amongst functions.")

(defvar howdoi-requests-cache (make-hash-table :test 'equal)
  "Keeps cached answers to avoid surplus http requests.")

(defvar howdoi-original-buffer nil "Keeps a reference to the
original buffer in which user might performs howdoi query")


(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
  White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n\r]*" ""
                            (replace-regexp-in-string "[ \t\n\r]*\\'" "" string)))

;;;###autoload
(defun howdoi-query-line-at-point ()
  "Take a line at point, make the search using that line as a
query and pop up a buffer displaying the answer."
  (interactive)
  (let ((query (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
    (howdoi-request query 'howdoi-pop-answer-to-buffer-callback)))

;;;###autoload
(defun howdoi-query-line-at-point-replace-by-code-snippet ()
  "Take a line at the point, make the search using that line as a
query and replace the line by a found code snippet."
  (interactive)
  (let* ((query (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
        (setq howdoi-original-buffer (current-buffer))
        (howdoi-request query
                        'howdoi-replace-line-at-point-callback)))

(defun howdoi-replace-line-at-point-callback (question answers snippets)
  "Callback which calls immediately after http request. It
replaces a line at point by code snippet."
  (with-current-buffer howdoi-original-buffer
    (move-beginning-of-line nil)
    (kill-line nil)
    (insert (nth 0 snippets))))

(defun howdoi-format-question-and-answers (question answers)
  "Format output of QUESTION and ANSWERS."
  (let ((result (mapconcat (function (lambda (x)
                                       (trim-string x)))
                           answers "\n\n-------\n\n")))
    (when (and question howdoi-display-question)
      (setq result
            (format "%s\n\n=======\n\n%s" (trim-string question) result)))
    result))

(defun howdoi-pop-answer-to-buffer-callback (question answers snippets)
  "Callback which calls immediately after http request. Pop up a
buffer named *How do I* displaying the QUESTION, ANSWERS and SNIPPETS."
  (howdoi-pop-answer-to-buffer question answers))

(defun howdoi-pop-answer-to-buffer (question answers)
  "Pop up a buffer with an answer."
  (let ((howdoi-buffer (get-buffer-create "*How do I*")))
    (save-selected-window
      (with-current-buffer howdoi-buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert (howdoi-format-question-and-answers question answers))
        (howdoi-mode)
        (goto-char (point-min)))
      (pop-to-buffer howdoi-buffer))))

;;;###autoload
(defun howdoi-query (query)
  "Prompts for the QUERY and performs the search for an answer.
Pop up a buffer displaying an answer."
  (interactive "sQuery: ")
  (howdoi-request query 'howdoi-pop-answer-to-buffer-callback))

(defun howdoi-insert-code-snippet-at-point-callback (question answers snippets)
  "Callback which calls immediately after http request. Insert the first
snippet from SNIPPETS at point."
  (with-current-buffer howdoi-original-buffer
    (insert (car snippets))))

;;;###autoload
(defun howdoi-query-insert-code-snippet-at-point (query)
  "Prompt for the QUERY and perform the search for an answer.
Insert a found code snippet at point."
  (interactive "sQuery: ")
  (setq howdoi-original-buffer (current-buffer))
  (howdoi-request query 'howdoi-insert-code-snippet-at-point-callback))

(defun howdoi-request (query callback &optional &key full-answer question)
  "Make http request to the Google. Use QUERY as search
string. CALLBACK calls after http request to display the
results."
  (setq howdoi-display-callback callback)
  (setq howdoi-current-question-num 0)
  (setq howdoi-requests-cache (make-hash-table :test 'equal))
  (let ((url-request-method "GET")
        (url "http://google.com/search")
        (args (concat "?q="
                      (url-hexify-string "site:stackoverflow.com ")
                      (url-hexify-string query))))
    (message "Retrieving answer...")
    (url-retrieve (concat url args)
                  (lambda (status)
                    (setq howdoi-question-urls (howdoi-retrive-links-from-google
                                                (current-buffer)))
                    (howdoi-stackoverflow-request (nth howdoi-current-question-num
                                                       howdoi-question-urls)))
                  nil t)))

(defun howdoi-retrive-links-from-google (buffer)
  "Retrieves links from a google search result page."
  (let ((result '()))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (search-forward "<h3 class=\"r\">" nil t)
        (when (search-forward-regexp "<a href=\"\\([^\"]*\\)\".*?>" nil t)
          (let ((str (match-string 1)))
            (setq str (nth 1 (split-string str "q=")))
            (setq str (nth 0 (split-string str "&")))
            (setq result (append result `(,str)))))))
    result))

(defun howdoi-stackoverflow-request (url)
  "Make http request to the stackoverflow."
  (let ((url-request-method "GET")
        (cache (gethash url howdoi-requests-cache)))
    (if cache
        (funcall howdoi-display-callback
                 (nth 0 cache)
                 (nth 1 cache)
                 (nth 2 cache))
      (progn
        (setq howdoi-current-stackoverflow-url url)
        (message "Retrieving answer...")
        (url-retrieve url
                      (lambda (status)
                        (let ((answers '()) (snippets '()) (question ""))
                          (setq answers (howdoi-stackoverflow-retrieve-answer))
                          (setq snippets (howdoi-stackoverflow-retrieve-code-snippets))
                          (when howdoi-display-question
                            (setq question (howdoi-stackoverflow-retrieve-question)))
                          (puthash howdoi-current-stackoverflow-url
                                   `(,question ,answers ,snippets) howdoi-requests-cache)
                          (message "Answer retrieved!")
                          (funcall howdoi-display-callback question answers snippets)))
                      nil t)))))

(defun howdoi-browse-url (button)
  "Retrieve URL from a BUTTON's property and browse it."
  (interactive)
  (let ((url (button-get button 'url)))
    (browse-url url)))

(defun howdoi-replace-html-ahref-tags ()
  "Replace a-href tags by text buttons."
  (interactive)
  (let (p1 p2 p3 p4 p5 p6 (ahref-tag "") (href "") (link-text ""))
    (while (search-forward-regexp "<a[^>]*href=\"[^\"]*?\"[^>]*?>" nil t)
      (setq p5 (point))
      (search-backward "<a" (point-min) t)
      (setq p1 (point))
      (search-forward "href=\"" nil t)
      (setq p2 (point))
      (search-forward "\"" nil t)
      (setq p3 (- (point) 1))
      (setq href (buffer-substring-no-properties p2 p3))
      (search-forward "</a>" nil t)
      (setq p6 (point))
      (setq link-text (buffer-substring-no-properties p5 (- p6 4)))
      (delete-region p1 p6)
      (insert-text-button link-text 'action 'howdoi-browse-url 'url href))))

(defun howdoi-stackoverflow-retrieve-question ()
  "Retrieve a question from the stackoverflow."
  (goto-char (point-min))
  (let ((result ""))
    (when (search-forward-regexp "<div[^>]*?class=\"question" nil t)
      (when (search-forward-regexp "<td class=\"postcell\">" nil t)
        (when (search-forward-regexp "<div class=\"post-text\"[^>]*>\\(\\(.*?\n?\\)*\\)</div>" nil t)
          (let ((str (match-string 1)))
            (with-temp-buffer
              (erase-buffer)
              (insert str)
              (goto-char (point-min))
              (howdoi-replace-html-ahref-tags)
              (html2text)
              (howdoi-strip-html-tags '("code"))
              (setq result (buffer-substring
                            (point-min)
                            (point-max))))))))
    result))

(defun howdoi-stackoverflow-retrieve-answer ()
  "Retrieve an answer from the stackoverflow."
  (goto-char (point-min))
  (let ((result '()))
    (while (search-forward-regexp "<div[^>]*?class=\"answer" nil t)
      (when (search-forward-regexp "<td class=\"answercell\">" nil t)
        (when (search-forward-regexp "<div class=\"post-text\">\\(\\(.*?\n?\\)*\\)</div>" nil t)
          (let ((str (match-string 1)))
            (with-temp-buffer
              (erase-buffer)
              (insert str)
              (goto-char (point-min))
              (howdoi-replace-html-ahref-tags)
              (html2text)
              (howdoi-strip-html-tags '("code"))
              (setq result (append result `(,(buffer-substring (point-min) (point-max))))))))))
    result))

(defun howdoi-strip-html-tags (tags)
  "Strip given html TAGS."
  (dolist (tagn tags)
    (dolist (tag `(,(format "<%s>" tagn) ,(format "</%s>" tagn)))
      (goto-char (point-min))
      (while (search-forward tag nil t)
        (replace-match "" nil t)))))

(defun howdoi-stackoverflow-retrieve-code-snippets ()
  "Retrieve code snippets from the stackoverflow."
  (goto-char (point-min))
  (let ((result '()))
    (while (search-forward-regexp "<div[^>]*?class=\"answer" nil t)
      (if (search-forward-regexp "<pre[^>]*>" nil t)
          (when (search-forward-regexp "<code>\\([^<]*?\\)</code>" nil t)
            (let ((str (match-string 1)))
              (setq result (append result `(,str)))))
        (when (search-forward-regexp "<code>\\(.*?\\)</code>" nil t)
          (let ((str (match-string 1)))
            (setq result (append result `(,str)))))))
    result))

(defun howdoi-show-next-question ()
  "Show next question. The purpose of this function to use it in
the *How do I* pop up buffer to view next question."
  (interactive)
  (setq howdoi-display-callback 'howdoi-pop-answer-to-buffer-callback)
  (setq howdoi-current-question-num (+ howdoi-current-question-num 1))
  (when (> howdoi-current-question-num 10)
    (setq howdoi-current-question-num 10))
  (howdoi-stackoverflow-request (nth howdoi-current-question-num
                                     howdoi-question-urls)))

(defun howdoi-show-current-question ()
  "Pop up a buffer named *How do I* displaying the current found
question. It may be helpful to use after such command as
`howdoi-query-line-at-point-replace-by-code-snippet' to view more
details in a pop up buffer or to find more preferable code
snippet."
  (interactive)
  (let* ((url (nth howdoi-current-question-num howdoi-question-urls))
         (cache (gethash url howdoi-requests-cache)))
    (if cache
        (howdoi-pop-answer-to-buffer (nth 0 cache) (nth 1 cache))
      (message "Current question not found"))))

(defun howdoi-show-previous-question ()
  "Show previous question. The purpose of this function to use it in
the *How do I* pop up buffer to view previous question."
  (interactive)
  (setq howdoi-display-callback 'howdoi-pop-answer-to-buffer-callback)
  (setq howdoi-current-question-num (- howdoi-current-question-num 1))
  (when (< howdoi-current-question-num 0)
      (setq howdoi-current-question-num 0))
  (howdoi-stackoverflow-request (nth howdoi-current-question-num
                                     howdoi-question-urls)))

(defun howdoi-browse-current-question ()
  "Ask a WWW browser to open current question."
  (interactive)
  (let ((url (nth howdoi-current-question-num howdoi-question-urls)))
    (browse-url url)))

;;;###autoload
(define-minor-mode howdoi-minor-mode
  "Toggle howdoi minor mode. 

With a prefix argument ARG, enable Line Number mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

This minor mode provides a set of key bindings for easy access to
the howdoi.

The following keys are available in `howdoi-minor-mode':

  key             binding
-------         -----------

C-c C-o n       howdoi-show-next-question
C-c C-o p       howdoi-show-previous-question
C-c C-o c       howdoi-show-current-question
C-c C-o b       howdoi-browse-current-question
C-c C-o u       howdoi-query
C-c C-o l       howdoi-query-line-at-point
C-c C-o r       howdoi-query-line-at-point-replace-by-code-snippet
C-c C-o i       howdoi-query-insert-code-snippet-at-point"
  :lighter " HowDoI"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-o n") 'howdoi-show-next-question)
            (define-key map (kbd "C-c C-o p") 'howdoi-show-previous-question)
            (define-key map (kbd "C-c C-o c") 'howdoi-show-current-question)
            (define-key map (kbd "C-c C-o b") 'howdoi-browse-current-question)
            (define-key map (kbd "C-c C-o u") 'howdoi-query)
            (define-key map (kbd "C-c C-o l") 'howdoi-query-line-at-point)
            (define-key map (kbd "C-c C-o r") 'howdoi-query-line-at-point-replace-by-code-snippet)
            (define-key map (kbd "C-c C-o i") 'howdoi-query-insert-code-snippet-at-point)
            map)
  :group 'howdoi)

(define-derived-mode howdoi-mode special-mode "HowDoI"
  "Howdoi major mode. This major mode is mainly intended to
provide key bindings for easy navigation within a pop up buffer."
  :group 'howdoi)

(define-key howdoi-mode-map (kbd "n") 'howdoi-show-next-question)
(define-key howdoi-mode-map (kbd "p") 'howdoi-show-previous-question)
(define-key howdoi-mode-map (kbd "b") 'howdoi-browse-current-question)
(define-key howdoi-mode-map (kbd "u") 'howdoi-query)


(provide 'howdoi)

;;; howdoi.el ends here
