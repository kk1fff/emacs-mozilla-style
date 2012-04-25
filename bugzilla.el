(require 'url)

(defconst bugzilla-show-bug-url-format
  "https://bugzilla.mozilla.org/show_bug.cgi?id=%s")

(defvar bugzilla-previous-buffer nil)

(defun bugzilla-get-bug (n)
  (if (buffer-live-p bugzilla-previous-buffer)
      (kill-buffer bugzilla-previous-buffer))
  (setq bugzilla-previous-buffer
        (url-retrieve-synchronously (format bugzilla-show-bug-url-format n))))

(defun bugzilla-bug-from-firefox-title ()
  "Guess a default bug number by looking at Firefox's window's title.
Return the guess as a number; return nil if we have no plausible guess."
  (with-temp-buffer
    (unless (zerop (call-process "firefox-title" nil t))
      ;; trim trailing newline
      (when (and (> (buffer-size) 0)
                 (= (char-after (1- (point-max))) ?\n))
        (delete-region (1- (point-max)) (point-max)))
      (error "Error running firefox-title: %s" (buffer-string)))
    (goto-char (point-min))
    (if (looking-at "\\`\\([0-9]+\\) [-–] ")
        (string-to-number (match-string 1)))))

(defun bugzilla-bug-number (prompt)
  "Prompt for a bug number.
Default to the number in the Firefox window title or the number near point (if any)."
  (let ((default
          (or
           (bugzilla-bug-from-firefox-title)
           (save-excursion
             (unless (looking-at "\\<") (backward-word))
             (when (looking-at "\\<[0-9]+\\>")
               (string-to-number (match-string 0)))))))
    (read-number (concat prompt ": ") default)))

(defun bugzilla-bug-title (n)
  "Return the title of Bugzilla bug N."
  (let ((buffer (bugzilla-get-bug n)))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "<title>")
      (let ((start (point)))
        (re-search-forward "</title>")
        (narrow-to-region start (match-beginning 0))
        (goto-char (point-min))
        (while (re-search-forward "&\\([a-z]+\\);" nil t)
          (let* ((name (match-string 1))
                 (def (assoc name '(("quot" . "\"")
                                    ("ndash" . "-")
                                    ("gt" . ">")
                                    ("lt" . "<")
                                    ("amp" . "&")))))
            (when def
              (delete-region (match-beginning 0) (match-end 0))
              (insert (cdr def)))))
        (goto-char (point-min))
        (while (re-search-forward "&#\\([0-9a-z]+\\);" nil t)
          (let ((code (string-to-number (match-string 1))))
            (delete-region (match-beginning 0) (match-end 0))
            (insert-char code 1)))
        (buffer-substring (point-min) (point-max))))))

(defun bugzilla-clean-title-for-org-mode (title)
  "Return the argument with [square brackets] changed to (parens).
Brackets occur in bugzilla bug titles, but the Org mode link syntax doesn't like them."
  (let ((copy (concat title))
        (i 0))
    (while (< i (length copy))
      (let ((ch (aref copy i)))
        (aset copy i (cond
                      ((eq ch ?\[) ?\()
                      ((eq ch ?\]) ?\))
                      (t ch)))
        (setq i (1+ i))))
    copy))

(defun bugzilla-insert-link (n)
  "Insert an org-mode link to a Bugzilla bug."
  (interactive (list (bugzilla-bug-number "Bug number")))
  (let ((title (bugzilla-clean-title-for-org-mode (bugzilla-bug-title n))))
    (insert (format "[[%s][%s]]" 
                    (format bugzilla-show-bug-url-format n)
                    title))))

(provide 'bugzilla)
