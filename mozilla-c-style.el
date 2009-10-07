;;; mozilla-c-style.el --- Mozilla's C/C++ style for c-mode and c++-mode

;; Keywords: C, C++, tools

;;; Commentary:

;;; This file defines a C/C++ indentation style named "Mozilla",
;;; appropriate for use editing Mozilla source code.  To use it, place
;;; the following code in your .emacs file:
;;;
;;;   (require 'mozilla-c-style)
;;;   (mozilla-auto-select-style)
;;;
;;; This will cause Emacs to check for the Mozilla-style copyright
;;; block at the top of each C/C++ source file you visit, and select
;;; the Mozilla indentation style if the copyright block is present.
;;; If you want to make "Mozilla" the default style for all your C/C++
;;; code, you can simply say:
;;;
;;;   (require 'mozilla-c-style)
;;;   (add-hook 'c-mode-common-hook 'mozilla-c-mode-style-hook))

;;; Code:

(defconst mozilla-c-style
  '((c-basic-offset . 4)
    (c-backslash-column . 78)
    (c-backslash-max-column . 98)
    (c-offsets-alist . ((case-label . *)
                        (statement-case-intro . *)
                        (statement-case-open . 0)
                        (statement-block-intro
                         . mozilla-lineup-case-block-intro)))))

(c-add-style "Mozilla" mozilla-c-style)

(defun mozilla-lineup-case-block-intro (element)
  "Indent the first line of a { block } for Mozilla style.
The c-mode and c++-mode indenter invokes this function to decide
how to indent the first line of a curly-brace-enclosed block
statement.  Normally, we simply indent by c-basic-offset, which
is four spaces in Mozilla style:

    {
        block body statement;
        ...
    }

But when the block has a case label, in the body of a switch, we
want this:

    switch (expr) {
      case k:
      {
        block body statement;
        ...
      }  
    }

In other words, the body of the case is indented by four spaces,
as if it were a direct child of the switch, whereas the case
label and its curly braces get half-indentation."
  (let ((anchor (cdr element)))
    (save-excursion
      ;; For statement-block-intro, the anchor position is the opening
      ;; curly brace.  Go there and find that position's syntactic
      ;; context; if it's a case label, then we do a half indent;
      ;; otherwise, a full indent.
      (goto-char anchor)
      (let* ((context-element (c-save-buffer-state nil
                                (c-guess-basic-syntax)))
             (syntactic-symbol (caar context-element))
             (context-anchor (cdar context-element)))
        (if (eq syntactic-symbol 'statement-case-open)
            '*
          '+)))))

(defun mozilla-source ()
  "Return true if the current buffer holds Mozilla source code.
This just checks for the license block header."
  (save-excursion
    (goto-char (point-min))
    ;; Don't search too far.
    (let ((bound (save-excursion (forward-line 50) (point))))
      (not (null (re-search-forward "[^]]BEGIN LICENSE BLOCK" bound t))))))

(defun mozilla-c-mode-style-hook ()
  "If the current buffer seems to contain mozilla code, select Mozilla style.
This is suitable for inclusion on c-mode-common-hook."
  (if (mozilla-source)
      (c-set-style "Mozilla")))

(defun mozilla-auto-select-style ()
  "Add 'mozilla-c-mode-style-hook' to c-mode-common-hook."
  (add-hook 'c-mode-common-hook 'mozilla-c-mode-style-hook))

(provide 'mozilla-c-style)

;;; mozilla-c-style.el ends here
