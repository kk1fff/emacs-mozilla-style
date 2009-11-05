;;;; diff-find-mq-file.el --- find files to which MQ patches apply

;;; When using Mercurial Queues, if some patch named
;;; ROOT/.hg/patches/fix-blurgh.patch contains hunks for some file
;;; a/urgh/blurgh.cpp, then those hunks should be applied to
;;; ROOT/urgh/blurgh.cpp.
;;;
;;; However, Emacs's diff-mode isn't smart enough to figure that out,
;;; meaning that commands like `diff-goto-source' or `diff-apply-hunk'
;;; prompt for the filename in an annoying way. Furthermore, diff-mode
;;; doesn't even provide any sort of hook for us to tell it how to do
;;; the right thing.
;;;
;;; The copy of diff-mode.el in the same directory as this file has
;;; been patched to consult an "abnormal hook" variable called
;;; diff-find-file-name-functions for help. This file defines an
;;; appropriate function for dealing with Mercurial Queue patch files
;;; and sticks it on that function list.
;;;
;;; To use it, place this file (and the patched diff-mode.el) in your
;;; Emacs load-path, and then put the following in your .emacs file:
;;;
;;;   (require 'diff-find-mq-file)

(defun diff-find-mq-file-name (filenames)
  (when (string-match "\\`\\(.*/\\)\\.hg/patches/\\'" default-directory)
    (let ((root (match-string 1 default-directory))
          found)
      (while filenames
        (let ((filename (car filenames)))
          (when (string-match "\\`[ab]/\\(.*\\)\\'" filename)
            (let* ((tail (match-string 1 filename))
                   (filename (expand-file-name tail root)))
              (when (file-regular-p filename)
                (setq found filename
                      filenames nil)))))
        (setq filenames (cdr filenames)))
      found)))

(add-hook 'diff-find-file-name-functions 'diff-find-mq-file-name)

(provide 'diff-find-mq-file)
