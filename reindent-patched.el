;;;; reindent-patched.el --- reindent code affected by a patch

;;;; If you have applied a patch that makes the textual changes you
;;;; want, but screws up the identation of the code, this file defines
;;;; an Emacs command that walks the patch, and re-indents the region
;;;; to which each hunk in the patch applies.
;;;;
;;;; To use it:

;;;; 1) Load this file.  Use 'M-x eval-buffer RET', for example, or
;;;;    put it in a directory listed in your load-path and put
;;;;      (require 'reindent-patched)
;;;;    in your .emacs file.
;;;; 2) Visit the patch file.
;;;; 3) Type 'M-x reindent-patched RET'.

(require 'cl)
(require 'diff-mode)

(defun reindent-kill-patch-buffers ()
  "Kill all buffers for files affected by the patch in the current buffer.
Prompt before killing buffers with unsaved changes.
This is helpful in avoiding 'file has changed on disk' questions,
which frequently come up if you're using a version control system
to sanity-check your changes."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward diff-file-header-re nil t)
      (let ((b (get-file-buffer (diff-find-file-name))))
        (when b
          ;; This will prompt if there are unsaved changes.
          (kill-buffer b))))))

(defun reindent-patched ()
  "Reindent the regions affected by the patch in the current buffer.
For each hunk in the patch, visit the file it applies to, find
the affected text, and run 'indent-region' on it.
The patch should already have been applied."
  (interactive)

  (reindent-kill-patch-buffers)

  ;; Don't use save-excursion: we only restore point if we succeed in
  ;; re-indenting the patched text.
  (let ((saved-point (point))
        changed-buffers)
    (goto-char (point-min))
    (while (re-search-forward diff-hunk-header-re nil t)
      (destructuring-bind (buf line-offset pos src dst &optional switched)
          (diff-find-source-location)

        ;; We're dealing with applied hunks.  If it seems like the
        ;; file hasn't been patched, error out.
        (unless switched 
          (error "hunk not yet applied"))

        ;; Build a list of the buffers we've touched.
        (unless (eq buf (car changed-buffers))
          (push buf changed-buffers))
        
        (save-excursion
          (set-buffer buf)

          ;; indent-region uses fill-prefix if it's set, so make sure
          ;; it isn't.
          (let (fill-prefix)
            (indent-region (car pos) (cdr pos))))))

    (while changed-buffers
      (save-excursion
        (set-buffer (car changed-buffers))
        (save-buffer)
        (pop changed-buffers)))

    (goto-char saved-point)))

(provide 'reindent-patched)
