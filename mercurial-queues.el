;;; mercurial-queues.el --- commands for working with Mercurial patch queues

;; Copyright (C) 2009 Jim Blandy

;; Author: Jim Blandy <jimb@red-bean.com>
;; Version: 0.1

;; mercurial-queues.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 2, or (at your option) any later version.
;;
;; mercurial-queues.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides commands for working with Mercurial patch
;; queues, described here:
;;
;; http://mercurial.selenic.com/wiki/MqExtension
;; http://hgbook.red-bean.com/read/managing-change-with-mercurial-queues.html
;;
;; Briefly, a queue is an ordered series of patches meant to apply to
;; some source tree under Mercurial's control. The 'hg qpush' command
;; applies the next patch in the series; 'hg qpop' un-applies the last
;; applied patch; and 'hg qrefresh' integrates any changes you've made
;; to the working files into the top patch. Queues are useful for
;; maintaining a set of patches you intend to submit for review:
;; instead of merging others' changes into a queue, one pops the
;; queue, updates, and then re-pushes, adjusting and refreshing the
;; patches as appropriate. This way, the patches stay applicable to
;; the latest sources, ready for review.
;;
;; This package provides:
;;
;; - Emacs commands for pushing, refreshing, and popping patches,
;;   creating new patches, deleting patches, and incorporating patches
;;   into the ordinary Mercurial history, and
;;
;; - An Emacs mode for viewing and editing the patch series, showing
;;   which patches are currently applied.
;;
;; Installation
;; ============
;;
;; To use this package, place this file in a directory listed in your
;; load-path, and then put the following in your .emacs file:
;;
;;   (require 'mercurial-queues)
;;   (add-to-list 'auto-mode-alist '(".hg/patches/series$" . mq-queue-mode))
;;
;; Usage
;; =====
;; With this package loaded, the following commands are available:
;;
;;   C-x q n --- Apply the next patch in the series (as by 'hg qpush').
;;   C-x q p --- Un-apply the last applied patch (as by 'hg qpop').
;;   C-x q r --- Incorporate the current changes to the working files
;;               into the top patch (as by 'hg qrefresh').
;;   C-x q s --- Visit the series file in a Queue Mode buffer. Queue
;;               Mode shows which patches are currently applied, and
;;               provides commands to jump to a given patch, edit the
;;               series, and so on. Visit the Queue Mode documentation
;;               with `C-h m queue-mode RET' for details.
;;   C-x 4 q s --- Visit the series file in another window.
;;
;; This package takes over the global binding of `C-x q' as a prefix
;; for its commands. This is normally bound to `kbd-macro-query', but
;; that command is also available as `C-x C-k q', under the common
;; `C-x C-k' prefix for all macro-related commands.
;;
;; Ideas
;; =====
;;
;; We should refresh the patch buffer after a qrefresh.
;;
;; When pushing a patch produces a conflict, it would be nice to
;; gather up the .rej files into a diff-mode buffer. Users could use
;; the diff-mode commands to visit the erstwhile context, etc.

(require 'cl)


;;; Customizable things.

(defface mq-applied-patch '((t :weight bold))
  "Face for applied patches in a Mercurial Queues `series' file.")

(defface mq-condition-positive-selected
  '((((class color)) :foreground "#00c011"))
  "Face for positive guard conditions (#+) whose guards are selected.")

(defface mq-condition-negative-selected
  '((((class color)) :foreground "#b60000"))
  "Face for negative guard conditions (#+) whose guards are selected.")


;;; Utility functions.

(defun mq-find-hg-root ()
  "Return the root of the Mercurial tree containing the currently visited file.
This is the directory containing the `.hg' subdirectory.

Note that it can be useful for the ROOT/.hg/patches directory
itself to be a Mercurial root, with its own metadata in
ROOT/.hg/patches/.hg. Calling this function in such a directory
should arguably return ROOT/.hg/patches.

However, it doesn't seem very useful to have queues of patches to
patches (\"metaqueues\"!), and it does seem useful for the global
series commands --- mq-visit-series, etc. --- to operate using
ROOT when invoked in ROOT/.hg/patches, regardless of whether that
directory is itself a root.  So we do that."
  (let ((dir default-directory))
    (unless dir
      (error "Selected buffer has no default directory"))
    ;; If dir is ROOT/.hg/patches, return ROOT regardless of whether
    ;; the patches directory is itself an hg repo.
    (if (string-match "\\`\\(.*/\\)\\.hg/patches/\\'" dir)
        (match-string 1 dir)
      (while (not (file-directory-p (expand-file-name ".hg" dir)))
        (let ((parent (file-name-directory (directory-file-name dir))))
          ;; We've reached the root when the above hands us back the
          ;; same thing we gave it.
          (when (string-equal dir parent)
            (error "directory not managed by mercurial: %s" default-directory))
          (setq dir parent)))
      dir)))

(defun mq-find-patch-directory (root)
  "Return the patch directory for the Mercurial root directory ROOT."
  (let ((patches (expand-file-name ".hg/patches" root)))
    (unless (file-directory-p patches)
      (error "Mercurial tree has no patch queue: %s" root))
    patches))

(defun mq-find-series-file (root)
  "Return the series filename for the Mercurial root directory ROOT."
  (let* ((patch-dir (mq-find-patch-directory root))
         (series (expand-file-name "series" patch-dir)))
    (unless (file-exists-p series)
      (error "Patch queue has no 'series' file: %s" patch-dir))
    series))

(defun mq-check-for-queue ()
  "Raise an error if the current directory has no associated patch queue."
  (mq-find-patch-directory (mq-find-hg-root)))

(defun mq-read-status (filename)
  "Parse the contents of the MQ status file.
The return value is a list of the applied patches, from last to earliest."
  (if (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (let (patches)
          (while (< (point) (point-max))
            (unless (looking-at "[0-9a-f]*:\\(.*\\)$")
              (error "unrecognized line in MQ status file"))
            (push (match-string 1) patches)
            (forward-line 1))
          patches))
    nil))

(defun mq-read-guards (filename)
  "Parse the contents of the MQ guards file.
The return value is a list of the selected guards, as symbols.
If there is no 'guards' file, return nil."
  (if (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (let (guards)
          (while (< (point) (point-max))
            (let ((start (point)))
              (end-of-line)
              (let ((guard (intern (buffer-substring start (point)))))
                (push guard guards)
                (forward-line 1))))
          guards))
    nil))

(defun mq-parse-conditions ()
  "Parse the current patch's guard conditions.
This assumes point is on the patch's line in the series file.
The return value is a list (ANYPOS TABLE), where:
- ANYPOS is true (non-nil) if there are any positive guard conditions, and
- TABLE is a hash table mapping guard names (as interned symbols)
  to the symbols `+' (for positive guards) or `-' (for negative
  guards).
For example, parsing the line:

   printer-param.patch                     #+foo-bar -baz -quux

yields `(t TABLE)', where TABLE maps `foo-bar' to `+', and `baz' and
`quux' to `-'."
  (let (any-positive
        (table (make-hash-table :test (function eq))))
    (save-excursion
      (end-of-line)
      (let ((end (point)))
        (beginning-of-line)
        (while (re-search-forward "#\\([-+]\\)\\([^#[:blank:]]+\\)" end t)
          (let ((state (intern (match-string 1)))
                (guard (intern (match-string 2))))
            (puthash guard state table)
            (when (eq state '+)
              (setq any-positive t))))))
    (list any-positive table)))

(defun mq-patch-enabled-p (guards conditions)
  "Return true if GUARDS satisfies CONDITIONS.
GUARDS is a list of the currently selected guards, as interned symbols.
CONDITIONS is a (ANYPOS . TABLE) list representing a set of guard
conditions, of the type returned by mq-parse-conditions.

Mercurial seems to enable a patch if:
- it has no negative conditions whose guards are selected, and
- it has either:
  - no positive guard conditions, or
  - one of the positive conditions' guards is selected."
  (let ((anypos (car conditions))
        (table (cadr conditions))
        found-negative
        found-positive)
    (mapcar (lambda (g) (case (gethash g table)
                          ((+) (setq found-positive t))
                          ((-) (setq found-negative t))))
            guards)
    (and (not found-negative)
         (or (not anypos) found-positive))))

(defun mq-find-condition-guards ()
  "Return a list of all guards mentioned in the current buffer's conditions.
This scans the current buffer, presumed to be a series file, for
guard conditions, and returns a list of all the guards mentioned,
as symbols."
  (let ((guards (make-hash-table :test (function eq))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#[-+]\\([^#[:space:]]+\\)" nil t)
        (let ((guard (intern (match-string 1))))
          (puthash guard t guards))))
    (let (guard-list)
      (maphash (lambda (g v) (push g guard-list))
               guards)
      guard-list)))

(defun mq-read-series-line ()
  "Parse the current series file line, returning the patch name and conditions.
The return value is nil if the current line contains no patch, or 

 a list of the form (NAME CONDITIONS) where
NAME is the name of the patch file, and CONDITIONS represents the
guard conditions for enabling the patch. CONDITIONS is a value of
the sort returned by mq-parse-conditions."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "\\s-*\\([^#[:blank:]]+\\)")
        (list (match-string 1) (mq-parse-conditions)))))

(defun mq-shell-command (string &rest args)
  "Run the shell command COMMAND, and tell the user we're doing so."
  (let ((command (apply (function format) string args)))
    (message "Running command: %s" command)
    (shell-command command)))


;;; Queue Mode, for editing series files.

;; Buffer-local variables.
(defvar mq-status-filename nil
  "Name of the `status' file for this series buffer.")

(defvar mq-guards-filename nil
  "Name of the `guards' file for this series buffer.")

(defvar mq-status nil
  "A list of the patches currently applied for this series buffer.
The older pushed patches appear earlier in the list.")

(defvar mq-guards nil
  "The selected guards for this series buffer's queue.
This is a list of symbols named after the guards.")

(defvar mq-font-lock-keywords nil
  "A list of computed regular expressions for font-lock mode to highlight.
These include selected guards, applied patches, and so on.
The value of this variable changes as the user pushes and pops patches,
changes the set of selected guards, and so forth.  We abuse font-lock mode
a little bit to make this work.")

(defun mq-compute-font-lock ()
  "Compute the current buffer's font lock keywords, based on status and guards."
  (let (keywords)
    (mapc (lambda (patch)
            (let ((regexp (format "^\\s-*%s[# \t\n]" (regexp-quote patch))))
              (push `(,regexp . 'mq-applied-patch)
                    keywords)))
          mq-status)
    (mapc (lambda (guard)
            (let* ((guard-regexp (regexp-quote (symbol-name guard)))
                   (positive-regexp (concat "#\\+" guard-regexp))
                   (negative-regexp (concat "#-" guard-regexp)))
              (push `(,positive-regexp . 'mq-condition-positive-selected)
                    keywords)
              (push `(,negative-regexp . 'mq-condition-negative-selected)
                    keywords)))
          mq-guards)
    (setq mq-font-lock-keywords (nreverse keywords))
    ;; Tell font-lock mode to recompute its regexps.  This is kind of a kludge. 
    (set (make-local-variable 'font-lock-set-defaults) nil)
    (font-lock-fontify-buffer)))

(defun mq-refresh-status-and-guards ()
  "Reread the `status' and `guards' files for the current series buffer."
  (unless (eq major-mode 'mq-queue-mode)
    (error "mq-refresh-status-and-guards should only run in a series buffer"))
  (setq mq-status (mq-read-status mq-status-filename))
  (setq mq-guards (mq-read-guards mq-guards-filename))
  (mq-compute-font-lock))

(defun mq-queue-mode ()
  "A major mode for working with Mercurial Queue patch series files.
If the buffer visits a series file in the `.hg/patches' directory
of a Mercurial repository, then the buffer's default directory is
the top of that repository.

The following commands are available in all buffers:

  \\[mq-qpush]	Apply the next patch in the series (as by 'hg qpush').
  \\[mq-qpop]	Apply the next patch in the series (as by 'hg qpop').
  \\[mq-qrefresh]	Incorporate the current changes to the working files
		into the top patch (as by 'hg qrefresh').
  \\[mq-visit-series]	Visit the series file relevant to the current buffer in
                a Queue Mode buffer.
  \\[mq-visit-series]	As above, but visit the series file in another window.

Visiting a queue series file in Queue Mode provides highlighting
showing the current state of the queue, and special commands for
editing series files.

- Names of applied patches appear in bold (the `mq-applied-patch' face).

- Positive guard conditions (`#+foo') whose guards are selected
  appear in green (the `mq-condition-positive-selected' face).

- Negative guard conditions (`#-foo') whose guards are selected
  appear in red (the `mq-condition-negative-selected' face).

The following commands are available in the series file:
\\<mq-queue-mode-map>
  \\[mq-go-to-patch]	Push or pop patches as necessary to make the patch on the
		current line the top patch.
  \\[mq-find-patch]	Visit the patch file on the current line.
  \\[mq-find-patch-other-window]	Visit the patch file on the current line in another window.

Here is a complete list of the bindings available in Queue Mode:

\\{mq-queue-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mq-queue-mode)
  (setq mode-name "Queue")
  (use-local-map mq-queue-mode-map)
  (if (string-match "\\`\\(.*/\\)\\.hg/patches/series" buffer-file-name)
      (setq default-directory (match-string 1 buffer-file-name)))
  
  (set (make-local-variable 'mq-status-filename)
       (expand-file-name ".hg/patches/status"))
  (set (make-local-variable 'mq-guards-filename)
       (expand-file-name ".hg/patches/guards"))
  (make-local-variable 'mq-status)
  (make-local-variable 'mq-guards)
  (make-local-variable 'mq-font-lock-keywords)
  (setq font-lock-defaults '(mq-font-lock-keywords))
  (mq-refresh-status-and-guards)

  (run-mode-hooks 'mq-queue-mode-hook))

(defalias 'queue-mode 'mq-queue-mode)

(defvar mq-queue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'mq-go-to-patch)
    (define-key map "\C-c\C-f" 'mq-find-patch)
    (define-key map "\C-c4\C-f" 'mq-find-patch-other-window)
    map))

(defun mq-refresh-buffers (root)
  "Revert all unmodified buffers visiting changed files in ROOT."
  (let ((root-regexp (concat "\\`" (regexp-quote root))))
    (loop for buffer in (buffer-list)
          do (save-excursion
               (set-buffer buffer)
               (if (and (not (buffer-modified-p))
                        buffer-file-name
                        (string-match root-regexp buffer-file-name)
                        (file-exists-p buffer-file-name)
                        (not (verify-visited-file-modtime (current-buffer))))
                   (revert-buffer t t))))))

(defun mq-refresh ()
  "Refresh Emacs's state after pushing or popping patches.
This reverts all unmodified buffers visiting files in the the
current mercurial tree, if the visited file seems to have changed."
  (let* ((root (mq-find-hg-root))
         (series (mq-find-series-file root))
         (buffer (get-file-buffer series)))
    (if buffer
        (save-excursion
          (set-buffer buffer)
          (mq-refresh-status-and-guards)))
    (mq-refresh-buffers root)))

(defun mq-go-to-patch ()
  "Push or pop the queue to get to the patch on the current line."
  (interactive)
  (let ((line (mq-read-series-line)))
    (unless line
      (error "no patch name on current line"))
    (let* ((patch (car line))
           ;; If the patch is currently applied, we assume we should pop.
           ;; Otherwise, we assume we should push.
           (operation (if (member patch mq-status) "qpop" "qpush")))
      (mq-shell-command "hg %s '%s'" operation patch)
      (mq-refresh))))

(defun mq-find-patch ()
  "Visit the patch on the current line."
  (interactive)
  (let* ((root (mq-find-hg-root))
         (patch-directory (mq-find-patch-directory root))
         (line (mq-read-series-line))
         (patch (car line)))
    (find-file (expand-file-name patch patch-directory))))

(defun mq-find-patch-other-window ()
  "Visit the patch on the current line in another window."
  (interactive)
  (let* ((root (mq-find-hg-root))
         (patch-directory (mq-find-patch-directory root))
         (line (mq-read-series-line))
         (patch (car line)))
    (find-file-other-window (expand-file-name patch patch-directory))))

(defun mq-point-to-top-patch ()
  "Move point to the series buffer line for the top applied patch."
  (goto-char (point-min))
  (when mq-status
    (let ((patch (car mq-status)))
      (unless (re-search-forward (format "^\\s-*%s\\([#[:blank:]]\\|\\'\\)"
                                         (regexp-quote patch))
                                 nil t)
        (error "Couldn't find line in series file for top patch: %s"
               patch))
      (beginning-of-line))))


;;; Global commands, available in all files.

(defun mq-push-pop-command (command force)
  "Subroutine for Mercurial Queues push/pop commands.
Check that the current buffer is visiting a file to which some
queue applies; run the command `hg COMMAND'; and refresh Emacs's
state (buffer contents, series file markup).
If FORCE is non-nil, pass the `--force' flag to command as well."
  (mq-check-for-queue)
  (when force (setq command (concat command " --force")))
  (mq-shell-command command)
  (mq-refresh))

(defun mq-qpush ()
  "Apply the next patch in the Mercurial queue for the current buffer."
  (interactive)
  (mq-push-pop-command "hg qpush" nil))

(defun mq-qpush-all ()
  "Apply all patches in the Mercurial queue for the current buffer."
  (interactive)
  (mq-push-pop-command "hg qpush -a" nil))

(defun mq-qpop (force)
  "Un-apply the last applied patch in the Mercurial queue for the current buffer.
With a prefix argument, pass the '--force' flag, to pop even if there are
local changes."
  (interactive "P")
  (mq-push-pop-command "hg qpop" force))

(defun mq-qpop-all (force)
  "Un-apply all patches in the Mercurial queue for the current buffer.
With a prefix argument, pass the '--force' flag, to pop even if there are
local changes."
  (interactive "P")
  (mq-push-pop-command "hg qpop -a" force))

(defun mq-qrefresh ()
  "Incorporate changes made to working files into the top Mercurial queue patch."
  (interactive)
  (mq-check-for-queue)
  (mq-shell-command "hg qrefresh"))

(defun mq-visit-series ()
  "Visit the current buffer's series file."
  (interactive)
  (let* ((root (mq-find-hg-root))
         (series (mq-find-series-file root)))
    (find-file series)
    (mq-point-to-top-patch)))

(defun mq-visit-series-other-window ()
  "Visit the current buffer's series file in another window."
  (interactive)
  (let* ((root (mq-find-hg-root))
         (series (mq-find-series-file root)))
    (find-file-other-window series)
    (mq-point-to-top-patch)))


;;; Global key bindings.

(defvar mq-global-map
  (let ((map (make-keymap "Mercurial Patch Queue")))
    (define-key map "n" 'mq-qpush)
    (define-key map "p" 'mq-qpop)
    (define-key map "r" 'mq-qrefresh)
    (define-key map "s" 'mq-visit-series)
    (define-key map "<" 'mq-qpop-all)
    (define-key map ">" 'mq-qpush-all)
    map))

(defvar mq-global-other-window-map
  (let ((map (make-keymap "Mercurial Patch Queue (other window)")))
    (define-key map "s" 'mq-visit-series-other-window)
    map))

(global-set-key "\C-xq" mq-global-map)
(global-set-key "\C-x4q" mq-global-other-window-map)


(provide 'mercurial-queues)
