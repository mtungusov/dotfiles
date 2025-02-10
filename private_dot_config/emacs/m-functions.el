;;; m-functions.el --- helper functions -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(defun mt/indent-region-advice (&rest ignored)
  "Text ident after move string."
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

;; Move line up and down
(defun mt/move-line-up ()
  "Move line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun mt/move-line-down ()
  "Move line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(advice-add 'mt/move-line-up :after 'mt/indent-region-advice)
(advice-add 'mt/move-line-down :after 'mt/indent-region-advice)

;;; Crux
;; https://github.com/bbatsov/crux/blob/master/crux.el

(defvar crux-line-start-regex-alist
  '((term-mode . "^[^#$%>\n]*[#$%>] ")
    (eshell-mode . "^[^$\n]*$ ")
    (org-mode . "^\\(\*\\|[[:space:]]*\\)* ")
    (default . "^[[:space:]]*"))
  "Alist of major modes and line starts.

The key is a major mode.  The value is a regular expression
matching the characters to be skipped over.  If no major mode is
found, use the regex specified by the default key.

Used by crux functions like `crux-move-beginning-of-line' to skip
over whitespace, prompts, and markup at the beginning of the line."
  )

(defun crux-move-to-mode-line-start ()
  "Move to the beginning, skipping mode specific line start regex."
  (interactive)

  (move-beginning-of-line nil)

  (let ((line-start-regex (cdr (seq-find
                                (lambda (e) (derived-mode-p (car e)))
                                crux-line-start-regex-alist
                                (assoc 'default crux-line-start-regex-alist)))))
    (search-forward-regexp line-start-regex (line-end-position) t)))

(defun crux-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (crux-move-to-mode-line-start)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun crux-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (insert "\n")
  (if electric-indent-inhibit
      ;; We can't use `indent-according-to-mode' in languages like Python,
      ;; as there are multiple possible indentations with different meanings.
      (let* ((indent-end (progn (crux-move-to-mode-line-start) (point)))
             (indent-start (progn (move-beginning-of-line nil) (point)))
             (indent-chars (buffer-substring indent-start indent-end)))
        (forward-line -1)
        ;; This new line should be indented with the same characters as
        ;; the current line.
        (insert indent-chars))
    ;; Just use the current major-mode's indent facility.
    (forward-line -1)
    (indent-according-to-mode)))

(defun crux-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (crux-smart-open-line-above)
    (move-end-of-line nil)
    (newline-and-indent)))

(defun crux-kill-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (crux-move-to-mode-line-start))

(defun crux-smart-kill-line ()
  "Kill to the end of the line and kill whole line on the next call."
  (interactive)
  (let ((orig-point (point)))
    (move-end-of-line 1)
    (if (= orig-point (point))
        (crux-kill-whole-line)
      (goto-char orig-point)
      (kill-line))))

(defun crux-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun crux-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (dotimes (_i arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun crux-other-window-or-switch-buffer ()
  "Call `other-window' if more than one window is visible.
Switch to most recent buffer otherwise."
  (interactive)
  (if (one-window-p)
      (switch-to-buffer nil)
    (other-window 1)))

;;; Terminals

(defun mt/toggle-vterm ()
  "Toggle vterm buffer."
  (interactive)
  (if (get-buffer "*vterm*")
      (if (eq (current-buffer) (get-buffer "*vterm*"))
          (delete-window)
        (switch-to-buffer-other-window "*vterm*"))
    (vterm-other-window)))

;;; Treesitter

(defun mt/treesit-install-lang (lang)
  "Install lang grammar."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (when (member lang languages)
      (unless (treesit-language-available-p lang)
	(treesit-install-language-grammar lang)
	(message "`%s' parser was installed." lang)))))

(defun mt/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
      (unless (treesit-language-available-p lang)
	(treesit-install-language-grammar lang)
	(message "`%s' parser was installed." lang)
	(sit-for 0.75)))))

(provide 'm-functions)
;;; m-functions.el ends here
