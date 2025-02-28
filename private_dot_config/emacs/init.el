;;; init.el --- optimized init file -*- no-byte-compile: t; lexical-binding: t; -*-

;;; commentary:
;;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration

;;; code:

;;; Set Vars and Consts

;;; Check the system used

(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-BSD     (or ON-MAC (eq system-type 'berkeley-unix)))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst GIT-BIN (string-trim (shell-command-to-string "which git")))
(defconst PROJECTS-DIR "~/projects")

;;;; PATH

(when (and ON-MAC (display-graphic-p))
  (let ((brew-path "/opt/homebrew/bin"))
    (setenv "PATH" (concat (concat brew-path ":") (getenv "PATH")))
    (setq exec-path (append (list brew-path) exec-path))))

;;; Native compilation settings

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
	native-compile-prune-cache t
	native-comp-jit-compilation t))

;;; no backups

(setq-default
   auto-revert-verbose nil
   auto-save-default nil
   auto-save-list-file-name nil
   backup-inhibited t
   create-lockfiles nil
   make-backup-files nil)

;;; line wrapping

;(setq word-wrap t)
;(setq truncate-lines t)
;(setq truncate-partial-width-windows t)
;(setq global-visual-line-mode nil)

;;; UI

(fset 'yes-or-no-p 'y-or-n-p)
;; (global-visual-line-mode)
(show-paren-mode -1)

(setq-default display-line-numbers-type t)
(setq-default display-line-numbers-widen t)
(setq-default display-line-numbers-width 3)
(setq display-line-numbers-major-tick 0)
(setq display-line-numbers-minor-tick 0)

(setq-default ring-bell-function 'ignore)

(when (fboundp 'set-message-beep) (set-message-beep 'silent))

(setq column-number-mode t)
(setq mode-line-percent-position nil)

;;; UTF-8 everywhere

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;;; Font settings

(when (member "Input Mono Compressed" (font-family-list))
  (set-face-attribute 'default nil :family "Input Mono Compressed" :height 180 :weight 'light)
  (set-face-attribute 'fixed-pitch nil :family "Input Mono Compressed" :height 1.0)
  (set-face-attribute 'bold nil :weight 'regular))
(when (member "Input Serif Compressed" (font-family-list))
  (set-face-attribute 'variable-pitch nil :family "Input Serif Compressed" :height 1.0))
(when (member  "Symbola" (font-family-list))
  (set-fontset-font t 'symbol "Symbola" nil))
(setopt line-spacing 0.2)

;;; Set up the package manager

(require 'package)

(setq package-vc-register-as-project nil) ; Emacs 30

(add-hook 'package-menu-mode-hook #'hl-line-mode)

(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

(package-initialize)

;;; Load helper functions

(load (locate-user-emacs-file "m-functions.el"))

;;; Basic behaviour

(use-package diminish :ensure t)

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 25)
  (setq recentf-save-file-modes nil)
  (setq recentf-keep nil)
  (setq recentf-auto-cleanup nil)
  (setq recentf-initialize-file-name-history nil)
  (setq recentf-filename-handlers nil)
  (setq recentf-show-file-shortcuts-flag nil))

;;;; Auto revert mode
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-verbose t))

;;;; Tooltips (tooltip-mode)
(use-package tooltip
  :ensure nil
  :hook (after-init . tooltip-mode)
  :config
  (setq tooltip-delay 0.5
        tooltip-short-delay 0.5
        tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 10)
          (border-width . 0)
          (no-special-glyphs . t))))

;;; Themes

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

(use-package modus-themes
  :ensure t
  :config
  (setq modus-vivendi-palette-overrides
	'((bg-main "#171717")
	  (comment yellow-faint))))

(use-package ef-themes
  :ensure t
  :init
  (defun my-ef-themes-mode-line ()
    "Tweak the style of the mode lines."
    (ef-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c :background ,bg-active :foreground ,fg-main :box (:line-width 1 :color ,fg-dim))))
       `(mode-line-inactive ((,c :box (:line-width 1 :color ,bg-active)))))))
  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-mode-line)
  :config
  (setq ef-themes-to-toggle '(ef-cyprus ef-owl))
  ;; (setq ef-themes-mixed-fonts nil)
  ;; (setq ef-themes-variable-pitch-ui nil)
  ;; Theme overrides
  (setq ef-autumn-palette-overrides
	'((cursor magenta-cooler)))
  (setq ef-owl-palette-overrides
	'((comment yellow-faint))))

(load-theme 'ef-owl :no-confirm-loading)

(use-package pulsar
  :ensure t
  :config
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-green
          pulsar-highlight-face 'pulsar-magenta)

  (pulsar-global-mode 1)
  :hook
  ((next-error . (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
   (minibuffer-setup . pulsar-pulse-line-red)
   (consult-after-jump . (pulsar-recenter-top pulsar-reveal-entry))))

;;; Icons

;; Remember to do M-x and run `nerd-icons-install-fonts' to get the
;; font files.  Then restart Emacs to see the effect.
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :if (display-graphic-p)
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :if (display-graphic-p)
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;; Configure the minibuffer and completions

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-count 5)
  (setq vertico-cycle t)
  (setq vertico-resize t)
  (setq vertico-scroll-margin 0))

;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
;; In addition to that, Marginalia also enhances Vertico by adding rich
;; annotations to the completion candidates displayed in Vertico's interface.
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

;;; General minibuffer settings

(use-package minibuffer
  :ensure nil
  :config
  (setq completion-styles '(basic substring initials flex orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
        ;; `partial-completion' is a killer app for files, because it
        ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
        ;;
        ;; If `basic' cannot match my current input, Emacs tries the
        ;; next completion style in the given order.
        '((file (styles . (basic partial-completion orderless)))
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles . (emacs22 substring orderless))))))

(use-package orderless
  :ensure t
  :demand t
  :after (minibuffer vertico)
  :config
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp))
  ;; SPC should never complete: use it for `orderless' groups.
  ;; The `?' is a regexp construct.
  :bind (:map minibuffer-local-completion-map
              ("SPC" . nil)
              ("?" . nil)))

(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind
  (:map corfu-map
	("<tab>" . corfu-complete)
	([remap completion-at-point] . corfu-complete)
	("SPC" . corfu-insert-separator)
	("C-j" . corfu-next)
	("C-k" . corfu-previous)
	:map corfu-popupinfo-map
	("M-j" . corfu-popupinfo-scroll-down)
	("M-k" . corfu-popupinfo-scroll-up)
	("M-h" . corfu-popupinfo-toggle))
  :init
  :config
  (setq tab-always-indent 'complete)

  (setq corfu-min-width 20)
  (setq corfu-preview-current nil)

  (setq corfu-auto-delay 0.2)
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)
  :commands (corfu-terminal-mode)
  :hook (after-init . corfu-terminal-mode))

(use-package cape
  :ensure t
  :after corfu
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Enhanced minibuffer commands
;; https://github.com/minad/consult
(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (("C-c i" . consult-info)
   ([remap Info-search] . consult-info)
   ("M-y" . consult-yank-pop)
   ("M-g f" . consult-flycheck)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s l" . (lambda () (interactive) (consult-line (thing-at-point 'symbol))))
   ("M-s r" . consult-ripgrep)
   ([remap switch-to-buffer] . consult-buffer) ; C-x b
   ("C-x C-r" . consult-recent-file)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-line-numbers-widen t)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key nil)
  (setq consult-preview-key 'any)
  (setq consult-project-function nil) ; always work from the current directory
  (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring)))

(use-package consult-flycheck
  :ensure t
  :after consult)

;;; The file manager (Dired)

;; Install gls: brew install coreutils (for Dired)
(when ON-MAC (setq insert-directory-program "gls" dired-use-ls-dired t))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-listing-switches "-ADlGh --group-directories-first")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  ;; single buffer view
  (setq dired-kill-when-opening-new-dired-buffer t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

;;; Neotree

(use-package find-file-in-project :ensure t)

(use-package neotree
  :ensure t
  :bind (("s-b" . neotree-project-dir))
  :init
  (setq
   neo-smart-open t
   neo-mode-line-type 'neotree
   neo-window-width 30)
  (setq neo-theme (if (display-graphic-p) 'nerd 'arrow))
  :config
   (add-hook 'neo-after-create-hook
             (lambda (&rest _) (display-line-numbers-mode -1)))
   (defun neotree-project-dir ()
     "Open NeoTree using the git root."
     (interactive)
     (let ((project-dir (ffip-project-root))
           (file-name (buffer-file-name)))
       (neotree-toggle)
       (if project-dir
           (if (neo-global--window-exists-p)
               (progn
                 (neotree-dir project-dir)
                 (neotree-find file-name)))
         (message "Could not find git project root.")))))

;;; Search

;; https://protesilaos.com/codelog/2023-06-10-emacs-search-replace-basics/
(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq search-whitespace-regexp ".*?"
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil)
  (setq search-highlight t
	isearch-lazy-highlight t
	lazy-highlight-initial-delay 0.5
	lazy-highlight-no-delay-length 4)
  (setq isearch-lazy-count t
	lazy-count-prefix-format "(%s/%s) "
	lazy-count-suffix-format nil))

(use-package rg
  :ensure t
  :ensure-system-package rg)

;;; Help UI

(use-package helpful :ensure t)

;; (use-package which-key
;;   :ensure t
;;   :commands (which-key-mode)
;;   :init
;;   (which-key-mode))

(use-package which-key
  :ensure nil ; built into Emacs 30
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-separator "  ")
  (setq which-key-prefix-prefix "... ")
  (setq which-key-max-display-columns 3)
  (setq which-key-idle-delay 1.5)
  (setq which-key-idle-secondary-delay 0.25)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length 40))

;;; Editor

(use-package comment-dwim-2
  :ensure t
  :bind ([remap comment-dwim] . comment-dwim-2))

(use-package editorconfig
  :ensure t
  :defer t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("<escape>" . mc/keyboard-quit)
	 ("C->"      . mc/mark-next-like-this)
	 ("C-M->"    . mc/skip-to-next-like-this)
	 ("C-<"      . mc/mark-previous-like-this)
	 ("C-M-<"    . mc/skip-to-previous-like-this)
	 ("s->"      . mc/edit-lines)
	 ("s-<mouse-1>" . mc/add-cursor-on-click)))

(use-package puni
  :ensure t
  :bind
  (:map puni-mode-map
	("M-]" . puni-slurp-forward)
	("M-}" . puni-barf-forward)
	("M-[" . puni-slurp-backward)
	("M-{" . puni-barf-backward)))

(use-package undo-fu
  :ensure t
  :bind (("C-/" . undo-fu-only-undo)
	 ("C-?" . undo-fu-only-redo)))

;;; Project

(use-package project
  :ensure nil
  :bind
  (("C-x p ." . project-dired)
   ("C-x p C-g" . keyboard-quit)
   ("C-x p <return>" . project-dired)
   ("C-x p <delete>" . project-forget-project))
  :config
  (setopt project-switch-commands
          '((project-find-file "Find file")
            (project-find-regexp "Find regexp")
            (project-find-dir "Find directory")
            (project-dired "Root dired")
            (project-vc-dir "VC-Dir")
            (project-shell "Shell")
            (keyboard-quit "Quit")))
  (setq project-vc-extra-root-markers '(".project" "deps.edn" "config.ru" "go.mod")) ; Emacs 29
  (setq project-key-prompt-style t) ; Emacs 30

  (advice-add #'project-switch-project :after #'prot-common-clear-minibuffer-message))

;;; Terminal emulator

(use-package vterm
  :ensure t
  :defer t
  :bind (("s-\\" . mt/toggle-vterm))
  :commands vterm
  :custom
  (vterm-kill-buffer-on-exit t)
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  ;; Speed up vterm
  (setq vterm-timer-delay 0.01))

;;; Programming

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode)
	 (prog-mode . electric-indent-mode)
	 (prog-mode . electric-pair-mode)))

;;;; Eldoc (Emacs live documentation feedback)
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-message-function #'message)) ; don't use mode line for M-x eval-expression, etc

;;;; Consult interface to dash docs
(use-package consult-dash
  :ensure t
  :after consult
  :bind (("M-s d" . consult-dash))
  :config
  ;; Use the symbol at point as initial search term
  (consult-customize consult-dash :initial (thing-at-point 'symbol)))

;;;; Parentheses (show-paren-mode)
(use-package paren
  :ensure nil
  :hook (prog-mode . show-paren-local-mode)
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'overlay)) ; Emacs 29

;;; Header line context of symbol/heading (breadcrumb.el)
(use-package breadcrumb
  :ensure t
  :functions (prot/breadcrumb-local-mode)
  ;; :hook ((text-mode prog-mode) . prot/breadcrumb-local-mode)
  :config
  (setq breadcrumb-project-max-length 0.5)
  (setq breadcrumb-project-crumb-separator "/")
  (setq breadcrumb-imenu-max-length 1.0)
  (setq breadcrumb-imenu-crumb-separator " > ")

  (defun prot/breadcrumb-local-mode ()
    "Enable `breadcrumb-local-mode' if the buffer is visiting a file."
    (when buffer-file-name
      (breadcrumb-local-mode 1))))

;; https://github.com/purcell/envrc
; (use-package envrc
;   :ensure t
;   :hook (after-init . envrc-global-mode))

;; https://github.com/eki3z/mise.el
(use-package mise
  :ensure t
  :hook (after-init . global-mise-mode))

;; https://github.com/fgeller/highlight-thing.el
(use-package highlight-thing
  :ensure t
  :custom
  (highlight-thing-exclude-thing-under-point t)
  (highlight-thing-prefer-active-region t)
  (highlight-thing-limit-to-region-in-large-buffers-p nil)
  (highlight-thing-narrow-region-lines 15)
  (highlight-thing-large-buffer-limit 5000))

;;; VC

;;;; `ediff'
(use-package ediff
  :ensure nil
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t))

;;; Interactive and powerful git front-end (Magit)
(use-package transient
  :defer t
  :config
  (setq transient-show-popup 0.5))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :commands (magit-status magit-ediff-show-working-tree)
  :init
  (setq magit-define-global-key-bindings nil)
  (setq magit-section-visibility-indicator '("⮧"))
  :custom
  (git-commit-summary-max-length 50)
  (git-commit-style-convention-checks '(non-empty-second-line))

  (magit-diff-refine-hunk t)

  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-repos
  :ensure nil ; part of `magit'
  :commands (magit-list-repositories)
  :init
  (setq magit-repository-directories
        `((,PROJECTS-DIR . 3))))

(use-package git-gutter
  :ensure git-gutter
  ;; :diminish ""
  :functions global-git-gutter-mode)

;;; Flycheck

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;; Treesitter & Langs

(load (locate-user-emacs-file "m-langs.el") :no-error-if-file-is-missing)

(use-package consult-eglot
  :ensure t
  :after consult)

(use-package eglot
  :ensure nil
  :bind (("C-h ." . display-local-help)
	 :map eglot-mode-map
	 ("C-c l a" . eglot-code-actions)
	 ("C-c l f" . eglot-format)))

(use-package treesit
  :ensure nil
  :bind (("C-c t b" . treesit-beginning-of-defun)
	 ("C-c t e" . treesit-end-of-defun)))

(use-package inf-ruby
  :ensure nil
  :bind (:map inf-ruby-minor-mode-map
	      ("C-c r c" . inf-ruby-console-auto)))

(use-package apheleia
  :ensure t
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :bind (("C-c f" . apheleia-format-buffer))
  :config
  (setf (alist-get 'ruby-mode apheleia-mode-alist) 'rubocop)
  (setf (alist-get 'ruby-ts-mode apheleia-mode-alist) 'rubocop))

;;; global keys

(global-unset-key (kbd "C-z")) ;; No suspend frame
(global-unset-key (kbd "s-o")) ;; No open file frame

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)
(define-key global-map [escape] #'prot/keyboard-quit-dwim)

(define-key global-map (kbd "s-p") #'project-find-file)

(define-key global-map (kbd "M-<up>") #'mt/move-line-up)
(define-key global-map (kbd "M-<down>") #'mt/move-line-down)
(define-key global-map (kbd "M-S-<down>") #'crux-duplicate-current-line-or-region)
(define-key global-map (kbd "M-<return>") #'crux-smart-open-line)
(define-key global-map (kbd "M-S-<return>") #'crux-smart-open-line-above)
(define-key (current-global-map)
  [remap move-beginning-of-line] 'crux-move-beginning-of-line) ; C-a
(define-key (current-global-map)
  [remap kill-line] 'crux-smart-kill-line) ; C-k

(define-key global-map (kbd "M-o") #'crux-other-window-or-switch-buffer)
(define-key global-map (kbd "s-w") #'kill-current-buffer) ; s-k


;;; display-buffer-alist

;; slots {:-1 [python ruby vterm xwidget-plot] :1 [eldoc helpful] :2 [aichat]}

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;; (add-to-list 'display-buffer-alist
;; 	     '("\\*[Cc]ompilation\\|\\*Rubo[Cc]op"
;; 	       (display-buffer-no-window)
;; 	       (allow-no-window . t)))

(add-to-list 'display-buffer-alist
	     '("\\*vterm\\*"
	       (display-buffer-reuse-window display-buffer-at-bottom)
	       (dedicated . t)
	       (reusable-frames . visible)
	       (slot . -1)
	       (window-height . 0.3)))

(add-to-list 'display-buffer-alist
	     '("\\*pry\\*"
	       (display-buffer-reuse-window display-buffer-at-bottom)
	       (dedicated . t)
	       (reusable-frames . visible)
	       (slot . -1)
	       (window-height . 0.3)))

;;; custom.el

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; --- Speed benchmarking -----------------------------------------------------
(let ((init-time (float-time (time-subtract (current-time) init-start-time)))
      (total-time (string-to-number (emacs-init-time "%f"))))
  (message (concat
    (propertize "Startup time: " 'face 'bold)
    (format "%.2fs " init-time)
    (propertize (format "(+ %.2fs system time)"
                        (- total-time init-time)) 'face 'shadow))))

(provide 'init)
;;; init.el ends here
