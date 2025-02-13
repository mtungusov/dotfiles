;;; m-langs.el --- file -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;;; Eglot

(use-package eglot
  :ensure nil
  :init
  (setq eglot-report-progress nil))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

;;; Treesitter

(use-package treesit
  :ensure nil
  :preface
  (defun mt/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	       (c "https://github.com/tree-sitter/tree-sitter-c")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
	       (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (java . ("https://github.com/tree-sitter/tree-sitter-java"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
	       (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar))
	(message "`%s' parser was installed." (car grammar))
	(sit-for 0.75))))
  :config
  (mt/setup-install-grammars))

(use-package treesit
  :ensure nil
  :mode (("\\.cmake\\'" . cmake-ts-mode)
         ("\\.css\\'" . css-ts-mode)
         ("\\.java\\'" . java-ts-mode)
	 ("\\.json\\'" . json-ts-mode)
         ("\\.py\\'" . python-ts-mode)
         ("\\.js\\'" . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode)
         ("\\.yml\\'" . yaml-ts-mode))
  :preface
  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((bash-mode . bash-ts-mode)
             (c-mode . c-ts-mode)
             (css-mode . css-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
	     (python-mode . python-ts-mode)
	     (ruby-mode . ruby-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)
	     (typescript-mode . typescript-ts-mode)
	     (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

;;; Langs

;;;; csv-mode
(use-package csv-mode
  :ensure t
  :commands (csv-align-mode))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;;;; go-mode
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :init
  (defun eglot-format-buffer-before-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
  :hook
  (go-mode . eglot-ensure)
  (go-mode . eglot-format-buffer-before-save)
  :config
  (add-hook 'before-save-hook
	    (lambda ()
              (call-interactively 'eglot-code-action-organize-imports))
	    nil t))

(use-package go-ts-mode
  :hook (go-mode . go-ts-mode))

;;;; markdown-mode
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

;;;; ruby
(use-package ruby-ts-mode
  :ensure nil
  :dash "Ruby"
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :hook
  (ruby-ts-mode . (lambda ()
		    (eglot-ensure)
		    (subword-mode 1)))
  :config
  (add-to-list 'eglot-server-programs
	       '((ruby-mode ruby-ts-mode) "ruby-lsp")))

;; https://github.com/nonsequitur/inf-ruby
(use-package inf-ruby
  :ensure t
  :defer t
  :commands (inf-ruby inf-ruby-console-auto)
  :hook
  (ruby-base-mode . inf-ruby-minor-mode)
  (compilation-filter-hook . inf-ruby-auto-enter))

;;;; toml & yaml
(use-package toml-ts-mode
  :ensure nil
  :mode "\\.toml\\'")

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.yaml\\'")

(provide 'm-langs)
;;; m-langs.el ends here
