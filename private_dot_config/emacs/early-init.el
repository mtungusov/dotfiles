;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;; --- Speed benchmarking -----------------------------------------------------
(setq init-start-time (current-time))

(setq gc-cons-threshold (* 1000 1000 8)
      gc-cons-percentage 0.1)

(setq initial-scratch-message nil

      inhibit-compacting-font-caches t
      inhibit-splash-screen t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t
      inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-x-resources t

      frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")

      ;; redisplay-dont-pause t

      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq package-enable-at-startup t)

(setq-default default-frame-alist
	      (append (list '(fullscreen . fullheight)
			    '(width . 150))))

(provide 'early-init)
;;; early-init.el ends here
