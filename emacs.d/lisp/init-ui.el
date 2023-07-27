(require 'init-elpa)

(set-face-attribute 'default nil :font "DroidSansM Nerd Font Mono")
;; Theme
(require-package 'atom-one-dark-theme)
(load-theme 'atom-one-dark t)

(set-cursor-color "#cccccc")

;; Change width threshold for splitting vertically
(setq-default split-width-threshold 125)
(setq-default split-height-threshold 200)
;; Inhibit startup message
(setq inhibit-startup-message t)
;; Disable menu bars and graphic stuff
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode  -1))

(setq
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t)

(blink-cursor-mode 0)

;; Relative numbers
(display-line-numbers-mode)
(setq-default display-line-numbers 'relative)

;; Show trailing white space
(setq-default show-trailing-whitespace t)

;; Show line at 120 characters
(require 'whitespace)
(setq-default whitespace-line-column 120)
(setq-default whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Org options
(require-package 'all-the-icons)
(require-package 'org-bullets)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq inhibit-splash-screen t)
(transient-mark-mode 1)
(require 'org)

;;(require-package 'diminish)
;;(diminish 'visual-line-mode)

;; Status bar
(setq doom-modeline-height 25)
(setq doom-modeline-bar-width 1)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-buffer-modification-icon t)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-enable-word-count nil)
(setq doom-modeline-buffer-encoding t)
(setq doom-modeline-indent-info nil)
(setq doom-modeline-checker-simple-format t)
(setq doom-modeline-vcs-max-length 20)
(setq doom-modeline-env-version t)
(setq doom-modeline-irc-stylize 'identity)
(setq doom-modeline-github-timer nil)
(setq doom-modeline-gnus-timer nil)
(require-package 'doom-modeline)
(doom-modeline-mode)

(provide 'init-ui)
