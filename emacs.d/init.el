;;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pabs emacs config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start config and general appeareance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; minimal UI
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode 1)

(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/opt/homebrew/bin:/opt/homebrew/sbin:" (getenv "PATH")))
    (setq exec-path (split-string (getenv "PATH") path-separator)))

;; No need for ~ files when editing
(setq
 create-lockfiles nil
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(setq custom-file "~/.emacs.custom")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; project.el
(use-package project
  :ensure t
  :bind-keymap ("M-p" . project-prefix-map)
  :config
  ;; Ignore build directories
  (add-to-list 'project-vc-ignores "build"))

(defun my/project-switch-hook (orig-fun &rest args)
  "Run custom actions after switching project."
  (let ((result (apply orig-fun args)))
    (tab-bar-rename-tab
     (file-name-nondirectory (directory-file-name (car args))))
    result))

(advice-add 'project-switch-project :around #'my/project-switch-hook)

;; Vterm
(use-package vterm
  :ensure t
  :hook ((vterm-mode . (lambda ()
                         (display-line-numbers-mode 0)
                         (setq show-trailing-whitespace nil)))))

;; termcontrol my own package
(use-package term-control
  :load-path "~/utils/term-control.el/"
  :custom
    (term-control-vsize 33)
    (term-control-hsize 33)
  :bind
    (("s-h" . term-control-switch-to-term)
    ("s-j" . term-control-toggle)
    ("s-H" . term-control-switch-to-term-ver)
    ("s-J" . term-control-toggle-ver)))

(use-package tasker
  :load-path "~/utils/tasker.el/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language specific config and packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; Autoclose brackets
(electric-pair-mode t)

;; C++ options
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))

(setq language-modes '(haskell-mode
                      json-mode
                      package-lint))

(dolist (pkg language-modes)
  (eval `(use-package ,pkg
           :ensure t
           :defer t)))

(use-package geiser-guile :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Tramp config
(setq vc-handled-backends '(Git))
(setq tramp-verbose 1)
(setq auto-revert-remote-files nil)
(setq tramp-use-ssh-controlmaster-options nil)
(setq tramp-auto-save-directory "/tmp")

;; Personal settings
;;;; Auto reload buffers on file change
(global-auto-revert-mode t)

;;;; Imenu to jump to functions in code
(setq-default imenu-auto-rescan t)
(setq-default imenu-auto-rescan-maxout 1200000)
(global-set-key (kbd "M-i") 'imenu)

;;;; Open other file in split window
(global-set-key (kbd "M-o") 'ff-find-other-file-other-window)
(global-set-key (kbd "M-O") 'ff-find-other-file)

;;;; Improves size info in dired
(setq dired-listing-switches "-alh")

;;;; Disables abbrev mode
(setq-default abbrev-mode nil)

;;;; Replaces dired buffer instead of creating a new one
(put 'dired-find-alternate-file 'disabled nil)

;; Fuzzy search
(setq completion-styles '(flex))
(fido-vertical-mode 1)

;; Easily move around panes with shift+arrows
(windmove-default-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)
(set-face-font 'default "CommitMono Light 14")

(setq ns-use-srgb-colorspace t)

(global-prettify-symbols-mode +1)

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; tab-bar
(require 'tab-bar)       ; load the tab-bar feature immediately
(tab-bar-mode 1)      ; enable the C-x t prefix at once
(setq-default tab-bar-new-button-show nil ;; don't show new tab button
        tab-bar-close-button-show nil ;; don't show tab close button
        tab-bar-new-tab-choice "*scratch*"
        tab-line-close-button-show nil) ;; don't show tab close button

;; Rebalance windows everytime you split or close
(add-hook 'window-configuration-change-hook #'balance-windows)

(setq-default
  x-select-enable-clipboard t
  x-select-enable-primary t
  save-interprogram-paste-before-kill t
  apropos-do-all t
  mouse-yank-at-point t)

;; Relative numbers
;; Set the line numbers type to relative
(setq display-line-numbers-type 'relative)
(global-hl-line-mode)
(global-display-line-numbers-mode)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-light :no-confirm))

(use-package auto-dark
  :ensure t
  :config
  ;; When OS switches to dark mode:
  (setq auto-dark-themes '((ef-dream) (ef-light)))
  (setq custom-safe-themes t)
  ;; Start auto-dark-mode to begin monitoring system appearance
  (auto-dark-mode t))

