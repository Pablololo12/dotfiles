;;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pabs emacs config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start config and general appeareance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/opt/homebrew/bin:/opt/homebrew/sbin:" (getenv "PATH")))
    (setq exec-path (split-string (getenv "PATH") path-separator)))

;; Avoid seeing warning errors
(customize-set-variable 'native-comp-async-report-warnings-errors nil)
;; We set the garbage collector threshold to 512MB
(setq gc-cons-threshold (* 512 1024 1024))

;; minimal UI
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; No need for ~ files when editing
(setq
 create-lockfiles nil
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(setq auto-save-visited-interval 5) ;; seconds of idle before saving
(auto-save-visited-mode 1)

;; We run the garbage-collector when we go out of focus
(add-function :after after-focus-change-function (lambda () (garbage-collect)))

;; Not show emacs news
(defalias 'view-emacs-news 'ignore)
(defalias 'describe-gnu-project 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Personal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vterm-new-tab ()
  "Opens a vterm in a new tab"
  (interactive)
  (tab-bar-new-tab)
  (vterm (generate-new-buffer-name "*vterm*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; projectile
(use-package projectile
  :ensure t
  :bind
    (("M-p" . 'projectile-command-map))
  :custom
    (projectile-globally-ignored-directories '("build"))
    (projectile-switch-project-action
     (lambda ()
       (let ((project-name (projectile-project-name)))
         (tab-bar-rename-tab project-name))
       (projectile-dired)))
  :config
    (projectile-mode +1))

;; Evil mode
(use-package evil
  :ensure t
  :custom
    (evil-want-C-u-scroll t)
    (evil-want-C-i-jump nil)
    (evil-search-module 'evil-search)
    (evil-ex-search-highlight-all t)
    (lazy-highlight-cleanup nil)
  :config
    (evil-mode 1)
    (evil-set-initial-state 'vterm-mode 'emacs))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language specific config and packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-set-style "k&r")
(setq c-basic-offset 4)
;; Autoclose brackets
(electric-pair-mode t)

;; C++ options
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))

(setq language-modes '(haskell-mode
                      lua-mode
                      json-mode
                      clojure-mode
                      package-lint
                      rust-mode
                      cargo))

(dolist (pkg language-modes)
  (eval `(use-package ,pkg
           :ensure t
           :defer t)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)
(set-face-font 'default "FiraCode Nerd Font Mono Light 14")

(setq ns-use-srgb-colorspace t)

(global-prettify-symbols-mode +1)

;; Line cursor and no blink
(set-default 'cursor-type  '(bar . 1))
(blink-cursor-mode 0)

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; tab-bar
(require 'tab-bar)       ; load the tab-bar feature immediately
(tab-bar-mode 1)      ; enable the C-x t prefix at once
(setq-default tab-bar-new-button-show nil ;; don't show new tab button
        tab-bar-close-button-show nil ;; don't show tab close button
        tab-bar-new-tab-choice #'my/startup-projects-dashboard
        tab-line-close-button-show nil) ;; don't show tab close button

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-bar-tab ((t (:background "forest green" :foreground "#ffffff" :box nil)))))
 ;; active tab

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

;; Show trailing white space
(setq-default show-trailing-whitespace t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MODELINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(column-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dashboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'widget)
(defun my/startup-projects-dashboard ()
  "Display an interactive list of recent projects on startup."
  (let ((buf (get-buffer-create "*Start Page*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (kill-all-local-variables)       ;; clean slate; no read-only major mode
        (erase-buffer)
        (remove-overlays)

        (projectile-mode 1)
        (unless projectile-known-projects
          (projectile-load-known-projects))

        ;; Image at top
        (when (display-graphic-p)
          (let* ((candidates (list (expand-file-name "images/splash.svg" data-directory)
                                   (expand-file-name "images/splash.png" data-directory)
                                   (expand-file-name "images/splash.xpm"  data-directory)))
                 (file (seq-find #'file-exists-p candidates)))
            (when file
              (insert-image (create-image file nil nil :ascent 'center))
              (insert "\n\n"))))

        ;; ASCII fallback
        (unless (display-graphic-p)
          (widget-insert "EMACS\n\n"))

        ;; Header + rule
        (widget-insert "📁   Recent Projects:\n")
        (widget-insert (make-string 25 (if (char-displayable-p ?─) ?─ ?-)) "\n\n")

        ;; Buttons
        (if (and projectile-known-projects (> (length projectile-known-projects) 0))
            (dolist (proj (cl-subseq projectile-known-projects
                                     0 (min 10 (length projectile-known-projects))))
              (let ((proj-name (file-name-nondirectory (directory-file-name proj))))
                (widget-create 'push-button
                               :tag proj-name
                               :value proj
                               :notify (lambda (widget &rest _)
                                         (let ((proj-path (widget-value widget)))
                                           (let ((default-directory proj-path))
                                             (funcall projectile-switch-project-action)))))
                (widget-insert "\n")))
          (widget-insert "No recent projects found.\n"))

        ;; Finalize
        (use-local-map widget-keymap)))
    (with-current-buffer buf
      (widget-setup)                      ;; this makes the buffer read-only appropriately
      (goto-char (point-min))
      (widget-forward 1))
    buf))

;; Use the custom dashboard at startup (if no file is opened)
(setq inhibit-startup-screen t
      initial-buffer-choice #'my/startup-projects-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Do not touch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("59c36051a521e3ea68dc530ded1c7be169cd19e8873b7994bfc02a216041bf3b"
     "c46651ab216eb31e699be1bd5e6df8229b08005b534194c1ea92519b09661d71"
     "98b4ef49c451350c28a8c20c35c4d2def5d0b8e5abbc962da498c423598a1cdd"
     default))
 '(package-selected-packages nil))
