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

;; Improve openning times by allowing the garbage collector to run less often
(setq prev-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 50000000)

(customize-set-variable 'native-comp-async-report-warnings-errors nil)
(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-deferred-compilation nil)

(add-hook 'emacs-startup-hook 'my/set-gc-threshold)
(defun my/set-gc-threshold ()
  "Reset `gc-cons-threshold' to its default value."
  (setq gc-cons-threshold prev-gc-cons-threshold))
;; Reset gc to default

;; minimal UI
(when tool-bar-mode
  (tool-bar-mode -1))
(when scroll-bar-mode
  (scroll-bar-mode -1))
(when menu-bar-mode
  (menu-bar-mode -1))

;; Ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; No need for ~ files when editing
(setq
 create-lockfiles nil
 make-backup-files nil
 create-lockfiles nil)

;; Not show emacs news
(defalias 'view-emacs-news 'ignore)
(defalias 'describe-gnu-project 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/change-tab-name ()
  "Rename the current tab to the name of the Projectile project."
  (when (projectile-project-p)
    (let ((project-name (projectile-project-name)))
      (tab-bar-rename-tab project-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Fuzzy search
(use-package ivy
  :defer 0.1
  :ensure t
  :diminish
  :custom
    (ivy-count-format "(%d/%d) ")
    (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package counsel
  :after ivy
  :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("C-c g" . counsel-git-grep)
   ("C-c j" . counsel-ag)
   ("C-c f" . counsel-describe-function))
  :config (counsel-mode))

;; projectile
(use-package projectile
  :ensure t
  :bind
    (("M-p" . 'projectile-command-map))
  :custom
    (projectile-globally-ignored-directories '("build"))
    (projectile-switch-project-action
     (lambda ()
       (my/change-tab-name)
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
  :config
    (evil-mode 1)
    (evil-set-initial-state 'vterm-mode 'emacs))

(setq evil-ex-search-highlight-all t)
(setq lazy-highlight-cleanup nil)

;; Vterm
(use-package vterm
    :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language specific config and packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Company autocomplete
(use-package company
  :ensure t
  :defer t
  :config
  (global-company-mode))

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

;;(use-package lsp-mode
;;  :ensure t
;;  :defer t
;;  :hook
;;    ((c-mode . lsp)
;;    (c++-mode . lsp))
;;  :init
;;  (setq lsp-ui-doc-enable nil)
;;  (setq lsp-ui-sideline-enable nil)
;;  (setq lsp-headerline-breadcrumb-enable nil
;;        lsp-keymap-prefix "C-c l")
;;  (with-eval-after-load 'lsp-mode
;;    (define-key lsp-mode-map (kbd "C-x t 2") nil)
;;    (define-key lsp-mode-map (kbd "M-p") nil)
;;    (define-key lsp-mode-map (kbd "<mouse-1>") nil)
;;    (define-key lsp-mode-map (kbd "<mouse-2>") nil)
;;    (define-key lsp-mode-map (kbd "<mouse-3>") nil)
;;    (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)))

;; Haskell
(use-package haskell-mode
  :defer t
  :ensure t)

;; Lua
(use-package lua-mode
  :defer t
  :ensure t)

;; json
(use-package json-mode
  :defer t
  :ensure t)

;; clojure
(use-package clojure-mode
  :defer t
  :ensure t)

(use-package package-lint
  :defer t
  :ensure t)

(use-package rust-mode
  :defer t
  :ensure t)

(use-package cargo
  :defer t
  :ensure t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terminal popup Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;; Dashboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'widget)
(defun my/startup-projects-dashboard ()
  "Display an interactive list of recent projects on startup."
  (let ((buf (generate-new-buffer "*Start Page*")))
    (with-current-buffer buf
      ;; Prepare buffer for widgets
      (erase-buffer)
      (remove-overlays)
      (projectile-mode 1)
      (unless projectile-known-projects
        (projectile-load-known-projects))
      (widget-insert "📁 **Recent Projects:**\n")
      (widget-insert "========================\n\n")
      ;; Create a button for each project (limit to 5 recent ones)
      (if (and projectile-known-projects (> (length projectile-known-projects) 0))
          (dolist (proj (cl-subseq projectile-known-projects 
                                   0 (min 10 (length projectile-known-projects))))
            (let ((proj-name (file-name-nondirectory (directory-file-name proj))))
              (widget-create 'push-button
                             :tag proj-name             ; Displayed name
                             :value proj                ; Store full path
                             :notify (lambda (widget &rest _ignore)
                                       ;; Action: Open the project
                                       (let ((proj-path (widget-value widget)))
                                         (let ((default-directory proj-path))
                                           (funcall projectile-switch-project-action)))))
              (widget-insert "\n")))
        (widget-insert "No recent projects found.\n"))
      (widget-insert "\n\n")
      (when (display-graphic-p)
        (let ((img (create-image (concat data-directory "images/splash.svg")
                                 'png nil :ascent 'center)))
          (when img
            (insert-image img)
            (insert "\n\n"))))

      ;; ASCII Logo for Terminal Users
      (unless (display-graphic-p)
        (Widget-insert "EMACS\n\n\n"))

      ;; Finalize widget setup
      (use-local-map widget-keymap)            ; use widget keymap for Tab/RET behavior
      (widget-setup)
      (goto-char (point-min)))
    buf))  ; return the buffer

;; Use the custom dashboard at startup (if no file is opened)
(setq inhibit-startup-screen t
      initial-buffer-choice #'my/startup-projects-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)
(set-face-font 'default "FiraCode Nerd Font Mono Light 14")
(setq default-frame-alist
      (append (list '(width  . 72) '(height . 40)
                    '(vertical-scroll-bars . nil)
                    '(internal-border-width . 24)
                    ;;'(undecorated-round . t)
                    '(font . "FiraCode Nerd Font Mono Light 14"))))
(set-frame-parameter (selected-frame)
                     'internal-border-width 24)

(setq ns-use-srgb-colorspace nil)

(global-prettify-symbols-mode +1)
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; Line spacing, can be 0 for code and 1 or 2 for text
(setq-default line-spacing 0)

;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

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
 '(tab-bar-tab ((t (:background "blue" :foreground "#ffffff" :box t)))))
 ;; active tab

;; Change width threshold for splitting vertically
(setq-default split-width-threshold 125)
(setq-default split-height-threshold 100)
;; Rebalance windows everytime you split or close
(dolist (fn '(split-window-right split-window-below delete-window))
  (advice-add fn :after #'balance-windows))

(setq-default
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t)

(blink-cursor-mode 0)

;; Relative numbers
;; Set the line numbers type to relative
(setq display-line-numbers-type 'relative)
(global-hl-line-mode)
(global-display-line-numbers-mode 'relative)

;; Show trailing white space
(setq-default show-trailing-whitespace t)

(add-to-list 'display-buffer-alist
            '("\\*xref\\*"
                (display-buffer-in-side-window)
                (side . right)))

(add-to-list 'display-buffer-alist
            '("\\*compilation\\*"
                (display-buffer-at-bottom)
                (window-height . 0.25)))

(add-to-list 'display-buffer-alist
             '("\\*ag search.*\\*"
               (display-buffer-at-bottom)
               (window-height . 0.25)))

(defun my-close-window-on-kill ()
  "Close the window when killing an `ag` search or `*compilation*` buffer."
  (when (or (string-match-p "\\*ag search.*\\*" (buffer-name))
            (string-match-p "\\*compilation\\*" (buffer-name)))
    (delete-window)))

(add-hook 'kill-buffer-hook 'my-close-window-on-kill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(load-theme 'modus-operandi :no-confirm)
(use-package catppuccin-theme
  :ensure t
  :init
  ;; Load the Catppuccin theme (without prompt). 
  ;; This will apply the default flavor (Mocha) initially.
  (setq catppuccin-flavor 'frappe)
  (load-theme 'catppuccin :no-confirm))

(use-package auto-dark
  :ensure t
  :custom
  ;; Tell auto-dark to use Catppuccin for both dark and light modes
  (auto-dark-themes '((catppuccin) (catppuccin)))
  :config
  ;; When OS switches to dark mode:
  (add-hook 'auto-dark-dark-mode-hook
            (lambda ()
              (setq catppuccin-flavor 'frappe)   ;; use Mocha (dark variant)
              (catppuccin-reload)))
  ;; When OS switches to light mode:
  (add-hook 'auto-dark-light-mode-hook
            (lambda ()
              (setq catppuccin-flavor 'latte)   ;; use Latte (light variant)
              (catppuccin-reload)))
  ;; Start auto-dark-mode to begin monitoring system appearance
  (auto-dark-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MODELINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun active-buffer-icon ()
  "Returns active icon based on the buffer type"
(let* ((extension (file-name-extension (or (buffer-file-name) ""))))
    (cond
      ((string-match-p (regexp-quote "*toggle-term-vterm*") (buffer-name)) (propertize " ≥ " 'face '(:background "black" :foreground "white" :height 220)))
     ((string-equal extension "cpp") (propertize " ☲ " 'face '(:background "NavajoWhite2" :foreground "black" :height 220)))
     ((string-equal extension "h") (propertize " ∴ " 'face '(:background "SeaGreen1" :foreground "black" :height 220)))
     (t (propertize " ☰ " 'face '(:background "HotPink2" :foreground "black" :height 220))))))

(defun inactive-buffer-icon ()
  "Returns active icon based on the buffer type"
(let* ((extension (file-name-extension (or (buffer-file-name) ""))))
    (cond
      ((string-match-p (regexp-quote "*toggle-term-vterm*") (buffer-name)) (propertize " ≥ " 'face '(:background "black" :foreground "white" :height 220)))
     ((string-equal extension "cpp") (propertize " ☲ " 'face '(:background "gray" :foreground "white" :height 220)))
     ((string-equal extension "h") (propertize " ∴ " 'face '(:background "gray" :foreground "white" :height 220)))
     (t (propertize " ☰ " 'face '(:background "gray" :foreground "white" :height 220))))))

(defun buffer-icon ()
  (if (eq (current-buffer) (window-buffer))
      (active-buffer-icon)
    (inactive-buffer-icon)))

(defun ml-fill-to-right (reserve face)
  "Return empty space, leaving RESERVE space on the right."
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin)
                                             ,reserve)))
              'face face))

(defun ml-render (left right &optional fill-face)
  (concat left
          (ml-fill-to-right (string-width (format-mode-line right)) fill-face)
          right))

(defun ml-left ()
  (concat
          (eval (buffer-icon))
          " "
          (format "%s" (buffer-name))))

(defun ml-right ()
  (concat
    (format "%d" (line-number-at-pos))
    ":"
    (format "%d" (current-column))
    " "))

(setq-default header-line-format
  `((:eval (ml-render (ml-left) (ml-right)))))

(defun update-header-line ()
  "Update the header-line to reflect the current state."
  (force-mode-line-update t))
(add-hook 'post-command-hook 'update-header-line)

(setq-default window-divider-default-bottom-width 3)
(setq-default window-divider-default-places 'bottom-only)
(window-divider-mode 1)
(setq-default mode-line-format nil)

;; Set the header line face to match the background color
(set-face-attribute 'header-line nil
                    :background (face-attribute 'default :background)
                    :foreground (face-attribute 'default :foreground)
                    :box nil)

;; Set the color for the line numbers
(set-face-attribute 'line-number nil
                    :foreground (face-attribute 'default :foreground)
                    :background (face-attribute 'default :background))

(set-face-attribute 'fringe nil
                    :foreground (face-attribute 'default :foreground)
                    :background (face-attribute 'default :background))

(set-face-attribute 'mode-line nil
                    :box nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("98b4ef49c451350c28a8c20c35c4d2def5d0b8e5abbc962da498c423598a1cdd"
     default))
 '(package-selected-packages nil))

