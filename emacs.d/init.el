;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pabs emacs config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start config and general appeareance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(setq inhibit-splash-screen t ;; no thanks
        use-file-dialog nil) ;; don't use system file dialog
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
;;; General Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fuzzy search
(use-package ivy
  :defer 0.1
  :ensure t
  :diminish
  :custom
    (ivy-count-format "(%d/%d) ")
    (ivy-use-virtual-buffers t)
  :config (ivy-mode))

;; projectile
(use-package projectile
  :ensure t
  :bind
    (("M-p" . 'projectile-command-map))
  :custom
    (projectile-globally-ignored-directories '("build"))
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
    (evil-mode 1))

;; Magit
(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function (lambda (buffer) (display-buffer buffer '(display-buffer-same-window)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language specific config and packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-set-style "k&r")
(setq c-basic-offset 4)

;; C++ options
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))

;; Haskell
(use-package haskell-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tramp config
(setq vc-handled-backends '(Git))
(setq tramp-verbose 1)
(setq auto-revert-remote-files nil)
(setq tramp-use-ssh-controlmaster-options nil)
(setq tramp-auto-save-directory "/tmp")
;; Personal settings
;; Auto reload buffers on file change
(global-auto-revert-mode t)

;; Imenu to jump to functions in code
(setq-default imenu-auto-rescan t)
(setq-default imenu-auto-rescan-maxout 1200000)
(global-set-key (kbd "M-i") 'imenu)

;; Open other file in split window
(global-set-key (kbd "M-o") 'ff-find-other-file-other-window)
(global-set-key (kbd "M-O") 'ff-find-other-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 130 :width 'semi-expanded)

;; tab-bar
(setq-default tab-bar-new-button-show nil ;; don't show new tab button
        tab-bar-close-button-show nil ;; don't show tab close button
        tab-bar-new-tab-choice "*scratch*"
        tab-line-close-button-show nil) ;; don't show tab close button

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

(global-display-line-numbers-mode 'relative)

;; Show trailing white space
(setq-default show-trailing-whitespace t)

;; Theme
(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-dream))

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
;;; MODELINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq dark1 "snow1")
(setq dark2 "snow3")
(setq dark3 "snow4")
(setq lineback "gray32")
(setq mode-normal "deep sky blue")
(setq mode-insert "SeaGreen1")
(setq mode-visual "sienna1")

(defface mode-line-modified-face
  `((t (:foreground "red" :background ,dark1)))
  "Face for the modified buffer indicator in the mode-line.")

(defface mode-line-unmodified-face
  `((t (:foreground "green" :background ,dark1)))
  "Face for the unmodified buffer indicator in the mode-line.")

(defface mode-line-filename-face
  `((t (:foreground "black" :background ,dark1 :weight bold)))
  "Face for the filename in the mode-line.")

(defface mode-line-position-face
  `((t (:foreground "black" :background ,dark2 :weight bold)))
  "Face for the line position")

(defface mode-line-host-face
  `((t (:foreground "black" :background ,dark3 :weight bold)))
  "Face for the line position")

(defface mode-line-background-face
  `((t (:foreground "white" :background ,lineback :weight bold)))
  "Face for the line position")

(defface mode-line-project-face
  `((t (:foreground "black" :background ,dark2 :weight bold)))
  "Face for the line position")

(defface evil-normal-state-tag-face
  `((t (:background ,mode-normal :foreground "black" :weight bold)))
  "Face for Evil Normal state tag in mode line.")

(defface evil-insert-state-tag-face
  `((t (:background ,mode-insert :foreground "black" :weight bold)))
  "Face for Evil Insert state tag in mode line.")

(defface evil-visual-state-tag-face
  `((t (:background ,mode-visual :foreground "black" :weight bold)))
  "Face for Evil Visual state tag in mode line.")

(defface mode-normal-word-separator
  `((t (:foreground ,mode-normal :background ,dark1)))
  "Normal separator")

(defface mode-insert-word-separator
  `((t (:foreground ,mode-insert :background ,dark1)))
  "Normal separator")

(defface mode-visual-word-separator
  `((t (:foreground ,mode-visual :background ,dark1)))
  "Normal separator")

(defface file-position-separator
  `((t (:foreground ,dark1 :background ,dark2)))
  "separator")

(defface position-host-separator
  `((t (:foreground ,dark2 :background ,dark3)))
  "separator")

(defface host-bar-separator
  `((t (:foreground ,dark3 :background ,lineback)))
  "separator")

(defface position-bar-separator
  `((t (:foreground ,dark2 :background ,lineback)))
  "separator")

(defface bar-project-separator
  `((t (:foreground ,dark2 :background ,lineback)))
  "separator")

(defface project-size-separator
  `((t (:foreground ,dark1 :background ,dark2)))
  "separator")

(setq mode-line-right-align-edge 'window)
;; Custom faces for mode-line
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background nil :overline nil :box nil))))
 '(mode-line-active ((t (:background "gray32" :overline nil :box nil))))
 '(mode-line-inactive ((t (:background nil :overline nil :box nil)))))

(setq-default mode-line-format
  (list
    '(:eval (cond
       (( eq evil-state 'visual) (list (propertize " Visual " 'face 'evil-visual-state-tag-face) (propertize " " 'face 'mode-visual-word-separator)))
       (( eq evil-state 'normal) (list (propertize " Normal " 'face 'evil-normal-state-tag-face) (propertize " " 'face 'mode-normal-word-separator)))
       (( eq evil-state 'insert) (list (propertize " Insert " 'face 'evil-insert-state-tag-face) (propertize " " 'face 'mode-insert-word-separator)))
       (t (list (propertize " ● " 'face 'evil-visual-state-tag-face) (propertize " " 'face 'mode-visual-word-separator)))))

   '(:eval (propertize "%b "
                       'face 'mode-line-filename-face
                       'help-echo (buffer-file-name)))

   '(:eval (propertize " ● "
                       'face (if (buffer-modified-p)
                                 'mode-line-modified-face
                               'mode-line-unmodified-face)))
   (propertize " " 'face 'file-position-separator)

   '(:eval (propertize "(%02l,%02c) " 'face 'mode-line-position-face))

   '(:eval (let ((remote (file-remote-p default-directory 'host)))
             (cond
              ((null remote) (propertize "" 'face 'position-bar-separator))
              (t (list (propertize " " 'face 'position-host-separator)(propertize remote 'face 'mode-line-host-face) (propertize "" 'face 'host-bar-separator))))))

   'mode-line-format-right-align
   ;; the current major mode
   (propertize " %m " 'face 'mode-line-background-face)

   '(:eval (if vc-mode
               (propertize
                (let* ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode))
                       (face (cond ((string-match "^ -" noback) 'mode-line-vc)
                                   ((string-match "^ [:@]" noback) 'mode-line-vc-edit)
                                   ((string-match "^ [!\\?]" noback) 'mode-line-vc-modified))))
                  (format " %s" (substring noback 2)))
                'face 'mode-line-background-face)))

   (propertize " " 'face 'bar-project-separator)
   '(:eval (let ((name (project-current)))
             (cond
              ((null name) (propertize "No Project" 'face mode-line-position-face))
              (t (propertize (project-name name)
                             'face 'mode-line-position-face)))))
   (propertize " " 'face 'project-size-separator)
   ;; relative position, size of file
   (propertize " [%I] " 'face 'mode-line-filename-face) ;; size
   ))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
