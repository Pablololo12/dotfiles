(require 'init-elpa)

;; Fuzzy search
(require-package 'ivy)
(require-package 'counsel)
(require-package 'ag)
(ivy-mode 1)

;; project.el
(require 'project)
(bind-key "M-p" project-prefix-map)
(setq project-kill-buffers-display-buffer-list t)
(setq project-ignores (list "build"))

;; Evil mode
(require-package 'evil)

(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-search-module 'evil-search)
(require 'evil)
(evil-mode 1)

;; Haskell
(require-package 'haskell-mode)

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

;; Set 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-set-style "k&r")
(setq c-basic-offset 4)

;; C++ options
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(provide 'init-edit)
