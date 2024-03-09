(require 'init-elpa)

;; Fuzzy search
(require-package 'ivy)
(require-package 'ag)
(ivy-mode)

;; project.el
(bind-key "M-p" project-prefix-map)
(setq project-ignores (list "build"))

;; Evil mode
(require-package 'evil)

(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-search-module 'evil-search)
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
(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))

;; Magit
(require-package 'magit)
;; make magit use same buffer
(setq magit-display-buffer-function (lambda (buffer)
                                    (display-buffer buffer '(display-buffer-same-window))))

(provide 'init-edit)
