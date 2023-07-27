(require 'init-elpa)

;; Fuzzy search
(require-package 'ivy)
(require-package 'counsel)
(ivy-mode 1)

;; project.el
(require-package 'projectile)
(require 'projectile)
(projectile-mode +1)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
(setq projectile-globally-ignored-directories '("build"))

;; Evil mode
(require-package 'evil)

(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(require 'evil)
(evil-mode 1)

;; Personal settings
;; Auto reload buffers on file change
(global-auto-revert-mode t)

;; org options
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
			     (python . t)))


;; Imenu to jump to functions in code
(global-set-key (kbd "M-i") 'imenu)

;; Open other file in split window
(global-set-key (kbd "M-o") 'ff-find-other-file-other-window)

;; Set 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-set-style "k&r")
(setq c-basic-offset 4)

(provide 'init-edit)
