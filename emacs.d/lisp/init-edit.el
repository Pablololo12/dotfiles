(require 'init-elpa)

;; Fuzzy search
(require-package 'ivy)
(require-package 'counsel)
(require-package 'ag)
(ivy-mode 1)

;; project.el
(defun before-switch-project-hook ()
  "Removes other windows when openning a new project"
  (delete-other-windows))
(setq projectile-before-switch-project-hook '(before-switch-project-hook))

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
(setq evil-search-module 'evil-search)
(require 'evil)
(evil-mode 1)

;; Personal settings
;; Auto reload buffers on file change
(global-auto-revert-mode t)

;; org options
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
			                 (python . t)
                             (rust . t)))

(require-package 'ob-rust)

;; Imenu to jump to functions in code
(setq-default imenu-auto-rescan t)
(setq-default imenu-auto-rescan-maxout 1200000)
(global-set-key (kbd "M-i") 'imenu)

;; Open other file in split window
(global-set-key (kbd "M-o") 'ff-find-other-file-other-window)

;; Set 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-set-style "k&r")
(setq c-basic-offset 4)

;; Open pane with ToDo
(defun open-todo ()
  "Opens a pane with the ToDo file"
  (interactive)
  (select-window (split-window (frame-root-window) nil 'below))
  (find-file "~/OneDrive - Arm/Documents/Notes/todo.org"))
;; TODO check env to find Todo
(defun toggle-todo ()
  "Toggles the todo"
  (interactive)
  (let ((win nil))
  (walk-windows (lambda (window)
                  (select-window window)
                  (if (get-file-buffer "todo.org")
                          (setq win window))))
  (if win
        (delete-window win)
        (open-todo))))

(global-set-key (kbd "M-t") 'toggle-todo)

(provide 'init-edit)
