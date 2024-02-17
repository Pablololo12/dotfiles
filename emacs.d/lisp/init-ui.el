(require 'init-elpa)

;; Open maximized
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;;(set-face-attribute 'default nil :font "DroidSansM Nerd Font Mono")
(set-face-attribute 'default nil :font "Hack Nerd Font")
;; Theme
(require-package 'modus-themes)
(require-package 'catppuccin-theme)
;;(load-theme 'modus-vivendi t)
(load-theme 'catppuccin :no-confirm)
(setq catppuccin-flavor 'frappe) ;; or 'frappe', 'latte, 'macchiato, or 'mocha
(catppuccin-reload)

(set-cursor-color "#cccccc")

;; Change width threshold for splitting vertically
(setq-default split-width-threshold 125)
(setq-default split-height-threshold 100)
;; Rebalance windows everytime you split or close
(dolist (fn '(split-window-right split-window-below delete-window))
  (advice-add fn :after #'balance-windows))
;; Inhibit startup message
(setq inhibit-startup-message t)
;; Disable menu bars and graphic stuff
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode  -1))

(setq-default
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

;; Popup shell
(defun gk-pop-shell (arg)
  "Pop a shell in a side window.
Pass arg to ‘shell’."
  (interactive "P")
  (select-window
   (display-buffer-in-side-window
    (save-window-excursion
      (let ((prefix-arg arg))
        (call-interactively #'eshell))
      (current-buffer))
    '((side . bottom)))))
(global-set-key (kbd "C-c ]") #'gk-pop-shell)
(global-set-key (kbd "C-c [") #'window-toggle-side-windows)

;; Status bar
;; https://github.com/gonsie/dotfiles/blob/main/emacs/theme.el
(setq-default mode-line-format
              (list

               ;; day and time
               '(:eval (propertize (format-time-string " %b %d %H:%M ")
                                   'face 'font-lock-builtin-face))


               '(:eval (propertize (projectile-project-name)
                                   'face 'font-lock-keyword-face))
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b "
                                   'face
                                   (let ((face (buffer-modified-p)))
                                     (if face 'font-lock-warning-face
                                       'font-lock-type-face))
                                   'help-echo (buffer-file-name)))

               ;; line and column
               " (" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%02l" 'face 'font-lock-keyword-face) ","
               (propertize "%02c" 'face 'font-lock-keyword-face)
               ") "

               '(:eval (propertize (file-remote-p default-directory 'host)
                                   'face 'font-lock-type-face))

               ;; spaces to align right
               '(:eval (propertize
                " " 'display
                `((space :align-to (- (+ right right-fringe right-margin)
                                      ,(+ 20 (string-width (if (listp mode-name) (car mode-name) mode-name))))))))

               '(:eval
                (if vc-mode
                    (propertize
                    (let* ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode))
                           (face (cond ((string-match "^ -" noback) 'mode-line-vc)
                            ((string-match "^ [:@]" noback) 'mode-line-vc-edit)
                            ((string-match "^ [!\\?]" noback) 'mode-line-vc-modified))))
                      (format " %s" (substring noback 2)))
                    'face 'font-lock-keyword-face)))
               ;; the current major mode
               (propertize " %m " 'face 'font-lock-string-face)

               ;; relative position, size of file
               " ["
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "

               ))


(set-face-attribute 'mode-line nil
                    :background "#353644"
                    :foreground "white"
                    :box '(:line-width 8 :color "#353644")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#565063"
                    :foreground "white"
                    :box '(:line-width 8 :color "#565063")
                    :overline nil
                    :underline nil)

(provide 'init-ui)
