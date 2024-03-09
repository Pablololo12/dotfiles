(require 'init-elpa)

;;(transient-mark-mode 1)

(set-face-attribute 'default nil :font "Hack Nerd Font")

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
(display-line-numbers-mode)
(setq-default display-line-numbers 'relative)

;; Show trailing white space
(setq-default show-trailing-whitespace t)

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
               ;; spaces to align righT
               '(:eval (propertize
                " " 'display
                `((space :align-to (- (+ right right-fringe right-margin)
                                      ,(+ 20 (string-width (if (listp mode-name) (car mode-name) mode-name))))))))
               '(:eval (propertize (project-name (project-current))
                                   'face 'font-lock-keyword-face))
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

(defun set-light()
  (interactive)
    (setq modus-themes-common-palette-overrides
          '((bg-mode-line-active bg-term-white-bright)
            (border-mode-line-active bg-term-white-bright)
            (border-mode-line-inactive unspecified)
            ))
    (require-package 'modus-themes)
    (load-theme 'modus-operandi :no-confirm))

(defun set-dark()
  (interactive)
    (set-face-attribute 'mode-line nil
                        :background "#353644"
                        :foreground "black" ;;
                        :box '(:line-width 8 :color "#353644")
                        :overline nil
                        :underline nil)

    (set-face-attribute 'mode-line-inactive nil
                        :background "#565063"
                        :foreground "black"
                        :box '(:line-width 8 :color "#565063")
                        :overline nil
                        :underline nil)
    ;; Theme
    (require-package 'catppuccin-theme)
    (setq catppuccin-flavor 'frappe)
    (load-theme 'catppuccin :no-confirm)) ;; or 'frappe', 'latte, 'macchiato, or 'mocha


(defun set-system-dark-mode ()
  (interactive)
  (if (string= (shell-command-to-string "defaults read -g AppleInterfaceStyle") "Dark\n")
       (set-dark)
    (set-light)))

;; Normally set to light
(set-light)

(provide 'init-ui)
