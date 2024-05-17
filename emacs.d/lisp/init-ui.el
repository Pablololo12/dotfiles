(require 'init-elpa)

;;(transient-mark-mode 1)

;;(set-face-attribute 'default nil :font "Hack Nerd Font")
(set-face-attribute 'default nil :font "Zed Mono" :height 140 :width 'semi-expanded)
;;(set-face-attribute 'default nil :font "Comic Mono")

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
    '(:eval (let ((remote (file-remote-p default-directory 'host)))
                (cond
                ((null remote) "")
                (t (propertize remote
                        'face 'font-lock-type-face)))))
    ;; spaces to align righT
    '(:eval (propertize
    " " 'display
    `((space :align-to (- (+ right right-fringe right-margin)
                            ,(+ 25 (string-width (if (listp mode-name) (car mode-name) mode-name))))))))
    '(:eval (let ((name (project-current)))
                (cond
                ((null name) "")
                (t (propertize (project-name name)
                        'face 'font-lock-keyword-face)))))
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

;;(require-package 'ef-themes)
;;(setq ef-themes-to-toggle '(ef-cyprus ef-elea-dark))
;;(mapc #'disable-theme custom-enabled-themes)
;;(load-theme 'ef-cyprus :no-confirm)
;;
;;;; Can toggle with modus-themes-toggle for dark mode
;;(define-key global-map (kbd "<f5>") #'ef-themes-toggle)
;;(define-key global-map (kbd "<f6>") #'ef-themes-select)

(require-package 'catppuccin-theme)
(load-theme 'catppuccin :no-confirm)
(setq catppuccin-flavor 'latte) ;; or 'frappe', 'latte, 'macchiato, or 'mocha
(catppuccin-reload)
(define-key global-map (kbd "<f5>") #'catppuccin-load-flavor)

(provide 'init-ui)
