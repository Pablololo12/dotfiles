(require 'init-elpa)

(require 'init-modeline)

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

;; Enables search in terminal
(defun bb/send-C-r () (interactive) (term-send-raw-string "\C-r"))
(defun bb/send-C-p () (interactive) (term-send-raw-string "\C-p"))
(defun bb/send-C-n () (interactive) (term-send-raw-string "\C-n"))

(defun bb/setup-term-mode ()
  (evil-local-set-key 'insert (kbd "C-r") 'bb/send-C-r)
  (evil-local-set-key 'insert (kbd "C-p") 'bb/send-C-p)
  (evil-local-set-key 'insert (kbd "C-n") 'bb/send-C-n)
  )

(add-hook 'term-mode-hook 'bb/setup-term-mode)

;; Theme
(require-package 'ef-themes)
(ef-themes-select 'ef-dream)

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

(provide 'init-ui)
