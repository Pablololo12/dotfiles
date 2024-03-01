(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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
        use-file-dialog nil ;; don't use system file dialog
        tab-bar-new-button-show nil ;; don't show new tab button
        tab-bar-close-button-show nil ;; don't show tab close button
        tab-line-close-button-show nil) ;; don't show tab close button

(require 'init-elpa)
(require 'init-edit)
(require 'init-others)
(require 'init-ui)
(require 'init-org)
(require 'init-cppref)

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8" "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18" "2cc1b50120c0d608cc5064eb187bcc22c50390eb091fddfa920bf2639112adb6" "fc608d4c9f476ad1da7f07f7d19cc392ec0fb61f77f7236f2b6b42ae95801a62" default))
 '(org-agenda-files nil)
 '(package-selected-packages
   '(magit catppuccin-theme nord-theme esup doom-modeline org-bullets all-the-icons atom-one-dark-theme elfeed evil projectile counsel ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
