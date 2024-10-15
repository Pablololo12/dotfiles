(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/13:/opt/homebrew/opt/libgccjit/lib/gcc/13:/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin23/13")

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

(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin:/usr/local/bin"))
(add-to-list 'exec-path "/usr/local/bin/")
(add-to-list 'exec-path "/opt/homebrew/bin/")

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
 '(package-selected-packages
   '(projectile ivy-posframe ef-themes org-bullets modus-themes magit ivy haskell-mode evil denote ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
