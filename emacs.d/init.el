(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-elpa)
(require 'init-edit)
(require 'init-addon)
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
   '(nord-theme esup doom-modeline org-bullets all-the-icons atom-one-dark-theme elfeed evil projectile counsel ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
