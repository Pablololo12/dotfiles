(require 'init-elpa)

;; Org options
(require-package 'org-bullets)

;; org options
(with-eval-after-load 'org
    (require 'org-bullets)
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    (setq org-babel-python-command "python3")
    (org-babel-do-load-languages
        'org-babel-load-languages '((C . t)
                                    (python . t)))
  )

(provide 'init-org)
