(require 'init-elpa)

(require 'org)

;; org options
(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
			                 (python . t)))

(provide 'init-org)
