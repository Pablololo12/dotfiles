(require 'init-elpa)

;; Feed
(unless (package-installed-p 'elfeed)
  (package-install 'elfeed))
(require 'elfeed)
(global-set-key (kbd "C-x w") 'elfeed)
(setq elfeed-feeds
      '("https://hnrss.org/frontpage"
      "https://gizmodo.com/rss"))
;; disable evil mode for elfeed
(add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
(add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)

(provide 'init-addon)
