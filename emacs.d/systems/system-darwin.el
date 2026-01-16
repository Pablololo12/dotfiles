;;; -*- lexical-binding: t; -*-

;; Specific config for macos with homebrew

(setenv "PATH" (concat "/opt/homebrew/bin:/opt/homebrew/sbin:" (getenv "PATH")))
(setq exec-path (split-string (getenv "PATH") path-separator))


(provide 'system-darwin)
