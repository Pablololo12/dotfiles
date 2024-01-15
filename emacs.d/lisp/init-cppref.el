
(defun parse-url (web)
  (interactive "P")
  (select-window
    (display-buffer-in-side-window
        (save-window-excursion
          (eww web)
          (current-buffer))
        '((side . left)))))

(defun search-cpp-ref (args)
  "Search in cppreference for the indicated term"
  (interactive "P")
  (let ((search (read-string "Search: ")))
   ;;(my-render-url
    (parse-url
      (shell-command-to-string
         (concat
          "curl -s 'https://duckduckgo.com/?sites=cppreference.com&q=\\"
          search
          "' | sed -n 's/.*2Fen\\([^&]*\\).*/https:\\/\\/en\\1/gp' | sed -n 's/%2F/\\//gp'")))))


(global-set-key (kbd "C-c }") #'search-cpp-ref)
(global-set-key (kbd "C-c {") #'window-toggle-side-windows)

(provide 'init-cppref)
