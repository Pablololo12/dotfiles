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

(require-package 'denote)
(setq denote-directory (expand-file-name "~/Documents/Notes/"))
(setq denote-save-buffer-after-creation nil)
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-prompts '(title keywords))
(setq denote-rename-no-confirm nil) ; Set to t if you are familiar with `denote-rename-file'
(setq denote-backlinks-show-context t)
(add-hook 'find-file-hook #'denote-link-buttonize-buffer)

(let ((map global-map))
  (define-key map (kbd "C-c n n") #'denote)
  (define-key map (kbd "C-c n c") #'denote-region) ; "contents" mnemonic
  (define-key map (kbd "C-c n N") #'denote-type)
  (define-key map (kbd "C-c n d") #'denote-date)
  (define-key map (kbd "C-c n z") #'denote-signature) ; "zettelkasten" mnemonic
  (define-key map (kbd "C-c n s") #'denote-subdirectory)
  (define-key map (kbd "C-c n t") #'denote-template)
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
  (define-key map (kbd "C-c n I") #'denote-add-links)
  (define-key map (kbd "C-c n b") #'denote-backlinks)
  (define-key map (kbd "C-c n f f") #'denote-find-link)
  (define-key map (kbd "C-c n f b") #'denote-find-backlink)
  ;; Note that `denote-rename-file' can work from any context, not just
  ;; Dired bufffers.  That is why we bind it here to the `global-map'.
  (define-key map (kbd "C-c n r") #'denote-rename-file)
  (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

;; Key bindings specifically for Dired.
(eval-after-load "dired" '(let ((map dired-mode-map))
  (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
  (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-files)
  (define-key map (kbd "C-c C-d C-k") #'denote-dired-rename-marked-files-with-keywords)
  (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter)))

(with-eval-after-load 'org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("n" "New note (with denote.el)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

(defun get-denote-notes ()
    "It returns all the notes under the denote filesystem"
    (cl-remove-if (lambda (name) (string= (substring name 0 1) "."))
    (directory-files denote-directory nil directory-files-no-dot-files-regexp)))

(defun find-note (fun)
    (interactive)
        (let ((dir (ivy-read "Directory: "
                        (get-denote-notes)
                        :re-builder #'ivy--regex
                        :sort nil
                        :initial-input nil)))
        (funcall fun (concat denote-directory dir))))

(global-set-key (kbd "C-c n o") (lambda () (interactive) (find-note 'find-file)))
(global-set-key (kbd "C-c n O") (lambda () (interactive) (find-note 'find-file-other-window)))

(provide 'init-org)
