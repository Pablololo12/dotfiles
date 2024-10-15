(require 'init-elpa)

;;(setq dark1 "SkyBlue3")
;;(setq dark2 "light steel blue")
;;(setq dark3 "gray81")
;;(setq lineback "gray32")
;;(setq mode-normal "deep sky blue")
;;(setq mode-insert "SeaGreen1")
;;(setq mode-visual "sienna1")

(setq dark1 "snow1")
(setq dark2 "snow3")
(setq dark3 "snow4")
(setq lineback "gray32")
(setq mode-normal "deep sky blue")
(setq mode-insert "SeaGreen1")
(setq mode-visual "sienna1")

(defface mode-line-modified-face
  `((t (:foreground "red" :background ,dark1)))
  "Face for the modified buffer indicator in the mode-line.")

(defface mode-line-unmodified-face
  `((t (:foreground "green" :background ,dark1)))
  "Face for the unmodified buffer indicator in the mode-line.")

(defface mode-line-filename-face
  `((t (:foreground "black" :background ,dark1 :weight bold)))
  "Face for the filename in the mode-line.")

(defface mode-line-position-face
  `((t (:foreground "black" :background ,dark2 :weight bold)))
  "Face for the line position")

(defface mode-line-host-face
  `((t (:foreground "black" :background ,dark3 :weight bold)))
  "Face for the line position")

(defface mode-line-background-face
  `((t (:foreground "white" :background ,lineback :weight bold)))
  "Face for the line position")

(defface mode-line-project-face
  `((t (:foreground "black" :background ,dark2 :weight bold)))
  "Face for the line position")

(defface evil-normal-state-tag-face
  `((t (:background ,mode-normal :foreground "black" :weight bold)))
  "Face for Evil Normal state tag in mode line.")

(defface evil-insert-state-tag-face
  `((t (:background ,mode-insert :foreground "black" :weight bold)))
  "Face for Evil Insert state tag in mode line.")

(defface evil-visual-state-tag-face
  `((t (:background ,mode-visual :foreground "black" :weight bold)))
  "Face for Evil Visual state tag in mode line.")

(defface mode-normal-word-separator
  `((t (:foreground ,mode-normal :background ,dark1)))
  "Normal separator")

(defface mode-insert-word-separator
  `((t (:foreground ,mode-insert :background ,dark1)))
  "Normal separator")

(defface mode-visual-word-separator
  `((t (:foreground ,mode-visual :background ,dark1)))
  "Normal separator")

(defface file-position-separator
  `((t (:foreground ,dark1 :background ,dark2)))
  "separator")

(defface position-host-separator
  `((t (:foreground ,dark2 :background ,dark3)))
  "separator")

(defface host-bar-separator
  `((t (:foreground ,dark3 :background ,lineback)))
  "separator")

(defface position-bar-separator
  `((t (:foreground ,dark2 :background ,lineback)))
  "separator")

(defface bar-project-separator
  `((t (:foreground ,dark2 :background ,lineback)))
  "separator")

(defface project-size-separator
  `((t (:foreground ,dark1 :background ,dark2)))
  "separator")

(setq mode-line-right-align-edge 'window)
;; Custom faces for mode-line
(custom-set-faces
 '(mode-line ((t (:background nil :overline nil :box nil))))
 `(mode-line-active ((t (:background ,lineback :overline nil :box nil))))
 '(mode-line-inactive ((t (:background nil :overline nil :box nil))))
)

(setq-default mode-line-format
  (list
    '(:eval (cond
       (( eq evil-state 'visual) (list (propertize " Visual " 'face 'evil-visual-state-tag-face) (propertize " " 'face 'mode-visual-word-separator)))
       (( eq evil-state 'normal) (list (propertize " Normal " 'face 'evil-normal-state-tag-face) (propertize " " 'face 'mode-normal-word-separator)))
       (( eq evil-state 'insert) (list (propertize " Insert " 'face 'evil-insert-state-tag-face) (propertize " " 'face 'mode-insert-word-separator)))
       (t (list (propertize " ● " 'face 'evil-visual-state-tag-face) (propertize " " 'face 'mode-visual-word-separator)))))

   '(:eval (propertize "%b "
                       'face 'mode-line-filename-face
                       'help-echo (buffer-file-name)))

   '(:eval (propertize " ● "
                       'face (if (buffer-modified-p)
                                 'mode-line-modified-face
                               'mode-line-unmodified-face)))
   (propertize " " 'face 'file-position-separator)

   '(:eval (propertize "(%02l,%02c) " 'face 'mode-line-position-face))

   '(:eval (let ((remote (file-remote-p default-directory 'host)))
             (cond
              ((null remote) (propertize "" 'face 'position-bar-separator))
              (t (list (propertize " " 'face 'position-host-separator)(propertize remote 'face 'mode-line-host-face) (propertize "" 'face 'host-bar-separator))))))

   'mode-line-format-right-align
   ;; the current major mode
   (propertize " %m " 'face 'mode-line-background-face)

   '(:eval (if vc-mode
               (propertize
                (let* ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode))
                       (face (cond ((string-match "^ -" noback) 'mode-line-vc)
                                   ((string-match "^ [:@]" noback) 'mode-line-vc-edit)
                                   ((string-match "^ [!\\?]" noback) 'mode-line-vc-modified))))
                  (format " %s" (substring noback 2)))
                'face 'mode-line-background-face)))

   (propertize " " 'face 'bar-project-separator)
   '(:eval (let ((name (project-current)))
             (cond
              ((null name) (propertize "No Project" 'face mode-line-position-face))
              (t (propertize (project-name name)
                             'face 'mode-line-position-face)))))
   (propertize " " 'face 'project-size-separator)
   ;; relative position, size of file
   (propertize " [%I] " 'face 'mode-line-filename-face) ;; size
   ))

(provide 'init-modeline)
