;;; -*- lexical-binding: t; -*-
(ql:quickload :split-sequence)

(defun help-me ()
  (format t "(read-lines filename &opt func~%")
  (format t "(write-lines filename lines)~%")
  (format t "(memo func &key key #'first test #'eql)"))

(defun read-lines (filename &optional (func (lambda (_) (declare (ignore _)) t)))
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
                if (funcall func line)
                    collect line)))

(defun write-lines (filename lines)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (line lines)
      (write-line line stream))))

;; https://github.com/norvig/paip-lisp/blob/main/docs/chapter9.md
;; (funcall (memo #'func) ...)
(defun utils-memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'utils-memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

;; (memoize 'func)
(defun utils-memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (setf (symbol-function fn-name) (utils-memo (symbol-function fn-name))))

(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(utils-memoize (defun ,fn ,args . ,body)))
