
;; include this in your .emacs file

(setq ulisp-term-buffer-name "/dev/ttyACM0")

(defun ulisp-eval-last-expression-in-term ()
  (interactive)
  (let ((expr (buffer-substring-no-properties  
                     (save-excursion (backward-sexp) (point))
                     (point))))
      (with-current-buffer ulisp-term-buffer-name
          (insert expr)
          (term-send-input))))

(global-set-key (kbd "C-x e") 'ulisp-eval-last-expression-in-term)

(defun ulisp-setup-workspace ()
  (interactive)

  (split-window-right)
  (other-window 1)

  (serial-term ulisp-term-buffer-name 9600)
  (term-line-mode)

  (other-window 1)
  (find-file "ulisp-project.lisp"))
