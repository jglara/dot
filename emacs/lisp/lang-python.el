;; ~/.emacs.d/lisp/lang-python.el
(use-package python
  :ensure nil
  :init
  (setq python-indent-offset 4))

(use-package pyvenv)

(defun my-project-root ()
  (or (ignore-errors (projectile-project-root))
      (when-let ((proj (project-current))) (car (project-roots proj)))
      default-directory))

(defun my-uv-auto-activate-venv ()
  "If PROJECT/.venv exists, activate it (pyvenv) and set python shell."
  (let* ((root (file-truename (my-project-root)))
         (venv (expand-file-name ".venv" root)))
    (when (file-directory-p venv)
      (pyvenv-activate venv)
      ;; ensure subprocesses use it
      (setq-local python-shell-interpreter (expand-file-name "bin/python" venv)))))

(add-hook 'python-mode-hook #'my-uv-auto-activate-venv)

;; LSP: pyright
(with-eval-after-load 'lsp-mode
  (setq lsp-pyright-python-executable-cmd "python")
  (setq lsp-pyright-typechecking-mode "basic"))

(use-package lsp-pyright
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(provide 'lang-python)
