;; ~/.emacs.d/lisp/lang-rust.el
(use-package rust-mode
  :mode "\\.rs\\'")

(with-eval-after-load 'lsp-mode
  (setq lsp-rust-analyzer-cargo-watch-command "check")
  (setq lsp-rust-analyzer-server-display-inlay-hints t))

(defun my-rust-format-buffer ()
  (interactive)
  (when (executable-find "rustfmt")
    (rust-format-buffer)))

(add-hook 'rust-mode-hook
          (lambda ()
            (when (bound-and-true-p my-format-on-save)
              (add-hook 'before-save-hook #'my-rust-format-buffer nil t))))

(provide 'lang-rust)
