;; ~/.emacs.d/lisp/core-lsp.el
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  ;; performance-ish knobs
  (setq lsp-idle-delay 0.35)
  (setq lsp-log-io nil)
  (setq lsp-enable-snippet t)
  (setq lsp-enable-file-watchers t)
  (setq lsp-file-watch-threshold 2000)
  (setq lsp-file-watch-ignored-directories
        (append (and (boundp 'lsp-file-watch-ignored-directories)
                     lsp-file-watch-ignored-directories)
                '("[/\\\\]build\\'"
                  "[/\\\\]\\.venv\\'"
                  "[/\\\\]target\\'")))
  ;; keep it sane with big repos
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  :hook
  ((c-mode c++-mode cuda-mode rust-mode python-mode python-ts-mode) . lsp-deferred)
  :commands (lsp lsp-deferred))

(with-eval-after-load 'lsp-mode
  ;; Avoid noisy startup errors when semgrep CLI is not installed.
  (add-to-list 'lsp-disabled-clients 'semgrep-ls)
  (add-to-list 'lsp-disabled-clients 'semgrep-ls-tramp))

(use-package lsp-ui
  :after lsp-mode
  :init
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-imenu-enable t))

(use-package company
  :config
  (global-company-mode 1))

(use-package yasnippet
  :config (yas-global-mode 1))

(provide 'core-lsp)
