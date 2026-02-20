;; ~/.emacs.d/lisp/lang-cpp.el
(use-package cc-mode
  :ensure nil
  :init
  (setq c-basic-offset 4)
  (setq c-default-style "linux"))

;; CMake files
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; Formatting
(use-package clang-format)

(defun my-cpp-format-buffer ()
  "Format current buffer with clang-format if available."
  (interactive)
  (when (executable-find "clang-format")
    (clang-format-buffer)))

(add-hook 'c++-mode-hook
          (lambda ()
            ;; common: format on save (toggle per project via dir-locals)
            (when (bound-and-true-p my-format-on-save)
              (add-hook 'before-save-hook #'my-cpp-format-buffer nil t))))

;; clangd settings for lsp-mode
(with-eval-after-load 'lsp-clangd
  ;; Normalize accidental "clangd <args...>" executable strings.
  (when (and (stringp lsp-clients-clangd-executable)
             (string-match-p "[[:space:]]" lsp-clients-clangd-executable))
    (let ((parts (split-string-and-unquote lsp-clients-clangd-executable)))
      (setq lsp-clients-clangd-executable (car parts))
      (setq lsp-clients-clangd-args
            (append (cdr parts) lsp-clients-clangd-args))))
  (setq lsp-clients-clangd-executable "clangd")
  (setq lsp-clients-clangd-args
        '("--header-insertion=never"
          "--clang-tidy"
          "--completion-style=detailed")))

(with-eval-after-load 'lsp-mode
  (require 'lsp-clangd))

(provide 'lang-cpp)
