;; ~/.emacs.d/lisp/lang-cuda.el
;; If you want real cuda-mode:
(use-package cuda-mode
  :mode "\\.cu\\'")

;; Many people just use c++-mode for .cu/.cuh:
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))

;; Reuse C++ format on save + clangd LSP
(add-hook 'cuda-mode-hook
          (lambda ()
            (when (bound-and-true-p my-format-on-save)
              (add-hook 'before-save-hook #'my-cpp-format-buffer nil t))))

(provide 'lang-cuda)
