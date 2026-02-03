;; ~/.emacs.d/lisp/core-code-review.el

(use-package forge
  :after magit)

(use-package code-review
  :after forge
  :config
  ;; Keep review comments readable in terminal widths.
  (setq code-review-fill-column 80))

(provide 'core-code-review)
