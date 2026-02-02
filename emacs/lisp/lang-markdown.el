;; ~/.emacs.d/lisp/lang-markdown.el
(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :init
  (setq markdown-fontify-code-blocks-natively t))

(use-package plantuml-mode
  :mode ("\\.puml\\'" "\\.plantuml\\'" "\\.pu\\'")
  :init
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-executable-path "plantuml"))

(provide 'lang-markdown)
