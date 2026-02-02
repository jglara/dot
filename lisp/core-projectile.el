;; ~/.emacs.d/lisp/core-projectile.el
(use-package projectile
  :init
  (setq projectile-project-search-path '("~/DEV"))
  (setq projectile-completion-system 'auto)
  (setq projectile-enable-caching t)
  :config
  (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Optional but popular with Projectile:
(use-package ripgrep)
;;(use-package counsel :after projectile)   ;; if you use ivy/counsel
;;(use-package ivy :config (ivy-mode 1))    ;; optional UI

