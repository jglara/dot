;; ~/.emacs.d/init.el
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'core-packages)
(require 'core-projectile)
(require 'core-lsp)
(require 'core-build)

(require 'lang-cpp)
(require 'lang-cuda)
(require 'lang-python)
(require 'lang-rust)

;; Small, useful defaults
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-display-line-numbers-mode 1)

;; Terminal-friendly theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))


;; Terminal-friendly: keep ESC responsive
(setq tty-escape-delay 0)

;; Better clipboard integration in terminals that support it
(setq select-enable-clipboard t)

;; Don’t rely on GUI features
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Mouse works in terminal (optional)
(xterm-mouse-mode 1)

;; Avoid weird Meta key delays in some terminals
(setq xterm-extra-capabilities '(modifyOtherKeys))
