;; ~/.emacs.d/init.el
;; Prefer newer .el files over stale .elc files in this dotfiles repo.
(setq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'core-packages)
(require 'core-projectile)
(require 'core-lsp)
(require 'core-build)
(require 'core-code-review)

(require 'lang-cpp)
(require 'lang-cuda)
(require 'lang-python)
(require 'lang-rust)
(require 'lang-markdown)
(require 'lang-yaml)

;; Small, useful defaults
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-display-line-numbers-mode 1)

;; Suppress byte-compilation warnings during startup
(setq byte-compile-warnings nil)
(setq warning-minimum-level :emergency)
(setq y-or-n-p-as-yes-or-no nil)

;; Terminal-friendly theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))


;; Terminal-friendly: keep ESC responsive
(setq tty-escape-delay 0)

;; Better clipboard integration in terminals that support it
(setq select-enable-clipboard t)

;; Don’t rely on GUI features
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Mouse works in terminal (optional)
(xterm-mouse-mode 1)

;; Avoid weird Meta key delays in some terminals
(setq xterm-extra-capabilities '(modifyOtherKeys))


;; --- Clipboard integration for terminal Emacs ---
(defun my/copy-to-clipboard (text &optional push)
  (when text
    (cond
     ((and (getenv "WAYLAND_DISPLAY") (executable-find "wl-copy"))
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "wl-copy")))
     ((executable-find "xclip")
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "xclip" nil nil nil "-selection" "clipboard"))))))

(defun my/paste-from-clipboard ()
  (cond
   ((and (getenv "WAYLAND_DISPLAY") (executable-find "wl-paste"))
    (shell-command-to-string "wl-paste"))
   ((executable-find "xclip")
    (shell-command-to-string "xclip -o -selection clipboard"))
   (t "")))

(setq interprogram-cut-function 'my/copy-to-clipboard)
(setq interprogram-paste-function 'my/paste-from-clipboard)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cmake-mode code-review forge zenburn-theme yasnippet which-key rust-mode ripgrep pyvenv projectile magit lsp-ui lsp-pyright flycheck cuda-mode company clang-format)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
