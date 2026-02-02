;; ~/.emacs.d/lisp/core-packages.el
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

;; Basics you'll likely want
(use-package which-key
  :config (which-key-mode 1))

(use-package magit)

(use-package flycheck)

(provide 'core-packages)
