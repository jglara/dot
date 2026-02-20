;; ~/.emacs.d/lisp/lang-markdown.el
(autoload 'markdown-mode "markdown-mode" "Major mode for Markdown files." t)
(add-to-list 'auto-mode-alist '("\\.qmd\\'" . markdown-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'" "\\.qmd\\'")
  :bind (:map markdown-mode-map
              ("C-c q n" . my-quarto-new-slide-file)
              ("C-c q s" . my-quarto-insert-slide-template)
              ("C-c q r" . my-quarto-render-buffer))
  :init
  (setq markdown-fontify-code-blocks-natively t))

(defun my-quarto--default-title ()
  "Return a default title inferred from current buffer file name."
  (if-let ((file buffer-file-name))
      (capitalize (file-name-base file))
    "Talk Title"))

(defun my-quarto-insert-slide-template (title)
  "Insert a Quarto reveal.js slide template using TITLE."
  (interactive (list (read-string "Slide title: " (my-quarto--default-title))))
  (insert
   (format
    (concat "---\n"
            "title: \"%s\"\n"
            "format:\n"
            "  revealjs:\n"
            "    slide-number: true\n"
            "    chalkboard: false\n"
            "---\n\n"
            "# Intro\n\n"
            "## Agenda\n\n"
            "- Point 1\n"
            "- Point 2\n")
    title)))

(defun my-quarto-new-slide-file (file title)
  "Create FILE as a Quarto slide deck and insert a starter template."
  (interactive
   (let* ((default-name (concat (format-time-string "%Y-%m-%d") "-slides.qmd"))
          (file (read-file-name "New Quarto slide file: "
                                default-directory default-name nil default-name)))
     (list file (read-string "Slide title: "
                             (capitalize (file-name-base file))))))
  (find-file file)
  (when (= (point-min) (point-max))
    (my-quarto-insert-slide-template title)
    (save-buffer)))

(defun my-quarto-render-buffer ()
  "Render current Quarto file using `quarto render`."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (unless (string-match-p "\\.qmd\\'" buffer-file-name)
    (user-error "Current file is not a .qmd Quarto file"))
  (unless (executable-find "quarto")
    (user-error "Cannot find `quarto` in PATH"))
  (when (buffer-modified-p)
    (save-buffer))
  (let ((default-directory (file-name-directory buffer-file-name)))
    (compile (format "quarto render %s"
                     (shell-quote-argument
                      (file-name-nondirectory buffer-file-name))))))

(use-package plantuml-mode
  :mode ("\\.puml\\'" "\\.plantuml\\'" "\\.pu\\'")
  :init
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-executable-path "plantuml"))

(provide 'lang-markdown)
