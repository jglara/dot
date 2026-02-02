;; ~/.emacs.d/lisp/core-build.el
(require 'cl-lib)
(require 'compile)
(require 'json)
(require 'subr-x)

(defun my-dev-project-root ()
  "Return current project root, falling back to `default-directory'."
  (or (ignore-errors (projectile-project-root))
      (when-let ((proj (project-current))) (car (project-roots proj)))
      default-directory))

(defun my-dev--root-file (name)
  "Return absolute path to NAME at project root, or nil."
  (let ((path (expand-file-name name (file-truename (my-dev-project-root)))))
    (when (file-exists-p path) path)))

(defun my-dev--json-file (file)
  "Parse FILE and return a plist, or nil on parse errors."
  (when (and file (file-readable-p file))
    (condition-case nil
        (json-parse-file file :object-type 'plist :array-type 'list)
      (error nil))))

(defun my-dev--preset-names (key)
  "Return names from KEY preset array in CMakePresets.json."
  (let* ((json (my-dev--json-file (my-dev--root-file "CMakePresets.json")))
         (presets (plist-get json key)))
    (delq nil (mapcar (lambda (p) (plist-get p :name)) presets))))

(defun my-dev--read-preset (key prompt)
  "Prompt for preset under KEY using PROMPT."
  (let ((choices (my-dev--preset-names key)))
    (if choices
        (completing-read prompt choices nil t nil nil (car choices))
      (user-error "No presets found for %s" key))))

(defun my-dev-cmake-configure ()
  "Run `cmake --preset ...' from project root."
  (interactive)
  (let ((default-directory (file-truename (my-dev-project-root)))
        (preset (my-dev--read-preset :configurePresets "Configure preset: ")))
    (compile (format "cmake --preset %s" preset))))

(defun my-dev-cmake-build ()
  "Run `cmake --build --preset ...' from project root."
  (interactive)
  (let* ((default-directory (file-truename (my-dev-project-root)))
         (build-presets (my-dev--preset-names :buildPresets))
         (cmd
          (if build-presets
              (format "cmake --build --preset %s"
                      (completing-read "Build preset: " build-presets nil t nil nil
                                       (car build-presets)))
            (let ((configure (my-dev--read-preset :configurePresets "Configure preset: ")))
              (format "cmake --build build/%s" configure)))))
    (compile cmd)))

(defun my-dev-cmake-test ()
  "Run `ctest --preset ...' from project root."
  (interactive)
  (let* ((default-directory (file-truename (my-dev-project-root)))
         (test-presets (my-dev--preset-names :testPresets))
         (cmd
          (if test-presets
              (format "ctest --preset %s"
                      (completing-read "Test preset: " test-presets nil t nil nil
                                       (car test-presets)))
            "ctest --test-dir build")))
    (compile cmd)))

(defun my-dev-cargo-check ()
  "Run `cargo check` from project root."
  (interactive)
  (let ((default-directory (file-truename (my-dev-project-root))))
    (compile "cargo check")))

(defun my-dev-cargo-test ()
  "Run `cargo test` from project root."
  (interactive)
  (let ((default-directory (file-truename (my-dev-project-root))))
    (compile "cargo test")))

(defun my-dev-cargo-clippy ()
  "Run `cargo clippy` from project root."
  (interactive)
  (let ((default-directory (file-truename (my-dev-project-root))))
    (compile "cargo clippy")))

(defun my-dev-uv-sync ()
  "Run `uv sync` from project root."
  (interactive)
  (let ((default-directory (file-truename (my-dev-project-root))))
    (compile "uv sync")))

(defun my-dev-uv-test ()
  "Run `uv run -m pytest` from project root."
  (interactive)
  (let ((default-directory (file-truename (my-dev-project-root))))
    (compile "uv run -m pytest")))

(defun my-dev-uv-run (cmd)
  "Run arbitrary CMD under uv."
  (interactive "suv run ...: ")
  (let ((default-directory (file-truename (my-dev-project-root))))
    (compile (format "uv run %s" cmd))))

(defvar my-dev-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c c") #'my-dev-cmake-configure)
    (define-key map (kbd "c b") #'my-dev-cmake-build)
    (define-key map (kbd "c t") #'my-dev-cmake-test)
    (define-key map (kbd "r c") #'my-dev-cargo-check)
    (define-key map (kbd "r t") #'my-dev-cargo-test)
    (define-key map (kbd "r l") #'my-dev-cargo-clippy)
    (define-key map (kbd "p s") #'my-dev-uv-sync)
    (define-key map (kbd "p t") #'my-dev-uv-test)
    (define-key map (kbd "p r") #'my-dev-uv-run)
    map)
  "Keymap for stack-specific build/test helpers.")

(define-key global-map (kbd "C-c m") my-dev-command-map)

(provide 'core-build)
