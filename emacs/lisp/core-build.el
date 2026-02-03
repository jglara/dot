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

(defun my-dev--root-file (name &optional root)
  "Return absolute path to NAME at ROOT (or project root), or nil."
  (let ((path (expand-file-name name (file-truename (or root (my-dev-project-root))))))
    (when (file-exists-p path) path)))

(defun my-dev--cmake-root ()
  "Return nearest directory containing CMake preset files."
  (file-truename
   (or (locate-dominating-file default-directory "CMakePresets.json")
       (locate-dominating-file default-directory "CMakeUserPresets.json")
       (my-dev-project-root))))

(defun my-dev--json-file (file)
  "Parse FILE and return a plist/alist, or nil on parse errors."
  (when (and file (file-readable-p file))
    (or
     (condition-case nil
         (json-parse-file file :object-type 'plist :array-type 'list)
       (error nil))
     (condition-case nil
         (let ((json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'string))
           (json-read-file file))
       (error nil)))))

(defun my-dev--obj-get (obj key)
  "Get KEY from OBJ, tolerant of plist/alist/hash and key type."
  (let* ((ks (cond ((keywordp key) (substring (symbol-name key) 1))
                   ((symbolp key) (symbol-name key))
                   (t key)))
         (ksym (and (stringp ks) (intern ks)))
         (kkey (and (stringp ks) (intern (concat ":" ks)))))
    (cond
     ((hash-table-p obj)
      (or (gethash key obj)
          (and ksym (gethash ksym obj))
          (and (stringp ks) (gethash ks obj))
          (and kkey (gethash kkey obj))))
     ((and (listp obj) (consp (car obj)))
      (or (cdr (assoc key obj))
          (and ksym (cdr (assoc ksym obj)))
          (and (stringp ks) (cdr (assoc ks obj)))
          (and kkey (cdr (assoc kkey obj)))))
     ((listp obj)
      (or (plist-get obj key)
          (and ksym (plist-get obj ksym))
          (and kkey (plist-get obj kkey))))
     (t nil))))

(defun my-dev--preset-names (key)
  "Return names from KEY preset array in CMakePresets.json or CMakeUserPresets.json."
  (let* ((root (my-dev--cmake-root))
         (json-a (my-dev--json-file (my-dev--root-file "CMakePresets.json" root)))
         (json-b (my-dev--json-file (my-dev--root-file "CMakeUserPresets.json" root)))
         (presets-a (my-dev--obj-get json-a key))
         (presets-b (my-dev--obj-get json-b key))
         (presets (append presets-a presets-b)))
    (delq nil (mapcar (lambda (p) (my-dev--obj-get p :name)) presets))))

(defun my-dev--read-preset (key prompt)
  "Prompt for preset under KEY using PROMPT."
  (let ((choices (my-dev--preset-names key)))
    (if choices
        (completing-read prompt choices nil t nil nil (car choices))
      (let* ((root (my-dev--cmake-root))
             (a (my-dev--root-file "CMakePresets.json" root))
             (b (my-dev--root-file "CMakeUserPresets.json" root)))
        (cond
         ((not (or a b))
          (user-error "No CMake presets files found (looked in %s)" root))
         (t
          (user-error "No presets found for %s in %s" key root)))))))

(defun my-dev-cmake-configure ()
  "Run `cmake --preset ...' from project root."
  (interactive)
  (let ((default-directory (my-dev--cmake-root))
        (preset (my-dev--read-preset :configurePresets "Configure preset: ")))
    (compile (format "cmake --preset %s" preset))))

(defun my-dev-cmake-build ()
  "Run `cmake --build --preset ...' from project root."
  (interactive)
  (let* ((default-directory (my-dev--cmake-root))
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
  (let* ((default-directory (my-dev--cmake-root))
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
  "Run `uv sync` from the nearest pyproject.toml."
  (interactive)
  (let ((default-directory (file-truename (my-dev--uv-project-root t))))
    (compile "uv sync")))

(defun my-dev-uv-test ()
  "Run `uv run -m pytest` from the nearest pyproject.toml."
  (interactive)
  (let ((default-directory (file-truename (my-dev--uv-project-root t))))
    (compile "uv run -m pytest")))

(defun my-dev-uv-run (cmd)
  "Run arbitrary CMD under uv from the nearest pyproject.toml."
  (interactive "suv run ...: ")
  (let ((default-directory (file-truename (my-dev--uv-project-root t))))
    (compile (format "uv run %s" cmd))))

(defun my-dev--uv-project-root (&optional prompt)
  "Return directory containing pyproject.toml.

If PROMPT is non-nil and none is found, prompt for a path."
  (let ((found (locate-dominating-file default-directory "pyproject.toml")))
    (cond
     (found (file-truename found))
     (prompt
      (let* ((start (file-truename (my-dev-project-root)))
             (file (read-file-name "pyproject.toml: " start nil t "pyproject.toml")))
        (unless (and (stringp file) (file-exists-p file))
          (user-error "No pyproject.toml selected"))
        (file-name-directory (file-truename file))))
     (t nil))))

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
