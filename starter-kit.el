(let ((elisp-dir (expand-file-name "src" starter-kit-dir)))
  ;; add the src directory to the load path
  (add-to-list 'load-path elisp-dir)
  ;; load specific files
  (when (file-exists-p elisp-dir)
    (let ((default-directory elisp-dir))
      (normal-top-level-add-subdirs-to-load-path))))
(setq autoload-file (concat starter-kit-dir "loaddefs.el"))
(setq package-user-dir (concat starter-kit-dir "elpa"))
(setq custom-file (concat starter-kit-dir "custom.el"))

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

(defun starter-kit-loadable-p (package)
  "Check if PACKAGE is loadable from a directory in `load-path'."
  (let ((load-file (concat (symbol-name package) ".el")))
    (catch 'file-found
      (dolist (dir load-path)
        (let ((path (expand-file-name load-file dir)))
          (when (file-exists-p path)
            (throw 'file-found path)))))))

(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("melpa"       . "http://melpa.milkbox.net/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")))
(package-initialize)

(defvar starter-kit-packages nil
  "Libraries that should be installed by default (currently none).")

(defun starter-kit-install-if-needed (&rest packages)
  "Install PACKAGES using ELPA if they are not loadable or installed locally."
  (when packages
    (unless package-archive-contents
      (package-refresh-contents))
    (dolist (package packages)
      (unless (or (starter-kit-loadable-p package)
                  (package-installed-p package))
        (package-install package)))))

(defun starter-kit-load (file &optional header-or-tag)
  "Load configuration from other starter-kit-*.org files.
If the optional argument is the id of a subtree then only
configuration from within that subtree will be loaded.  If it is
not an id then it will be interpreted as a tag, and only subtrees
marked with the given tag will be loaded.

For example, to load all of starter-kit-lisp.org simply
add (starter-kit-load \"lisp\") to your configuration.

To load only the 'window-system' config from
starter-kit-misc-recommended.org add
 (starter-kit-load \"misc-recommended\" \"window-system\")
to your configuration."
  (let ((file (expand-file-name (if (string-match "starter-kit-.+\.org" file)
                                    file
                                  (format "starter-kit-%s.org" file))
                                starter-kit-dir)))
    (org-babel-load-file
     (if header-or-tag
         (let* ((base (file-name-nondirectory file))
                (dir  (file-name-directory file))
                (partial-file (expand-file-name
                               (concat "." (file-name-sans-extension base)
                                       ".part." header-or-tag ".org")
                               dir)))
           (unless (file-exists-p partial-file)
             (with-temp-file partial-file
               (insert
                (with-temp-buffer
                  (insert-file-contents file)
                  (save-excursion
                    (condition-case nil ;; collect as a header
                        (progn
                          (org-link-search (concat"#"header-or-tag))
                          (org-narrow-to-subtree)
                          (buffer-string))
                      (error ;; collect all entries with as tags
                       (let (body)
                         (org-map-entries
                          (lambda ()
                            (save-restriction
                              (org-narrow-to-subtree)
                              (setq body (concat body "\n" (buffer-string)))))
                          header-or-tag)
                         body))))))))
           partial-file)
       file))))

(if (or
    (eq system-type 'darwin)
    (eq system-type 'berkeley-unix))
    (setq system-name (car (split-string system-name "\\."))))

(starter-kit-load "starter-kit-defuns.org")

(starter-kit-load "starter-kit-bindings.org")

(starter-kit-load "starter-kit-misc.org")

(starter-kit-load "starter-kit-registers.org")

(flet ((sk-load (base)
         (let* ((path          (expand-file-name base starter-kit-dir))
                (literate      (concat path ".org"))
                (encrypted-org (concat path ".org.gpg"))
                (plain         (concat path ".el"))
                (encrypted-el  (concat path ".el.gpg")))
           (cond
            ((file-exists-p encrypted-org) (org-babel-load-file encrypted-org))
            ((file-exists-p encrypted-el)  (load encrypted-el))
            ((file-exists-p literate)      (org-babel-load-file literate))
            ((file-exists-p plain)         (load plain)))))
       (remove-extension (name)
         (string-match "\\(.*?\\)\.\\(org\\(\\.el\\)?\\|el\\)\\(\\.gpg\\)?$" name)
         (match-string 1 name)))
  (let ((user-dir (expand-file-name user-login-name starter-kit-dir)))
    ;; load system-specific config
    (sk-load system-name)
    ;; load user-specific config
    (sk-load user-login-name)
    ;; load qmhe's config
    (sk-load "qmhe")
    ;; load any files in the user's directory
    (when (file-exists-p user-dir)
      (add-to-list 'load-path user-dir)
      (mapc #'sk-load
            (remove-duplicates
             (mapcar #'remove-extension
                     (directory-files user-dir t ".*\.\\(org\\|el\\)\\(\\.gpg\\)?$"))
             :test #'string=)))))

(load custom-file 'noerror)
