;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; activate debugging, similar to emacs --debug-init
(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

;; determine system type and emacs version
(setq *cygwin* (eq system-type 'cygwin))
(setq *linux*  (eq system-type 'gnu/linux))
(if (< emacs-major-version 24)
    (error "Emacs version should be >= 24.4")
  (when (< emacs-minor-version 4)
    (error "Emacs version should be >= 24.4")))
(unless (or *cygwin* *linux*)
  (error "Only support cygwin or gnu/linux system"))

;; get environment variables and paths
(setq gtd-root (getenv "GTD_ROOT"))
(setq dropbox-root (getenv "DROPBOX_ROOT"))
(setq midnight-root (getenv "MIDNIGHT_ROOT"))
(setq cygwin-root (getenv "CYGWIN_ROOT"))
(setq auctex-root (locate-library "auctex"))
(when auctex-root
  (setq auctex-root (file-name-directory auctex-root)))

;; load Org-mode from source when the ORG_HOME environment variable is set
(when (getenv "ORG_HOME")
  (let ((org-lisp-dir (expand-file-name "lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (require 'org))))

;; load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq starter-kit-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; only load org-mode later if we didn't load it just now
    ,(unless (and (getenv "ORG_HOME")
                  (file-directory-p
                   (expand-file-name "lisp" (getenv "ORG_HOME"))))
       '(require 'org))
    ;; load user settings org file which ofen contains user' private data
    (when (and
           gtd-root
           (file-exists-p (concat gtd-root "/source/user-settings.org")))
      (org-babel-load-file (concat gtd-root "/source/user-settings.org")))
    ;; load up the starter kit.
    ;; As the initial value of `org-babel-load-languages' is '(emacs-lisp . t),
    ;; only the emacs-lisp code block will be loaded.
    (org-babel-load-file (expand-file-name "starter-kit.org" starter-kit-dir))))

;;; init.el ends here
