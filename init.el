;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; activate debugging
;; (setq debug-on-error t
;;       debug-on-signal nil
;;       debug-on-quit nil)

;; load my own functions
(load-file "~/.emacs.d/my-functions.el")

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *win32* (string-match "win32" (my-system-type)))
(setq *cygwin* (string-match "cygwin" (my-system-type)))
(setq *linux* (string-match "linux" (my-system-type)))
(setq *terminal* (eq window-system nil))

;; get root of Cygwin in view of Windows
(if *cygwin*
    (if (getenv "CYGWIN_ROOT")
        (setq cygwin-root (getenv "CYGWIN_ROOT"))
      (setq cygwin-root "D:/cygwin")))

;; add path of org-8
;;(add-to-list 'load-path "~/.emacs.d/src/org-mode/lisp")
;;(add-to-list 'load-path "~/.emacs.d/src/org-mode/contrib/lisp" t)

;; archives for install packages such as org, jedi, ...
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
;; Never install org from package archives, but from Git version
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; enable auto revert-buffer
(global-auto-revert-mode 1)

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
                  (file-directory-p (expand-file-name "lisp"
                                                      (getenv "ORG_HOME"))))
       '(require 'org))
    ;; load up the starter kit
    (org-babel-load-file (expand-file-name "starter-kit.org" starter-kit-dir))))

;; Generate a temporary buffer for scratch
(generate-new-buffer "temporary")
(put 'erase-buffer 'disabled nil)

;;; init.el ends here
