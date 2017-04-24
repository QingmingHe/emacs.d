;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; activate debugging, similar to emacs --debug-init

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

;; determine system type and emacs version
(setq *cygwin* (eq system-type 'cygwin))
(setq *linux*  (eq system-type 'gnu/linux))
(setq *windows*  (eq system-type 'windows-nt))
;; (if (< emacs-major-version 24)
;;     (error "Emacs version should be >= 24.4")
;;   (when (< emacs-minor-version 4)
;;     (error "Emacs version should be >= 24.4")))
;; (unless (or *cygwin* *linux*)
;;   (error "Only support cygwin or gnu/linux system"))

;; get environment variables and paths
(setq gtd-root
      (if (file-directory-p (expand-file-name "gtd" "~"))
          (expand-file-name "gtd" "~")
        (getenv "GTD_ROOT")))
(setq dropbox-root
      (if (file-directory-p (expand-file-name "Dropbox" "~"))
          (expand-file-name "Dropbox" "~")
        (getenv "DROPBOX_ROOT")))
(setq midnight-root (getenv "MIDNIGHT_ROOT"))
(setq cygwin-root (getenv "CYGWIN_ROOT"))
(setq auctex-root (locate-library "auctex"))
(when auctex-root
  (setq auctex-root (file-name-directory auctex-root)))

;; set Fortran checker
(setq flycheck-Fortran-checker
      (or
       (when (and
              (getenv "FLYCHECK_Fortran_CHECKER")
              (string-match "ifort" (getenv "FLYCHECK_Fortran_CHECKER")))
         'fortran-ifort)
       'fortran-gfortran+))
(cond
 ((eq flycheck-Fortran-checker 'fortran-ifort)
  (setq prj/cmake-fortran-compiler "ifort"))
 (t
  (setq prj/cmake-fortran-compiler "gfortran")))

(defun starter-kit-fast-load (&optional file)
  "Load starter kit configuration FILE. A FILE can be file name such as
\"starter-kit-defuns.org\" or base-base configuration name such as \"defuns\"
or nil which means file starter-kit.org. If the corresponding elisp file of
org FILE exists and not older than the org FILE, It will be loaded directly
without loading the org FILE; otherwise the org FILE will be loaded by
`org-babel-load-file'."
  (let* ((last-time (current-time))
         (org-file
          (expand-file-name
           (cond ((not file) "starter-kit.org")
		 ((string-match "starter-kit-.+\.org" file) file)
                 (t (format "starter-kit-%s.org" file)))
           starter-kit-dir))
         (elisp-file
          (expand-file-name
           (format "%s.el" (file-name-base org-file))
           starter-kit-dir)))
    (if (or
         (not (file-exists-p elisp-file))
         (time-less-p
          (nth 5 (file-attributes elisp-file))
          (nth 5 (file-attributes org-file))))
        (progn
          (message "Loading %s ..." org-file)
          (org-babel-load-file org-file))
      (load-file elisp-file))
    (message
     (format "%s consumes %.06f to load."
             (or file "starter-kit.org")
             (float-time (time-since last-time))))))

;; load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq starter-kit-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; load up the starter kit.
    ;; As the initial value of `org-babel-load-languages' is '(emacs-lisp . t),
    ;; only the emacs-lisp code block will be loaded.
    (starter-kit-fast-load)))

;;; init.el ends here
