(defface dired-tar-mode-line-fail
  '((default :inherit compilation-error)
    (((class color) (min-colors 16)) (:foreground "Red1" :weight bold))
    (((class color) (min-colors 8)) (:foreground "red"))
    (t (:inverse-video t :weight bold)))
  "Face for `dired-tar-mode''s \"error\" mode line indicator.")

(defface dired-tar-mode-line-exit
  '((default :inherit compilation-info)
    (((class color) (min-colors 16))
     (:foreground "ForestGreen" :weight bold))
    (((class color)) (:foreground "green" :weight bold))
    (t (:weight bold)))
  "Face for `dired-tar-mode''s \"exit\" mode line indicator.")

(defface dired-tar-mode-line-run
  '((t :inherit compilation-warning))
  "Face for `dired-tar-mode''s \"running\" mode line indicator.")

(defvar dired-tar-tar-command "tar"
  "Command of tar.")

(defvar dired-tar-buffer-name "*tar*"
  "Buffer name of tar.")

(defvar dired-tar-process-name "dired-tar-process"
  "Buffer name of tar.")

(defvar dired-tar-dired-buffer nil
  "Dired buffer associated with buffer with `dired-tar-buffer-name'.")

(defvar dired-tar-tarball-regex "\\.\\(tar\\|gz\\)$"
  "Regex of tarball.")

(defvar dired-tar-mode-map nil
  "Map for `dired-tar-mode'.")
(let ((map (make-sparse-keymap)))
  (define-key map (kbd "C-c C-c") 'dired-tar-interupt)
  (define-key map (kbd "q") 'quit-window)
  (setq dired-tar-mode-map map))

(defun dired-tar-interupt ()
  (interactive)
  (let ((process (get-process dired-tar-process-name)))
    (when process
      (interrupt-process process))))

(defun dired-tar-revert-dired ()
  (when dired-tar-dired-buffer
    (with-current-buffer dired-tar-dired-buffer
      (revert-buffer))))

(defun dired-tar-sentinel (proc msg)
  (ding)
  (dired-tar-revert-dired)
  (setq dired-tar-dired-buffer nil)
  (setq mode-line-process
        (propertize
         (format ":%s [%s]" (process-status proc) (process-exit-status proc))
         'help-echo msg
         'face (if (> (process-exit-status proc) 0)
                   'dired-tar-mode-line-fail
                 'dired-tar-mode-line-exit))))

(defun dired-untar-file (arg)
  (interactive "P")
  (when (eq major-mode 'dired-mode)
    (setq dired-tar-dired-buffer (current-buffer))
    (let ((tarball (file-name-nondirectory (dired-get-file-for-visit)))
          dir)
      (when (not (string-match dired-tar-tarball-regex tarball))
        (user-error "%s is not a tarball" tarball))
      (when (get-process dired-tar-process-name)
        (user-error "a dired tar process is running"))
      (if arg
          (setq dir (ido-read-directory-name "extract to: "))
        (setq dir default-directory))
      (pop-to-buffer dired-tar-buffer-name)
      (dired-tar-mode)
      (erase-buffer)
      (setq process (start-process
                     dired-tar-process-name
                     dired-tar-buffer-name
                     dired-tar-tar-command
                     "xvf"
                     tarball
                     "-C"
                     dir))
      (setq mode-line-process
            '(:propertize ":%s" face dired-tar-mode-line-run))
      (set-process-sentinel process 'dired-tar-sentinel))))

(defun dired-tar-files (arg)
  (interactive "P")
  (when (eq major-mode 'dired-mode)
    (setq dired-tar-dired-buffer (current-buffer))
    (let ((files (mapcar
                  (lambda (file)
                    (file-name-nondirectory file))
                  (dired-get-marked-files)))
          tarball process)
      (if (or arg (> (length files) 1))
          (setq tarball (read-from-minibuffer "tarball: "))
        (setq tarball (format "%s.tar" (car files))))
      (when (get-process dired-tar-process-name)
        (user-error "a dired tar process is running"))
      (pop-to-buffer dired-tar-buffer-name)
      (dired-tar-mode)
      (erase-buffer)
      (setq process (eval
                     `(start-process
                       dired-tar-process-name
                       dired-tar-buffer-name
                       dired-tar-tar-command
                       "jcvf"
                       tarball
                       ,@files)))
      (setq mode-line-process
            '(:propertize ":%s" face dired-tar-mode-line-run))
      (set-process-sentinel process 'dired-tar-sentinel))))

(defun dired-tar-tar-untar ()
  (interactive)
  (when (eq major-mode 'dired-mode)
    (let ((tarball (file-name-nondirectory (dired-get-file-for-visit))))
      (if (and (= 1 (length (dired-get-marked-files)))
               (string-match dired-tar-tarball-regex tarball))
          (call-interactively 'dired-untar-file)
        (call-interactively 'dired-tar-files)))))

(with-eval-after-load "dired"
  (define-key dired-mode-map (kbd "T") 'dired-tar-tar-untar))

(define-derived-mode dired-tar-mode fundamental-mode "dired-tar"
  "A major mode to run tar."
  (use-local-map dired-tar-mode-map))

(provide 'dired-tar)
