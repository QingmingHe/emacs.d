(starter-kit-install-if-needed 'yasnippet)

;; If `yasnippet-bundle' has previously been installed through ELPA,
;; delete it before installing the new `yasnippet'
(let ((yas-bundle-desc (assq 'yasnippet-bundle package-alist)))
  (when yas-bundle-desc
    (package-delete "yasnippet-bundle"
                    (package-version-join
                     (package-desc-vers (cdr yas-bundle-desc))))))

(add-to-list 'load-path
             (expand-file-name  "yasnippet"
                                (expand-file-name "src"
                                                  starter-kit-dir)))

(require 'yasnippet)

(yas/load-directory (expand-file-name "snippets" starter-kit-dir))

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(defun yas/org-setup ()
  ;; yasnippet (using the new org-cycle hooks)
  (make-variable-buffer-local 'yas/trigger-key)
  (setq yas/trigger-key [tab])
  (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
  (define-key yas/keymap [tab] 'yas/next-field))

(add-hook 'org-mode-hook #'yas/org-setup)
