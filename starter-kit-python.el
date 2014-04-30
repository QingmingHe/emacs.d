(starter-kit-install-if-needed 'python-mode 'ipython)

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(add-hook 'python-mode-hook (lambda ()
                              (flyspell-prog-mode)
                              (outline-minor-mode)
                              (setq outline-regexp " *\\(def \\|class \\|if __name__\\)")
                              (hide-body)
                              ))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(when (require 'cython-mode nil 'no-error)
  (add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
  (add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
  (add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode)))

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require 'ipython)

(defun setup-ipython-inferior-shell (&optional oldversion)
  "Setup IPython as inferior python shell.

If OLDVERSION is non-nil, it will setup completion for ipython
0.10 or less (which is currently used in Sagemath)."
  (interactive)
  ;; common values
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args ""
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion")
  ;; completion setup is different for old IPython
  (if oldversion
      (setq python-shell-completion-string-code
            "';'.join(__IP.complete('''%s'''))\n"
            python-shell-completion-module-string-code "")
    (setq python-shell-completion-module-string-code
          "';'.join(module_completion('''%s'''))\n"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

;; Only for Emacs >= 24.3
(when (and (executable-find "ipython") 
           (or (> emacs-major-version 24)
               (and (>= emacs-major-version 24)
                    (>= emacs-minor-version 3))))
           (setup-ipython-inferior-shell))
