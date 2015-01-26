(setq starter-kit-required-pkgs
      `(
        ;; the basics
        cl
        saveplace
        ffap
        uniquify
        ansi-color
        recentf
        popup
        ido
        smex
        dash
        find-file
        ,(unless *terminal*
           'pos-tip)

        server
        package
        gnus
        flycheck
        flycheck-fortran-gfortran-zh
        smartparens
        smartparens-config
        rainbow-delimiters
        flx-ido
        dired
        dired-x
        ibuffer-vc
        ;; dired+ is required in starter-kit-misc.org for that something
        ;; should be set before loading dired+
        bookmark+
        linum-relative
        undo-tree
        expand-region
        window-numbering
        yasnippet
        goto-chg
        etags-select
        imenu
        imenu+
        ace-jump-mode
        git-rebase-mode
        git-commit-mode
        magit
        dictionary
        no-word
        remember
        quickrun
        diminish
        ,(when (executable-find "sdcv")
           'sdcv)

        ;; eshell, term
        em-cmpl
        em-prompt
        em-term
        term
        multi-term
        multi-eshell

        ;; my own package for handling projects, including project.el,
        ;; project-root.el, etags-update.el
        project
        ;; downloaded somewhere
        taglist

        ;; color theme, font ...
        color-theme
        highlight-indentation
        ;; powerline is loaded after setting color-theme, otherwise some
        ;; errors occur

        ;; auto complete
        auto-complete
        auto-complete-config
        ,(when (executable-find "clang")
           'auto-complete-clang)
        ac-c-headers
        ;; I've done some hack on etags
        ,(when (executable-find "ctags")
           'auto-complete-etags)
        ,(when (executable-find "gtags")
           'auto-complete-gtags)
        auto-complete-pcomplete
        auto-complete-cmake
        ring
        epc
        python-environment
        jedi

        ;; helm
        helm
        helm-config
        helm-gtags

        ;; prog mode
        cmake-mode
        cython-mode
        clojure-mode
        graphviz-dot-mode
        f90-interface-browser
        clojure-mode
        csv-mode
        gnuplot
        rnc-mode
	vimrc-mode

        ;; docs modes
        markdown-mode
        markdown-mode+
        rst

        ;;TODO auctex
        cdlatex

        ;; org is loaded in init.el or starter-kit-org.org
        ;; w3m is loaded optionally in starter-kit-w3m.org
        ;; evil and evil-* are handled in starter-kit-evil.org
        ))
(setq starter-kit-required-pkgs (remove nil starter-kit-required-pkgs))


;; require all pkgs
(mapcar (lambda (pkg)
          (require pkg))
        starter-kit-required-pkgs)


(defun my-gen-Cask (&optional cask-file)
  "Generate Cask from starter-kit-required-pkgs."
  (interactive)
  (unless cask-file
    (setq cask-file (format "%s%s"
                            (ido-read-directory-name "Directory: " "~/.emacs.d/")
                            (read-string "Cask file: " "Cask")))
    (when (file-exists-p cask-file)
      (unless (yes-or-no-p (format "%s exists. Overwrite it? " cask-file))
        (setq cask-file nil))))
  (when cask-file
    (with-temp-buffer
      (insert "(source gnu)\n(source melpa)\n\n")
      (mapcar (lambda (pkg)
                (insert (format "(depends-on \"%s\")\n" (symbol-name pkg))))
              starter-kit-required-pkgs)
      (write-file cask-file))))
