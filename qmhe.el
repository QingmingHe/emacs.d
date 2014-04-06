;; activate python configuration of starter-kit
(starter-kit-load "python")

;; activate org configuration of starter-kit
(starter-kit-load "org")

;; activate eshell configuration of starter-kit
;; This cause error
;; (starter-kit-load "eshell")

;; activate lisp configuration of starter-kit
(starter-kit-load "lisp")

;; activate misc-recommended configuration of starter-kit
(starter-kit-load "misc-recommended")

;; activate Yasnippet configuration of starter-kit
(starter-kit-load "yasnippet")

;-------------------------------------------------------------------------------
;; smartpatens
;-------------------------------------------------------------------------------
(defun my-smartparens-config ()
  (require 'smartparens-config)
  (smartparens-mode 1)
  (show-smartparens-global-mode t)
  )
  
;-------------------------------------------------------------------------------
;; aspell
;-------------------------------------------------------------------------------
(custom-set-variables
    '(ispell-dictionary "british")
    '(ispell-program-name "aspell"))
  
;-------------------------------------------------------------------------------
;; color theme
;-------------------------------------------------------------------------------
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-blue)

;-------------------------------------------------------------------------------
;; evil mode
;-------------------------------------------------------------------------------
(require 'evil)
(evil-mode 1)
(require 'goto-chg)

;-------------------------------------------------------------------------------
;; rst mode
;-------------------------------------------------------------------------------
(add-hook 'rst-mode-hook 'flyspell-mode)
(add-hook 'rst-mode-hook 'column-number-mode)
(add-hook 'rst-mode-hook (lambda () (my-smartparens-config)))
(add-hook 'rst-mode-hook 'linum-mode)

;-------------------------------------------------------------------------------
;; window numbering
;-------------------------------------------------------------------------------
(require 'window-numbering)
(window-numbering-mode 1)

;-------------------------------------------------------------------------------
;; auctex
;-------------------------------------------------------------------------------
(setq Tex-auto-save t)
(setq Tex-parse-self t)
(setq-default Tex-master nil)
(add-hook 'Latex-mode-hook 'visual-line-mode)
(add-hook 'Latex-mode-hook 'flyspell-mode)
(add-hook 'Latex-mode-hook 'Latex-math-mode)
(add-hook 'Latex-mode-hook 'true-on-reftex)
(add-hook 'Latex-mode-hook (lambda () (my-smartparens-config)))
(add-hook 'Latex-mode-hook 'linum-mode)
(setq reftex-plug-into-AUCTeX t)
;; Use evince to view PDF files. However, inverse search is not supported.
;; Note that Evence is opened in preview mode, to maxmize, M-SPACE, X and
;; M-<F4> to close
(setq TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-start-server t)
(setq TeX-view-program-list '(("Evince" "evince --preview --page-index=%(outpage) %o")))
(setq TeX-view-program-selection '((output-pdf "Evince")))
;; set XeTeX mode in TeX/LaTeX
(add-hook 'LaTeX-mode-hook 
    (lambda()
        (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --shell-escape --synctex=1 %(mode)%'  %t" TeX-run-TeX nil t))
        (setq TeX-command-default "XeLaTeX")
        (setq TeX-save-query nil)
        (setq TeX-show-compilation t)))

;-------------------------------------------------------------------------------
;; python
;-------------------------------------------------------------------------------
(add-hook 'python-mode-hook (lambda () (my-smartparens-config)))
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook 'linum-mode)
;; use jedi for completion
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;-------------------------------------------------------------------------------
;; emacs lisp
;-------------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook (lambda () (my-smartparens-config)))
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)

;-------------------------------------------------------------------------------
;; Set 78 column rule. 78 not 80 for that two spaces are spared to fill
;; continuation symbol
;-------------------------------------------------------------------------------
(setq default-fill-column 78)

;-------------------------------------------------------------------------------
;; smex
;-------------------------------------------------------------------------------
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;-------------------------------------------------------------------------------
;; w3m
;-------------------------------------------------------------------------------
(require 'w3m-load)
(setq w3m-command "/usr/bin/w3m")
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies t)
(setq w3m-home-page "https://www.google.com.hk")

;-------------------------------------------------------------------------------
;; company mode
;-------------------------------------------------------------------------------
(require 'company)
(setq company-idle-delay t)
(dolist (hook (list
               'fortran-mode-hook
               'emacs-lisp-mode-hook
               'lisp-mode-hook
               'lisp-interaction-mode-hook
               'scheme-mode-hook
               'c-mode-common-hook
;;               'python-mode-hook
               'haskell-mode-hook
               'asm-mode-hook
               'emms-tag-editor-mode-hook
               'sh-mode-hook))
(add-hook hook 'company-mode))

;-------------------------------------------------------------------------------
;; multi-eshell and multi-term
;-------------------------------------------------------------------------------
(require 'multi-eshell)
(require 'multi-term)
(setq multi-term-program "/usr/bin/bash")

;-------------------------------------------------------------------------------
;; archives for package install
;-------------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)
